;;; grease.el --- An oil.nvim-style file manager for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Grease provides a simple, text-based interface for managing files.
;; You edit a directory listing as if it were a normal text file using
;; standard Evil (or Emacs) commands, then commit your changes to the filesystem.
;;
;; Changes are staged until you save with `grease-save` (C-c C-s), or when
;; prompted before actions like visiting a file (`RET`) or quitting (`q`).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Optional support for nerd-icons and evil
(eval-when-compile (require 'nerd-icons nil t))
(eval-when-compile (require 'evil nil t))

(defgroup grease nil
  "Oil.nvim-style writable file manager."
  :group 'files)

;; Enable icons by default if available
(defvar grease-use-icons t
  "Non-nil to display icons next to filenames.")

(defun grease--icons-available-p ()
  "Return non-nil if nerd-icons is available and icons are enabled."
  (and grease-use-icons (featurep 'nerd-icons)))

;;;; Preview Configuration

(defvar grease-preview-window-width 0.4
  "Width of the preview window as a fraction of the frame.")

;; Preview state (buffer-local)
(defvar-local grease--preview-buffer nil
  "Buffer used for file previews.")

(defvar-local grease--preview-window nil
  "Window used for file previews.")

(defvar-local grease--preview-original-buffer nil
  "Buffer that was in the side window before grease took it over.
If non-nil, the window existed before grease and should be restored on close.")

(defvar grease--preview-timer nil
  "Timer for delayed preview updates.")

(defvar grease-preview-writable nil
  "When non-nil, preview buffer for files is writable.
Does not apply to directories.")

;;; Symlink display toggle & faces

(defcustom grease-show-symlink-targets t
  "When non-nil, show resolved symlink targets next to symlink entries.
If nil, symlink entries are shown as regular files/dirs without
the resolved target."
  :type 'boolean
  :group 'grease)

(defface grease-symlink-broken
  '((t :foreground "#ef4444" :background nil))
  "Face for the resolved target of a broken symlink.
Used when the symlink target does not exist on the filesystem."
  :group 'grease)

(defcustom grease-skip-confirm-for-simple-edits nil
  "When non-nil, save simple edits without asking for confirmation.
A simple edit has no deletes, at most five creates, at most one copy,
and at most one move or rename.  Copies, moves, and renames must stay on
the same local/remote filesystem adapter."
  :type 'boolean
  :group 'grease)

;;;; Sorting Configuration

(defvar grease-sort-method 'type
  "Method for sorting files in grease buffer.
Available options:
  - `type': Directories first, then files (default)
  - `name': Alphabetical by name
  - `size': By file size (smallest first)
  - `size-desc': By file size (largest first)
  - `date': By modification date (oldest first)
  - `date-desc': By modification date (newest first)
  - `extension': By file extension")

(defvar grease-sort-directories-first t
  "When non-nil, always show directories before files regardless of sort method.
Only applies when `grease-sort-method' is not `type'.")

(defvar-local grease--current-sort-method nil
  "Buffer-local sort method, overrides `grease-sort-method' when set.")

;;;; Hidden Files Configuration

(defvar grease-show-hidden nil
  "When non-nil, show hidden files (those starting with a dot).
Default is nil (hidden files are not shown).")

(defvar-local grease--current-show-hidden nil
  "Buffer-local setting for showing hidden files.
When nil, uses `grease-show-hidden' as default.")

(defvar-local grease--show-hidden-initialized nil
  "Non-nil once the hidden files setting has been initialized for this buffer.")

;;;; Global State and File Tracking

;; File tracking system - keeps track of all files by ID
(defvar grease--file-registry (make-hash-table :test 'eql)
  "Registry of all files seen during the current session.
Each entry is keyed by unique ID and contains:
/(:path PATH :type TYPE :committed-p BOOL :source-id ID :exists BOOL)
`:exists' is retained temporarily for compatibility with the old diff engine.")

;; Track directories we've visited
(defvar grease--visited-dirs nil
  "List of directories visited in the current session.")

;; Global clipboard for operations
(defvar grease--clipboard nil
  "Global clipboard for cross-directory operations.")

;; Session-wide ID counter to ensure IDs are always unique
(defvar grease--session-id-counter 1
  "Counter for generating unique file IDs within a session.")

;; Track what kind of operation was last performed
(defvar grease--last-op-type nil
  "Type of last yank/delete operation: 'file or 'text.")

;; Track the last kill ring entry from a grease operation
(defvar grease--last-kill-index nil
  "Index of the last kill ring entry from a grease operation.")

;; Track multiple files in visual selection
(defvar grease--multi-line-selection nil
  "Data about currently selected files in visual mode.")

;; Store multi-line selection data during deletion operations
(defvar grease--deleted-selection nil
  "Store multi-line selection data during deletion operations.")

;;;; Buffer-Local State

(defvar-local grease--root-dir nil
  "Current directory being displayed.")

(defvar-local grease--original-state nil
  "Hash table of original filenames -> file types.
Retained temporarily while the old diff engine is being replaced.")

(defvar-local grease--baseline-by-id nil
  "Committed filesystem snapshot keyed by stable file ID.
Each value records `:id', original `:path', `:type', and `:committed-p'.")

(defvar-local grease--buffer-dirty-p nil
  "Non-nil if buffer has unsaved changes.")

(defvar-local grease--change-hook-active nil
  "Flag to prevent recursive change hooks.")

;; List of pending operations to be applied on save
(defvar-local grease--pending-changes nil
  "Snapshots staged while navigating directories in the current buffer.
Each snapshot contains `:root-dir', `:baseline', and desired `:entries'.")

;; last directory and line number visited for cursor position persistence
(defvar grease--project-positions (make-hash-table :test 'equal)
  "Map project root -> plist of (:dir DIR :line LINE).")

;; Prefixes for hidden IDs
(defconst grease--id-prefix "/"
  "Prefix for hidden file IDs.")

;;;; Registry Functions

(defun grease--project-root ()
  "Return the root directory of the current project, or `default-directory`."
  (or
   (when (fboundp 'project-current)
     (when-let ((proj (project-current nil)))
       (if (fboundp 'project-root)
           (project-root proj)
         (car (project-roots proj)))))
   (when (fboundp 'projectile-project-root)
     (ignore-errors (projectile-project-root)))
   default-directory))


(defun grease--project-name ()
  "Return a short name for the current project root."
  (file-name-nondirectory (directory-file-name (grease--project-root))))

(defun grease--register-file (path type &optional id)
  "Register PATH of TYPE in registry and return its ID.
If ID is provided, use that ID instead of generating a new one."
  (let* ((abs-path (expand-file-name path))
         (file-id (or id (cl-incf grease--session-id-counter))))
    ;; Store in registry
    (let ((committed-p (file-exists-p abs-path)))
      (puthash file-id
               (list :path abs-path
                     :type type
                     :committed-p committed-p
                     :source-id nil
                     :exists committed-p)
               grease--file-registry))
    file-id))

(defun grease--refresh-file-registration (id path type)
  "Refresh registry entry ID with current PATH, TYPE, and filesystem existence."
  (when id
    (let ((committed-p (file-exists-p path)))
      (puthash id
               (list :path (expand-file-name path)
                     :type type
                     :committed-p committed-p
                     :source-id nil
                     :exists committed-p)
               grease--file-registry))
    id))

(defun grease--get-file-by-id (id)
  "Get file info for ID from registry."
  (gethash id grease--file-registry))

(defun grease--real-file-id-p (id)
  "Return non-nil if ID represents an existing filesystem entry."
  (let* ((entry (and id (grease--get-file-by-id id)))
         (path (and entry (plist-get entry :path))))
    (and entry
         path
         (plist-get entry :exists)
         (file-exists-p path))))

(defun grease--line-data-real-file-p (data)
  "Return non-nil if DATA represents an existing filesystem entry."
  (let ((id (plist-get data :id))
        (path (plist-get data :full-path)))
    (and data
         (not (plist-get data :is-new))
         (or (grease--real-file-id-p id)
             (and (not id) path (file-exists-p path))))))

(defun grease--line-data-source-kind (data)
  "Return `file' for existing entries and `text' for pending/new entries."
  (if (grease--line-data-real-file-p data) 'file 'text))

(defun grease--get-id-by-path (path)
  "Find the ID of file at PATH in registry, or nil if not found."
  (let ((abs-path (expand-file-name path))
        found-id)
    (maphash (lambda (id data)
               (when (equal (plist-get data :path) abs-path)
                 (setq found-id id)))
             grease--file-registry)
    found-id))

(defun grease--register-directory (dir)
  "Register DIR and all its files in the registry."
  (let ((abs-dir (file-name-as-directory (expand-file-name dir))))
    ;; Add to visited directories if not already there
    (unless (member abs-dir grease--visited-dirs)
      (push abs-dir grease--visited-dirs)

      ;; Register all files in this directory
      (let* ((all-files (directory-files abs-dir nil nil t))
             (files (cl-remove-if (lambda (f) (member f '("." ".."))) all-files)))
        (dolist (file files)
          (let* ((abs-path (expand-file-name file abs-dir))
                 (type (if (file-directory-p abs-path) 'dir 'file))
                 (existing-id (grease--get-id-by-path abs-path)))
            ;; Register new entries and refresh existing entries, since pending
            ;; creates can become real filesystem entries after save.
            (if existing-id
                (grease--refresh-file-registration existing-id abs-path type)
              (grease--register-file abs-path type))))))))

;;;; Core Helpers

(defun grease--is-dir-name (name)
  "Check if NAME represents a directory (ends with '/')."
  (and name (string-suffix-p "/" name)))

(defun grease--strip-trailing-slash (name)
  "Remove trailing slash from NAME if present."
  (if (grease--is-dir-name name)
      (substring name 0 -1)
    name))

(defun grease--normalize-name (name type)
  "Normalize NAME for display based on TYPE."
  (let ((canonical (grease--canonical-entry-name name)))
    (if (eq type 'dir) (concat canonical "/") canonical)))

(defun grease--canonical-entry-name (name)
  "Return structured entry NAME without display-only trailing slashes."
  (replace-regexp-in-string "/+\\'" "" name))

(defun grease--get-icon (name type full-path)
  "Get appropriate icon for NAME of TYPE at FULL-PATH.
For directories, uses folder icon directly to avoid nerd-icons regex
matching bugs (e.g., directory 'gobe' matching 'go' pattern)."
  (if (grease--icons-available-p)
      (if (eq type 'dir)
          ;; Use folder icon directly for directories to avoid regex matching issues
          (cond
           ((file-symlink-p full-path)
            (nerd-icons-codicon "nf-cod-file_symlink_directory"))
           ((file-exists-p (expand-file-name ".git" full-path))
            (nerd-icons-octicon "nf-oct-repo"))
           (t (nerd-icons-sucicon "nf-custom-folder_oct")))
        (nerd-icons-icon-for-file name))
    (if (eq type 'dir) "📁 " "📄 ")))

(defun grease--get-full-path (name)
  "Get the canonical full path for entry NAME in the current directory."
  (expand-file-name (grease--canonical-entry-name name) grease--root-dir))

(defun grease--get-create-path (name type)
  "Get full creation path for NAME and TYPE in current directory.
Directory creation paths keep a trailing slash so `grease--apply-changes'
creates a directory rather than an empty file."
  (let ((path (grease--get-full-path name)))
    (if (eq type 'dir)
        (file-name-as-directory path)
      path)))

(defun grease--format-id (id)
  "Format ID as a 3-digit string with leading zeroes."
  (format "%03d" id))


(defun grease--extract-filename (text)
  "Extract just the filename from TEXT, removing ID, icon, and symlink suffix."
  (let* ((filename
          (if (string-match (concat "^" grease--id-prefix "[0-9]+\\s-+\\(.*\\)$") text)
              (let ((content (match-string 1 text)))
                (if (string-match "\\(?:[^\n[:alnum:]/._+-]\\s-*\\)*\\([[:alnum:]/._+-].*\\)$" content)
                    (match-string 1 content)
                  content))
            (if (string-match "\\(?:[^\n[:alnum:]/._+-]\\s-*\\)*\\([[:alnum:]/._+-].*\\)$" text)
                (match-string 1 text)
              (string-trim text)))))
    (if (string-match "" filename)
        (string-trim-right (substring filename 0 (match-beginning 0)))
      filename)))

(defun grease--extract-id (text)
  "Extract the file ID from TEXT if present."
  (when (string-match (concat "^" grease--id-prefix "\\([0-9]+\\)") text)
    (string-to-number (match-string 1 text))))

(defun grease--add-copy-suffix (filename)
  "Add '-copy' suffix to FILENAME, preserving extension."
  (if (grease--is-dir-name filename)
      (format "%s-copy/" (grease--strip-trailing-slash filename))
    (let ((ext-pos (string-match-p "\\.[^./]+$" filename)))
      (if ext-pos
          ;; File has extension - insert before extension
          (concat (substring filename 0 ext-pos) "-copy" (substring filename ext-pos))
        ;; No extension
        (concat filename "-copy")))))

;;;; Sorting Functions

(defun grease--get-sort-method ()
  "Get the current sort method for the buffer."
  (or grease--current-sort-method grease-sort-method))

(defun grease--file-extension (filename)
  "Extract file extension from FILENAME, or empty string if none."
  (let ((ext (file-name-extension filename)))
    (or ext "")))

(defun grease--sort-files (files dir)
  "Sort FILES list according to current sort method.
DIR is the directory containing the files."
  (let* ((method (grease--get-sort-method))
         (with-attrs (mapcar (lambda (f)
                               (let* ((path (expand-file-name f dir))
                                      (is-dir (file-directory-p path))
                                      (attrs (file-attributes path)))
                                 (list :name f
                                       :path path
                                       :is-dir is-dir
                                       :size (or (file-attribute-size attrs) 0)
                                       :mtime (or (file-attribute-modification-time attrs) 0)
                                       :ext (grease--file-extension f))))
                             files))
         (sorted (grease--sort-by-method with-attrs method)))
    (mapcar (lambda (item) (plist-get item :name)) sorted)))

(defun grease--sort-by-method (items method)
  "Sort ITEMS by METHOD.
ITEMS is a list of plists with :name, :is-dir, :size, :mtime, :ext keys."
  (let ((dirs-first grease-sort-directories-first))
    (pcase method
      ('type
       ;; Directories first, then files, both alphabetically
       (sort items (lambda (a b)
                     (let ((a-dir (plist-get a :is-dir))
                           (b-dir (plist-get b :is-dir)))
                       (cond
                        ((and a-dir (not b-dir)) t)
                        ((and (not a-dir) b-dir) nil)
                        (t (string< (plist-get a :name) (plist-get b :name))))))))
      ('name
       ;; Alphabetical, optionally with dirs first
       (sort items (lambda (a b)
                     (if dirs-first
                         (let ((a-dir (plist-get a :is-dir))
                               (b-dir (plist-get b :is-dir)))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (string< (plist-get a :name) (plist-get b :name)))))
                       (string< (plist-get a :name) (plist-get b :name))))))
      ('size
       ;; Smallest first
       (sort items (lambda (a b)
                     (if dirs-first
                         (let ((a-dir (plist-get a :is-dir))
                               (b-dir (plist-get b :is-dir)))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (< (plist-get a :size) (plist-get b :size)))))
                       (< (plist-get a :size) (plist-get b :size))))))
      ('size-desc
       ;; Largest first
       (sort items (lambda (a b)
                     (if dirs-first
                         (let ((a-dir (plist-get a :is-dir))
                               (b-dir (plist-get b :is-dir)))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (> (plist-get a :size) (plist-get b :size)))))
                       (> (plist-get a :size) (plist-get b :size))))))
      ('date
       ;; Oldest first
       (sort items (lambda (a b)
                     (if dirs-first
                         (let ((a-dir (plist-get a :is-dir))
                               (b-dir (plist-get b :is-dir)))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (time-less-p (plist-get a :mtime) (plist-get b :mtime)))))
                       (time-less-p (plist-get a :mtime) (plist-get b :mtime))))))
      ('date-desc
       ;; Newest first
       (sort items (lambda (a b)
                     (if dirs-first
                         (let ((a-dir (plist-get a :is-dir))
                               (b-dir (plist-get b :is-dir)))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (time-less-p (plist-get b :mtime) (plist-get a :mtime)))))
                       (time-less-p (plist-get b :mtime) (plist-get a :mtime))))))
      ('extension
       ;; By extension, then name
       (sort items (lambda (a b)
                     (if dirs-first
                         (let ((a-dir (plist-get a :is-dir))
                               (b-dir (plist-get b :is-dir)))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (let ((ext-cmp (string< (plist-get a :ext) (plist-get b :ext))))
                                 (if (string= (plist-get a :ext) (plist-get b :ext))
                                     (string< (plist-get a :name) (plist-get b :name))
                                   ext-cmp)))))
                       (let ((ext-cmp (string< (plist-get a :ext) (plist-get b :ext))))
                         (if (string= (plist-get a :ext) (plist-get b :ext))
                             (string< (plist-get a :name) (plist-get b :name))
                           ext-cmp))))))
      (_
       ;; Default to type sort
       (grease--sort-by-method items 'type)))))

;;;; Sorting Commands

(defun grease--sort-and-refresh (method message)
  "Set sort METHOD, refresh buffer, maintain cursor position, and show MESSAGE."
  (let ((current-line (line-number-at-pos)))
    (setq grease--current-sort-method method)
    (grease--render grease--root-dir t)
    (grease--goto-line-clamped current-line)
    (message "%s" message)))

(defun grease-sort-by-type ()
  "Sort grease buffer by type (directories first, then files)."
  (interactive)
  (grease--sort-and-refresh 'type "Sorted by type"))

(defun grease-sort-by-name ()
  "Sort grease buffer alphabetically by name."
  (interactive)
  (grease--sort-and-refresh 'name "Sorted by name"))

(defun grease-sort-by-size ()
  "Sort grease buffer by file size (smallest first)."
  (interactive)
  (grease--sort-and-refresh 'size "Sorted by size (smallest first)"))

(defun grease-sort-by-size-desc ()
  "Sort grease buffer by file size (largest first)."
  (interactive)
  (grease--sort-and-refresh 'size-desc "Sorted by size (largest first)"))

(defun grease-sort-by-date ()
  "Sort grease buffer by modification date (oldest first)."
  (interactive)
  (grease--sort-and-refresh 'date "Sorted by date (oldest first)"))

(defun grease-sort-by-date-desc ()
  "Sort grease buffer by modification date (newest first)."
  (interactive)
  (grease--sort-and-refresh 'date-desc "Sorted by date (newest first)"))

(defun grease-sort-by-extension ()
  "Sort grease buffer by file extension."
  (interactive)
  (grease--sort-and-refresh 'extension "Sorted by extension"))

(defun grease-cycle-sort ()
  "Cycle through sort methods."
  (interactive)
  (let* ((methods '(type name size size-desc date date-desc extension))
         (current (grease--get-sort-method))
         (pos (cl-position current methods))
         (next (nth (mod (1+ (or pos 0)) (length methods)) methods)))
    (grease--sort-and-refresh next (format "Sorted by %s" next))))

;;;; Hidden Files Functions

(defun grease--hidden-file-p (filename)
  "Return non-nil if FILENAME is a hidden file (starts with a dot)."
  (string-prefix-p "." filename))

(defun grease--show-hidden-p ()
  "Return non-nil if hidden files should be shown."
  (if grease--show-hidden-initialized
      grease--current-show-hidden
    grease-show-hidden))

(defun grease--filter-hidden (files)
  "Filter FILES list, removing hidden files if they should not be shown."
  (if (grease--show-hidden-p)
      files
    (cl-remove-if #'grease--hidden-file-p files)))

(defun grease-toggle-hidden ()
  "Toggle display of hidden files (those starting with a dot)."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (setq grease--show-hidden-initialized t)
    (setq grease--current-show-hidden (not grease--current-show-hidden))
    (grease--render grease--root-dir t)
    (grease--goto-line-clamped current-line)
    (message "Hidden files: %s" (if grease--current-show-hidden "shown" "hidden"))))

;;;; Buffer Rendering and Management

(defun grease--insert-entry (id name type &optional source-id is-duplicate)
  "Insert a formatted line for file with ID, NAME and TYPE.
SOURCE-ID is the ID of the source file if this is a copy.
IS-DUPLICATE indicates if this is a copy of another file."
  (let* ((is-dir (eq type 'dir))
         (canonical-name (grease--canonical-entry-name name))
         (display-name (if is-dir (concat canonical-name "/") canonical-name))
         (id-str (grease--format-id id))
         (full-id (concat grease--id-prefix id-str))
         (start (point))
         (full-path (grease--get-full-path canonical-name)))

    ;; Entries introduced in the editor are desired state, not committed
    ;; filesystem state.  Copies get a new ID and retain their source identity.
    (unless (grease--get-file-by-id id)
      (puthash id
               (list :path full-path
                     :type type
                     :committed-p nil
                     :source-id source-id
                     :exists nil)
               grease--file-registry))

    ;; Insert hidden ID - invisible but not read-only
    (insert full-id " ")
    (put-text-property start (point) 'invisible t)
    (put-text-property start (point) 'grease-prefix t)

    ;; Insert icon if enabled
    (when (grease--icons-available-p)
      (let* ((icon-start (point))
             (icon (grease--get-icon display-name type full-path)))
        (insert icon "  ")   ;; added two extra spaces after icon
        (put-text-property icon-start (point) 'grease-icon t)))

    ;; Insert the visible filename
    (let ((name-start (point)))
      (insert display-name)

      ;; Store metadata as text properties on the whole line
      (add-text-properties start (point)
                          (list 'grease-id id
                                'grease-name canonical-name
                                'grease-type type
                                'grease-source-id source-id
                                'grease-is-duplicate is-duplicate
                                'grease-full-path full-path))

      ;; Add face to the name part only
      (put-text-property name-start (point) 'face 'font-lock-function-name-face)

      ;; Optionally append symlink target (unless disabled)
      (when (and grease-show-symlink-targets
                 (file-symlink-p full-path))
        (let* ((link-target (file-symlink-p full-path))
               (resolved (expand-file-name link-target
                                           (file-name-directory full-path)))
               (target-face (if (file-exists-p resolved)
                                'font-lock-comment-face
                              'grease-symlink-broken)))
          (insert " ")
          (let ((mark-start (point)))
            (insert " ")
            (put-text-property mark-start (point) 'face target-face))
          (let ((target-start (point)))
            (insert resolved)
            (put-text-property target-start (point) 'face target-face)))))

    (insert "\n")))


(defun grease--save-position ()
  "Save last visited directory and line for the current project."
  (when grease--root-dir
    (let ((proj (grease--project-root)))
      (puthash proj
               (list :dir grease--root-dir
                     :line (line-number-at-pos))
               grease--project-positions))))


(defun grease--restore-position ()
  "Restore last visited directory and line for the current project."
  (let* ((proj (grease--project-root))
         (state (gethash proj grease--project-positions)))
    (if state
        (progn
          (grease--render (plist-get state :dir) t)
          (goto-char (point-min))
          (forward-line (max 1 (1- (or (plist-get state :line) 1))))
          (grease--constrain-cursor))
      ;; fallback if no saved state
      (goto-char (point-min))
      (forward-line 1)
      (grease--constrain-cursor))))

(defun grease--count-file-lines ()
  "Count the number of file entry lines in the buffer (excluding header and blank)."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1) ; Skip header
    (let ((count 0))
      (while (not (eobp))
        (when (grease--get-line-data)
          (cl-incf count))
        (forward-line 1))
      count)))

(defun grease--goto-line-clamped (target-line)
  "Go to TARGET-LINE, clamped to valid file lines.
Line 1 is the header, so file lines start at 2.
If target exceeds available files, go to last file line."
  (let ((max-file-line (1+ (grease--count-file-lines)))) ; +1 because header is line 1
    (goto-char (point-min))
    (forward-line (1- (max 2 (min target-line max-file-line))))
    (grease--constrain-cursor)))

(defun grease--render (dir &optional keep-changes)
  "Render the contents of DIR into the current buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq buffer-read-only nil)
    (setq grease--root-dir (file-name-as-directory (expand-file-name dir)))
    (grease--register-directory grease--root-dir)
    (erase-buffer)
    (setq grease--original-state (make-hash-table :test 'equal))
    (setq grease--baseline-by-id (make-hash-table :test 'eql))
    (unless keep-changes
      (setq grease--pending-changes nil))
    (setq grease--buffer-dirty-p nil)

    ;; Header line
    (let ((header-start (point)))
      (insert (format " Grease — %s" grease--root-dir))
      (add-text-properties header-start (point)
                           '(read-only t front-sticky nil face mode-line-inactive))
      (insert "\n"))

    ;; Files
    (let* ((all-files (directory-files grease--root-dir nil nil t))
           (files (cl-remove-if (lambda (f) (member f '("." ".."))) all-files))
           (files (grease--filter-hidden files)))
      (dolist (file (grease--sort-files files grease--root-dir))
        (let* ((abs-path (expand-file-name file grease--root-dir))
               (type (if (file-directory-p abs-path) 'dir 'file))
               (existing-id (grease--get-id-by-path abs-path)))
          (progn
            (puthash (grease--normalize-name file type) type grease--original-state)
            (when existing-id
              (grease--refresh-file-registration existing-id abs-path type)
              (puthash existing-id
                       (list :id existing-id
                             :path abs-path
                             :type type
                             :committed-p t)
                       grease--baseline-by-id))
            (grease--insert-entry
             (or existing-id (cl-incf grease--session-id-counter))
             file type nil nil)))))

    ;; Always add one editable line at the end
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'grease-editable t))))


;;;; Cursor Control and Evil Integration

(defun grease--constrain-cursor ()
  "Ensure cursor is positioned after the hidden ID, icon, and trailing space."
  (when (and (derived-mode-p 'grease-mode)
             (> (line-number-at-pos) 1)) ; skip header
    (let ((bol (line-beginning-position))
          (pos (point))
          prefix-end)
      (unless (get-text-property bol 'grease-editable)
        (save-excursion
          ;; skip hidden ID
          (goto-char bol)
          (when (re-search-forward (concat grease--id-prefix "[0-9]+\\s-+") (line-end-position) t)
            (setq prefix-end (point))))

        ;; if we land inside icon+space, jump past it
        (while (and (< (point) (line-end-position))
                    (get-text-property (point) 'grease-icon))
          (forward-char))
        (when (and prefix-end (< prefix-end (point)))
          (setq prefix-end (point)))

        ;; clamp if cursor is left of safe zone
        (when (and prefix-end (< pos prefix-end))
          (goto-char prefix-end))))))

(defun grease--get-line-data (&optional pos)
  "Get file data from current line or at POS."
  (let* ((pos (or pos (point)))
         (line-beg (save-excursion
                     (goto-char pos)
                     (line-beginning-position))))
    (when (> (line-number-at-pos pos) 1)  ; Skip header
      (cond
       ;; Case 1: Line with standard metadata
       ((get-text-property line-beg 'grease-name)
        (list :name (get-text-property line-beg 'grease-name)
              :type (get-text-property line-beg 'grease-type)
              :id (get-text-property line-beg 'grease-id)
              :source-id (get-text-property line-beg 'grease-source-id)
              :is-duplicate (get-text-property line-beg 'grease-is-duplicate)
              :full-path (get-text-property line-beg 'grease-full-path)))

       ;; Case 2: Special editable blank line
       ((get-text-property line-beg 'grease-editable)
        (let* ((line-text (buffer-substring-no-properties line-beg (line-end-position)))
               (filename (string-trim line-text)))
          (when (not (string-empty-p filename))
            (list :name (grease--strip-trailing-slash filename)
                  :type (if (grease--is-dir-name filename) 'dir 'file)
                  :is-new t))))

       ;; Case 3: Plain text line (newly added)
       (t
        (let* ((line-text (buffer-substring-no-properties line-beg (line-end-position)))
               (filename (string-trim line-text)))
          (when (not (string-empty-p filename))
            (list :name (grease--strip-trailing-slash filename)
                  :type (if (grease--is-dir-name filename) 'dir 'file)
                  :is-new t))))))))

;; Core function to detect file vs text operations
(defun grease--mark-as-grease-op ()
  "Mark the current kill ring operation as a grease file operation if appropriate."
  (when (derived-mode-p 'grease-mode)
    (cond
     ;; Multi-line selection in visual mode
     ((and (boundp 'evil-state) 
           (eq evil-state 'visual)
           (memq (evil-visual-type) '(line block)))
      (let* ((beg (line-number-at-pos (region-beginning)))
             (end (line-number-at-pos (region-end)))
             (files '())
             (names '())
             (types '())
             (ids '())
             (source-kinds '())
             (paths '()))
        
        ;; Collect all selected files
        (save-excursion
          (goto-char (region-beginning))
          (while (and (<= (line-number-at-pos) end)
                      (not (eobp)))
            (let ((data (grease--get-line-data)))
              (when data
                (push (plist-get data :name) names)
                (push (plist-get data :type) types)
                (push (plist-get data :id) ids)
                (push (grease--line-data-source-kind data) source-kinds)
                (push (grease--get-full-path (plist-get data :name)) paths)))
            (forward-line 1)))
        
        ;; Store the multi-line selection data if we found files
        (when names
          (setq grease--multi-line-selection
                (list :paths (nreverse paths)
                      :names (nreverse names)
                      :types (nreverse types)
                      :ids (nreverse ids)
                      :source-kinds (nreverse source-kinds)
                      :original-dir grease--root-dir))
          (setq grease--last-op-type 'file)
          (setq grease--last-kill-index 0))))
     
     ;; Single line operation (yy/dd)
     ((let ((data (grease--get-line-data)))
        (when (and data (bolp))
          ;; This is a single file operation
          (setq grease--last-op-type 'file)
          (setq grease--last-kill-index 0)
          ;; Clear multi-selection data
          (setq grease--multi-line-selection nil)
          t)))
     
     ;; Regular text operation
     (t
      (setq grease--last-op-type 'text)
      (setq grease--multi-line-selection nil)))))

;; Evil integration hooks

(defvar grease--pending-cut nil
  "Non-nil when a cut operation is in progress (before evil’s implicit yank).")

(defun grease--on-evil-yank (beg end &rest _)
  "Intercept Evil yanks in grease-mode.
Handles both multi-line visual yanks and single-line `yy` yanks."
  (when (derived-mode-p 'grease-mode)
    (if grease--pending-cut
        ;; Ignore the yank Evil does as part of delete
        (setq grease--pending-cut nil)
      (cond
       ;; Multi-line visual yank
       ((and (boundp 'evil-state)
             (eq evil-state 'visual)
             (memq (evil-visual-type) '(line block)))
        (let ((names '()) (types '()) (ids '()) (source-kinds '()) (paths '()))
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (let ((data (grease--get-line-data)))
                (when data
                  (push (plist-get data :name) names)
                  (push (plist-get data :type) types)
                  (push (plist-get data :id) ids)
                  (push (grease--line-data-source-kind data) source-kinds)
                  (push (grease--get-full-path (plist-get data :name)) paths)))
              (forward-line 1)))
          (when names
            (setq grease--clipboard
                  (list :paths (nreverse paths)
                        :names (nreverse names)
                        :types (nreverse types)
                        :ids (nreverse ids)
                        :source-kinds (nreverse source-kinds)
                        :original-dir grease--root-dir
                        :operation 'copy))
            (setq grease--last-op-type 'file
                  grease--last-kill-index 0
                  grease--multi-line-selection nil)
            (message "Copied %d item%s"
                     (length names) (if (= (length names) 1) "" "s")))))

       ;; Single-line yank (`yy` or evil-yank-line)
       (t
        (let ((data (grease--get-line-data beg)))
          (when data
            (let* ((name (plist-get data :name))
                   (type (plist-get data :type))
                   (path (grease--get-full-path name))
                   (id   (plist-get data :id))
                   (source-kind (grease--line-data-source-kind data)))
              (setq grease--clipboard
                    (list :paths (list path)
                          :names (list name)
                          :types (list type)
                          :ids (list id)
                          :source-kinds (list source-kind)
                          :original-dir grease--root-dir
                          :operation 'copy))
              (setq grease--last-op-type 'file
                    grease--last-kill-index 0
                    grease--multi-line-selection nil)
              (message "Copied file: %s" name)))))))))

(defun grease--before-evil-delete (&rest _)
  "Collect files about to be deleted and stage a CUT clipboard.
Runs BEFORE Evil's delete (which will also yank)."
  (when (derived-mode-p 'grease-mode)
    ;; Tell on-evil-yank to ignore the upcoming yank from delete.
    (setq grease--pending-cut t)

    (cond
     ;; Visual line/block cut of multiple entries
     ((and (boundp 'evil-state)
           (eq evil-state 'visual)
           (memq (evil-visual-type) '(line block)))
      (let ((names '()) (types '()) (ids '()) (source-kinds '()) (paths '()))
        (save-excursion
          (goto-char (region-beginning))
          (let ((end (save-excursion (goto-char (region-end)) (line-end-position))))
            (while (< (point) end)
              (let ((data (grease--get-line-data)))
                (when data
                  (let* ((name (plist-get data :name))
                         (type (plist-get data :type))
                         (id   (plist-get data :id))
                         (path (grease--get-full-path name))
                         (source-kind (grease--line-data-source-kind data)))
                    (push name names)
                    (push type types)
                    (push id ids)
                    (push source-kind source-kinds)
                    (push path paths)
                    )))
              (forward-line 1))))
        (setq grease--clipboard
              (list :paths (nreverse paths)
                    :names (nreverse names)
                    :types (nreverse types)
                    :ids   (nreverse ids)
                    :source-kinds (nreverse source-kinds)
                    :original-dir grease--root-dir
                    :operation 'cut))
        (setq grease--last-op-type 'cut)
        (setq grease--last-kill-index 0)
        (setq grease--buffer-dirty-p t)
        (message "Cut (staged): %d item%s"
                 (length names) (if (= (length names) 1) "" "s"))))

     ;; Single-line cut
     (t
      (let ((data (grease--get-line-data)))
        (when data
          (let* ((name (plist-get data :name))
                 (type (plist-get data :type))
                 (path (grease--get-full-path name))
                 (id   (plist-get data :id))
                 (source-kind (grease--line-data-source-kind data)))
            (setq grease--clipboard
                  (list :paths (list path)
                        :names (list name)
                        :types (list type)
                        :ids   (list id)
                        :source-kinds (list source-kind)
                        :original-dir grease--root-dir
                        :operation 'cut))
            (setq grease--last-op-type 'cut)
            (setq grease--last-kill-index 0)
            (setq grease--buffer-dirty-p t)
            (message "Cut (staged): %s" name))))))))


(defun grease--after-evil-delete (&rest _)
  "Cleanup after delete. Nothing to restore if we ignored the yank."
  (when (derived-mode-p 'grease-mode)
    ;; Safety: ensure the flag isn't left set if Evil didn't yank for some reason.
    (setq grease--pending-cut nil)))

;; Track the last yanked text to handle paste properly
(defvar grease--last-yanked-text nil
  "Last text yanked in grease mode, for paste handling.")

(defun grease--extract-line-info-from-text (text)
  "Extract file info from TEXT yanked from a grease buffer."
  (when (string-match (concat "^\\(" grease--id-prefix "[0-9]+\\)\\s-+\\(.*\\)$") text)
    (let* ((id-str (match-string 1 text))
           (content (match-string 2 text))
           (id (grease--extract-id text))
           (name (grease--extract-filename text))
           (is-dir (grease--is-dir-name name))
           (type (if is-dir 'dir 'file))
           (file-info (when id (grease--get-file-by-id id))))

      ;; If we found an ID and it's in our registry, return info about it
      (when (and id file-info)
        (list :id id
              :name name
              :type type
              :path (plist-get file-info :path))))))

(defun grease--intercept-yanked-text (text)
  (when (derived-mode-p 'grease-mode)
    (setq grease--last-yanked-text text)
    (cond
     ;; CUT operation – use clipboard as truth
     ((eq grease--last-op-type 'cut)
      ;; don’t trust raw text, reinsert from clipboard
      (setq grease--last-yanked-text nil))
     ;; COPY case – your existing logic
     (t
      (let ((info (grease--extract-line-info-from-text text)))
        (when info
          (setq grease--clipboard
                (list :paths (list (plist-get info :path))
                      :names (list (plist-get info :name))
                      :types (list (plist-get info :type))
                      :ids   (list (plist-get info :id))
                      :source-kinds (list (if (grease--real-file-id-p (plist-get info :id))
                                              'file
                                            'text))
                      :original-dir grease--root-dir
                      :operation 'copy))))))))

(defun grease--get-next-id ()
  "Get the next available ID in the buffer."
  (cl-incf grease--session-id-counter))

(defun grease--handle-paste-line (line-text)
  "Process pasted LINE-TEXT to properly format it for grease buffer."
  (when (not (string-empty-p line-text))
    (let* ((clipboard-op (and grease--clipboard (plist-get grease--clipboard :operation)))
           (clipboard-id (and grease--clipboard (car (plist-get grease--clipboard :ids))))
           (clipboard-name (and grease--clipboard (car (plist-get grease--clipboard :names))))
           (clipboard-type (and grease--clipboard (car (plist-get grease--clipboard :types))))
           (id-from-text (grease--extract-id line-text)))

      ;; Clear the current line content before inserting the new entry
      (delete-region (line-beginning-position) (line-end-position))

      (cond
        ;; Case 1: CUT or MOVE operation (clipboard is authoritative)
        ((memq clipboard-op '(cut move))
        ;; Use original ID and metadata from clipboard
        (grease--insert-entry clipboard-id clipboard-name clipboard-type nil nil))

       ;; Case 2: COPY with ID in pasted text
       (id-from-text
        (let* ((file-info (grease--extract-line-info-from-text line-text))
               (source-id (plist-get file-info :id))
               (name (plist-get file-info :name))
               (type (plist-get file-info :type)))
          ;; Existing entries paste as copies. Pending/new entries paste as
          ;; fresh creates, just like typing the filename by hand.
          (if (grease--real-file-id-p source-id)
              (grease--insert-entry (grease--get-next-id) name type source-id t)
            (grease--insert-entry (grease--get-next-id) name type nil nil))))

       ;; Case 3: Plain text (new file)
       (t
        (let* ((file-name (grease--extract-filename line-text))
               (is-dir (grease--is-dir-name file-name))
               (type (if is-dir 'dir 'file)))
          ;; Treat as a new file creation
          (grease--insert-entry (grease--get-next-id)
                                (grease--strip-trailing-slash file-name)
                                type)))))))

(defun grease--after-paste (beg end)
  "Process pasted text between BEG and END."
  (when (derived-mode-p 'grease-mode)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when (not (= (line-number-at-pos) 1)) ; Skip header
          (let ((line-text (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
            (when (not (string-empty-p line-text))
              (grease--handle-paste-line line-text))))
        (forward-line 1)))

    ;; Mark buffer as dirty
    (setq grease--buffer-dirty-p t)))

;; Add hooks for cursor position constraints
(defun grease--setup-cursor-constraints ()
  "Set up hooks to constrain cursor position."
  (add-hook 'post-command-hook #'grease--constrain-cursor nil t))

;; Advice for yank operations
(when (fboundp 'evil-yank-line)
  (advice-add 'evil-yank-line :after #'grease--on-evil-yank))

(when (fboundp 'evil-yank)
  (advice-add 'evil-yank :after #'grease--on-evil-yank))

;; Advice for delete operations  
(when (fboundp 'evil-delete-line)
  (advice-add 'evil-delete-line :before #'grease--before-evil-delete))

(when (fboundp 'evil-delete)
  (advice-add 'evil-delete :before #'grease--before-evil-delete))

;; Add after-delete hooks for multi-line operations
(when (fboundp 'evil-delete-line)
  (advice-add 'evil-delete-line :after #'grease--after-evil-delete))

(when (fboundp 'evil-delete)
  (advice-add 'evil-delete :after #'grease--after-evil-delete))

;; For regular Emacs operations that change the kill ring
(advice-add 'kill-new :after
            (lambda (&rest _)
              (unless (and (derived-mode-p 'grease-mode) 
                           (eq grease--last-op-type 'file))
                (setq grease--last-op-type 'text)
                (setq grease--last-kill-index nil))))

;; Intercept Evil's paste commands
(defun grease--intercept-paste (orig-fun &rest args)
  "Intercept paste commands in `grease-mode`.
  For cuts, bypass the kill-ring completely and insert from
  `grease--clipboard`, since the kill-ring contains raw buffer text
  (including hidden IDs and icons)."
  (if (and (derived-mode-p 'grease-mode)
           grease--clipboard)
      (let ((op (plist-get grease--clipboard :operation)))
        (cond
         ;; Always bypass kill-ring for cuts
         ((eq op 'cut)
          (grease-paste))
         ;; Copies behave like before
         ((and (memq grease--last-op-type '(file copy))
               (eq grease--last-kill-index 0))
          (grease-paste))
         ;; Fallback to normal paste
         (t (apply orig-fun args))))
    ;; Not in grease-mode or no clipboard → normal paste
    (apply orig-fun args)))

;; Apply advice to Evil paste commands
(when (fboundp 'evil-paste-after)
  (advice-add 'evil-paste-after :around #'grease--intercept-paste))

(when (fboundp 'evil-paste-before)
  (advice-add 'evil-paste-before :around #'grease--intercept-paste))

;; Handle rotation of the kill ring
(when (fboundp 'evil-paste-pop)
  (advice-add 'evil-paste-pop :before
              (lambda (&rest _)
                ;; Update our index when user rotates through the kill ring
                (when grease--last-kill-index
                  (setq grease--last-kill-index 
                        (mod (1+ grease--last-kill-index) (length kill-ring)))))))

;; Evil ex-command for save
(when (fboundp 'evil-ex-define-cmd)
  (evil-ex-define-cmd "w[rite]" (lambda ()
                                  (interactive)
                                  (if (derived-mode-p 'grease-mode)
                                      (grease-save)
                                    (call-interactively #'evil-write)))))

;;;; Buffer Change Tracking

(defun grease--update-line-metadata ()
  "Update line metadata based on current visible text."
  (when (derived-mode-p 'grease-mode)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (let* ((line-beg (line-beginning-position))
               (line-end (line-end-position))
               (line-data (grease--get-line-data (point))))
          (when line-data
            (let* ((old-path (get-text-property line-beg 'grease-full-path))
                   (visible-text (buffer-substring-no-properties line-beg line-end))
                   (clean-text (grease--extract-filename visible-text))
                   (is-dir (grease--is-dir-name clean-text))
                   (name (grease--canonical-entry-name clean-text))
                   (type (if is-dir 'dir 'file))
                   (full-path (grease--get-full-path name)))
              (when (get-text-property line-beg 'grease-name)
                (put-text-property line-beg line-end 'grease-name name)
                (put-text-property line-beg line-end 'grease-type type)
                (put-text-property line-beg line-end 'grease-full-path full-path)
                (unless (equal old-path full-path)
                  (message "grease--update-line-metadata: line=%d old=%s new=%s path-changed"
                           (line-number-at-pos) old-path full-path))))))
        (forward-line 1)))))

(defun grease--format-plain-lines ()
  "Format any plain text lines into properly structured entries."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1) ; Skip header
    (while (not (eobp))
      (let* ((line-beg (line-beginning-position))
             (line-end (line-end-position))
             (has-grease-props (get-text-property line-beg 'grease-name))
             (is-editable-line (get-text-property line-beg 'grease-editable)))

        ;; Process non-empty lines without our standard metadata
        (when (and (not has-grease-props)
                   (not (string-empty-p (string-trim
                                         (buffer-substring-no-properties line-beg line-end)))))
          (let* ((line-text (buffer-substring-no-properties line-beg line-end))
                 (file-name (string-trim line-text))
                 (id-from-text (grease--extract-id line-text)))

            (when (not (string-empty-p file-name))
              (let* ((is-dir (grease--is-dir-name file-name))
                     (type (if is-dir 'dir 'file))
                     (next-id (or id-from-text (grease--get-next-id))))

                ;; Delete the line content and insert properly formatted entry
                (delete-region line-beg line-end)
                (goto-char line-beg)

                ;; If line was editable, we add another editable line after this one
                (when is-editable-line
                  (put-text-property line-beg line-beg 'grease-was-editable t))

                ;; Insert the formatted entry
                (grease--insert-entry next-id
                                     (grease--canonical-entry-name file-name)
                                     type)

                ;; Add a new editable line if needed
                (when (get-text-property line-beg 'grease-was-editable)
                  (let ((editable-start (point)))
                    (insert "\n")
                    (put-text-property editable-start (point) 'grease-editable t)))

                (setq grease--buffer-dirty-p t)))))

        (forward-line 1)))))

(defun grease--scan-buffer ()
  "Scan buffer and update file tracking data."
  ;; First ensure all plain text lines are properly formatted
  (grease--format-plain-lines)

  ;; Now update metadata for all lines
  (grease--update-line-metadata)

  (let ((entries '())
        (name-to-entries (make-hash-table :test 'equal))
        (id-to-entries (make-hash-table :test 'eql)))

    ;; Collect all entries
    (save-excursion
      (goto-char (point-min))
      (forward-line 1) ; Skip header
      (while (not (eobp))
        (let* ((line-beg (line-beginning-position))
               (line-data (grease--get-line-data (point))))

          ;; Process the line if it has file data
          (when line-data
            (let* ((name (plist-get line-data :name))
                   (type (plist-get line-data :type))
                   (id (plist-get line-data :id))
                   (source-id (plist-get line-data :source-id))
                   (is-duplicate (plist-get line-data :is-duplicate))
                   (full-path (plist-get line-data :full-path))
                   (is-new (plist-get line-data :is-new))
                   (registry-entry (and id (grease--get-file-by-id id)))
                   (effective-source-id
                    (or source-id
                        (and registry-entry
                             (plist-get registry-entry :source-id))))
                   (source-entry
                    (and effective-source-id
                         (grease--get-file-by-id effective-source-id)))
                   (data (list :name name
                               :path full-path
                               :type type
                               :id id
                               :committed-p (and registry-entry
                                                 (plist-get registry-entry :committed-p))
                               :source-id effective-source-id
                               :source-path (and source-entry
                                                 (plist-get source-entry :path))
                               :source-committed-p
                               (and source-entry
                                    (plist-get source-entry :committed-p))
                               :is-duplicate is-duplicate
                               :full-path full-path
                               :is-new is-new)))

              (push data entries)
              ;; Track by name for duplicate detection
              (push data (gethash name name-to-entries nil))
              ;; Also track by ID for duplicate detection
              (when id
                (push data (gethash id id-to-entries nil))))))

        (forward-line 1)))

    ;; First mark duplicates by name
    (maphash (lambda (name entry-list)
               (when (> (length entry-list) 1)
                 ;; Mark all but the last as duplicates
                 (let ((entries-sorted (sort (copy-sequence entry-list)
                                             (lambda (a b)
                                               (< (or (plist-get a :id) 0)
                                                  (or (plist-get b :id) 0))))))
                   (let ((original (car (last entries-sorted))))
                     (dolist (entry (butlast entries-sorted))
                       (setf (plist-get entry :is-duplicate) t))))))
             name-to-entries)

    ;; Return the collected entries
    (nreverse entries)))

(defun grease--on-change (_beg _end _len)
  "Hook run after buffer changes to mark it dirty."
  (unless grease--change-hook-active
    (let ((grease--change-hook-active t))
      (setq grease--buffer-dirty-p t))))

;;;; Change Calculation (Diff Engine)
(defun grease--relative-path (path)
  "Return PATH relative to `grease--root-dir', or PATH if outside."
  (let ((rel (file-relative-name path grease--root-dir)))
    (if (string-prefix-p "../" rel)
        path
      rel)))

(defun grease--format-change (change)
  "Format CHANGE for display in confirmation prompt."
  (pcase change
    (`(:create ,path)
     (format "  [Create] %s" (grease--relative-path path)))
    (`(:delete ,path)
     (format "  [Delete] %s" (grease--relative-path path)))
    (`(:rename ,old ,new)
     (format "  [Rename] %s -> %s"
             (grease--relative-path old)
             (grease--relative-path new)))
    (`(:move ,src ,dst)
     (format "  [Move]   %s -> %s"
             (grease--relative-path src)
             (grease--relative-path dst)))
    (`(:copy ,src ,dst)
     (format "  [Copy]   %s -> %s"
             (grease--relative-path src)
             (grease--relative-path dst)))))

(defun grease--format-semantic-path (path type)
  "Format PATH relative to the current root, showing a slash for directory TYPE."
  (let ((display-path (grease--relative-path path)))
    (if (eq type 'dir)
        (file-name-as-directory display-path)
      display-path)))

(defun grease--format-semantic-operation (operation)
  "Format semantic OPERATION for a type-aware confirmation prompt."
  (let ((kind (plist-get operation :kind))
        (type (plist-get operation :type))
        (src (plist-get operation :src))
        (dst (plist-get operation :dst)))
    (pcase kind
      ('create
       (format "  [Create] %s" (grease--format-semantic-path dst type)))
      ('delete
       (format "  [Delete] %s" (grease--format-semantic-path src type)))
      ('relocate
       (format (if (equal (file-name-directory src)
                          (file-name-directory dst))
                   "  [Rename] %s -> %s"
                 "  [Move]   %s -> %s")
               (grease--format-semantic-path src type)
               (grease--format-semantic-path dst type)))
      ('copy
       (format "  [Copy]   %s -> %s"
               (grease--format-semantic-path src type)
               (grease--format-semantic-path dst type)))
      (_ (format "  [Unknown] %S" operation)))))

(defun grease--same-filesystem-adapter-p (src dst)
  "Return non-nil when SRC and DST use the same local/remote adapter."
  (equal (file-remote-p src) (file-remote-p dst)))

(defun grease--simple-edit-p (changes)
  "Return non-nil if CHANGES are simple enough to skip confirmation."
  (let ((num-create 0)
        (num-copy 0)
        (num-move 0)
        (simple t))
    (dolist (change changes)
      (pcase change
        (`(:delete ,_path)
         (setq simple nil))
        (`(:create ,_path)
         (cl-incf num-create))
        (`(:copy ,src ,dst)
         (cl-incf num-copy)
         (unless (grease--same-filesystem-adapter-p src dst)
           (setq simple nil)))
        (`(:move ,src ,dst)
         (cl-incf num-move)
         (unless (grease--same-filesystem-adapter-p src dst)
           (setq simple nil)))
        (`(:rename ,src ,dst)
         (cl-incf num-move)
         (unless (grease--same-filesystem-adapter-p src dst)
           (setq simple nil)))))
    (and simple
         (<= num-create 5)
         (<= num-copy 1)
         (<= num-move 1))))

(defun grease--should-skip-confirm-p (changes)
  "Return non-nil when CHANGES should be applied without confirmation."
  (and grease-skip-confirm-for-simple-edits
       (grease--simple-edit-p changes)))


(defun grease--detect-name-conflicts (entries)
  "Check for duplicate filenames in the same directory.
Return a list of conflicting names."
  (let ((names (make-hash-table :test 'equal))
        (conflicts '()))
    (dolist (entry entries)
      (let ((name (plist-get entry :name)))
        (let ((count (gethash name names 0)))
          (when (> count 0)
            ;; second time we see this exact name → conflict
            (push name conflicts))
          (puthash name (1+ count) names))))
    (cl-remove-duplicates conflicts :test #'equal)))

(defun grease--diff-by-id (baseline current-entries)
  "Return semantic operations between BASELINE and CURRENT-ENTRIES."
  (let ((current-by-id (make-hash-table :test 'eql)) operations)
    (dolist (entry current-entries)
      (when-let ((id (plist-get entry :id)))
        (puthash id entry current-by-id)))
    (maphash
     (lambda (id original)
       (let ((current (gethash id current-by-id)))
         (cond
          ((not current)
           (push (list :kind 'delete :id id :src (plist-get original :path)
                       :type (plist-get original :type))
                 operations))
          ((not (equal (plist-get original :path)
                       (plist-get current :path)))
           (push (list :kind 'relocate :id id
                       :src (plist-get original :path)
                       :dst (plist-get current :path)
                       :type (plist-get current :type))
                 operations)))))
     baseline)
    (dolist (entry current-entries)
      (let* ((id (plist-get entry :id))
             (source-id (plist-get entry :source-id)))
        (unless (gethash id baseline)
          (let* ((source-baseline (and source-id (gethash source-id baseline)))
                 (source-path (or (and source-baseline
                                       (plist-get source-baseline :path))
                                  (plist-get entry :source-path)))
                 (source-committed-p
                  (or (and source-baseline
                           (plist-get source-baseline :committed-p))
                      (plist-get entry :source-committed-p))))
            (if (and source-id source-path source-committed-p)
                (push (list :kind 'copy :id id :source-id source-id
                            :src source-path :dst (plist-get entry :path)
                            :type (plist-get entry :type))
                      operations)
              (push (list :kind 'create :id id
                          :dst (plist-get entry :path)
                          :type (plist-get entry :type))
                    operations))))))
    (nreverse operations)))

(defun grease--semantic-operation-to-legacy (operation)
  "Convert semantic OPERATION to the positional executor format.
This compatibility adapter can be removed when the transaction executor
accepts named operation plists directly."
  (pcase (plist-get operation :kind)
    ('create
     (let ((path (plist-get operation :dst)))
       (list :create (if (eq (plist-get operation :type) 'dir)
                         (file-name-as-directory path)
                       path))))
    ('delete (list :delete (plist-get operation :src)))
    ('copy (list :copy (plist-get operation :src) (plist-get operation :dst)))
    ('relocate
     (let ((src (plist-get operation :src))
           (dst (plist-get operation :dst)))
       (list (if (equal (file-name-directory src)
                        (file-name-directory dst))
                 :rename
               :move)
             src dst)))
    (kind (error "Unknown semantic operation kind %S" kind))))

(defun grease--calculate-changes ()
  "Calculate all current and staged changes in this Grease buffer."
  (let* ((transaction (grease--build-transaction (list (current-buffer))))
         (changes
          (mapcar #'grease--semantic-operation-to-legacy
                  (plist-get transaction :operations))))
    ;; Preserve the old executor's deterministic deletion-first behavior.
    (sort changes
          (lambda (left right)
            (and (eq (car left) :delete)
                 (not (eq (car right) :delete)))))))

(defun grease--live-buffers ()
  "Return every live buffer using `grease-mode'."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (derived-mode-p 'grease-mode))))
   (buffer-list)))

(defun grease--snapshot-current-directory ()
  "Return an identity-based snapshot of the current displayed directory."
  (let ((entries (grease--scan-buffer)))
    (when-let ((conflicts (grease--detect-name-conflicts entries)))
      (user-error "Filename conflicts detected in this directory: %s"
                  (mapconcat #'identity conflicts ", ")))
    (list :root-dir grease--root-dir
          :baseline (copy-hash-table grease--baseline-by-id)
          :entries (copy-tree entries))))

(defun grease--stage-current-directory ()
  "Stage the current directory snapshot before navigating elsewhere."
  (let* ((snapshot (grease--snapshot-current-directory))
         (operations (grease--diff-by-id
                      (plist-get snapshot :baseline)
                      (plist-get snapshot :entries))))
    ;; Revisiting and editing a directory replaces its older staged view.
    (setq grease--pending-changes
          (cl-remove (plist-get snapshot :root-dir) grease--pending-changes
                     :key (lambda (state) (plist-get state :root-dir))
                     :test #'equal))
    (when operations
      (push snapshot grease--pending-changes))
    (setq grease--buffer-dirty-p nil)
    operations))

(defun grease--collect-buffer-state (buffer)
  "Collect effective staged and current directory states from BUFFER.
Whitespace-only dirty buffers are marked clean and contribute no state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'grease-mode)
        (let ((states (copy-sequence grease--pending-changes)))
          (when grease--buffer-dirty-p
            (let* ((snapshot (grease--snapshot-current-directory))
                   (operations
                    (grease--diff-by-id (plist-get snapshot :baseline)
                                        (plist-get snapshot :entries))))
              (if operations
                  (push snapshot states)
                (setq grease--buffer-dirty-p nil))))
          (when states
            (list :buffer buffer :states (nreverse states))))))))

(defun grease--build-transaction (&optional buffers)
  "Build one semantic transaction from dirty Grease BUFFERS.
When BUFFERS is nil, inspect every live Grease buffer.  Conflicting desired
paths or authoritative states signal `user-error' before any mutation."
  (let ((collections (delq nil
                           (mapcar #'grease--collect-buffer-state
                                   (or buffers (grease--live-buffers)))))
        (baseline (make-hash-table :test 'eql))
        (claims (make-hash-table :test 'eql))
        participants)
    (dolist (collection collections)
      (push (plist-get collection :buffer) participants)
      (dolist (state (plist-get collection :states))
        (let ((state-baseline (plist-get state :baseline))
              (current-by-id (make-hash-table :test 'eql)))
          (dolist (entry (plist-get state :entries))
            (puthash (plist-get entry :id) entry current-by-id)
            (push (list :entry entry
                        :authoritative-p
                        (not (null (gethash (plist-get entry :id)
                                            state-baseline))))
                  (gethash (plist-get entry :id) claims)))
          (maphash
           (lambda (id original)
             (let ((known (gethash id baseline)))
               (when (and known
                          (not (equal (plist-get known :path)
                                      (plist-get original :path))))
                 (user-error "Conflicting committed paths for ID %s" id))
               (unless known
                 (puthash id original baseline)))
             (unless (gethash id current-by-id)
               (push (list :entry nil :authoritative-p t)
                     (gethash id claims))))
           state-baseline))))

    (let (desired-entries)
      (maphash
       (lambda (id id-claims)
         (let* ((entries (delq nil (mapcar (lambda (claim)
                                            (plist-get claim :entry))
                                          id-claims)))
                (paths (cl-remove-duplicates
                        (mapcar (lambda (entry) (plist-get entry :path)) entries)
                        :test #'equal))
                (deleted-p (cl-some (lambda (claim)
                                      (and (plist-get claim :authoritative-p)
                                           (not (plist-get claim :entry))))
                                    id-claims))
                (authoritative-present-p
                 (cl-some (lambda (claim)
                            (and (plist-get claim :authoritative-p)
                                 (plist-get claim :entry)))
                          id-claims)))
           (when (> (length paths) 1)
             (user-error "Conflicting desired paths for ID %s: %s"
                         id (mapconcat #'identity paths ", ")))
           (when (and deleted-p authoritative-present-p)
             (user-error "Conflicting deletion and placement for ID %s" id))
           ;; An absent source claim plus a non-authoritative destination claim
           ;; is a cross-buffer relocation, not a conflict.
           (when (and entries (not (and deleted-p authoritative-present-p)))
             (push (car entries) desired-entries))))
       claims)

      (let ((destinations (make-hash-table :test 'equal)))
        (dolist (entry desired-entries)
          (let* ((path (plist-get entry :path))
                 (other-id (gethash path destinations)))
            (when (and other-id (not (equal other-id (plist-get entry :id))))
              (user-error "Multiple IDs claim destination %s" path))
            (puthash path (plist-get entry :id) destinations))))

      (list :operations (grease--diff-by-id baseline desired-entries)
            :buffers (nreverse (cl-remove-duplicates participants))
            :baseline baseline
            :entries desired-entries))))

(defun grease--operation-destination (operation)
  "Return OPERATION's destination path, if it has one."
  (pcase (plist-get operation :kind)
    ((or 'create 'copy 'relocate) (plist-get operation :dst))))

(defun grease--normalize-operation-paths (semantic-operations)
  "Return a copy of SEMANTIC-OPERATIONS with canonical source and destination paths."
  (mapcar
   (lambda (operation)
     (let ((normalized (copy-tree operation)))
       (dolist (property '(:src :dst))
         (when-let ((path (plist-get normalized property)))
           (setf (plist-get normalized property)
                 (directory-file-name path))))
       normalized))
   semantic-operations))

(defun grease--path-occupied-p (path)
  "Return non-nil when PATH names an existing entry or symbolic link."
  (or (file-exists-p path) (file-symlink-p path)))

(defun grease--expand-parent-directory-creates (semantic-operations)
  "Add explicit create operations for missing destination parent directories."
  (let ((providers (make-hash-table :test 'equal))
        (destinations (make-hash-table :test 'equal))
        (needed (make-hash-table :test 'equal))
        implicit-parents)
    (dolist (operation semantic-operations)
      (when-let ((dst (grease--operation-destination operation)))
        (puthash dst operation destinations)
        (when (and (memq (plist-get operation :kind) '(create relocate))
                   (eq (plist-get operation :type) 'dir))
          (puthash dst operation providers))))
    (dolist (operation semantic-operations)
      (when-let ((dst (grease--operation-destination operation)))
        (let ((parent
               (directory-file-name
                (file-name-directory (directory-file-name dst)))))
          (while (and parent
                      (not (equal parent (directory-file-name dst)))
                      (not (file-directory-p parent))
                      (not (gethash parent providers)))
            (when (grease--path-occupied-p parent)
              (user-error "Destination ancestor is not a directory: %s" parent))
            (when-let ((claim (gethash parent destinations)))
              (unless (and (memq (plist-get claim :kind) '(create relocate))
                           (eq (plist-get claim :type) 'dir))
                (user-error "Scheduled destination ancestor is not a directory: %s"
                            parent)))
            (unless (gethash parent needed)
              (puthash parent t needed)
              (push (list :kind 'create
                          :id nil
                          :dst parent
                          :type 'dir
                          :implicit-parent-p t)
                    implicit-parents))
            (let ((next (directory-file-name (file-name-directory parent))))
              (setq parent (unless (equal next parent) next)))))))
    (append (nreverse implicit-parents) semantic-operations)))

(defun grease--temporary-cycle-path (operation reserved-paths)
  "Return a unique temporary sibling path for OPERATION.
RESERVED-PATHS contains every transaction source and destination."
  (let* ((src (plist-get operation :src))
         (directory (file-name-directory src))
         (id (plist-get operation :id))
         (counter 1)
         candidate)
    (while
        (progn
          (setq candidate
                (expand-file-name
                 (format ".grease-tmp-%s-%d" id counter) directory))
          (cl-incf counter)
          (or (grease--path-occupied-p candidate)
              (gethash candidate reserved-paths)
              (not (equal (file-remote-p src)
                          (file-remote-p candidate))))))
    candidate))

(defun grease--break-relocation-cycles (semantic-operations)
  "Break every relocation cycle in SEMANTIC-OPERATIONS with one temporary path."
  (let* ((operations (copy-tree semantic-operations))
         (relocations
          (cl-remove-if-not (lambda (operation)
                              (eq (plist-get operation :kind) 'relocate))
                            operations))
         (source-map (make-hash-table :test 'equal))
         (visited (make-hash-table :test 'eq))
         (reserved (make-hash-table :test 'equal))
         cycles temporary-operations)
    (dolist (operation operations)
      (when-let ((src (plist-get operation :src)))
        (puthash src t reserved))
      (when-let ((dst (grease--operation-destination operation)))
        (puthash dst t reserved)))
    (dolist (operation relocations)
      (puthash (plist-get operation :src) operation source-map))

    ;; Relocation dependencies form a functional graph: each relocation can
    ;; be blocked by at most one operation occupying its destination.
    (dolist (start relocations)
      (unless (gethash start visited)
        (let ((current start)
              (positions (make-hash-table :test 'eq))
              path cycle)
          (while (and current
                      (not (gethash current visited))
                      (not (gethash current positions)))
            (puthash current (length path) positions)
            (setq path (append path (list current)))
            (setq current (gethash (plist-get current :dst) source-map)))
          (when (and current (gethash current positions))
            (setq cycle (nthcdr (gethash current positions) path))
            (push cycle cycles))
          (dolist (member path)
            (puthash member t visited)))))

    (dolist (cycle cycles)
      (let* ((breaker
              (car (sort (copy-sequence cycle)
                         (lambda (left right)
                           (< (or (plist-get left :id) most-positive-fixnum)
                              (or (plist-get right :id) most-positive-fixnum))))))
             (original-src (plist-get breaker :src))
             (temporary-path
              (grease--temporary-cycle-path breaker reserved)))
        (puthash temporary-path t reserved)
        (push (list :kind 'relocate
                    :id (plist-get breaker :id)
                    :src original-src
                    :dst temporary-path
                    :type (plist-get breaker :type)
                    :temporary-p t)
              temporary-operations)
        (setf (plist-get breaker :src) temporary-path)))
    (append (nreverse temporary-operations) operations)))

(defun grease--plan-transaction (semantic-operations)
  "Validate and safely order SEMANTIC-OPERATIONS, breaking relocation cycles."
  (let* ((operations
          (grease--break-relocation-cycles
           (grease--expand-parent-directory-creates
            (grease--normalize-operation-paths semantic-operations))))
         (sources (make-hash-table :test 'equal))
         (destinations (make-hash-table :test 'equal))
         (creates (make-hash-table :test 'equal))
         (dependencies (make-hash-table :test 'eq)))
    (cl-labels
        ((add-dependency
          (operation prerequisite)
          (unless (eq operation prerequisite)
            (cl-pushnew prerequisite (gethash operation dependencies)
                        :test #'eq)))
         (provider-for-directory
          (directory)
          (or (gethash directory creates)
              (cl-find-if
               (lambda (operation)
                 (and (eq (plist-get operation :kind) 'relocate)
                      (eq (plist-get operation :type) 'dir)
                      (equal (plist-get operation :dst) directory)))
               operations))))

      ;; Index paths and reject malformed or ambiguous operations first.
      (dolist (operation operations)
        (let ((kind (plist-get operation :kind))
              (src (plist-get operation :src))
              (dst (grease--operation-destination operation)))
          (unless (memq kind '(create copy relocate delete))
            (user-error "Unknown filesystem operation kind %S" kind))
          (when (memq kind '(copy relocate delete))
            (unless src
              (user-error "Operation %S has no source" operation)))
          (when (memq kind '(relocate delete))
            (when (gethash src sources)
              (user-error "Multiple operations consume source %s" src))
            (puthash src operation sources))
          (when dst
            (when (gethash dst destinations)
              (user-error "Multiple operations claim destination %s" dst))
            (puthash dst operation destinations))
          (when (eq kind 'create)
            (puthash dst operation creates))
          (when (and (eq kind 'relocate)
                     (eq (plist-get operation :type) 'dir)
                     (or (equal src dst)
                         (file-in-directory-p dst
                                              (file-name-as-directory src))))
            (user-error "Cannot relocate directory %s into itself" src))))

      ;; Reject ambiguous edits below a directory that is itself relocating.
      (dolist (ancestor operations)
        (when (and (eq (plist-get ancestor :kind) 'relocate)
                   (eq (plist-get ancestor :type) 'dir))
          (let ((ancestor-src (file-name-as-directory
                               (plist-get ancestor :src))))
            (dolist (operation operations)
              (unless (eq ancestor operation)
                (when (cl-some
                       (lambda (path)
                         (and path
                              (not (equal (directory-file-name path)
                                          (directory-file-name ancestor-src)))
                              (file-in-directory-p path ancestor-src)))
                       (list (plist-get operation :src)
                             (grease--operation-destination operation)))
                  (user-error
                   "Overlapping transaction below relocated directory %s"
                   (plist-get ancestor :src))))))))

      (dolist (operation operations)
        (let* ((kind (plist-get operation :kind))
               (src (plist-get operation :src))
               (dst (grease--operation-destination operation))
               (occupied-p (and dst (grease--path-occupied-p dst)))
               (blocker (and occupied-p (gethash dst sources))))
          ;; Existing destinations must be vacated or explicitly deleted.
          (when occupied-p
            (unless (and blocker
                         (memq (plist-get blocker :kind) '(relocate delete)))
              (user-error "Destination is occupied outside transaction: %s" dst)))
          (when blocker
            (add-dependency operation blocker))

          ;; Destination parents must exist or be produced first.
          (when dst
            (let ((parent
                   (directory-file-name
                    (file-name-directory (directory-file-name dst)))))
              (unless (or (file-directory-p parent)
                          (equal parent dst))
                (let ((provider (provider-for-directory parent)))
                  (unless provider
                    (user-error "Destination parent does not exist: %s" parent))
                  (add-dependency operation provider)))))

          ;; Copies must read their source before another operation consumes it.
          (when (eq kind 'copy)
            (dolist (consumer operations)
              (when (and (not (eq operation consumer))
                         (equal src (plist-get consumer :src))
                         (memq (plist-get consumer :kind) '(relocate delete)))
                (add-dependency consumer operation))))))

      ;; Stable topological sort.  No filesystem mutation occurs in planning.
      (let ((remaining (copy-sequence operations))
            ordered)
        (while remaining
          (let ((ready (cl-find-if
                        (lambda (operation)
                          (not (cl-intersection (gethash operation dependencies)
                                                remaining :test #'eq)))
                        remaining)))
            (unless ready
              (user-error "Cyclic filesystem operation dependency"))
            (setq remaining (delq ready remaining))
            (push ready ordered)))
        (nreverse ordered)))))

(defun grease--copy-path-without-overwrite (src dst)
  "Copy SRC to DST, refusing to overwrite any existing destination."
  (when (grease--path-occupied-p dst)
    (error "Destination already exists: %s" dst))
  (unless (file-directory-p (file-name-directory dst))
    (error "Destination parent does not exist: %s"
           (file-name-directory dst)))
  (if (file-directory-p src)
      (copy-directory src dst nil nil t)
    (copy-file src dst nil)))

(defun grease--delete-path (path)
  "Delete file or directory PATH."
  (if (file-directory-p path)
      (delete-directory path t)
    (delete-file path)))

(defun grease--execute-operation (operation)
  "Execute one planned semantic OPERATION without overwriting destinations."
  (let ((kind (plist-get operation :kind))
        (src (plist-get operation :src))
        (dst (grease--operation-destination operation)))
    (pcase kind
      ('create
       (when (grease--path-occupied-p dst)
         (error "Destination already exists: %s" dst))
       (unless (file-directory-p (file-name-directory dst))
         (error "Destination parent does not exist: %s"
                (file-name-directory dst)))
       (if (eq (plist-get operation :type) 'dir)
           (make-directory dst nil)
         (write-region "" nil dst nil 'silent nil t)))
      ('copy
       (grease--copy-path-without-overwrite src dst))
      ('relocate
       (when (grease--path-occupied-p dst)
         (error "Destination already exists: %s" dst))
       (unless (file-directory-p (file-name-directory dst))
         (error "Destination parent does not exist: %s"
                (file-name-directory dst)))
       (if (grease--same-filesystem-adapter-p src dst)
           (rename-file src dst nil)
         ;; Cross-filesystem relocation is copy-then-delete.  The source is
         ;; retained if copying fails.
         (grease--copy-path-without-overwrite src dst)
         (grease--delete-path src)))
      ('delete
       (grease--delete-path src))
      (_ (error "Unknown filesystem operation kind %S" kind)))))

(defun grease--execute-transaction (plan)
  "Execute semantic PLAN and return an explicit result plist.
Execution stops at the first error.  No buffer, registry, clipboard, or staged
state is cleared here; the save coordinator owns transaction commit state."
  (let (completed temporary-paths failure failure-operation)
    (catch 'stop
      (dolist (operation plan)
        (condition-case err
            (progn
              (grease--execute-operation operation)
              (push operation completed)
              (when (plist-get operation :temporary-p)
                (push (plist-get operation :dst) temporary-paths))
              (when-let ((src (plist-get operation :src)))
                (setq temporary-paths (delete src temporary-paths))))
          (error
           (setq failure err
                 failure-operation operation)
           (throw 'stop nil)))))
    (if failure
        (list :success-p nil
              :operation failure-operation
              :error failure
              :completed (nreverse completed)
              :temporary-paths (nreverse temporary-paths))
      (list :success-p t
            :completed (nreverse completed)
            :temporary-paths (nreverse temporary-paths)))))

(defun grease--legacy-change-to-semantic (change)
  "Convert positional CHANGE for compatibility with older internal callers."
  (pcase change
    (`(:create ,path)
     (list :kind 'create :dst (directory-file-name path)
           :type (if (string-suffix-p "/" path) 'dir 'file)))
    (`(:delete ,path)
     (list :kind 'delete :src path
           :type (if (file-directory-p path) 'dir 'file)))
    ((or `(:rename ,src ,dst) `(:move ,src ,dst))
     (list :kind 'relocate :src src :dst dst
           :type (if (file-directory-p src) 'dir 'file)))
    (`(:copy ,src ,dst)
     (list :kind 'copy :src src :dst dst
           :type (if (file-directory-p src) 'dir 'file)))
    (_ (error "Unknown legacy filesystem change %S" change))))

(defun grease--apply-changes (changes)
  "Compatibility wrapper executing positional CHANGES.
Return the explicit executor result and do not clear any global state."
  (grease--execute-transaction
   (grease--plan-transaction
    (mapcar #'grease--legacy-change-to-semantic changes))))

;;;; Preview Buffer System

(defun grease--preview-buffer-name ()
  "Return the name of the preview buffer for current grease buffer."
  (format "*grease-preview:%s*" (grease--project-name)))

(defun grease-toggle-preview ()
  "Toggle the preview window on the right side.
Shows file contents for files, or directory listing for directories."
  (interactive)
  (if (and grease--preview-window (window-live-p grease--preview-window))
      (grease--close-preview)
    (grease--open-preview)))

(defun grease-toggle-preview-writable ()
  "Toggle whether the preview buffer is writable for files.
When writable, you can edit file previews directly.
Does not apply to directory listings, which remain read-only."
  (interactive)
  (setq grease-preview-writable (not grease-preview-writable))
  (message "Preview writable: %s" (if grease-preview-writable "on" "off"))
  ;; Update current preview buffer if open
  (when (and grease--preview-buffer (buffer-live-p grease--preview-buffer))
    (grease--update-preview)))

(defun grease--open-preview ()
  "Open the preview window."
  (let* ((buf-name (grease--preview-buffer-name))
         (buf (get-buffer-create buf-name))
         ;; First check for any window to the right (regular split)
         (right-window (window-in-direction 'right))
         ;; Also check for existing side window on the right
         (existing-side-window
          (cl-find-if (lambda (w)
                        (eq (window-parameter w 'window-side) 'right))
                      (window-list)))
         ;; Prefer regular right window, fall back to side window
         (target-window (or right-window existing-side-window))
         (original-buf (when target-window
                         (window-buffer target-window))))
    (setq grease--preview-buffer buf)
    (setq grease--preview-original-buffer original-buf)
    (if target-window
        ;; Reuse existing window (regular split or side window)
        (progn
          (set-window-buffer target-window buf)
          (setq grease--preview-window target-window))
      ;; No window to the right - create a new side window
      (setq grease--preview-window
            (display-buffer-in-side-window
             buf
             `((side . right)
               (slot . 0)
               (window-width . ,grease-preview-window-width)
               (preserve-size . (t . nil))))))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (setq truncate-lines t))
    (grease--update-preview)
    (add-hook 'post-command-hook #'grease--schedule-preview-update nil t)))

(defun grease--close-preview ()
  "Close the preview window and clean up."
  (when grease--preview-timer
    (cancel-timer grease--preview-timer)
    (setq grease--preview-timer nil))
  (remove-hook 'post-command-hook #'grease--schedule-preview-update t)
  (when (and grease--preview-window (window-live-p grease--preview-window))
    (if (and grease--preview-original-buffer
             (buffer-live-p grease--preview-original-buffer))
        ;; Window existed before grease - restore original buffer
        (set-window-buffer grease--preview-window grease--preview-original-buffer)
      ;; Window was created by grease - delete it
      (delete-window grease--preview-window)))
  (when (and grease--preview-buffer (buffer-live-p grease--preview-buffer))
    (kill-buffer grease--preview-buffer))
  (setq grease--preview-window nil)
  (setq grease--preview-buffer nil)
  (setq grease--preview-original-buffer nil))

(defun grease--schedule-preview-update ()
  "Schedule a preview update after a short delay (debounced)."
  (when (and grease--preview-window
             (window-live-p grease--preview-window)
             (derived-mode-p 'grease-mode))
    (when grease--preview-timer
      (cancel-timer grease--preview-timer))
    (setq grease--preview-timer
          (run-with-idle-timer 0.1 nil #'grease--update-preview (current-buffer)))))

(defun grease--update-preview (&optional source-buffer)
  "Update the preview buffer with content from file at point.
SOURCE-BUFFER is the Grease buffer whose point and buffer-local state should
be used.  Timers do not reliably run with the Grease buffer current."
  (let ((source-buffer (or source-buffer (current-buffer))))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (when (and grease--preview-window
                   (window-live-p grease--preview-window)
                   grease--preview-buffer
                   (buffer-live-p grease--preview-buffer))
          (let ((data (grease--get-line-data)))
            (when data
              (let* ((name (plist-get data :name))
                     (type (plist-get data :type))
                     (path (grease--get-full-path name))
                     (preview-buffer grease--preview-buffer)
                     (preview-writable grease-preview-writable)
                     (root-dir grease--root-dir)
                     (current-sort-method grease--current-sort-method)
                     (show-hidden-initialized grease--show-hidden-initialized)
                     (current-show-hidden grease--current-show-hidden)
                     (is-file (and (eq type 'file)
                                   (file-exists-p path)
                                   (file-readable-p path))))
                (with-current-buffer preview-buffer
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t)
                        (buffer-undo-list t))
                    ;; Preview buffers are reused across many file types.  Reset
                    ;; the old mode before replacing text so stale font-lock,
                    ;; treesit, or track-changes hooks from the previous preview
                    ;; cannot observe an erase/insert for unrelated content.
                    (setq buffer-read-only nil)
                    (fundamental-mode)
                    ;; Preserve the Grease buffer's view options while rendering
                    ;; directory previews from the preview buffer.
                    (let ((grease--root-dir root-dir)
                          (grease--current-sort-method current-sort-method)
                          (grease--show-hidden-initialized show-hidden-initialized)
                          (grease--current-show-hidden current-show-hidden))
                      (erase-buffer)
                      (cond
                       ((eq type 'dir)
                        (grease--render-preview-directory path))
                       (is-file
                        (grease--render-preview-file path))
                       ((not (file-exists-p path))
                        (insert (format "New file: %s\n\n(File will be created on save)" name)))
                       (t
                        (insert (format "Cannot preview: %s" path))))
                      (goto-char (point-min)))
                    ;; Set read-only based on type and setting.
                    ;; Directories are always read-only, files respect
                    ;; `grease-preview-writable'.
                    (setq buffer-read-only (or (eq type 'dir)
                                               (not preview-writable)))))))))))))

(defun grease--render-preview-directory (dir)
  "Render directory listing for DIR in preview buffer (read-only)."
  (insert (propertize (format "Directory: %s\n\n" dir) 'face 'font-lock-comment-face))
  (if (file-exists-p dir)
      (let* ((files (directory-files dir nil nil t))
             (files (cl-remove-if (lambda (f) (member f '("." ".."))) files))
             (files (grease--filter-hidden files)))
        (if files
            (dolist (file (grease--sort-files files dir))
              (let* ((path (expand-file-name file dir))
                     (is-dir (file-directory-p path))
                     (icon (if (grease--icons-available-p)
                               (if is-dir
                                   (nerd-icons-sucicon "nf-custom-folder_oct")
                                 (nerd-icons-icon-for-file file))
                             (if is-dir "dir" "file"))))
                (insert icon "  " file (if is-dir "/" "") "\n")))
          (insert (propertize "(empty directory)" 'face 'font-lock-comment-face))))
    (insert (propertize "(directory will be created on save)" 'face 'font-lock-comment-face))))

(defun grease--render-preview-file (path)
  "Render file content for PATH in preview buffer."
  (condition-case err
      (let* ((attrs (file-attributes path))
             (size (file-attribute-size attrs)))
        (cond
         ((> size (* 1024 1024))
          (insert (propertize (format "File too large to preview\nSize: %d bytes" size)
                              'face 'font-lock-warning-face)))
         ((with-temp-buffer
            (insert-file-contents path nil 0 (min size 8192))
            (goto-char (point-min))
            (search-forward "\0" nil t))
          (insert (propertize "Binary file\n" 'face 'font-lock-warning-face))
          (insert (format "Size: %d bytes\n" size))
          (insert (format "Type: %s" (or (file-name-extension path) "unknown"))))
         (t
          (insert-file-contents path)
          (let ((mode (assoc-default path auto-mode-alist 'string-match)))
            (when (and mode (functionp mode))
              (condition-case nil
                  (delay-mode-hooks (funcall mode))
                (error nil))))
          (font-lock-ensure))))
    (error
     (insert (propertize (format "Error reading file: %s" (error-message-string err))
                         'face 'font-lock-warning-face)))))

;;;; User Commands

(defun grease--commit-registry-operations (operations)
  "Commit successful semantic OPERATIONS to the global identity registry."
  (dolist (operation operations)
    (let ((id (plist-get operation :id))
          (kind (plist-get operation :kind)))
      (pcase kind
        ('delete
         (when id (remhash id grease--file-registry)))
        ((or 'create 'copy 'relocate)
         (if id
             (puthash id
                      (list :path (plist-get operation :dst)
                            :type (plist-get operation :type)
                            :committed-p t
                            :source-id nil
                            :exists t)
                      grease--file-registry)
           (when (and (plist-get operation :implicit-parent-p)
                      (not (grease--get-id-by-path
                            (plist-get operation :dst))))
             (grease--register-file (plist-get operation :dst) 'dir))))))))

(defun grease--clipboard-consumed-p (operations)
  "Return non-nil when OPERATIONS consume the current Grease clipboard."
  (let ((ids (plist-get grease--clipboard :ids)))
    (and ids
         (cl-some (lambda (operation)
                    (or (memq (plist-get operation :id) ids)
                        (memq (plist-get operation :source-id) ids)))
                  operations))))

(defun grease--rerender-live-buffers (buffers operations)
  "Rerender live Grease BUFFERS after successful OPERATIONS."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((root grease--root-dir))
          (dolist (operation operations)
            (when (and (eq (plist-get operation :kind) 'relocate)
                       (eq (plist-get operation :type) 'dir)
                       (equal (directory-file-name root)
                              (directory-file-name
                               (plist-get operation :src))))
              (setq root (file-name-as-directory
                          (plist-get operation :dst)))))
          (while (and root (not (file-directory-p root)))
            (let ((parent (file-name-directory (directory-file-name root))))
              (setq root (unless (equal parent root) parent))))
          (setq grease--pending-changes nil
                grease--buffer-dirty-p nil)
          (when root
            (grease--render root)))))))

(defun grease--discard-transaction (buffers)
  "Discard staged state in participating BUFFERS and rerender from disk."
  (setq grease--clipboard nil)
  (grease--rerender-live-buffers buffers nil))

(defun grease--save-all-buffers-transaction (all-buffers)
  "Build and save one transaction from ALL-BUFFERS."
  (let* ((transaction (grease--build-transaction all-buffers))
         (operations (plist-get transaction :operations))
         (participants (plist-get transaction :buffers)))
    (if (not operations)
        (progn
          (message "Grease: No changes to save.")
          t)
      (let* ((plan (grease--plan-transaction operations))
             (display-changes
              (mapcar #'grease--semantic-operation-to-legacy operations))
             (choice
              (if (grease--should-skip-confirm-p display-changes)
                  ?y
                (read-char-choice
                 (format
                  (concat "Apply all staged Grease-buffer changes? "
                          "(y=yes, n=cancel, d=discard all)\n%s\n")
                  (mapconcat #'grease--format-semantic-operation operations "\n"))
                 '(?y ?n ?d)))))
        (pcase choice
          (?n
           (message "Grease: Save cancelled; no changes were applied.")
           nil)
          (?d
           (grease--discard-transaction participants)
           (message "Grease: Discarded changes from all participating buffers.")
           t)
          (?y
           (let ((result (grease--execute-transaction plan)))
             (if (not (plist-get result :success-p))
                 (progn
                   (message "Grease: Transaction failed at %S: %s"
                            (plist-get result :operation)
                            (error-message-string (plist-get result :error)))
                   nil)
               (let ((clipboard-consumed
                      (grease--clipboard-consumed-p operations)))
                 (grease--commit-registry-operations plan)
                 (when clipboard-consumed
                   (setq grease--clipboard nil))
                 (grease--rerender-live-buffers all-buffers operations)
                 (message "Grease: Transaction committed successfully.")
                 t)))))))))

(defun grease--buffers-with-staged-state (buffers)
  "Return BUFFERS with dirty or pending Grease state."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (derived-mode-p 'grease-mode)
                 (or grease--buffer-dirty-p grease--pending-changes)))))
   buffers))

(defun grease--handle-transaction-conflict (error buffers)
  "Offer to keep or discard staged state after transaction ERROR in BUFFERS."
  (let* ((staged-buffers (grease--buffers-with-staged-state buffers))
         (choice
          (read-char-choice
           (format (concat "Grease transaction conflict:\n%s\n\n"
                           "k = keep editing\n"
                           "d = discard all staged Grease-buffer changes\n")
                   (error-message-string error))
           '(?k ?d))))
    (pcase choice
      (?d
       (grease--discard-transaction staged-buffers)
       (message "Grease: Discarded all staged changes after conflict.")
       t)
      (_
       (message "Grease: Conflict preserved; keep editing to resolve it.")
       nil))))

;;;###autoload
(defun grease-save-all-buffers ()
  "Save all staged Grease-buffer changes as one filesystem transaction.
If transaction construction or validation finds a conflict, offer to keep
editing or discard all staged Grease-buffer changes."
  (interactive)
  (let ((all-buffers (grease--live-buffers)))
    (condition-case error
        (grease--save-all-buffers-transaction all-buffers)
      (user-error
       (grease--handle-transaction-conflict error all-buffers)))))

(defun grease-save ()
  "Delegate saving to `grease-save-all-buffers'."
  (interactive)
  (grease-save-all-buffers))

(defun grease-duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((data (grease--get-line-data)))
    (when data
      (let* ((name (plist-get data :name))
             (type (plist-get data :type))
             (id (plist-get data :id))
             (next-id (grease--get-next-id)))
        ;; Add a new line with the duplicated content.  Existing files keep a
        ;; source ID so they become filesystem copies; pending/new files are
        ;; inserted as fresh creates.
        (end-of-line)
        (insert "\n")
        (grease--insert-entry next-id name type
                              (when (grease--line-data-real-file-p data) id)
                              (grease--line-data-real-file-p data))
        (setq grease--buffer-dirty-p t)
        (message "Duplicated: %s" name)))))

(defun grease-cut ()
  "Cut the current file/directory."
  (interactive)
  (let ((data (grease--get-line-data)))
    (when data
      (let* ((name (plist-get data :name))
             (type (plist-get data :type))
             (path (grease--get-full-path name))
             (id (plist-get data :id)))
        ;; Clipboard = CUT (pending move for existing files, text move for new entries)
        (let ((source-kind (grease--line-data-source-kind data)))
          (setq grease--clipboard
                (list :paths (list path)
                      :names (list name)
                      :types (list type)
                      :ids (list id)
                      :source-kinds (list source-kind)
                      :original-dir grease--root-dir
                      :operation 'cut)))
        (setq grease--last-op-type 'cut)
        (setq grease--last-kill-index 0)

        ;; Delete the line visually.  The committed registry remains unchanged
        ;; until the unified transaction succeeds.
        (delete-region (line-beginning-position) (line-end-position))
        (when (eobp) (delete-char -1))
        (setq grease--buffer-dirty-p t)
        (message "Cut (staged): %s" name)))))

(defun grease-copy ()
  "Copy the current file/directory."
  (interactive)
  (let ((data (grease--get-line-data)))
    (when data
      (let* ((name (plist-get data :name))
             (type (plist-get data :type))
             (path (grease--get-full-path name))
             (id (plist-get data :id)))
        ;; Store in clipboard.  Existing files paste as filesystem copies;
        ;; pending/new entries paste as fresh creates.
        (setq grease--clipboard
              (list :paths (list path)
                    :names (list name)
                    :types (list type)
                    :ids (list id)
                    :source-kinds (list (grease--line-data-source-kind data))
                    :original-dir grease--root-dir
                    :operation 'copy))
        
        ;; Mark this as a file operation
        (setq grease--last-op-type 'file)
        (setq grease--last-kill-index 0)
        
        (message "Copied file: %s" name)))))

(defun grease-paste ()
  "Paste the cut/copied file/directory."
  (interactive)
  (if (not grease--clipboard)
      (message "Nothing to paste.")
    (let* ((operation    (plist-get grease--clipboard :operation))
           (paths        (plist-get grease--clipboard :paths))
           (names        (plist-get grease--clipboard :names))
           (types        (plist-get grease--clipboard :types))
           (ids          (plist-get grease--clipboard :ids))
           (source-kinds (or (plist-get grease--clipboard :source-kinds)
                             (mapcar (lambda (path)
                                       (if (file-exists-p path) 'file 'text))
                                     paths)))
           (original-dir (plist-get grease--clipboard :original-dir))
           (is-cross-dir (not (string= original-dir grease--root-dir)))
           (total-count  (length names)))

      ;; Move to insertion point. If current line has content, create a fresh line first.
      (end-of-line)
      (when (not (string-empty-p
                  (string-trim
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))))
        (insert "\n"))
      (beginning-of-line)

      ;; Insert all entries sequentially with no extra blank lines.
      (cl-loop for name in names
               for type in types
               for id   in ids
               for source-kind in source-kinds
               do
               (let* ((next-id     (grease--get-next-id))
                      ;; Existing files paste as filesystem copies/moves.
                      ;; Pending/new entries paste like freshly typed text.
                      (copying     (and (eq operation 'copy)
                                        (eq source-kind 'file)))
                      (moving      (and (memq operation '(move cut))
                                        (eq source-kind 'file)))
                      (creating    (eq source-kind 'text))
                      (target-name (if (and copying (not is-cross-dir))
                                       (if (eq type 'dir)
                                           (concat (grease--strip-trailing-slash name) "-copy")
                                         (grease--add-copy-suffix name))
                                     name)))
                 (cond
                  (copying
                   (grease--insert-entry next-id target-name type id t))
                  (moving
                   ;; Keep same ID for cut/move
                   (grease--insert-entry id name type nil nil))
                  (creating
                   (grease--insert-entry next-id name type nil nil))
                  (t
                   (grease--insert-entry next-id name type nil nil)))))

      ;; Update state
      (setq grease--last-op-type 'file)
      (setq grease--last-kill-index 0)
      (setq grease--buffer-dirty-p t)
      (message "%s %d file%s"
               (if (eq operation 'copy) "Copied" "Moved")
               total-count
               (if (= total-count 1) "" "s")))))

(defun grease--with-commit-prompt (action-fn)
  "Run ACTION-FN after saving if buffer is dirty."
  (if (not grease--buffer-dirty-p) (funcall action-fn)
    (let ((result (grease-save)))
      (when result (funcall action-fn)))))

(defun grease-visit ()
  "Visit the file or directory at point."
  (interactive)
  (grease--save-position)
  (let ((data (grease--get-line-data))
        (current-line (line-number-at-pos)))
    (if (not data)
        (user-error "Not on a file or directory line.")
      (let* ((name (plist-get data :name))
             (type (plist-get data :type))
             (path (grease--get-full-path name)))
        (if (eq type 'dir)
            (progn
              ;; Store an identity-based directory snapshot before moving.
              (when grease--buffer-dirty-p
                (grease--stage-current-directory))
              (grease--render path t)
              ;; Restore cursor position, clamped to valid lines
              (grease--goto-line-clamped current-line))
          (grease--with-commit-prompt
           (lambda ()
             (kill-buffer (current-buffer))
             (find-file path))))))))

(defun grease-up-directory ()
  "Move to the parent directory, placing cursor on the directory just exited."
  (interactive)
  (grease--save-position)
  (let* ((child-dir (directory-file-name grease--root-dir))
         (child-name (file-name-nondirectory child-dir))
         (parent-dir (expand-file-name ".." grease--root-dir)))
    ;; Store an identity-based directory snapshot before moving.
    (when grease--buffer-dirty-p
      (grease--stage-current-directory))
    (grease--render parent-dir t)
    (grease--goto-file child-name)))

(defun grease-refresh ()
  "Discard all changes and reload the directory from disk."
  (interactive)
  (let ((changes (and grease--buffer-dirty-p (grease--calculate-changes))))
    (if (and changes
             (not (y-or-n-p "Discard all uncommitted changes?")))
        (message "Refresh cancelled.")
      (setq grease--pending-changes nil)
      (setq grease--clipboard nil)
      (setq grease--buffer-dirty-p nil)
      ;; Re-scan the directory on disk
      (grease--render grease--root-dir)
      (message "Grease: Refreshed."))))

(defun grease-quit ()
  "Quit this Grease buffer only after the unified save succeeds."
  (interactive)
  (grease--save-position)
  (if (or grease--buffer-dirty-p grease--pending-changes)
      (when (grease-save-all-buffers)
        (kill-buffer (current-buffer)))
    (kill-buffer (current-buffer))))

;;;; Major Mode Definition

(defvar grease-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'grease-save)
    (define-key map (kbd "C-c C-d") #'grease-duplicate-line)
    (define-key map (kbd "C-c C-x") #'grease-cut)
    (define-key map (kbd "C-c C-c") #'grease-copy)
    (define-key map (kbd "C-c C-v") #'grease-paste)
    (define-key map (kbd "C-c C-p") #'grease-toggle-preview)
    (define-key map (kbd "C-c .") #'grease-toggle-hidden)
    ;; Standard Emacs, Evil, and Doom split commands should create a distinct
    ;; Grease buffer rather than a second window onto the same buffer.
    (define-key map [remap split-window-right] #'grease-split-window-right)
    (define-key map [remap split-window-below] #'grease-split-window-below)
    (define-key map [remap evil-window-vsplit] #'grease-split-window-right)
    (define-key map [remap evil-window-split] #'grease-split-window-below)
    (define-key map [remap +evil/window-vsplit-and-follow]
      #'grease-split-window-right)
    (define-key map [remap +evil/window-split-and-follow]
      #'grease-split-window-below)
    ;; Closing the last window should dispose of its Grease buffer rather than
    ;; leaving a hidden transaction participant behind.
    (define-key map [remap delete-window] #'grease-close-window)
    (define-key map [remap kill-buffer-and-window] #'grease-close-window)
    (define-key map [remap evil-window-delete] #'grease-close-window)
    ;; Sorting keybindings
    (define-key map (kbd "C-c s s") #'grease-cycle-sort)
    (define-key map (kbd "C-c s t") #'grease-sort-by-type)
    (define-key map (kbd "C-c s n") #'grease-sort-by-name)
    (define-key map (kbd "C-c s z") #'grease-sort-by-size)
    (define-key map (kbd "C-c s Z") #'grease-sort-by-size-desc)
    (define-key map (kbd "C-c s d") #'grease-sort-by-date)
    (define-key map (kbd "C-c s D") #'grease-sort-by-date-desc)
    (define-key map (kbd "C-c s e") #'grease-sort-by-extension)
    map)
  "Keymap for `grease-mode'.")

(define-derived-mode grease-mode fundamental-mode "Grease"
  "A major mode for oil.nvim-style file management."
  :syntax-table nil
  ;; Grease buffers are editable directory listings, not source files.  Avoid
  ;; `prog-mode' hooks such as tree-sitter/font-lock change tracking, which can
  ;; assert when Evil opens and edits synthetic listing lines.
  (setq-local truncate-lines t)
  (setq buffer-read-only nil) ;; Ensure buffer is not read-only
  (add-hook 'after-change-functions #'grease--on-change nil t)
  (add-hook 'kill-buffer-hook #'grease--close-preview nil t)
  (grease--setup-cursor-constraints))

;; Set up Evil keybindings
(when (fboundp 'evil-define-key*)
  (evil-define-key* 'normal grease-mode-map
    (kbd "RET") #'grease-visit
    (kbd "-") #'grease-up-directory
    (kbd "g r") #'grease-refresh
    (kbd "g p") #'grease-toggle-preview
    (kbd "g .") #'grease-toggle-hidden
    ;; Sorting: g s <key>
    (kbd "g s s") #'grease-cycle-sort
    (kbd "g s t") #'grease-sort-by-type
    (kbd "g s n") #'grease-sort-by-name
    (kbd "g s z") #'grease-sort-by-size
    (kbd "g s Z") #'grease-sort-by-size-desc
    (kbd "g s d") #'grease-sort-by-date
    (kbd "g s D") #'grease-sort-by-date-desc
    (kbd "g s e") #'grease-sort-by-extension))

;;;; Entry Points

(defun grease--goto-file (name)
  "Move cursor to the line corresponding to file NAME."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let* ((line-data (grease--get-line-data))
             (line-name (plist-get line-data :name)))
        (when (and line-name (string= line-name name))
          (setq found t))
        (unless found
          (forward-line 1))))
    (if found
        (grease--constrain-cursor)
      ;; If not found, go to first file
      (goto-char (point-min))
      (forward-line 1)
      (grease--constrain-cursor))))

(defun grease--create-buffer (dir &optional target-file)
  "Create and return a new Grease buffer displaying DIR.
If TARGET-FILE is non-nil, position point on that entry.  This function does
not display or select the returned buffer."
  (let* ((directory (file-name-as-directory (expand-file-name dir)))
         (default-directory directory)
         (buffer-name (format "*grease:%s*" (grease--project-name)))
         (buffer (generate-new-buffer buffer-name))
         succeeded)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (grease-mode)
            (grease--render directory)
            (if target-file
                (grease--goto-file target-file)
              (goto-char (point-min))
              (forward-line 1)
              (grease--constrain-cursor)))
          (setq succeeded t)
          buffer)
      (unless succeeded
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defvar grease--splitting-window-p nil
  "Non-nil while a Grease command performs its own managed split.")

(defun grease--advice-split-window (original-function &rest arguments)
  "Make direct calls to ORIGINAL-FUNCTION Grease-aware for ARGUMENTS.
The primitive keeps its normal selection behavior; only the buffer displayed
in a newly split window changes when the source window displays Grease."
  (let* ((source-window (or (car arguments) (selected-window)))
         (source-buffer (and (window-live-p source-window)
                             (window-buffer source-window)))
         (grease-source-p
          (and (not grease--splitting-window-p)
               (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 (derived-mode-p 'grease-mode))))
         (directory
          (and grease-source-p
               (buffer-local-value 'grease--root-dir source-buffer)))
         (new-window (apply original-function arguments))
         new-buffer succeeded)
    (if (not grease-source-p)
        new-window
      (unwind-protect
          (progn
            (setq new-buffer (grease--create-buffer directory))
            (set-window-buffer new-window new-buffer)
            (setq succeeded t)
            new-window)
        (unless succeeded
          (when (buffer-live-p new-buffer)
            (kill-buffer new-buffer))
          (when (window-live-p new-window)
            (let ((grease--deleting-window-p t))
              (delete-window new-window))))))))

(defun grease--split-window-with-new-buffer (split-function)
  "Split the selected Grease window using SPLIT-FUNCTION.
Display a new Grease buffer for the current directory in the new window and
select it.  The source buffer's uncommitted text is intentionally not cloned."
  (unless (derived-mode-p 'grease-mode)
    (user-error "Current buffer is not a Grease buffer"))
  (let* ((directory grease--root-dir)
         (new-buffer (grease--create-buffer directory))
         new-window
         succeeded)
    (unwind-protect
        (progn
          (let ((grease--splitting-window-p t))
            (setq new-window (funcall split-function)))
          (set-window-buffer new-window new-buffer)
          (select-window new-window)
          (setq succeeded t)
          new-window)
      (unless succeeded
        (when (and (window-live-p new-window)
                   (not (eq new-window (selected-window))))
          (delete-window new-window))
        (when (buffer-live-p new-buffer)
          (kill-buffer new-buffer))))))

;;;###autoload
(defun grease-split-window-right ()
  "Split right and select a new Grease buffer for the current directory."
  (interactive)
  (grease--split-window-with-new-buffer #'split-window-right))

;;;###autoload
(defun grease-split-window-below ()
  "Split below and select a new Grease buffer for the current directory."
  (interactive)
  (grease--split-window-with-new-buffer #'split-window-below))

;;;###autoload
(defvar grease--deleting-window-p nil
  "Non-nil while Grease is delegating an approved window deletion.")

(defun grease--advice-delete-window (original-function &optional window)
  "Dispose of a Grease buffer when ORIGINAL-FUNCTION deletes its last WINDOW."
  (let* ((target-window (or window (selected-window)))
         (buffer (and (window-live-p target-window)
                      (window-buffer target-window))))
    (if (or grease--deleting-window-p
            (not (buffer-live-p buffer))
            (not (with-current-buffer buffer
                   (derived-mode-p 'grease-mode))))
        (funcall original-function target-window)
      (let* ((last-window-p
              (= 1 (length (get-buffer-window-list buffer nil t))))
             (may-close-p
              (or (not last-window-p)
                  (with-current-buffer buffer
                    (not (or grease--buffer-dirty-p
                             grease--pending-changes)))
                  (with-current-buffer buffer
                    (grease-save-all-buffers)))))
        (when may-close-p
          (let ((grease--deleting-window-p t))
            (funcall original-function target-window))
          (when (and last-window-p
                     (buffer-live-p buffer)
                     (not (get-buffer-window buffer t)))
            (kill-buffer buffer))
          t)))))

(defun grease-close-window ()
  "Close the selected window, disposing of its last displayed Grease buffer."
  (interactive)
  (unless (derived-mode-p 'grease-mode)
    (user-error "Current buffer is not a Grease buffer"))
  (delete-window (selected-window)))

;;;###autoload
(defun grease-kill-hidden-buffers ()
  "Kill live Grease buffers that are not displayed in any window.
If any hidden buffer has staged changes, run the unified save first.  A
cancelled or failed save leaves every hidden buffer alive."
  (interactive)
  (let* ((hidden
          (cl-remove-if
           (lambda (buffer) (get-buffer-window buffer t))
           (grease--live-buffers)))
         (dirty-p
          (cl-some (lambda (buffer)
                     (with-current-buffer buffer
                       (or grease--buffer-dirty-p grease--pending-changes)))
                   hidden)))
    (if (and dirty-p (not (grease-save-all-buffers)))
        (progn
          (message "Grease: Hidden-buffer cleanup cancelled.")
          0)
      (let ((count 0))
        (dolist (buffer hidden)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)
            (cl-incf count)))
        (message "Grease: Killed %d hidden buffer%s."
                 count (if (= count 1) "" "s"))
        count))))

;;;###autoload
(defun grease-open (dir &optional target-file)
  "Open a new Grease buffer for DIR.
If TARGET-FILE is provided, position cursor on it."
  (interactive "DGrease directory: ")
  (switch-to-buffer (grease--create-buffer dir target-file)))

;;;###autoload
(defun grease-toggle ()
  "Toggle Grease buffer for the current project.
If already open, quit (saving position). Otherwise open in current directory.
If called from a preview buffer, close the associated grease buffer.
If the current directory does not exist, traverse up to find the first valid one."
  (interactive)
  (let* ((proj-root (file-name-as-directory (expand-file-name (grease--project-root))))
         (proj-name (grease--project-name))
         (bufname   (format "*grease:%s*" proj-name))
         (preview-bufname (format "*grease-preview:%s*" proj-name))
         (start-dir (expand-file-name default-directory))
         (valid-dir start-dir))

    ;; If we're in the preview buffer, switch to grease buffer and quit
    (when (string= (buffer-name) preview-bufname)
      (let ((grease-buf (cl-find-if (lambda (b) 
                                      (with-current-buffer b 
                                        (derived-mode-p 'grease-mode)))
                                    (buffer-list))))
        (when grease-buf
          (switch-to-buffer grease-buf)
          (grease-quit))
        (cl-return-from grease-toggle)))

    ;; Find nearest existing parent directory
    (while (and (not (file-exists-p valid-dir))
                (not (string= valid-dir "/"))
                (not (string= (directory-file-name valid-dir) valid-dir))) ;; Break if no change
      (setq valid-dir (file-name-directory (directory-file-name valid-dir))))

    (let ((target-file (if (string= start-dir valid-dir)
                           (when buffer-file-name (file-name-nondirectory buffer-file-name))
                         nil)))

      (if (derived-mode-p 'grease-mode)
          (grease-quit)
        (grease-open valid-dir target-file)))))

;;;###autoload
(defun grease-here ()
  "Open Grease buffer for the current project’s root.
If already open, quit (saving position). Otherwise open project root."
  (interactive)
  (let* ((proj-root (file-name-as-directory (expand-file-name (grease--project-root))))
         (proj-name (grease--project-name))
         (bufname   (format "*grease:%s*" proj-name)))
    (if (derived-mode-p 'grease-mode)
        (grease-quit)
      (grease-open proj-root))))

;; Integrate with save-buffer
(defun grease-advice-save-buffer (orig-fun &rest args)
  "Advice function to make `save-buffer` call `grease-save` in grease-mode."
  (if (derived-mode-p 'grease-mode)
      (grease-save)
    (apply orig-fun args)))

(advice-add 'save-buffer :around #'grease-advice-save-buffer)
;; Window managers often call the primitives directly, bypassing command
;; remapping.  These advices delegate unchanged for non-Grease source windows.
(advice-add 'split-window :around #'grease--advice-split-window)
(advice-add 'delete-window :around #'grease--advice-delete-window)

;; Reset file registry on initial load
(defun grease-reset-registry ()
  "Reset the file registry."
  (interactive)
  (setq grease--file-registry (make-hash-table :test 'eql))
  (setq grease--visited-dirs nil)
  (setq grease--session-id-counter 1)
  (message "Grease file registry reset."))

;; Add Evil leader key integration (example)
(when (and (fboundp 'evil-define-key*) (boundp 'evil-leader/map))
  (evil-leader/set-key-for-mode 'grease-mode
    "w" 'grease-save))
(defun grease-debug-state ()
  "Display current global clipboard and file registry for debugging."
  (interactive)
  (with-output-to-temp-buffer "*Grease Debug*"
    (princ "=== Grease Debug State ===\n\n")
    (princ "Clipboard:\n")
    (pp grease--clipboard)
    (princ "\nFile Registry:\n")
    (maphash (lambda (id data)
               (princ (format "  %s: %S\n" id data)))
             grease--file-registry)))

(provide 'grease)
;;; grease.el ends here
