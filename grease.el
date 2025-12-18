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
/(:path PATH :type TYPE :exists BOOL)")

;; Track directories we've visited
(defvar grease--visited-dirs nil
  "List of directories visited in the current session.")

;; Global clipboard for operations
(defvar grease--clipboard nil
  "Global clipboard for cross-directory operations.")

;; Track deleted file IDs and their original paths
(defvar grease--deleted-file-ids (make-hash-table :test 'eql)
  "Track deleted files by ID. Value is the original path.")

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
  "Hash table of original filenames -> file types.")

(defvar-local grease--buffer-dirty-p nil
  "Non-nil if buffer has unsaved changes.")

(defvar-local grease--change-hook-active nil
  "Flag to prevent recursive change hooks.")

;; List of pending operations to be applied on save
(defvar-local grease--pending-changes nil
  "List of pending file operations in current buffer.")

;; last directory and line number visited for cursor position persistence
(defvar grease--project-positions (make-hash-table :test 'equal)
  "Map project root -> plist of (:dir DIR :line LINE).")

;; Prefixes for hidden IDs
(defconst grease--id-prefix "/"
  "Prefix for hidden file IDs.")

;;;; Registry Functions

(defun grease--project-root ()
  "Return the root directory of the current project, or `default-directory`."
  (cond
   ((fboundp 'project-current)
    (when-let ((proj (project-current nil)))
      (car (project-roots proj))))
   ((fboundp 'projectile-project-root)
    (ignore-errors (projectile-project-root)))
   (t default-directory)))


(defun grease--project-name ()
  "Return a short name for the current project root."
  (file-name-nondirectory (directory-file-name (grease--project-root))))

(defun grease--register-file (path type &optional id)
  "Register PATH of TYPE in registry and return its ID.
If ID is provided, use that ID instead of generating a new one."
  (let* ((abs-path (expand-file-name path))
         (file-id (or id (cl-incf grease--session-id-counter))))
    ;; Store in registry
    (puthash file-id
             (list :path abs-path
                   :type type
                   :exists (file-exists-p abs-path))
             grease--file-registry)
    file-id))

(defun grease--mark-file-deleted (id path)
  "Mark file with ID at PATH as deleted."
  (let ((entry (gethash id grease--file-registry)))
    (when entry
      ;; Update registry to mark as non-existent
      (puthash id
               (plist-put entry :exists nil)
               grease--file-registry)
      ;; Track the ID and original path
      (puthash id path grease--deleted-file-ids))))

(defun grease--get-file-by-id (id)
  "Get file info for ID from registry."
  (gethash id grease--file-registry))

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
            ;; Only register if not already in registry
            (unless existing-id
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
  "Normalize NAME based on TYPE, ensuring directories have trailing slashes."
  (if (eq type 'dir)
      (if (grease--is-dir-name name) name (concat name "/"))
    (grease--strip-trailing-slash name)))

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
  "Get full path for NAME in current directory."
  (expand-file-name (grease--strip-trailing-slash name) grease--root-dir))

(defun grease--format-id (id)
  "Format ID as a 3-digit string with leading zeroes."
  (format "%03d" id))

(defun grease--extract-filename (text)
  "Extract just the filename from TEXT, removing ID and icon."
  ;; First try to match with hidden ID format
  (if (string-match (concat "^" grease--id-prefix "[0-9]+\\s-+\\(.*\\)$") text)
      ;; Got the text after the ID, now extract just the filename (after any icon)
      (let ((content (match-string 1 text)))
        ;; Look for the first alphanumeric or allowed special char that starts the filename
        (if (string-match "\\(?:[^\n[:alnum:]/._-]\\s-*\\)*\\([[:alnum:]/._-].*\\)$" content)
            (match-string 1 content)
          content)) ;; Fallback to the whole content
    ;; No ID, try to extract filename directly
    (if (string-match "\\(?:[^\n[:alnum:]/._-]\\s-*\\)*\\([[:alnum:]/._-].*\\)$" text)
        (match-string 1 text)
      ;; Last resort fallback
      (string-trim text))))

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
         (display-name (if is-dir (concat name "/") name))
         (id-str (grease--format-id id))
         (full-id (concat grease--id-prefix id-str))
         (start (point))
         (full-path (grease--get-full-path name)))

    ;; Register this file in registry if not already there
    (unless (grease--get-file-by-id id)
      (grease--register-file full-path type id))

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
                                'grease-name name
                                'grease-type type
                                'grease-source-id source-id
                                'grease-is-duplicate is-duplicate
                                'grease-full-path full-path))

      ;; Add face to the name part only
      (put-text-property name-start (point) 'face 'font-lock-function-name-face))

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
          (unless (and existing-id
                       (gethash existing-id grease--deleted-file-ids))
            (puthash (grease--normalize-name file type) type grease--original-state)
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
                (push (grease--get-full-path (plist-get data :name)) paths)))
            (forward-line 1)))
        
        ;; Store the multi-line selection data if we found files
        (when names
          (setq grease--multi-line-selection
                (list :paths (nreverse paths)
                      :names (nreverse names)
                      :types (nreverse types)
                      :ids (nreverse ids)
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
        (let ((names '()) (types '()) (ids '()) (paths '()))
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (let ((data (grease--get-line-data)))
                (when data
                  (push (plist-get data :name) names)
                  (push (plist-get data :type) types)
                  (push (plist-get data :id) ids)
                  (push (grease--get-full-path (plist-get data :name)) paths)))
              (forward-line 1)))
          (when names
            (setq grease--clipboard
                  (list :paths (nreverse paths)
                        :names (nreverse names)
                        :types (nreverse types)
                        :ids (nreverse ids)
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
                   (id   (plist-get data :id)))
              (setq grease--clipboard
                    (list :paths (list path)
                          :names (list name)
                          :types (list type)
                          :ids (list id)
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
      (let ((names '()) (types '()) (ids '()) (paths '()))
        (save-excursion
          (goto-char (region-beginning))
          (let ((end (save-excursion (goto-char (region-end)) (line-end-position))))
            (while (< (point) end)
              (let ((data (grease--get-line-data)))
                (when data
                  (let* ((name (plist-get data :name))
                         (type (plist-get data :type))
                         (id   (plist-get data :id))
                         (path (grease--get-full-path name)))
                    (push name names)
                    (push type types)
                    (push id ids)
                    (push path paths)
                    (when id (grease--mark-file-deleted id path)))))
              (forward-line 1))))
        (setq grease--clipboard
              (list :paths (nreverse paths)
                    :names (nreverse names)
                    :types (nreverse types)
                    :ids   (nreverse ids)
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
                 (id   (plist-get data :id)))
            (setq grease--clipboard
                  (list :paths (list path)
                        :names (list name)
                        :types (list type)
                        :ids   (list id)
                        :original-dir grease--root-dir
                        :operation 'cut))
            (setq grease--last-op-type 'cut)
            (setq grease--last-kill-index 0)
            (when id (grease--mark-file-deleted id path))
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
          ;; Create a new ID but reference the source
          (grease--insert-entry (grease--get-next-id) name type source-id t)))

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
      (forward-line 1) ; Skip header
      (while (not (eobp))
        (let* ((line-beg (line-beginning-position))
               (line-end (line-end-position))
               (line-data (grease--get-line-data (point))))

          ;; Only process lines with our metadata
          (when line-data
            (let* ((visible-text (buffer-substring-no-properties
                                  line-beg line-end))
                   (clean-text (grease--extract-filename visible-text))
                   (is-dir (grease--is-dir-name clean-text))
                  (name (grease--normalize-name clean-text (if is-dir 'dir 'file)))
                  (type (if is-dir 'dir 'file))
                   (full-path (grease--get-full-path name)))

              ;; Update text properties with new name
              (when (get-text-property line-beg 'grease-name)
                (put-text-property line-beg line-end 'grease-name name)
                (put-text-property line-beg line-end 'grease-type type)
                (put-text-property line-beg line-end 'grease-full-path full-path)))))

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
                                     (grease--normalize-name file-name type)
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
                   (data (list :name name
                               :type type
                               :id id
                               :source-id source-id
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

(defun grease--check-for-renames (new-entries original-state)
  "Detect file renames by comparing NEW-ENTRIES with ORIGINAL-STATE.
Returns a list of rename operations to be performed."
  (let ((renames '())
        (seen-ids (make-hash-table :test 'eql))
        (seen-names (make-hash-table :test 'equal)))
    
    ;; First mark all IDs that are still present
    (dolist (entry new-entries)
      (let ((id (plist-get entry :id))
            (name (plist-get entry :name)))
        (when id
          (puthash id name seen-ids)
          (puthash name t seen-names))))
    
    ;; Check for files that had their name changed but ID preserved
    (maphash (lambda (orig-name _orig-type)
               (let* ((orig-path (grease--get-full-path orig-name))
                      (id (grease--get-id-by-path orig-path)))
                 (when id
                   (let ((new-name (gethash id seen-ids)))
                     (when (and new-name 
                                (not (equal new-name orig-name))
                                (not (gethash orig-name seen-names)))
                       ;; Found a rename: same ID, different name, old name not present
                       (let ((old-path (grease--get-full-path orig-name))
                             (new-path (grease--get-full-path new-name)))
                         (push `(:rename ,old-path ,new-path) renames)))))))
             original-state)
    
    renames))

(defun grease--calculate-changes ()
  "Calculate the changes between original state and current buffer."
  (let* ((entries (grease--scan-buffer))
         (changes '())
         (original-files (copy-hash-table grease--original-state))
         (seen-originals (make-hash-table :test 'equal))
         (seen-ids (make-hash-table :test 'eql))
         ;; detect conflicts first
         (name-conflicts (grease--detect-name-conflicts entries))
         (renames (grease--check-for-renames entries original-files)))

    ;; Abort immediately if name conflicts exist
    (when name-conflicts
      (user-error "Filename conflicts detected in this directory: %s"
                  (mapconcat #'identity name-conflicts ", ")))

    ;; Add detected renames to changes
    (setq changes (append renames changes))

    ;; First process current buffer entries
    (dolist (entry entries)
      (let ((name (plist-get entry :name))
            (type (plist-get entry :type))
            (id (plist-get entry :id))
            (source-id (plist-get entry :source-id))
            (is-duplicate (plist-get entry :is-duplicate))
            (full-path (plist-get entry :full-path))
            (is-new (plist-get entry :is-new)))

        ;; Track this ID as seen
        (when id
          (puthash id t seen-ids))

        ;; Track this name as seen in original files (if it exists there)
        (when (gethash name original-files)
          (puthash name t seen-originals))

        (cond
         ;; Skip files that are part of rename operations
         ((cl-find-if (lambda (change)
                        (and (eq (car change) :rename)
                             (equal (grease--get-full-path name) (nth 2 change))))
                      renames))

         ;; Case 1: File copied from another file via source-id
         ((and source-id (not (eq source-id id)))
          (let* ((source-info (grease--get-file-by-id source-id))
                 (source-path (plist-get source-info :path)))
            (when source-path
              ;; Add a copy operation from source to destination
              (push `(:copy ,source-path ,(grease--get-full-path name)) changes))))

         ;; Case 2: A duplicated line within the same directory
         (is-duplicate
          (let* ((matching-entries
                  (cl-remove-if-not
                   (lambda (e)
                     (and (string= (plist-get e :name) name)
                          (not (plist-get e :is-duplicate))))
                   entries))
                 (source-entry (car matching-entries)))
            (when source-entry
              (push `(:copy ,(grease--get-full-path (plist-get source-entry :name))
                      ,(grease--get-full-path name))
                    changes))))

         ;; Case 3: Was this file previously deleted and moved here?
         ((and id (gethash id grease--deleted-file-ids))
          (let ((src-path (gethash id grease--deleted-file-ids))
                (dst-path (grease--get-full-path name)))
            (when (and src-path (not (equal src-path dst-path)))
              ;; This is a moved file - add as move operation
              (push `(:move ,src-path ,dst-path) changes))))

         ;; Case 4: A newly created file
         (is-new
          (push `(:create ,(grease--get-full-path name)) changes))

         ;; Case 6: New file that doesn't match original state and isn't a rename
         ((not (gethash name original-files))
          (push `(:create ,(grease--get-full-path name)) changes)))))

    ;; Process deletions - any original file not seen in the current buffer
    (maphash (lambda (name type)
               (unless (gethash name seen-originals)
                 (let* ((path (grease--get-full-path name))
                        (id (grease--get-id-by-path path)))
                   ;; Skip if the file was renamed or moved
                   (unless (or (and id (gethash id seen-ids))
                               (cl-find-if (lambda (change)
                                            (and (eq (car change) :rename)
                                                 (equal path (nth 1 change))))
                                          renames))
                     ;; File was deleted
                     (push `(:delete ,path) changes)))))
             original-files)

    ;; Sort changes for consistent application order (deletes first)
    (sort changes (lambda (a b) (if (eq (car a) :delete) t (not (eq (car b) :delete)))))))

(defun grease--apply-changes (changes)
  "Apply CHANGES to the filesystem."
  (let ((errors '()))
    (dolist (change changes)
      (pcase change
        (`(:create ,path)
         (message "Grease: Creating %s" path)
         (condition-case e 
             (if (string-suffix-p "/" path)
                 (make-directory path t)
               (make-directory (file-name-directory path) t)
               (write-region "" nil path t)) ; 't' creates the file
           (error (push (format "Failed to create %s: %s" path e) errors))))

        (`(:delete ,path)
         (message "Grease: Deleting %s" path)
         (condition-case e (if (file-directory-p path)
                             (delete-directory path t)
                           (delete-file path))
           (error (push (format "Failed to delete %s: %s" path e) errors))))

        (`(:rename ,old-path ,new-path)
         (message "Grease: Renaming %s -> %s" old-path new-path)
         (condition-case e 
             (progn
               (make-directory (file-name-directory new-path) t)
               (rename-file old-path new-path t))
           (error (push (format "Failed to rename %s: %s" old-path e) errors))))

        (`(:copy ,src-path ,dst-path)
         (if (string= src-path dst-path)
             (message "Grease: Skipping copy to same location %s" src-path)
           (message "Grease: Copying %s -> %s" src-path dst-path)
           (condition-case e
               (progn
                 (make-directory (file-name-directory dst-path) t)
                 (if (file-directory-p src-path)
                     (copy-directory src-path dst-path)
                   (copy-file src-path dst-path t)))
             (error (push (format "Failed to copy %s: %s" src-path e) errors)))))

        (`(:move ,src-path ,dst-path)
         (message "Grease: Moving %s -> %s" src-path dst-path)
         (condition-case e
             (progn
               (make-directory (file-name-directory dst-path) t)
               (if (file-directory-p src-path)
                   (copy-directory src-path dst-path)
                 (copy-file src-path dst-path t))
               (if (file-directory-p src-path)
                   (delete-directory src-path t)
                 (delete-file src-path)))
           (error (push (format "Failed to move %s: %s" src-path e) errors))))))

    ;; Clear all pending changes after successful operations
    (when (and changes (not errors))
      (setq grease--pending-changes nil)
      (clrhash grease--deleted-file-ids))

    (if errors
        (warn "Grease: Encountered errors:\n%s" (mapconcat #'identity errors "\n"))
      (message "Grease: All changes applied successfully."))))

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
          (run-with-idle-timer 0.1 nil #'grease--update-preview))))

(defun grease--update-preview ()
  "Update the preview buffer with content from file at point."
  (when (and grease--preview-window
             (window-live-p grease--preview-window)
             grease--preview-buffer
             (buffer-live-p grease--preview-buffer))
    (let ((data (grease--get-line-data)))
      (when data
        (let* ((name (plist-get data :name))
               (type (plist-get data :type))
               (path (grease--get-full-path name))
               (is-file (and (eq type 'file)
                             (file-exists-p path)
                             (file-readable-p path))))
          (with-current-buffer grease--preview-buffer
            (let ((inhibit-read-only t))
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
              (goto-char (point-min))
              ;; Set read-only based on type and setting
              ;; Directories are always read-only, files respect grease-preview-writable
              (setq buffer-read-only (or (eq type 'dir)
                                         (not grease-preview-writable))))))))))

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

(defun grease-save ()
  "Save changes to the filesystem."
  (interactive)
  (let ((changes (grease--calculate-changes)))
    (if (not changes)
        (message "Grease: No changes to save.")
      (let* ((prompt (format "Apply these changes? (y=yes, n=cancel, d=discard)\n%s\n"
                             (mapconcat #'grease--format-change changes "\n")))
             (choice (read-char-choice prompt '(?y ?n ?d))))
        (pcase choice
          (?y
           (grease--apply-changes changes)
           (grease--render grease--root-dir)
           t)
          (?d
           (setq grease--pending-changes nil
                 grease--clipboard nil
                 grease--buffer-dirty-p nil)
           (clrhash grease--deleted-file-ids)
           (grease--render grease--root-dir)
           (message "Grease: Discarded all pending changes.")
           t)
          (_ (message "Grease: Save cancelled.") nil))))))

(defun grease-duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((data (grease--get-line-data)))
    (when data
      (let* ((name (plist-get data :name))
             (type (plist-get data :type))
             (id (plist-get data :id))
             (next-id (grease--get-next-id)))
        ;; Add a new line with the duplicated content
        (end-of-line)
        (insert "\n")
        (grease--insert-entry next-id name type id t)
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
        ;; Clipboard = CUT (pending move)
        (setq grease--clipboard
              (list :paths (list path)
                    :names (list name)
                    :types (list type)
                    :ids (list id)
                    :original-dir grease--root-dir
                    :operation 'cut))
        (setq grease--last-op-type 'cut)
        (setq grease--last-kill-index 0)

        ;; Mark file as deleted by ID
        (when id (grease--mark-file-deleted id path))

        ;; Delete the line visually
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
        ;; Store in clipboard
        (setq grease--clipboard
              (list :paths (list path)
                    :names (list name)
                    :types (list type)
                    :ids (list id)
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
      (cl-loop for path in paths
               for name in names
               for type in types
               for id   in ids
               do
               (let* ((next-id     (grease--get-next-id))
                      (copying     (eq operation 'copy))
                      (moving      (memq operation '(move cut))) ; treat cut as move
                      (target-name (if (and copying (not is-cross-dir))
                                       (if (eq type 'dir)
                                           (concat (grease--strip-trailing-slash name) "-copy/")
                                         (grease--add-copy-suffix name))
                                     name)))
                 (cond
                  (copying
                   (grease--insert-entry next-id target-name type id t))
                  (moving
                   ;; Keep same ID for cut/move
                   (grease--insert-entry id name type nil nil))
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
              ;; Store any changes before moving
              (when grease--buffer-dirty-p
                (let ((changes (grease--calculate-changes)))
                  (when changes
                    (setq grease--pending-changes
                          (append changes grease--pending-changes)))))
              (grease--render path t)
              ;; Restore cursor position, clamped to valid lines
              (grease--goto-line-clamped current-line))
          (grease--with-commit-prompt
           (lambda ()
             (kill-buffer (current-buffer))
             (find-file path))))))))

(defun grease-up-directory ()
  "Move to the parent directory."
  (interactive)
  (grease--save-position)
  (let ((parent-dir (expand-file-name ".." grease--root-dir))
        (current-line (line-number-at-pos)))
    ;; Store any changes before moving
    (when grease--buffer-dirty-p
      (let ((changes (grease--calculate-changes)))
        (when changes
          (setq grease--pending-changes
                (append changes grease--pending-changes)))))
    (grease--render parent-dir t)
    ;; Restore cursor position, clamped to valid lines
    (grease--goto-line-clamped current-line)))

(defun grease-refresh ()
  "Discard all changes and reload the directory from disk."
  (interactive)
  (if (and grease--buffer-dirty-p
           (not (y-or-n-p "Discard all uncommitted changes?")))
      (message "Refresh cancelled.")
    (setq grease--pending-changes nil)
    (setq grease--clipboard nil)
    (clrhash grease--deleted-file-ids)
    ;; Re-scan the directory on disk
    (grease--render grease--root-dir)
    (message "Grease: Refreshed.")))

(defun grease-quit ()
  "Quit the grease buffer, prompting to save or discard changes."
  (interactive)
  (grease--save-position)
  (if (or grease--buffer-dirty-p grease--pending-changes)
      (let* ((changes (grease--calculate-changes))
             (prompt (format "Save changes before quitting? (y=yes, n=cancel, d=discard)\n%s\n"
                             (mapconcat #'grease--format-change changes "\n")))
             (choice (read-char-choice prompt '(?y ?n ?d))))
        (pcase choice
          (?y
           (grease-save)
           (kill-buffer (current-buffer)))
          (?d
           (setq grease--pending-changes nil
                 grease--clipboard nil
                 grease--buffer-dirty-p nil)
           (clrhash grease--deleted-file-ids)
           (kill-buffer (current-buffer))
           (message "Grease: Discarded changes and quit."))
          (_ (message "Quit cancelled."))))
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

(define-derived-mode grease-mode prog-mode "Grease"
  "A major mode for oil.nvim-style file management."
  :syntax-table nil
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

;;;###autoload
(defun grease-open (dir &optional target-file)
  "Open a Grease buffer for DIR, reusing the project’s Grease buffer.
If TARGET-FILE is provided, position cursor on it."
  (interactive "DGrease directory: ")
  (let* ((proj-root (file-name-as-directory (expand-file-name (grease--project-root))))
         (proj-name (grease--project-name))
         (bufname   (format "*grease:%s*" proj-name)))
    (with-current-buffer (get-buffer-create bufname)
      (grease-mode)
      ;; Always render the requested directory
      (grease--render dir)
      (if target-file
          (grease--goto-file target-file)
        ;; Default to top
        (goto-char (point-min))
        (forward-line 1)
        (grease--constrain-cursor)))
    (switch-to-buffer bufname)))

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
      (let ((grease-buf (get-buffer bufname)))
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

      (if (and (derived-mode-p 'grease-mode)
               (string= (buffer-name) bufname))
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
    (if (and (derived-mode-p 'grease-mode)
             (string= (buffer-name) bufname))
        (grease-quit)
      (grease-open proj-root))))

;; Integrate with save-buffer
(defun grease-advice-save-buffer (orig-fun &rest args)
  "Advice function to make `save-buffer` call `grease-save` in grease-mode."
  (if (derived-mode-p 'grease-mode)
      (grease-save)
    (apply orig-fun args)))

(advice-add 'save-buffer :around #'grease-advice-save-buffer)

;; Reset file registry on initial load
(defun grease-reset-registry ()
  "Reset the file registry."
  (interactive)
  (setq grease--file-registry (make-hash-table :test 'eql))
  (setq grease--visited-dirs nil)
  (setq grease--session-id-counter 1)
  (setq grease--deleted-file-ids (make-hash-table :test 'eql))
  (message "Grease file registry reset."))

;; Add Evil leader key integration (example)
(when (and (fboundp 'evil-define-key*) (boundp 'evil-leader/map))
  (evil-leader/set-key-for-mode 'grease-mode
    "w" 'grease-save))
(defun grease-debug-state ()
  "Display current global clipboard and deleted IDs for debugging."
  (interactive)
  (with-output-to-temp-buffer "*Grease Debug*"
    (princ "=== Grease Debug State ===\n\n")
    (princ "Clipboard:\n")
    (pp grease--clipboard)
    (princ "\nDeleted IDs:\n")
    (maphash (lambda (id path)
               (princ (format "  %s -> %s\n" id path)))
             grease--deleted-file-ids)
    (princ "\nFile Registry:\n")
    (maphash (lambda (id data)
               (princ (format "  %s: %S\n" id data)))
             grease--file-registry)))

(provide 'grease)
;;; grease.el ends here
