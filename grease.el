;;; grease.el --- An oil.nvim-style file manager for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Grease provides a simple, text-based interface for managing files.
;; You edit a directory listing as if it were a normal text file using
;; standard Evil (or Emacs) commands, then commit your changes to the filesystem.
;;
;; - Edit a filename to rename it (`ciw`).
;; - Delete a line to delete the file (`dd`).
;; - Create a file by adding a new line (`o`, `O`). End with "/" for a directory.
;; - Copy/paste lines to copy files (`yy`, `p`).
;; - Cut/paste lines to move files (`dd`, `p`).
;; - Use visual mode to select and edit multiple lines at once.
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
(defconst grease--use-icons (featurep 'nerd-icons)
  "Non-nil to display icons next to filenames.")

(defconst grease--have-icons (and grease--use-icons (featurep 'nerd-icons))
  "Non-nil if nerd-icons package is available and enabled.")

;;;; Global State and File Tracking

;; File tracking system - keeps track of all files by ID
(defvar grease--file-registry (make-hash-table :test 'eql)
  "Registry of all files seen during the current session.
Each entry is keyed by unique ID and contains:
(:path PATH :type TYPE :exists BOOL)")

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

;; Prefixes for hidden IDs
(defconst grease--id-prefix "/"
  "Prefix for hidden file IDs.")

;;;; Registry Functions

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

(defun grease--get-icon (name)
  "Get appropriate icon for NAME."
  (if grease--have-icons
      (if (grease--is-dir-name name)
          (nerd-icons-icon-for-dir name)
        (nerd-icons-icon-for-file name))
    (if (grease--is-dir-name name) "📁 " "📄 ")))

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
    (when grease--use-icons
      (let* ((icon-start (point))
             (icon (grease--get-icon display-name)))
        (insert icon)
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

(defun grease--render (dir &optional keep-changes)
  "Render the contents of DIR into the current buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (next-id 1))
    ;; Make sure the buffer is not read-only
    (setq buffer-read-only nil)

    ;; Register the directory in our visited directories list
    (setq grease--root-dir (file-name-as-directory (expand-file-name dir)))
    (grease--register-directory grease--root-dir)

    ;; Setup buffer state
    (erase-buffer)
    (setq grease--original-state (make-hash-table :test 'equal))

    ;; Clear pending changes if not keeping them
    (unless keep-changes
      (setq grease--pending-changes nil))

    (setq grease--buffer-dirty-p nil)

    ;; Insert header
    (let ((header-start (point)))
      (insert (format " Greasy Fork — %s\n" grease--root-dir))
      (add-text-properties header-start (point)
                           '(read-only t front-sticky nil face mode-line-inactive)))

    ;; Insert files
    (let* ((all-files (directory-files grease--root-dir nil nil t))
           (files (cl-remove-if (lambda (f) (member f '("." ".."))) all-files)))
      (dolist (file (sort files #'string<))
        (let* ((abs-path (expand-file-name file grease--root-dir))
               (type (if (file-directory-p abs-path) 'dir 'file))
               (existing-id (grease--get-id-by-path abs-path)))

          ;; Record original state for this directory
          (puthash file type grease--original-state)

          ;; Insert the entry
          (grease--insert-entry
           (or existing-id (cl-incf grease--session-id-counter))
           file type nil nil))))

    ;; Always add an empty editable line at the end for adding new files
    (let ((start (point)))
      (insert "\n")
      ;; Mark this line as specially editable
      (put-text-property start (point) 'grease-editable t))

    (goto-char (point-min))
    (forward-line 1)))

;;;; Cursor Control and Evil Integration

(defun grease--constrain-cursor ()
  "Ensure cursor is positioned after the file prefix and icon."
  (when (and (derived-mode-p 'grease-mode)
             (not (= (line-number-at-pos) 1))) ; Skip header line
    (let ((pos (point))
          (bol (line-beginning-position))
          (prefix-end nil))

      ;; If this is an editable blank line, allow cursor anywhere
      (unless (get-text-property bol 'grease-editable)
        ;; Find the end of the prefix (ID + icon)
        (save-excursion
          (goto-char bol)
          (if (re-search-forward (concat grease--id-prefix "[0-9]+\\s") (line-end-position) t)
              (setq prefix-end (point))
            (setq prefix-end bol)))

        ;; If icon exists, move past it too
        (when (and prefix-end (> prefix-end bol))
          (save-excursion
            (goto-char prefix-end)
            (while (and (< prefix-end (line-end-position))
                        (get-text-property prefix-end 'grease-icon))
              (setq prefix-end (1+ prefix-end)))))

        ;; Move cursor if it's before the filename part
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
(defun grease--on-evil-yank (beg end &rest _)
  "Handle yank operation for clipboard."
  (when (derived-mode-p 'grease-mode)
    (grease--mark-as-grease-op)
    (if grease--multi-line-selection
        ;; Handle multi-line selection
        (setq grease--clipboard 
              (append grease--multi-line-selection
                      (list :operation 'copy)))
      ;; Handle single line yank
      (let ((data (grease--get-line-data)))
        (when data
          (let* ((name (plist-get data :name))
                 (type (plist-get data :type))
                 (path (grease--get-full-path name))
                 (id (plist-get data :id)))
            ;; Store in clipboard with additional metadata for proper copy handling
            (setq grease--clipboard
                  (list :paths (list path)
                        :names (list name)
                        :types (list type)
                        :ids (list id)
                        :original-dir grease--root-dir
                        :operation 'copy))
            (message "Copied file: %s" name)))))))

(defun grease--before-evil-delete (&rest _)
  "Handle delete operation for clipboard, run BEFORE the actual delete."
  (when (derived-mode-p 'grease-mode)
    (grease--mark-as-grease-op)
    (if grease--multi-line-selection
        ;; Handle multi-line selection
        (let ((ids (plist-get grease--multi-line-selection :ids))
              (paths (plist-get grease--multi-line-selection :paths))
              (names (plist-get grease--multi-line-selection :names)))
          ;; Save the multi-selection for after the delete completes
          (setq grease--deleted-selection grease--multi-line-selection)
          
          ;; Store in clipboard
          (setq grease--clipboard 
                (append grease--multi-line-selection
                        (list :operation 'move)))
          
          ;; Mark files as deleted
          (cl-loop for id in ids
                   for path in paths
                   when id
                   do (grease--mark-file-deleted id path))
          
          (message "Cut %d file%s" 
                   (length names) 
                   (if (= (length names) 1) "" "s")))
      
      ;; Handle single line delete  
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
                        :operation 'move))

            ;; CRITICAL: Mark this file ID as deleted with its original path
            (when id
              (grease--mark-file-deleted id path))

            (message "Cut file: %s" name)))))))

(defun grease--after-evil-delete (&rest _)
  "Handle operations after a delete action completes."
  (when (and (derived-mode-p 'grease-mode) 
             grease--deleted-selection)
    ;; Restore the operation type to ensure paste works
    (setq grease--last-op-type 'file)
    (setq grease--last-kill-index 0)
    ;; Clear the saved selection (but keep clipboard data)
    (setq grease--deleted-selection nil)))

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
  "Track yanked TEXT from grease buffer and update clipboard if needed."
  (when (derived-mode-p 'grease-mode)
    (setq grease--last-yanked-text text)

    ;; Try to extract file info from yanked text
    (let ((info (grease--extract-line-info-from-text text)))
      (when info
        ;; Update clipboard to store info about the yanked file
        (setq grease--clipboard
              (list :paths (list (plist-get info :path))
                    :names (list (plist-get info :name))
                    :types (list (plist-get info :type))
                    :ids (list (plist-get info :id))
                    :original-dir grease--root-dir
                    :operation 'copy))))))

(defun grease--get-next-id ()
  "Get the next available ID in the buffer."
  (cl-incf grease--session-id-counter))

(defun grease--handle-paste-line (line-text)
  "Process pasted LINE-TEXT to properly format it for grease buffer."
  (when (not (string-empty-p line-text))
    (let* ((id-from-text (grease--extract-id line-text))
           (clipboard-op (and grease--clipboard (plist-get grease--clipboard :operation)))
           (clipboard-id (and grease--clipboard (car (plist-get grease--clipboard :ids))))
           (is-move (and (eq clipboard-op 'move)
                         id-from-text
                         (eq id-from-text clipboard-id))))

      ;; Clear the current line content before inserting the new entry
      (delete-region (line-beginning-position) (line-end-position))

      (cond
       ;; Case 1: This paste completes a MOVE operation.
       (is-move
        (let* ((file-info (grease--extract-line-info-from-text line-text))
               (id (plist-get file-info :id))
               (name (plist-get file-info :name))
               (type (plist-get file-info :type)))
          ;; Re-insert the entry with its original ID, completing the move.
          (grease--insert-entry id name type nil nil)))

       ;; Case 2: Pasted text has an ID, but it's a COPY.
       (id-from-text
        (let* ((file-info (grease--extract-line-info-from-text line-text))
               (source-id (plist-get file-info :id))
               (name (plist-get file-info :name))
               (type (plist-get file-info :type)))
          ;; Create a new ID but reference the source.
          (grease--insert-entry (grease--get-next-id) name type source-id t)))

       ;; Case 3: Pasted text is plain text (no ID), so it's a new file.
       (t
        (let* ((file-name (grease--extract-filename line-text))
               (is-dir (grease--is-dir-name file-name))
               (type (if is-dir 'dir 'file)))
          ;; Treat as a new file creation.
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
  "Intercept paste commands to use grease-paste when appropriate."
  (if (and (derived-mode-p 'grease-mode)
           ;; Only use grease-paste when:
           (eq grease--last-op-type 'file)  ;; Last op was a file operation
           (eq grease--last-kill-index 0)   ;; Current kill is the most recent
           grease--clipboard)               ;; We have clipboard data
      ;; Use grease-paste for file operations
      (grease-paste)
    ;; Use normal paste for text operations
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
                   (name (grease--strip-trailing-slash clean-text))
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
                                     (grease--strip-trailing-slash file-name)
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

(defun grease--format-change (change)
  "Format CHANGE for display in confirmation prompt."
  (pcase change
    (`(:create ,path)
     (format "  [Create] %s" (file-name-nondirectory path)))
    (`(:delete ,path)
     (format "  [Delete] %s" (file-name-nondirectory path)))
    (`(:rename ,old ,new)
     (format "  [Rename] %s -> %s"
             (file-name-nondirectory old)
             (file-name-nondirectory new)))
    (`(:move ,src ,dst)
     (format "  [Move]   %s -> %s"
             (file-name-nondirectory src)
             (file-name-nondirectory dst)))
    (`(:copy ,src ,dst)
     (format "  [Copy]   %s -> %s"
             (file-name-nondirectory src)
             (file-name-nondirectory dst)))))

(defun grease--detect-name-conflicts (entries)
  "Check for duplicate filenames that aren't marked as duplicates in ENTRIES."
  (let ((names (make-hash-table :test 'equal))
        (conflicts '()))

    (dolist (entry entries)
      (let* ((name (plist-get entry :name))
             (is-duplicate (plist-get entry :is-duplicate)))
        ;; Only count non-duplicates for conflict detection
        (unless is-duplicate
          (let ((count (gethash name names 0)))
            (when (> count 0)
              (push name conflicts))
            (puthash name (1+ count) names)))))

    conflicts))

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
         (name-conflicts (grease--detect-name-conflicts entries))
         (renames (grease--check-for-renames entries original-files)))

    ;; Check for naming conflicts
    (when name-conflicts
      (user-error "Filename conflicts detected: %s" (mapconcat #'identity name-conflicts ", ")))

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
         (condition-case e (if (string-suffix-p "/" path)
                             (make-directory path)
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
         (condition-case e (rename-file old-path new-path t)
           (error (push (format "Failed to rename %s: %s" old-path e) errors))))

        (`(:copy ,src-path ,dst-path)
         (if (string= src-path dst-path)
             (message "Grease: Skipping copy to same location %s" src-path)
           (message "Grease: Copying %s -> %s" src-path dst-path)
           (condition-case e
               (if (file-directory-p src-path)
                   (copy-directory src-path dst-path)
                 (copy-file src-path dst-path t))
             (error (push (format "Failed to copy %s: %s" src-path e) errors)))))

        (`(:move ,src-path ,dst-path)
         (message "Grease: Moving %s -> %s" src-path dst-path)
         (condition-case e
             (progn
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

;;;; User Commands

(defun grease-save ()
  "Save changes to the filesystem."
  (interactive)
  (let ((changes (grease--calculate-changes)))
    (if (not changes)
        (message "Grease: No changes to save.")
      (if (y-or-n-p (format "Apply these changes?\n%s\n" (mapconcat #'grease--format-change changes "\n")))
          (progn
            (grease--apply-changes changes)
            (grease--render grease--root-dir)
            t)
        (message "Grease: Save cancelled.") nil))))

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
        ;; Store in clipboard
        (setq grease--clipboard
              (list :paths (list path)
                    :names (list name)
                    :types (list type)
                    :ids (list id)
                    :original-dir grease--root-dir
                    :operation 'move))
        
        ;; Mark this as a file operation
        (setq grease--last-op-type 'file)
        (setq grease--last-kill-index 0)
        
        ;; Mark file as deleted by ID
        (when id
          (grease--mark-file-deleted id path))

        ;; Delete the line
        (delete-region (line-beginning-position) (line-end-position))
        (when (eobp) (delete-char -1))
        (setq grease--buffer-dirty-p t)
        (message "Cut file: %s" name)))))

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
    (let* ((operation (plist-get grease--clipboard :operation))
           (paths (plist-get grease--clipboard :paths))
           (names (plist-get grease--clipboard :names))
           (types (plist-get grease--clipboard :types))
           (ids (plist-get grease--clipboard :ids))
           (original-dir (plist-get grease--clipboard :original-dir))
           (is-cross-dir (not (string= original-dir grease--root-dir)))
           (total-count (length names)))

      ;; Insert new entries for each item
      (cl-loop for path in paths
               for name in names
               for type in types
               for id in ids
               for i from 0
               do
               (let* ((next-id (grease--get-next-id))
                      ;; For same-directory copies, we need to handle duplicate names
                      (target-name (if (and (eq operation 'copy)
                                           (not is-cross-dir))
                                      (if (eq type 'dir)
                                          (concat (grease--strip-trailing-slash name) "-copy/")
                                        (grease--add-copy-suffix name))
                                    name)))
                 ;; Insert the new entry
                 (save-excursion
                   (end-of-line)
                   (insert "\n")
                   (beginning-of-line)
                   ;; For copies, use source ID as source-id; for moves keep ID
                   (if (eq operation 'copy)
                       (grease--insert-entry next-id target-name type id t)
                     ;; Keep the same ID for moves
                     (grease--insert-entry id name type nil nil)))))

      ;; Keep the clipboard data valid to allow multiple pastes
      (setq grease--last-op-type 'file)
      (setq grease--last-kill-index 0)
      
      ;; Mark buffer as dirty
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
  (let ((data (grease--get-line-data)))
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
              (grease--render path t))
          (grease--with-commit-prompt
           (lambda ()
             (kill-buffer (current-buffer))
             (find-file path))))))))

(defun grease-up-directory ()
  "Move to the parent directory."
  (interactive)
  (let ((parent-dir (expand-file-name ".." grease--root-dir)))
    ;; Store any changes before moving
    (when grease--buffer-dirty-p
      (let ((changes (grease--calculate-changes)))
        (when changes
          (setq grease--pending-changes
                (append changes grease--pending-changes)))))
    (grease--render parent-dir t)))

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
  "Quit the grease buffer, prompting to save changes."
  (interactive)
  (if (or grease--buffer-dirty-p grease--pending-changes)
      (when (y-or-n-p "Save changes before quitting?")
        (grease-save))
    (kill-buffer (current-buffer))))

;;;; Major Mode Definition

(defvar grease-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'grease-save)
    (define-key map (kbd "C-c C-d") #'grease-duplicate-line)
    (define-key map (kbd "C-c C-x") #'grease-cut)
    (define-key map (kbd "C-c C-c") #'grease-copy)
    (define-key map (kbd "C-c C-v") #'grease-paste)
    map)
  "Keymap for `grease-mode'.")

(define-derived-mode grease-mode prog-mode "Grease"
  "A major mode for oil.nvim-style file management."
  :syntax-table nil
  (setq-local truncate-lines t)
  (setq buffer-read-only nil) ;; Ensure buffer is not read-only
  (add-hook 'after-change-functions #'grease--on-change nil t)
  (grease--setup-cursor-constraints))

;; Set up Evil keybindings
(when (fboundp 'evil-define-key*)
  (evil-define-key* 'normal grease-mode-map
    (kbd "RET") #'grease-visit
    (kbd "-") #'grease-up-directory
    (kbd "g r") #'grease-refresh)
    ; (kbd "q") #'grease-quit
    ; (kbd "y y") #'grease-copy
    ; (kbd "d d") #'grease-cut)
  )

;;;; Entry Points

;;;###autoload
(defun grease-open (dir)
  "Open a Grease buffer for DIR in full window."
  (interactive "DGrease directory: ")
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (bufname (format "*grease: %s*" dir))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (grease-mode)
      (grease--render dir))
    (switch-to-buffer buf)))

;;;###autoload
(defun grease-toggle ()
  "Toggle between current buffer and Grease for the current directory."
  (interactive)
  (if (derived-mode-p 'grease-mode)
      (grease-quit)
    (grease-open default-directory)))

;;;###autoload
(defun grease-here ()
  "Open Grease for the current directory, or quit if already in a Grease buffer."
  (interactive)
  (if (derived-mode-p 'grease-mode)
      (grease-quit)
    (let* ((dir (file-name-as-directory default-directory))
           (bufname (format "*grease: %s*" dir))
           (buf (get-buffer bufname)))
      (if buf
          (switch-to-buffer buf)
        (grease-open dir)))))

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

(provide 'grease)
;;; grease.el ends here
