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

;;;; Buffer-Local State

(defvar-local grease--root-dir nil
  "Current directory being displayed.")

(defvar-local grease--original-state nil
  "Hash table of original filenames -> file types.")

(defvar-local grease--buffer-dirty-p nil
  "Non-nil if buffer has unsaved changes.")

(defvar-local grease--change-hook-active nil
  "Flag to prevent recursive change hooks.")

;; Enhanced global clipboard for cross-directory operations
(defvar grease--clipboard nil
  "Global clipboard for cross-directory operations.
Structure: (:paths (PATH) :operation 'move|'copy)")

(defvar-local grease--pending-changes nil
  "List of pending file operations in current buffer.")

;; Tracking for duplicate sets
(defvar-local grease--duplicate-sets nil
  "Hash table tracking groups of duplicated files.")

;; Prefixes for hidden IDs
(defconst grease--id-prefix "/"
  "Prefix for hidden file IDs.")

;; Track IDs to original file names for copy detection
(defvar-local grease--id-to-original-name (make-hash-table :test 'eql)
  "Hash table mapping IDs to original file names.")

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

(defun grease--get-line-text (line-number)
  "Get the text of LINE-NUMBER without properties."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun grease--format-id (id)
  "Format ID as a 3-digit string with leading zeroes."
  (format "%03d" id))

(defun grease--parse-line (line-text)
  "Parse LINE-TEXT into components.
Handles oil.nvim-style hidden IDs."
  (if (string-match (concat "^" grease--id-prefix "\\([0-9]\\{3\\}\\)\\s-*\\(.*\\)$") line-text)
      (let* ((id-str (match-string 1 line-text))
             (name (match-string 2 line-text))
             (id (string-to-number id-str)))
        (list :id id :name name))
    ;; For lines without IDs (newly inserted)
    (list :id nil :name (string-trim line-text))))

(defun grease--extract-filename (text)
  "Extract just the filename from TEXT, removing ID and icon.
This is a critical function for proper filename handling."
  ;; First try to match with hidden ID format
  (if (string-match (concat "^" grease--id-prefix "[0-9]\\{3\\}\\s-+\\(.*\\)$") text)
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
  (when (string-match (concat "^" grease--id-prefix "\\([0-9]\\{3\\}\\)") text)
    (string-to-number (match-string 1 text))))

;;;; Buffer Rendering and Management

(defun grease--insert-entry (id name type &optional original-name is-duplicate source-name)
  "Insert a formatted line for file with ID, NAME and TYPE."
  (let* ((original-name (or original-name name))
         (is-dir (eq type 'dir))
         (display-name (if is-dir (concat name "/") name))
         (id-str (grease--format-id id))
         (full-id (concat grease--id-prefix id-str))
         (start (point)))
    
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
                                'grease-original-name original-name
                                'grease-is-duplicate is-duplicate
                                'grease-source-name source-name))
      
      ;; Add face to the name part only
      (put-text-property name-start (point) 'face 'font-lock-function-name-face))
    
    ;; Track original names by ID for copy detection
    (puthash id original-name grease--id-to-original-name)
    
    (insert "\n")))

(defun grease--render (dir &optional keep-changes)
  "Render the contents of DIR into the current buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (next-id 1))
    (erase-buffer)
    (setq grease--root-dir (file-name-as-directory (expand-file-name dir)))
    (setq grease--original-state (make-hash-table :test 'equal))
    (setq grease--duplicate-sets (make-hash-table :test 'equal))
    (setq grease--id-to-original-name (make-hash-table :test 'eql))
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
               (type (if (file-directory-p abs-path) 'dir 'file)))
          (puthash file type grease--original-state)
          (grease--insert-entry next-id file type file)
          (setq next-id (1+ next-id)))))
    
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
      
      ;; Find the end of the prefix (ID + icon)
      (save-excursion
        (goto-char bol)
        (if (re-search-forward (concat grease--id-prefix "[0-9]\\{3\\} ") (line-end-position) t)
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
        (goto-char prefix-end)))))

(defun grease--get-line-data (&optional pos)
  "Get file data from current line or at POS."
  (let* ((pos (or pos (point)))
         (line-beg (save-excursion 
                     (goto-char pos)
                     (line-beginning-position))))
    (when (and (> (line-number-at-pos pos) 1)  ; Skip header
               (get-text-property line-beg 'grease-name))
      (list :name (get-text-property line-beg 'grease-name)
            :type (get-text-property line-beg 'grease-type)
            :original-name (get-text-property line-beg 'grease-original-name)
            :is-duplicate (get-text-property line-beg 'grease-is-duplicate)
            :source-name (get-text-property line-beg 'grease-source-name)
            :id (get-text-property line-beg 'grease-id)))))

;; Evil integration hooks
(defun grease--on-evil-yank (beg end &rest _)
  "Handle yank operation for clipboard."
  (when (derived-mode-p 'grease-mode)
    (let ((data (grease--get-line-data)))
      (when data
        (let* ((name (plist-get data :name))
               (type (plist-get data :type))
               (path (grease--get-full-path name))
               (original-name (plist-get data :original-name))
               (id (plist-get data :id)))
          ;; Store in clipboard with additional metadata for proper copy handling
          (setq grease--clipboard 
                (list :paths (list path)
                      :names (list name)
                      :types (list type)
                      :original-names (list (or original-name name))
                      :ids (list id)
                      :operation 'copy))
          (message "Copied file: %s" name))))))

(defun grease--on-evil-delete (beg end &rest _)
  "Handle delete operation for clipboard."
  (when (derived-mode-p 'grease-mode)
    (let ((data (grease--get-line-data)))
      (when data
        (let* ((name (plist-get data :name))
               (type (plist-get data :type))
               (path (grease--get-full-path name)))
          ;; Store in clipboard
          (setq grease--clipboard 
                (list :paths (list path)
                      :names (list name)
                      :types (list type)
                      :operation 'move))
          (message "Cut file: %s" name))))))

;; Track the last yanked text to handle paste properly
(defvar grease--last-yanked-text nil
  "Last text yanked in grease mode, for paste handling.")

(defun grease--intercept-yanked-text (text)
  "Track yanked TEXT from grease buffer."
  (when (derived-mode-p 'grease-mode)
    (setq grease--last-yanked-text text)))

(defun grease--get-next-id ()
  "Get the next available ID in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((max-id 0))
      (while (re-search-forward 
              (concat "^" grease--id-prefix "\\([0-9]\\{3\\}\\)") nil t)
        (let ((id (string-to-number (match-string 1))))
          (when (> id max-id)
            (setq max-id id))))
      (1+ max-id))))

(defun grease--handle-paste-line (line-text)
  "Process pasted LINE-TEXT to properly format it for grease buffer."
  (when (not (string-empty-p line-text))
    (let* ((file-name (grease--extract-filename line-text))
           (line-id (grease--extract-id line-text))
           (is-dir (grease--is-dir-name file-name))
           (type (if is-dir 'dir 'file))
           (next-id (grease--get-next-id)))
      
      ;; Check if this is a copy operation based on:
      ;; 1. Explicit clipboard contents
      ;; 2. The line ID refers to an existing file
      (let ((is-copy (or 
                      ;; Case 1: Clipboard indicates a copy
                      (and grease--clipboard 
                           (eq (plist-get grease--clipboard :operation) 'copy))
                      ;; Case 2: ID matches a known original file
                      (and line-id 
                           (gethash line-id grease--id-to-original-name)))))
        
        ;; Get original name for this line
        (let ((source-name 
               (cond 
                ;; If clipboard has original name info, use it
                ((and grease--clipboard 
                      (eq (plist-get grease--clipboard :operation) 'copy)
                      (car (plist-get grease--clipboard :original-names))))
                ;; If we have ID tracking info, use that
                ((and line-id (gethash line-id grease--id-to-original-name))
                 (gethash line-id grease--id-to-original-name))
                ;; Default to the filename itself
                (t file-name))))
          
          ;; Clear the line and insert properly formatted entry
          (delete-region (line-beginning-position) (line-end-position))
          (grease--insert-entry next-id 
                               (grease--strip-trailing-slash file-name)
                               type nil is-copy source-name))))))

(defun grease--after-paste (beg end)
  "Process pasted text between BEG and END."
  (when (derived-mode-p 'grease-mode)
    ;; Store the clipboard locally before processing paste
    (let ((local-clipboard grease--clipboard)) 
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
      (setq grease--buffer-dirty-p t)
      
      ;; Process clipboard for cross-directory operations
      (when local-clipboard
        (let* ((operation (plist-get local-clipboard :operation))
               (paths (plist-get local-clipboard :paths))
               (names (plist-get local-clipboard :names))
               (types (plist-get local-clipboard :types)))
          
          ;; Process each path in clipboard for cross-directory operations
          (cl-loop for path in paths
                   for name in names
                   for type in types
                   for i from 0
                   do
                   (let ((dest-path (grease--get-full-path name))
                         (source-dir (file-name-directory path)))
                     
                     ;; Only add to pending changes if cross-directory
                     (when (and (not (string= path dest-path))
                                (not (string= source-dir grease--root-dir)))
                       (push (list (if (eq operation 'move) :move :copy)
                                   path dest-path)
                             grease--pending-changes))))
          
          ;; Clear clipboard if it was a move operation
          (when (eq (plist-get local-clipboard :operation) 'move)
            (setq grease--clipboard nil)))))))

;; Add hooks for cursor position constraints
(defun grease--setup-cursor-constraints ()
  "Set up hooks to constrain cursor position."
  (add-hook 'post-command-hook #'grease--constrain-cursor nil t))

;; Add advice to Evil commands
(when (fboundp 'evil-yank-line)
  (advice-add 'evil-yank-line :after #'grease--on-evil-yank)
  (advice-add 'evil-yank :around
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  (when (derived-mode-p 'grease-mode)
                    (grease--intercept-yanked-text (current-kill 0)))
                  result))))

(when (fboundp 'evil-delete-line)
  (advice-add 'evil-delete-line :after #'grease--on-evil-delete))

(when (fboundp 'evil-paste-after)
  (advice-add 'evil-paste-after :around
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  (when (derived-mode-p 'grease-mode)
                    (grease--after-paste (evil-get-marker ?\[) (evil-get-marker ?\])))
                  result))))

(when (fboundp 'evil-paste-before)
  (advice-add 'evil-paste-before :around
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  (when (derived-mode-p 'grease-mode)
                    (grease--after-paste (evil-get-marker ?\[) (evil-get-marker ?\])))
                  result))))

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
                   (type (if is-dir 'dir 'file)))
              
              ;; Update text properties with new name
              (put-text-property line-beg line-end 'grease-name name)
              (put-text-property line-beg line-end 'grease-type type))))
        
        (forward-line 1)))))

(defun grease--scan-buffer ()
  "Scan buffer and update file tracking data."
  ;; First update line metadata based on current text
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
          
          ;; Process the line if it has our file metadata
          (when line-data
            (let* ((name (plist-get line-data :name))
                   (type (plist-get line-data :type))
                   (original-name (plist-get line-data :original-name))
                   (is-duplicate (plist-get line-data :is-duplicate))
                   (source-name (plist-get line-data :source-name))
                   (id (plist-get line-data :id))
                   (data (list :name name
                               :type type
                               :id id
                               :original-name original-name
                               :is-duplicate is-duplicate
                               :source-name source-name)))
              
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
                       (setf (plist-get entry :is-duplicate) t
                             (plist-get entry :source-name) 
                             (or (plist-get original :original-name)
                                 (plist-get original :name))))))))
             name-to-entries)
    
    ;; Then mark duplicates by ID (repeated IDs indicate copies)
    (maphash (lambda (id entry-list)
               (when (> (length entry-list) 1)
                 ;; First entry is the "original", rest are copies
                 (let* ((entries-sorted (sort (copy-sequence entry-list)
                                             (lambda (a b) 
                                               (< (or (plist-get a :id) 0)
                                                  (or (plist-get b :id) 0)))))
                        (first-entry (car entries-sorted))
                        (original-name (plist-get first-entry :original-name)))
                   (dolist (entry (cdr entries-sorted)) ; Skip first
                     (setf (plist-get entry :is-duplicate) t
                           (plist-get entry :source-name) original-name)))))
             id-to-entries)
    
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

(defun grease--calculate-changes ()
  "Calculate the changes between original state and current buffer."
  (let* ((entries (grease--scan-buffer))
         (changes '())
         (original-files (copy-hash-table grease--original-state))
         (seen-originals (make-hash-table :test 'equal))
         (name-conflicts (grease--detect-name-conflicts entries)))

    ;; Check for naming conflicts
    (when name-conflicts
      (user-error "Filename conflicts detected: %s" (mapconcat #'identity name-conflicts ", ")))
    
    ;; First add any pending changes (cross-directory operations)
    (setq changes (append grease--pending-changes changes))
    
    ;; Process all entries
    (dolist (entry entries)
      (let ((original (plist-get entry :original-name))
            (current (plist-get entry :name))
            (type (plist-get entry :type))
            (is-duplicate (plist-get entry :is-duplicate))
            (source-name (plist-get entry :source-name)))
        
        (cond
         ;; Case 1: A duplicated line, which is a copy.
         (is-duplicate
          (let ((source-path 
                 (if (file-name-absolute-p (or source-name original current))
                     (or source-name original current)
                   (expand-file-name (or source-name original current) grease--root-dir))))
            (push `(:copy ,source-path 
                    ,(grease--get-full-path current)) 
                  changes))
          ;; Mark the source as "seen" so it's not considered deleted.
          (when source-name
            (puthash (grease--strip-trailing-slash source-name) t seen-originals)))
         
         ;; Case 2: An existing file that was not duplicated.
         ((and original (gethash original original-files))
          (puthash original t seen-originals)
          ;; Check for rename.
          (unless (string= current original)
            (push `(:rename ,(grease--get-full-path original) 
                    ,(grease--get-full-path current)) 
                  changes)))
         
         ;; Case 3: New file, created from scratch.
         (t
          (push `(:create ,(grease--get-full-path current)) changes)))))
    
    ;; Case 4: Any original file not seen in the current buffer was deleted.
    (maphash (lambda (name type)
               (unless (gethash name seen-originals)
                 (push `(:delete ,(grease--get-full-path name)) changes)))
             original-files)
    
    ;; Sort changes for consistent application order (deletes first).
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
    
    ;; Clear pending changes after successful operations
    (when (and changes (not errors))
      (setq grease--pending-changes nil))
    
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
             (original-name (plist-get data :original-name))
             (next-id (grease--get-next-id)))
        ;; Add a new line with the duplicated content
        (end-of-line)
        (insert "\n")
        (grease--insert-entry next-id name type nil t (or original-name name))
        (setq grease--buffer-dirty-p t)
        (message "Duplicated: %s" name)))))

(defun grease-cut ()
  "Cut the current file/directory."
  (interactive)
  (let ((data (grease--get-line-data)))
    (when data
      (let* ((name (plist-get data :name))
             (type (plist-get data :type))
             (path (grease--get-full-path name)))
        ;; Store in clipboard
        (setq grease--clipboard 
              (list :paths (list path)
                    :names (list name)
                    :types (list type)
                    :operation 'move))
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
             (original-name (plist-get data :original-name)))
        ;; Store in clipboard
        (setq grease--clipboard 
              (list :paths (list path)
                    :names (list name)
                    :types (list type)
                    :original-names (list (or original-name name))
                    :operation 'copy))
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
           (original-names (plist-get grease--clipboard :original-names)))
      
      ;; Insert new entries for each item
      (cl-loop for path in paths
               for name in names
               for type in types
               for original-name in (or original-names names)
               for i from 0
               do
               (let ((next-id (grease--get-next-id)))
                 ;; Insert the new entry
                 (save-excursion
                   (end-of-line)
                   (insert "\n")
                   (grease--insert-entry next-id name type nil 
                                        (eq operation 'copy) original-name))))
      
      ;; Add cross-directory operations to pending changes
      (cl-loop for path in paths
               for name in names
               for i from 0
               do
               (let ((dest-path (grease--get-full-path name))
                     (source-dir (file-name-directory path)))
                 
                 ;; Only add to pending changes if cross-directory
                 (when (and (not (string= path dest-path))
                            (not (string= source-dir grease--root-dir)))
                   (push (list (if (eq operation 'move) :move :copy)
                               path dest-path)
                         grease--pending-changes))))
      
      (setq grease--buffer-dirty-p t)
      
      ;; Clear clipboard if it was a move operation
      (when (eq operation 'move)
        (setq grease--clipboard nil)))))

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
              (when grease--buffer-dirty-p
                (let ((changes (grease--calculate-changes)))
                  (when changes
                    (setq grease--pending-changes 
                          (append grease--pending-changes changes)))))
              (grease--render path t))
          (grease--with-commit-prompt
           (lambda () 
             (kill-buffer (current-buffer)) 
             (find-file path))))))))

(defun grease-up-directory ()
  "Move to the parent directory."
  (interactive)
  (let ((parent-dir (expand-file-name ".." grease--root-dir)))
    (when grease--buffer-dirty-p
      (let ((changes (grease--calculate-changes)))
        (when changes
          (setq grease--pending-changes 
                (append grease--pending-changes changes)))))
    (grease--render parent-dir t)))

(defun grease-refresh ()
  "Discard all changes and reload the directory from disk."
  (interactive)
  (if (and grease--buffer-dirty-p (not (y-or-n-p "Discard all uncommitted changes?")))
      (message "Refresh cancelled.")
    (setq grease--pending-changes nil)
    (setq grease--clipboard nil)
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
  (add-hook 'after-change-functions #'grease--on-change nil t)
  (grease--setup-cursor-constraints))

;; Set up Evil keybindings
(when (fboundp 'evil-define-key*)
  (evil-define-key* 'normal grease-mode-map
    (kbd "RET") #'grease-visit
    (kbd "-") #'grease-up-directory  
    (kbd "g r") #'grease-refresh
    (kbd "q") #'grease-quit))

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

;; Add Evil leader key integration (example)
(when (and (fboundp 'evil-define-key*) (boundp 'evil-leader/map))
  (evil-leader/set-key-for-mode 'grease-mode
    "w" 'grease-save))

(provide 'grease)
;;; grease.el ends here
