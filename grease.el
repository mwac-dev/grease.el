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
;;
;; Changes are staged until you save with `grease-save` (C-c C-s), or when
;; prompted before actions like visiting a file (`RET`) or quitting (`q`).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Optional support for nerd-icons and evil
(eval-when-compile (require 'nerd-icons nil t))
(eval-when-compile (require 'evil nil t))

(defconst grease--have-icons (featurep 'nerd-icons)
  "Non-nil if nerd-icons package is available.")

;;;; Buffer-Local State

(defvar-local grease--root-dir nil)
(defvar-local grease--original-state nil)
(defvar-local grease--buffer-dirty-p nil)
(defvar-local grease--change-hook-active nil)
(defvar grease--global-clipboard nil
  "Global clipboard for cross-directory operations. Contains :action :path :names :types.")
(defvar-local grease--global-changes nil
  "List of changes across directory navigation.")

;;;; Core Helpers

(defun grease--is-dir-name (name)
  (and name (string-suffix-p "/" name)))

(defun grease--strip-trailing-slash (name)
  (if (grease--is-dir-name name)
      (substring name 0 -1)
    name))

(defun grease--normalize-name (name type)
  "Normalize a name based on its type, ensuring directories have trailing slashes."
  (if (eq type 'dir)
      (if (grease--is-dir-name name) name (concat name "/"))
    (grease--strip-trailing-slash name)))

(defun grease--names-equal (name1 type1 name2 type2)
  "Compare two filenames, normalizing for directories with/without slashes."
  (string= (grease--normalize-name name1 type1)
           (grease--normalize-name name2 type2)))

(defun grease--get-icon (name)
  (if grease--have-icons
      (if (grease--is-dir-name name)
          (nerd-icons-icon-for-dir name)
        (nerd-icons-icon-for-file name))
    (if (grease--is-dir-name name) "📁" "📄")))

(defun grease--extract-clean-filename (line-text)
  "Extract just the filename portion from a line of text, removing icons and spaces."
  (when (string-match "\\(?:[📁📄]\\|\\S-+\\)\\s-+\\(.*\\)" line-text)
    (match-string 1 line-text)))

(defun grease--get-line-info ()
  "Return a plist of the current line's info.
Includes :original-name, :current-name, and the editable :region (cons BEG . END)."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (line-text (buffer-substring-no-properties bol eol)))
    
    ;; Skip empty lines and the header
    (unless (or (= (line-number-at-pos) 1) 
                (string-match-p "^\\s-*$" (string-trim line-text)))
      (let* ((name-start (next-single-property-change bol 'read-only nil eol))
             (has-read-only (text-property-any bol eol 'read-only t)))
        
        (if (and name-start has-read-only)
            ;; Normal line with icon and properties
            (let* ((original-name (get-text-property name-start 'grease-original-name))
                   (current-name (string-trim (buffer-substring-no-properties name-start eol)))
                   (file-type (get-text-property name-start 'grease-file-type))
                   (copy-id (get-text-property name-start 'grease-copy-id))
                   (is-duplicate (get-text-property name-start 'grease-is-duplicate))
                   (source-name (get-text-property name-start 'grease-source-name)))
              (list :original-name original-name
                    :current-name current-name
                    :region (cons name-start eol)
                    :file-type file-type
                    :copy-id copy-id
                    :is-duplicate is-duplicate
                    :source-name source-name))
          
          ;; New line without properties
          (let ((clean-text (string-trim line-text)))
            (unless (string-empty-p clean-text)
              (list :original-name nil
                    :current-name clean-text
                    :region (cons bol eol)))))))))

;;;; Buffer Rendering and Management

(defun grease--insert-entry (filename type &optional original-name copy-id is-duplicate source-name)
  "Insert a formatted line with read-only icon and editable filename.
ORIGINAL-NAME defaults to FILENAME."
  (let* ((original-name (or original-name filename))
         (is-dir (eq type 'dir))
         (display-name (if is-dir (concat filename "/") filename))
         (icon (grease--get-icon display-name)))
    ;; Insert icon as read-only text
    (let ((icon-start (point)))
      (insert icon " ")
      (add-text-properties icon-start (point) 
                          '(read-only t rear-nonsticky t front-sticky nil)))
    ;; Insert filename with editable properties
    (let ((name-start (point)))
      (insert display-name)
      (put-text-property name-start (point) 'grease-original-name original-name)
      (put-text-property name-start (point) 'grease-file-type type)
      (when copy-id
        (put-text-property name-start (point) 'grease-copy-id copy-id))
      (when is-duplicate
        (put-text-property name-start (point) 'grease-is-duplicate t))
      (when source-name
        (put-text-property name-start (point) 'grease-source-name source-name))
      (put-text-property name-start (point) 'face 'font-lock-function-name-face))
    (insert "\n")))

(defun grease--render (dir &optional keep-changes)
  "Render the contents of DIR into the current buffer from scratch.
If KEEP-CHANGES is non-nil, don't reset the global changes list."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)) ; Prevent hooks while we are building
    (erase-buffer)
    (setq grease--root-dir (file-name-as-directory (expand-file-name dir)))
    (setq grease--original-state (make-hash-table :test 'equal))
    (unless keep-changes
      (setq grease--global-changes nil))
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
          (grease--insert-entry file type))))
    (goto-char (point-min))
    (forward-line 1)))

;;;; Evil Integration

(defun grease--evil-advice-for-dd (orig-fun beg end &rest args)
  "Advice function to make `evil-delete-line` work with read-only text properties."
  (if (and (derived-mode-p 'grease-mode)
           (not (= (line-number-at-pos beg) 1))) ; Skip header line
      ;; In grease mode, handle specially
      (let ((inhibit-read-only t))
        (apply orig-fun beg end args))
    ;; Not in grease mode, use original function
    (apply orig-fun beg end args)))

;; Apply our advice to evil-delete-line to handle read-only properties
(when (fboundp 'evil-delete-line)
  (advice-add 'evil-delete-line :around #'grease--evil-advice-for-dd))

;; Add Evil ex command for saving
(when (fboundp 'evil-ex-define-cmd)
  (evil-ex-define-cmd "w[rite]" (lambda () 
                                  (interactive)
                                  (if (derived-mode-p 'grease-mode)
                                      (grease-save)
                                    (call-interactively #'evil-write)))))

;;;; Clipboard Operations

(defun grease-copy-line ()
  "Copy the current line to the grease clipboard."
  (interactive)
  (let* ((info (grease--get-line-info))
         (name (plist-get info :current-name))
         (type (or (plist-get info :file-type)
                  (get-text-property (point) 'grease-file-type)
                  (if (grease--is-dir-name name) 'dir 'file))))
    (when info
      (setq grease--global-clipboard 
            (list :action 'copy
                  :path grease--root-dir
                  :names (list name)
                  :types (list type)
                  :copy-ids (list (or (plist-get info :copy-id)
                                      (format "%s:%s" (or (plist-get info :original-name) name) (random))))))
      (message "Copied: %s" name))))

(defun grease-cut-line ()
  "Cut the current line to the grease clipboard."
  (interactive)
  (let* ((info (grease--get-line-info))
         (name (plist-get info :current-name))
         (type (or (plist-get info :file-type)
                  (get-text-property (point) 'grease-file-type)
                  (if (grease--is-dir-name name) 'dir 'file))))
    (when info
      (setq grease--global-clipboard 
            (list :action 'cut
                  :path grease--root-dir
                  :names (list name)
                  :types (list type)
                  :copy-ids (list (or (plist-get info :copy-id)
                                      (format "%s:%s" (or (plist-get info :original-name) name) (random))))))
      (let ((inhibit-read-only t))
        (kill-whole-line))
      (message "Cut: %s" name))))

(defun grease-paste ()
  "Paste from the grease clipboard.
Always creates a new line first to ensure proper handling of pastes."
  (interactive)
  (when grease--global-clipboard
    (let* ((action (plist-get grease--global-clipboard :action))
           (source-path (plist-get grease--global-clipboard :path))
           (names (plist-get grease--global-clipboard :names))
           (types (plist-get grease--global-clipboard :types))
           (copy-ids (plist-get grease--global-clipboard :copy-ids))
           (inhibit-read-only t))
      
      ;; Go to end of line and create a new line
      (end-of-line)
      (insert "\n")
      (forward-line -1)
      
      ;; Paste all items from clipboard
      (dolist (i (number-sequence 0 (1- (length names))))
        (let* ((name (nth i names))
               (type (nth i types))
               (copy-id (nth i copy-ids))
               (is-copy (eq action 'copy))
               (source-name (when is-copy name)))
          (grease--insert-entry name type nil copy-id is-copy source-name)))
      
      ;; Position cursor on the first pasted line
      (forward-line (- (length names)))
      (message "Pasted %d item(s)" (length names)))))

(defun grease-duplicate-line ()
  "Duplicate the current line with a unique copy ID."
  (interactive)
  (let* ((info (grease--get-line-info))
         (name (plist-get info :current-name))
         (type (or (plist-get info :file-type)
                  (get-text-property (point) 'grease-file-type)
                  (if (grease--is-dir-name name) 'dir 'file)))
         (copy-id (format "%s:%s" (or (plist-get info :original-name) name) (random))))
    (when info
      (let ((inhibit-read-only t))
        ;; Always create a new line first
        (end-of-line)
        (insert "\n")
        ;; Insert duplicate with proper properties
        (grease--insert-entry name type nil copy-id t name)
        ;; Position cursor on the duplicated line
        (forward-line -1))
      (message "Duplicated: %s" name))))

;;;; Change Calculation (Diff Engine)

(defun grease--format-change (change)
  (pcase change
    (`(:create ,name) (format "  [Create] %s" name))
    (`(:delete ,name) (format "  [Delete] %s" name))
    (`(:rename ,old ,new) (format "  [Rename] %s -> %s" old new))
    (`(:move ,src ,dst) (format "  [Move]   %s -> %s" src dst))
    (`(:copy ,src ,dst) (format "  [Copy]   %s -> %s" src dst))))

(defun grease--add-to-global-changes (changes dir)
  "Add CHANGES from DIR to the global changes list."
  (dolist (change changes)
    (let* ((change-type (car change))
           (modified-change 
            (pcase change
              (`(:create ,name) 
               `(:create ,(concat dir name)))
              (`(:delete ,name) 
               `(:delete ,(concat dir name)))
              (`(:rename ,old ,new) 
               `(:rename ,(concat dir old) ,(concat dir new)))
              (`(:move ,src ,dst) 
               `(:move ,(concat dir src) ,(concat dir dst)))
              (`(:copy ,src ,dst) 
               `(:copy ,(concat dir src) ,(concat dir dst))))))
      (push modified-change grease--global-changes))))

(defun grease--collect-current-state ()
  "Collect all current files in the buffer including their metadata."
  (let ((files '()))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1) ; Skip header
      (while (not (eobp))
        (let ((info (grease--get-line-info)))
          (when info
            (push info files)))
        (forward-line 1)))
    files))

(defun grease--detect-name-conflicts (files)
  "Check for duplicate filenames in the list of file infos."
  (let ((names (make-hash-table :test 'equal))
        (conflicts '()))
    (dolist (info files)
      (let* ((name (plist-get info :current-name))
             (count (gethash name names 0)))
        (when (> count 0) 
          (push name conflicts))
        (puthash name (1+ count) names)))
    conflicts))

(defun grease--calculate-changes ()
  "Calculate the changes between original state and current buffer."
  (let ((changes '())
        (original-files (copy-hash-table grease--original-state))
        (current-state (grease--collect-current-state))
        (copy-sources (make-hash-table :test 'equal))
        (files-by-original (make-hash-table :test 'equal))
        (name-conflicts (grease--detect-name-conflicts (grease--collect-current-state))))
    
    ;; Check for naming conflicts before proceeding
    (when name-conflicts
      (user-error "Filename conflicts detected: %s" 
                  (mapconcat #'identity name-conflicts ", ")))
    
    ;; First pass: Process originals and collect duplicates
    (dolist (info current-state)
      (let ((original (plist-get info :original-name))
            (current (plist-get info :current-name))
            (is-duplicate (plist-get info :is-duplicate))
            (source-name (plist-get info :source-name)))
        
        ;; Record duplicate sources for the second pass
        (when (and is-duplicate source-name)
          (puthash source-name t copy-sources))))
    
    ;; Second pass: Process files based on their type
    (dolist (info current-state)
      (let* ((original (plist-get info :original-name))
             (current (plist-get info :current-name))
             (type (plist-get info :file-type))
             (is-duplicate (plist-get info :is-duplicate))
             (source-name (plist-get info :source-name)))
        
        (cond
         ;; Case 1: Duplicate/Copy (has source-name)
         ((and is-duplicate source-name)
          ;; If the current name differs from source name, it's a copy+rename
          (push `(:copy ,source-name ,current) changes))
         
         ;; Case 2: Original file (possibly renamed)
         ((and original (not is-duplicate))
          (remhash original original-files)
          (when (and current (not (grease--names-equal current type original (gethash original grease--original-state))))
            (unless (gethash original copy-sources)
              ;; Only rename if not a source for copies
              (push `(:rename ,original ,current) changes))))
         
         ;; Case 3: New file without original name
         ((and (not original) (not is-duplicate))
          (push `(:create ,current) changes)))))
    
    ;; Handle any files left in original-files (these were deleted)
    (maphash
     (lambda (name type)
       (push `(:delete ,(if (eq type 'dir) (concat name "/") name)) changes))
     original-files)
    
    ;; Handle cross-directory operations from clipboard data
    (when grease--global-clipboard
      (let* ((action (plist-get grease--global-clipboard :action))
             (source-path (plist-get grease--global-clipboard :path))
             (names (plist-get grease--global-clipboard :names)))
        
        (when (and source-path names (not (string= source-path grease--root-dir)))
          (dolist (name names)
            (let ((full-source (concat source-path name))
                  (full-dest (concat grease--root-dir name)))
              (if (eq action 'cut)
                  ;; Cut/paste is a move operation
                  (push `(:move ,full-source ,full-dest) changes)
                ;; Copy/paste is a copy operation
                (push `(:copy ,full-source ,full-dest) changes)))))))
    
    ;; Sort changes for consistent application order - deletions first, then copies, then renames, then creates
    (sort changes
          (lambda (a b)
            (let ((type-a (car a))
                  (type-b (car b)))
              (cond
               ;; Deletes come first
               ((eq type-a :delete) (if (eq type-b :delete) nil t))
               ((eq type-b :delete) nil)
               ;; Then copies
               ((eq type-a :copy) (if (eq type-b :copy) nil t))
               ((eq type-b :copy) nil)
               ;; Then renames
               ((eq type-a :rename) (if (eq type-b :rename) nil t))
               ((eq type-b :rename) nil)
               ;; Then creates
               ((eq type-a :create) (if (eq type-b :create) nil t))
               ((eq type-b :create) nil)
               ;; Everything else alphabetically
               (t (string< (symbol-name type-a) (symbol-name type-b)))))))))

(defun grease--apply-changes (changes)
  "Apply the calculated changes to the filesystem."
  (let ((errors '()))
    (dolist (change changes)
      (pcase change
        (`(:create ,name)
         (let ((abs (expand-file-name (grease--strip-trailing-slash name) grease--root-dir)))
           (message "Grease: Creating %s" abs)
           (condition-case e (if (grease--is-dir-name name) 
                                (make-directory abs)
                              ;; Create empty file if it doesn't exist
                              (unless (file-exists-p abs)
                                (write-region "" nil abs)))
             (error (push (format "Failed to create %s: %s" abs e) errors)))))
        (`(:delete ,name)
         (let* ((stripped (grease--strip-trailing-slash name)) 
                (abs (expand-file-name stripped grease--root-dir)))
           (message "Grease: Deleting %s" abs)
           (condition-case e (if (file-directory-p abs) 
                                (delete-directory abs t) 
                              (delete-file abs))
             (error (push (format "Failed to delete %s: %s" abs e) errors)))))
        (`(:rename ,old ,new)
         (let ((old-abs (expand-file-name (grease--strip-trailing-slash old) grease--root-dir))
               (new-abs (expand-file-name (grease--strip-trailing-slash new) grease--root-dir)))
           (message "Grease: Renaming %s -> %s" old-abs new-abs)
           (condition-case e (rename-file old-abs new-abs t)
             (error (push (format "Failed to rename %s: %s" old e) errors)))))
        (`(:move ,src ,dst)
         (let ((src-abs (if (string-prefix-p "/" src)
                          (file-truename (grease--strip-trailing-slash src))
                        (expand-file-name (grease--strip-trailing-slash src) grease--root-dir)))
               (dst-abs (if (string-prefix-p "/" dst)
                          (file-truename (grease--strip-trailing-slash dst))
                        (expand-file-name (grease--strip-trailing-slash dst) grease--root-dir))))
           (message "Grease: Moving %s -> %s" src-abs dst-abs)
           (condition-case e (rename-file src-abs dst-abs t)
             (error (push (format "Failed to move %s: %s" src e) errors)))))
        (`(:copy ,src ,dst)
         (let ((src-abs (if (string-prefix-p "/" src)
                          (file-truename (grease--strip-trailing-slash src))
                        (expand-file-name (grease--strip-trailing-slash src) grease--root-dir)))
               (dst-abs (if (string-prefix-p "/" dst)
                          (file-truename (grease--strip-trailing-slash dst))
                        (expand-file-name (grease--strip-trailing-slash dst) grease--root-dir))))
           (if (string= src-abs dst-abs)
               (message "Grease: Skipping copy to same location %s" src-abs)
             (message "Grease: Copying %s -> %s" src-abs dst-abs)
             (condition-case e 
                 (if (file-directory-p src-abs) 
                     (copy-directory src-abs dst-abs) 
                   (copy-file src-abs dst-abs t))
               (error (push (format "Failed to copy %s: %s" src e) errors))))))))
    (if errors
        (warn "Grease: Encountered errors:\n%s" (mapconcat #'identity errors "\n"))
      (message "Grease: All changes applied successfully."))))

;;;; User Commands (Actions, not Edits)

(defun grease-save ()
  "Calculate and apply changes in the current buffer."
  (interactive)
  (let ((changes (grease--calculate-changes)))
    (if (not changes) 
        (message "Grease: No changes to save.")
      (if (y-or-n-p (format "Apply these changes?\n%s\n" (mapconcat #'grease--format-change changes "\n")))
          (progn 
            (grease--apply-changes changes) 
            (when grease--global-changes
              (setq grease--global-changes nil))
            (grease--render grease--root-dir) 
            t)
        (message "Grease: Save cancelled.") nil))))

(defun grease--with-commit-prompt (action-fn)
  "Run ACTION-FN, prompting to save changes if buffer is dirty."
  (if (not grease--buffer-dirty-p) (funcall action-fn)
    (let ((result (grease-save)))
      (when result (funcall action-fn)))))

(defun grease-visit ()
  "Visit the file or directory at point."
  (interactive)
  (let* ((info (grease--get-line-info))
         (name (plist-get info :current-name)))
    (if (not name) 
        (user-error "Not on a file or directory line.")
      ;; Ensure we're getting just the filename without any icons
      (let* ((clean-name (string-trim name))
             (path (expand-file-name (grease--strip-trailing-slash clean-name) grease--root-dir)))
        (if (grease--is-dir-name clean-name)
            (progn
              ;; Remember changes before navigating
              (when grease--buffer-dirty-p
                (let ((changes (grease--calculate-changes)))
                  (when changes
                    (grease--add-to-global-changes changes grease--root-dir))))
              (grease--render path t)) ; Keep global changes
          ;; For files, we still want to prompt for commit
          (grease--with-commit-prompt
           (lambda () 
             (kill-buffer (current-buffer)) 
             (find-file path))))))))

(defun grease-up-directory ()
  "Move to the parent directory."
  (interactive)
  (let ((parent-dir (expand-file-name ".." grease--root-dir)))
    ;; Remember changes before navigating
    (when grease--buffer-dirty-p
      (let ((changes (grease--calculate-changes)))
        (when changes
          (grease--add-to-global-changes changes grease--root-dir))))
    (grease--render parent-dir t))) ; Keep global changes

(defun grease-refresh ()
  "Discard all changes and reload the directory from disk."
  (interactive)
  (if (and grease--buffer-dirty-p (not (y-or-n-p "Discard all uncommitted changes?")))
      (message "Refresh cancelled.")
    (setq grease--global-changes nil)
    (grease--render grease--root-dir)
    (message "Grease: Refreshed.")))

(defun grease-quit ()
  "Quit the grease buffer, prompting to save changes."
  (interactive)
  (if (or grease--buffer-dirty-p grease--global-changes)
      (when (y-or-n-p "Save changes before quitting?")
        (if grease--global-changes
            (message "Global changes not yet implemented for save")
          (grease-save)))
    (kill-buffer (current-buffer))))

;;;; Major Mode Definition

(defun grease--on-change (_beg _end _len)
  "Hook run after buffer changes to mark it dirty."
  (unless grease--change-hook-active
    (let ((grease--change-hook-active t))
      (setq grease--buffer-dirty-p t))))

(defun grease--fix-line (line)
  "Format a line with proper icon based on its content."
  (save-excursion
    (goto-char (point-at-bol line))
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line-content (buffer-substring-no-properties bol eol)))
      
      ;; Skip header and empty lines
      (unless (or (= line 1) (string-match-p "^\\s-*$" line-content))
        (let* ((inhibit-read-only t)
               (inhibit-modification-hooks t)
               (has-icon (text-property-any bol eol 'read-only t))
               (name-start (if has-icon 
                              (next-single-property-change bol 'read-only nil eol)
                            bol))
               (name (if name-start 
                        (string-trim (buffer-substring-no-properties name-start eol))
                      (string-trim line-content)))
               (original-name (when name-start (get-text-property name-start 'grease-original-name)))
               (file-type (when name-start (get-text-property name-start 'grease-file-type)))
               (copy-id (when name-start (get-text-property name-start 'grease-copy-id)))
               (is-duplicate (when name-start (get-text-property name-start 'grease-is-duplicate)))
               (source-name (when name-start (get-text-property name-start 'grease-source-name))))
          
          ;; Only fix if needed
          (when (and name (not (string-empty-p name))
                     (or (not has-icon)
                         (not (string-match-p "^[📁📄]\\|^\\S-+ " line-content))))
            (delete-region bol eol)
            (let ((type (or file-type (if (grease--is-dir-name name) 'dir 'file))))
              (grease--insert-entry name type original-name copy-id is-duplicate source-name))))))))

(defun grease--after-change-hook ()
  "After change hook to fix lines when needed."
  (unless grease--change-hook-active
    (let ((grease--change-hook-active t))
      (let ((line (line-number-at-pos)))
        (grease--fix-line line)))))

(defvar grease-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'grease-save)
    map)
  "Keymap for `grease-mode'.")

(define-derived-mode grease-mode prog-mode "Grease"
  "A major mode for oil.nvim-style file management."
  :syntax-table nil
  (setq-local truncate-lines t)
  (add-hook 'after-change-functions #'grease--on-change nil t)
  (add-hook 'post-command-hook #'grease--after-change-hook nil t))

;; Set up Evil mode keybindings properly
(when (fboundp 'evil-define-key*)
  (evil-define-key* 'normal grease-mode-map
    (kbd "RET") #'grease-visit
    (kbd "-") #'grease-up-directory  
    (kbd "g r") #'grease-refresh
    (kbd "q") #'grease-quit
    (kbd "y y") #'grease-copy-line
    (kbd "d d") #'grease-cut-line
    (kbd "p") #'grease-paste
    (kbd "y p") #'grease-duplicate-line))

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
    (switch-to-buffer buf))) ; Use switch-to-buffer instead of pop-to-buffer

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
