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

;; --- CHANGE: Icons temporarily disabled via this const ---
(defconst grease--use-icons nil
  "Non-nil to display icons next to filenames.")

(defconst grease--have-icons (and grease--use-icons (featurep 'nerd-icons))
  "Non-nil if nerd-icons package is available and enabled.")

;;;; Buffer-Local State

(defvar-local grease--root-dir nil)
(defvar-local grease--original-state nil)
(defvar-local grease--buffer-dirty-p nil)
(defvar-local grease--change-hook-active nil)
(defvar grease--global-clipboard nil
  "Global clipboard for cross-directory operations.")
(defvar-local grease--global-changes nil
  "List of changes across directory navigation.")

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

(defun grease--names-equal (name1 type1 name2 type2)
  "Compare two filenames, normalizing for directories with/without slashes."
  (string= (grease--normalize-name name1 type1)
           (grease--normalize-name name2 type2)))

(defun grease--get-icon (name)
  "Get appropriate icon for NAME."
  (if grease--have-icons
      (if (grease--is-dir-name name)
          (nerd-icons-icon-for-dir name)
        (nerd-icons-icon-for-file name))
    (if (grease--is-dir-name name) "📁" "📄")))

(defun grease--parse-line (line-text)
  "Extract filename from LINE-TEXT, stripping potential icon prefix."
  (if (string-match "^\\(?:[📁📄]\\|\\S-+\\)\\s-+\\(.*\\)$" line-text)
      (match-string 1 line-text)
    (string-trim line-text)))

(defun grease--get-line-info ()
  "Return information about the current line."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (line-text (buffer-substring-no-properties bol eol)))
    
    ;; Skip empty lines and the header
    (unless (or (= (line-number-at-pos) 1) 
                (string-match-p "^\\s-*$" (string-trim line-text)))
      ;; --- CHANGE: Simplified parsing to handle with/without icons ---
      (let* ((name-start (if grease--use-icons
                             (or (next-single-property-change bol 'read-only nil eol) bol)
                           bol))
             (current-name (string-trim (buffer-substring-no-properties name-start eol)))
             (original-name (get-text-property name-start 'grease-original-name))
             (file-type (get-text-property name-start 'grease-file-type))
             (is-duplicate (get-text-property name-start 'grease-is-duplicate))
             (source-name (get-text-property name-start 'grease-source-name)))
        
        (list :original-name original-name
              :current-name current-name
              :file-type (or file-type (if (grease--is-dir-name current-name) 'dir 'file))
              :is-duplicate is-duplicate
              :source-name source-name)))))

;;;; Buffer Rendering and Management

(defun grease--insert-entry (filename type &optional original-name copy-id is-duplicate source-name)
  "Insert a formatted line with filename and metadata."
  (let* ((original-name (or original-name filename))
         (is-dir (eq type 'dir))
         (display-name (if is-dir (concat filename "/") filename))
         (start (point)))
    
    ;; --- CHANGE: Conditionally insert icon ---
    (when grease--use-icons
      (let ((icon (grease--get-icon display-name)))
        (insert icon " ")
        (add-text-properties start (point) 
                             '(read-only t rear-nonsticky t front-sticky nil))
        (setq start (point))))

    (insert display-name)
    (put-text-property start (point) 'grease-original-name original-name)
    (put-text-property start (point) 'grease-file-type type)
    (when copy-id
      (put-text-property start (point) 'grease-copy-id copy-id))
    (when is-duplicate
      (put-text-property start (point) 'grease-is-duplicate t))
    (when source-name
      (put-text-property start (point) 'grease-source-name source-name))
    (put-text-property start (point) 'face 'font-lock-function-name-face)
    (insert "\n")))

(defun grease--render (dir &optional keep-changes)
  "Render the contents of DIR into the current buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
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

;;;; Minimal Evil Integration

(defun grease--evil-advice-for-dd (orig-fun beg end &rest args)
  "Allow `evil-delete-line` to work with read-only icons."
  (if (and (derived-mode-p 'grease-mode)
           (not (= (line-number-at-pos beg) 1)))
      (let ((inhibit-read-only t))
        (apply orig-fun beg end args))
    (apply orig-fun beg end args)))

(when (fboundp 'evil-delete-line)
  (advice-add 'evil-delete-line :around #'grease--evil-advice-for-dd))

(when (fboundp 'evil-ex-define-cmd)
  (evil-ex-define-cmd "w[rite]" (lambda () 
                                  (interactive)
                                  (if (derived-mode-p 'grease-mode)
                                      (grease-save)
                                    (call-interactively #'evil-write)))))

;;;; File Operation Helpers

(defun grease-duplicate-line ()
  "Duplicate the current line with unique metadata."
  (interactive)
  (let* ((info (grease--get-line-info))
         (name (plist-get info :current-name))
         (type (plist-get info :file-type))
         ;; --- CHANGE: Use original-name from info for the copy ---
         (original-name (plist-get info :original-name))
         (copy-id (format "%s:%s" (or original-name name) (random))))
    (when info
      (end-of-line)
      (insert "\n")
      ;; --- CHANGE: Pass original-name, is-duplicate, and source-name correctly ---
      (grease--insert-entry name type original-name copy-id t name)
      (forward-line -1)
      (message "Duplicated: %s" name))))

;;;; Change Calculation (Diff Engine)

(defun grease--format-change (change)
  "Format CHANGE for display in confirmation prompt."
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
  "Collect all file entries in the buffer including metadata."
  (let ((files '()))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1) ; Skip header
      (while (not (eobp))
        (let ((info (grease--get-line-info)))
          (when info
            (push info files)))
        (forward-line 1)))
    (nreverse files)))

(defun grease--detect-name-conflicts (files)
  "Check for duplicate filenames in FILES."
  (let ((names (make-hash-table :test 'equal))
        (conflicts '()))
    (dolist (info files)
      (let* ((name (plist-get info :current-name))
             (count (gethash name names 0)))
        (when (> count 0) 
          (push name conflicts))
        (puthash name (1+ count) names)))
    conflicts))

;; --- CHANGE: Rewritten change calculation logic for clarity and correctness ---
(defun grease--calculate-changes ()
  "Calculate the changes between original state and current buffer."
  (let* ((changes '())
         (original-files (copy-hash-table grease--original-state))
         (current-state (grease--collect-current-state))
         (seen-originals (make-hash-table :test 'equal))
         (name-conflicts (grease--detect-name-conflicts current-state)))

    (when name-conflicts
      (user-error "Filename conflicts detected: %s" (mapconcat #'identity name-conflicts ", ")))

    (dolist (info current-state)
      (let ((original (plist-get info :original-name))
            (current (plist-get info :current-name))
            (type (plist-get info :file-type))
            (is-duplicate (plist-get info :is-duplicate))
            (source-name (plist-get info :source-name)))
        
        (cond
         ;; Case 1: A duplicated line, which is a copy.
         (is-duplicate
          (push `(:copy ,source-name ,current) changes)
          ;; Mark the source as "seen" so it's not considered deleted.
          (puthash source-name t seen-originals))

         ;; Case 2: An existing file that was not duplicated.
         (original
          (puthash original t seen-originals)
          ;; Check for rename.
          (unless (grease--names-equal current type original (gethash original grease--original-state))
            (push `(:rename ,original ,current) changes)))

         ;; Case 3: New file, created from scratch.
         ((not original)
          (push `(:create ,current) changes)))))

    ;; Case 4: Any original file not seen in the current buffer was deleted.
    (maphash (lambda (name type)
               (unless (gethash name seen-originals)
                 (push `(:delete ,(grease--normalize-name name type)) changes)))
             original-files)
    
    ;; Sort changes for consistent application order (deletes first).
    (sort changes (lambda (a b) (if (eq (car a) :delete) t (not (eq (car b) :delete)))))))

(defun grease--apply-changes (changes)
  "Apply CHANGES to the filesystem."
  (let ((errors '()))
    (dolist (change changes)
      (pcase change
        (`(:create ,name)
         (let ((abs (expand-file-name (grease--strip-trailing-slash name) grease--root-dir)))
           (message "Grease: Creating %s" abs)
           ;; --- CHANGE: Fix file creation ---
           (condition-case e (if (grease--is-dir-name name) 
                                (make-directory abs)
                              (write-region "" nil abs t)) ; 't' creates the file
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
        (`(:copy ,src ,dst)
         (let ((src-abs (expand-file-name (grease--strip-trailing-slash src) grease--root-dir))
               (dst-abs (expand-file-name (grease--strip-trailing-slash dst) grease--root-dir)))
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
            (when grease--global-changes
              (setq grease--global-changes nil))
            (grease--render grease--root-dir) 
            t)
        (message "Grease: Save cancelled.") nil))))

(defun grease--with-commit-prompt (action-fn)
  "Run ACTION-FN after saving if buffer is dirty."
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
      (let* ((clean-name (string-trim name))
             (path (expand-file-name (grease--strip-trailing-slash clean-name) grease--root-dir)))
        (if (grease--is-dir-name clean-name)
            (progn
              (when grease--buffer-dirty-p
                (let ((changes (grease--calculate-changes)))
                  (when changes
                    (grease--add-to-global-changes changes grease--root-dir))))
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
          (grease--add-to-global-changes changes grease--root-dir))))
    (grease--render parent-dir t)))

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
          
          (when (and name (not (string-empty-p name))
                     (or (not has-icon)
                         (not (string-match-p "^[📁📄]\\|^\\S-+ " line-content))))
            (delete-region bol eol)
            (let ((type (or file-type (if (grease--is-dir-name name) 'dir 'file))))
              (grease--insert-entry name type original-name copy-id is-duplicate source-name))))))))

(defun grease--after-change-hook ()
  "After change hook to fix lines when needed."
  ;; --- CHANGE: Disable this hook as it's not needed for the simplified, no-icon version ---
  ;; (unless grease--change-hook-active
  ;;   (let ((grease--change-hook-active t))
  ;;     (let ((line (line-number-at-pos)))
  ;;       (grease--fix-line line))))
  )

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

;; Set up minimal Evil keybindings - only for special functions
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
