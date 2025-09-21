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

(defvar-local grease--root-dir nil
  "Current directory being displayed.")

(defvar-local grease--original-state nil
  "Hash table of original filenames -> file types.")

(defvar-local grease--buffer-dirty-p nil
  "Non-nil if buffer has unsaved changes.")

(defvar-local grease--change-hook-active nil
  "Flag to prevent recursive change hooks.")

(defvar-local grease--id-to-file nil
  "Hash table mapping numeric IDs to file metadata.")

(defvar-local grease--next-id 1
  "Next available ID for file entries.")

(defvar grease--global-clipboard nil
  "Global clipboard for cross-directory operations.")

(defvar-local grease--global-changes nil
  "List of changes across directory navigation.")

;; New tracking for duplicate sets
(defvar-local grease--duplicate-sets nil
  "Hash table tracking groups of duplicated files.")

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

(defun grease--get-line-text (line-number)
  "Get the text of LINE-NUMBER without properties."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun grease--parse-line (line-text)
  "Parse LINE-TEXT into components.
Returns a plist with :id and :name."
  (if (string-match "^\\([0-9]+\\):\\s-*\\(.*\\)$" line-text)
      (let* ((id-str (match-string 1 line-text))
             (name (match-string 2 line-text))
             (id (string-to-number id-str)))
        (list :id id :name name))
    ;; For lines without IDs (newly inserted)
    (list :id nil :name (string-trim line-text))))

(defun grease--get-line-info ()
  "Return information about the current line."
  (let* ((line-text (buffer-substring-no-properties 
                    (line-beginning-position) (line-end-position)))
         (parsed (grease--parse-line line-text)))
    (when parsed
      (let* ((id (plist-get parsed :id))
             (metadata (when id (gethash id grease--id-to-file))))
        (or metadata
            (list :name (plist-get parsed :name)
                  :type (if (grease--is-dir-name (plist-get parsed :name)) 'dir 'file)))))))

;;;; Buffer Rendering and Management

(defun grease--insert-entry (id name type &optional original-name is-duplicate source-name duplicate-set)
  "Insert a formatted line for file with ID, NAME and TYPE."
  (let* ((original-name (or original-name name))
         (is-dir (eq type 'dir))
         (display-name (if is-dir (concat name "/") name))
         (start (point))
         (data (list :name name
                     :original-name original-name
                     :type type
                     :is-duplicate (or is-duplicate nil)
                     :source-name source-name
                     :duplicate-set duplicate-set)))
    
    ;; Store in ID map
    (puthash id data grease--id-to-file)
    
    ;; Insert the visible content
    (when grease--use-icons
      (let ((icon (grease--get-icon display-name)))
        (insert icon " ")
        (add-text-properties start (point) 
                             '(read-only t rear-nonsticky t front-sticky nil))
        (setq start (point))))

    (insert (format "%d: %s" id display-name))
    (put-text-property start (point) 'face 'font-lock-function-name-face)
    (insert "\n")))

(defun grease--render (dir &optional keep-changes)
  "Render the contents of DIR into the current buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (erase-buffer)
    (setq grease--root-dir (file-name-as-directory (expand-file-name dir)))
    (setq grease--original-state (make-hash-table :test 'equal))
    (setq grease--id-to-file (make-hash-table :test 'eql))
    (setq grease--duplicate-sets (make-hash-table :test 'equal))
    (setq grease--next-id 1)
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
               (type (if (file-directory-p abs-path) 'dir 'file))
               (id grease--next-id))
          (puthash file type grease--original-state)
          (grease--insert-entry id file type)
          (setq grease--next-id (1+ grease--next-id)))))
    
    (goto-char (point-min))
    (forward-line 1)))

;;;; Buffer Change Tracking

(defun grease--add-to-duplicate-set (set-id id)
  "Add ID to the duplicate SET-ID."
  (let ((set (gethash set-id grease--duplicate-sets)))
    (if set
        (push id set)
      (setq set (list id)))
    (puthash set-id set grease--duplicate-sets)))

(defun grease--scan-buffer ()
  "Scan buffer and update file tracking data."
  (let ((new-id-to-file (make-hash-table :test 'eql))
        (names-to-ids (make-hash-table :test 'equal))
        (new-duplicate-sets (make-hash-table :test 'equal)))
    
    ;; First pass: collect all entries
    (save-excursion
      (goto-char (point-min))
      (forward-line 1) ; Skip header
      (while (not (eobp))
        (let* ((line-text (buffer-substring-no-properties 
                           (line-beginning-position) (line-end-position)))
               (parsed (grease--parse-line line-text)))
          (when parsed
            (let* ((id (plist-get parsed :id))
                   (name (plist-get parsed :name))
                   (is-dir (grease--is-dir-name name))
                   (name-clean (if is-dir (grease--strip-trailing-slash name) name))
                   (type (if is-dir 'dir 'file))
                   (existing (and id (gethash id grease--id-to-file)))
                   (original-name (when existing (plist-get existing :original-name)))
                   (is-duplicate (when existing (plist-get existing :is-duplicate)))
                   (source-name (when existing (plist-get existing :source-name)))
                   (duplicate-set (when existing (plist-get existing :duplicate-set))))
              
              ;; Track entries by name
              (push id (gethash name-clean names-to-ids nil))
              
              ;; Store entry data
              (when id  ; Only store entries with IDs
                (puthash id 
                        (list :name name-clean
                              :original-name (or original-name name-clean)
                              :type type
                              :is-duplicate (or is-duplicate nil)
                              :source-name source-name
                              :duplicate-set duplicate-set)
                        new-id-to-file)))))
        (forward-line 1)))
    
    ;; Second pass: handle duplicates
    (maphash (lambda (name ids)
               (when (> (length ids) 1)
                 ;; Create a set ID based on the name
                 (let ((set-id (concat "dup-" name)))
                   ;; Update all entries in this set
                   (dolist (id ids)
                     (let ((data (gethash id new-id-to-file)))
                       (setf (plist-get data :duplicate-set) set-id)
                       (puthash id data new-id-to-file))
                     ;; Add to the duplicate set
                     (push id (gethash set-id new-duplicate-sets nil))))))
             names-to-ids)
    
    ;; Third pass: mark duplicates
    (maphash (lambda (set-id ids)
               (let ((ids-sorted (sort ids #'<))
                     (original-id (car (last (sort (copy-sequence ids) #'<)))))
                 ;; The last ID (highest number) is considered the original
                 (dolist (id ids-sorted)
                   (unless (= id original-id)
                     (let* ((data (gethash id new-id-to-file))
                            (orig-data (gethash original-id new-id-to-file)))
                       (setf (plist-get data :is-duplicate) t
                             (plist-get data :source-name) 
                             (or (plist-get orig-data :original-name)
                                 (plist-get orig-data :name)))
                       (puthash id data new-id-to-file))))))
             new-duplicate-sets)
    
    ;; Update the tracking data
    (setq grease--id-to-file new-id-to-file)
    (setq grease--duplicate-sets new-duplicate-sets)))

(defun grease--on-change (_beg _end _len)
  "Hook run after buffer changes to mark it dirty."
  (unless grease--change-hook-active
    (let ((grease--change-hook-active t))
      (setq grease--buffer-dirty-p t))))

;;;; Evil Integration

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

;; Add support for yanking/pasting with metadata
(defun grease--advice-evil-paste (orig-fun &rest args)
  "Advice to run after Evil paste commands."
  (let ((result (apply orig-fun args)))
    (when (derived-mode-p 'grease-mode)
      (grease--scan-buffer))
    result))

;; Add advice to Evil paste commands
(when (fboundp 'evil-paste-after)
  (advice-add 'evil-paste-after :around #'grease--advice-evil-paste))
(when (fboundp 'evil-paste-before)
  (advice-add 'evil-paste-before :around #'grease--advice-evil-paste))

;;;; File Operation Helpers

(defun grease-duplicate-line ()
  "Duplicate the current line with unique metadata."
  (interactive)
  (let* ((line-text (buffer-substring-no-properties 
                     (line-beginning-position) (line-end-position)))
         (parsed (grease--parse-line line-text)))
    
    (when parsed
      (let* ((id (plist-get parsed :id))
             (orig-data (when id (gethash id grease--id-to-file)))
             (name (if orig-data (plist-get orig-data :name) (plist-get parsed :name)))
             (type (if orig-data (plist-get orig-data :type) 
                     (if (grease--is-dir-name (plist-get parsed :name)) 'dir 'file)))
             (original-name (when orig-data (plist-get orig-data :original-name)))
             (new-id grease--next-id)
             (duplicate-set (or (when orig-data (plist-get orig-data :duplicate-set))
                              (concat "dup-" name))))
        
        ;; Create a duplicate set if this is the first duplication
        (unless (gethash duplicate-set grease--duplicate-sets)
          (puthash duplicate-set (list id) grease--duplicate-sets))
        
        ;; Add to duplicate tracking
        (grease--add-to-duplicate-set duplicate-set new-id)
        
        ;; Add the duplicate line
        (end-of-line)
        (insert "\n" (format "%d: %s" new-id 
                            (if (eq type 'dir) (concat name "/") name)))
        
        ;; Store the new entry metadata
        (puthash new-id
                (list :name name
                      :original-name (or original-name name)
                      :type type
                      :is-duplicate t
                      :source-name (or original-name name)
                      :duplicate-set duplicate-set)
                grease--id-to-file)
        
        ;; Update the existing entry to be part of the duplicate set too
        (when orig-data
          (setf (plist-get orig-data :duplicate-set) duplicate-set)
          (puthash id orig-data grease--id-to-file))
        
        (setq grease--next-id (1+ grease--next-id))
        (message "Duplicated: %s" name)))))

(defun grease-copy-filename ()
  "Copy the current filename to kill ring."
  (interactive)
  (let* ((line-text (buffer-substring-no-properties 
                     (line-beginning-position) (line-end-position)))
         (parsed (grease--parse-line line-text)))
    (when parsed
      (let ((name (plist-get parsed :name)))
        (kill-new name)
        (message "Copied filename: %s" name)))))

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

(defun grease--detect-name-conflicts ()
  "Check for duplicate filenames that aren't marked as duplicates."
  (let ((names (make-hash-table :test 'equal))
        (conflicts '()))
    
    (maphash (lambda (_id data)
               (let* ((name (plist-get data :name))
                      (is-duplicate (plist-get data :is-duplicate))
                      (duplicate-set (plist-get data :duplicate-set)))
                 ;; Only count non-duplicates for conflict detection
                 (unless (or is-duplicate duplicate-set)
                   (let ((count (gethash name names 0)))
                     (when (> count 0) 
                       (push name conflicts))
                     (puthash name (1+ count) names)))))
             grease--id-to-file)
    
    conflicts))

(defun grease--calculate-changes ()
  "Calculate the changes between original state and current buffer."
  ;; First scan the buffer to update entries
  (grease--scan-buffer)
  
  (let* ((changes '())
         (original-files (copy-hash-table grease--original-state))
         (seen-originals (make-hash-table :test 'equal))
         (name-conflicts (grease--detect-name-conflicts)))

    (when name-conflicts
      (user-error "Filename conflicts detected: %s" (mapconcat #'identity name-conflicts ", ")))
    
    ;; Process all entries
    (maphash (lambda (_id data)
               (let ((original (plist-get data :original-name))
                     (current (plist-get data :name))
                     (type (plist-get data :type))
                     (is-duplicate (plist-get data :is-duplicate))
                     (source-name (plist-get data :source-name))
                     (duplicate-set (plist-get data :duplicate-set)))
                 
                 (cond
                  ;; Case 1: A duplicated line, which is a copy.
                  ((or is-duplicate duplicate-set)
                   (push `(:copy ,(or source-name original current) 
                           ,(grease--normalize-name current type)) 
                         changes)
                   ;; Mark the source as "seen" so it's not considered deleted.
                   (puthash (or source-name original) t seen-originals))
                  
                  ;; Case 2: An existing file that was not duplicated.
                  ((and original (gethash original original-files))
                   (puthash original t seen-originals)
                   ;; Check for rename.
                   (unless (grease--names-equal current type original (gethash original original-files))
                     (push `(:rename ,original ,(grease--normalize-name current type)) changes)))
                  
                  ;; Case 3: New file, created from scratch.
                  (t
                   (push `(:create ,(grease--normalize-name current type)) changes)))))
             grease--id-to-file)
    
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
  (let* ((line-text (buffer-substring-no-properties 
                     (line-beginning-position) (line-end-position)))
         (parsed (grease--parse-line line-text)))
    
    (if (not parsed) 
        (user-error "Not on a file or directory line.")
      (let* ((id (plist-get parsed :id))
             (data (gethash id grease--id-to-file))
             (name (plist-get parsed :name))
             (type (if data 
                      (plist-get data :type)
                    (if (grease--is-dir-name name) 'dir 'file)))
             (path (expand-file-name (grease--strip-trailing-slash name) grease--root-dir)))
        (if (eq type 'dir)
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

(defvar grease-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'grease-save)
    (define-key map (kbd "C-c C-d") #'grease-duplicate-line) 
    (define-key map (kbd "C-c C-c") #'grease-copy-filename)
    map)
  "Keymap for `grease-mode'.")

(define-derived-mode grease-mode prog-mode "Grease"
  "A major mode for oil.nvim-style file management."
  :syntax-table nil
  (setq-local truncate-lines t)
  (add-hook 'after-change-functions #'grease--on-change nil t))

;; Set up minimal Evil keybindings - only for special functions
(when (fboundp 'evil-define-key*)
  (evil-define-key* 'normal grease-mode-map
    (kbd "RET") #'grease-visit
    (kbd "-") #'grease-up-directory  
    (kbd "g r") #'grease-refresh
    (kbd "q") #'grease-quit
    (kbd "yf") #'grease-copy-filename)) ; Use yf for copy filename

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
