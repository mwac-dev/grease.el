;;; grease-test.el --- Tests for grease.el -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'grease)


;;;; Test Setup

(defmacro grease-test-with-temp-dir (&rest body)
  "Execute BODY with a temporary directory, cleaning up afterward."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "grease-test-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

(defmacro grease-test-with-clean-state (&rest body)
  "Execute BODY with fresh grease state."
  (declare (indent 0))
  `(let ((grease--file-registry (make-hash-table :test 'eql))
         (grease--visited-dirs nil)
         (grease--session-id-counter 1)
         (grease--deleted-file-ids (make-hash-table :test 'eql))
         (grease--clipboard nil)
         (grease--last-op-type nil)
         (grease--last-kill-index nil)
         (grease--multi-line-selection nil))
     ,@body))

(defmacro grease-test-with-buffer (dir &rest body)
  "Execute BODY in a grease buffer for DIR."
  (declare (indent 1))
  `(grease-test-with-clean-state
     (with-temp-buffer
       (grease-mode)
       (setq grease--root-dir (file-name-as-directory (expand-file-name ,dir)))
       (grease--render grease--root-dir)
       ,@body)))

;;;; Helper Function Tests

(ert-deftest grease-test-is-dir-name ()
  "Test directory name detection."
  (should (grease--is-dir-name "foo/"))
  (should (grease--is-dir-name "bar/baz/"))
  (should-not (grease--is-dir-name "foo"))
  (should-not (grease--is-dir-name "foo.txt"))
  (should-not (grease--is-dir-name nil)))

(ert-deftest grease-test-strip-trailing-slash ()
  "Test trailing slash removal."
  (should (equal (grease--strip-trailing-slash "foo/") "foo"))
  (should (equal (grease--strip-trailing-slash "foo") "foo"))
  (should (equal (grease--strip-trailing-slash "bar/baz/") "bar/baz")))

(ert-deftest grease-test-normalize-name ()
  "Test name normalization for files and directories."
  (should (equal (grease--normalize-name "foo" 'dir) "foo/"))
  (should (equal (grease--normalize-name "foo/" 'dir) "foo/"))
  (should (equal (grease--normalize-name "foo" 'file) "foo"))
  (should (equal (grease--normalize-name "foo/" 'file) "foo")))

(ert-deftest grease-test-add-copy-suffix ()
  "Test copy suffix addition."
  (should (equal (grease--add-copy-suffix "file.txt") "file-copy.txt"))
  (should (equal (grease--add-copy-suffix "file") "file-copy"))
  (should (equal (grease--add-copy-suffix "file.tar.gz") "file.tar-copy.gz"))
  (should (equal (grease--add-copy-suffix "dir/") "dir-copy/")))

(ert-deftest grease-test-hidden-file-p ()
  "Test hidden file detection."
  (should (grease--hidden-file-p ".gitignore"))
  (should (grease--hidden-file-p ".hidden"))
  (should-not (grease--hidden-file-p "visible.txt"))
  (should-not (grease--hidden-file-p "not.hidden")))

(ert-deftest grease-test-file-extension ()
  "Test file extension extraction."
  (should (equal (grease--file-extension "file.txt") "txt"))
  (should (equal (grease--file-extension "file.tar.gz") "gz"))
  (should (equal (grease--file-extension "file") ""))
  (should (equal (grease--file-extension ".gitignore") "")))

(ert-deftest grease-test-format-id ()
  "Test ID formatting."
  (should (equal (grease--format-id 1) "001"))
  (should (equal (grease--format-id 42) "042"))
  (should (equal (grease--format-id 999) "999"))
  (should (equal (grease--format-id 1000) "1000"))
  (should (equal (grease--format-id 99999) "99999")))

(ert-deftest grease-test-extract-id ()
  "Test ID extraction from formatted text."
  (should (equal (grease--extract-id "/001 file.txt") 1))
  (should (equal (grease--extract-id "/042 somedir/") 42))
  (should (null (grease--extract-id "plain text")))
  (should (null (grease--extract-id ""))))

(ert-deftest grease-test-extract-filename ()
  "Test filename extraction from formatted lines."
  (should (equal (grease--extract-filename "/001 file.txt") "file.txt"))
  (should (equal (grease--extract-filename "/042 somedir/") "somedir/"))
  (should (equal (grease--extract-filename "plain.txt") "plain.txt")))

;;;; Filesystem Integration Tests

(ert-deftest grease-test-create-file ()
  "Test file creation via grease."
  (grease-test-with-temp-dir
    (let ((test-file (expand-file-name "newfile.txt" temp-dir)))
      (grease--apply-changes `((:create ,test-file)))
      (should (file-exists-p test-file)))))

(ert-deftest grease-test-create-directory ()
  "Test directory creation via grease."
  (grease-test-with-temp-dir
    (let ((test-dir (expand-file-name "newdir/" temp-dir)))
      (grease--apply-changes `((:create ,test-dir)))
      (should (file-directory-p test-dir)))))

(ert-deftest grease-test-create-nested-directory ()
  "Test nested directory creation."
  (grease-test-with-temp-dir
    (let ((test-dir (expand-file-name "a/b/c/" temp-dir)))
      (grease--apply-changes `((:create ,test-dir)))
      (should (file-directory-p test-dir)))))

(ert-deftest grease-test-delete-file ()
  "Test file deletion via grease."
  (grease-test-with-temp-dir
    (let ((test-file (expand-file-name "deleteme.txt" temp-dir)))
      (write-region "test" nil test-file)
      (should (file-exists-p test-file))
      (grease--apply-changes `((:delete ,test-file)))
      (should-not (file-exists-p test-file)))))

(ert-deftest grease-test-delete-directory ()
  "Test directory deletion via grease."
  (grease-test-with-temp-dir
    (let ((test-dir (expand-file-name "deleteme" temp-dir)))
      (make-directory test-dir)
      (should (file-directory-p test-dir))
      (grease--apply-changes `((:delete ,test-dir)))
      (should-not (file-exists-p test-dir)))))

(ert-deftest grease-test-delete-directory-recursive ()
  "Test recursive directory deletion."
  (grease-test-with-temp-dir
    (let ((test-dir (expand-file-name "parent" temp-dir)))
      (make-directory (expand-file-name "child" test-dir) t)
      (write-region "x" nil (expand-file-name "child/file.txt" test-dir))
      (grease--apply-changes `((:delete ,test-dir)))
      (should-not (file-exists-p test-dir)))))

(ert-deftest grease-test-rename-file ()
  "Test file renaming via grease."
  (grease-test-with-temp-dir
    (let ((old-file (expand-file-name "old.txt" temp-dir))
          (new-file (expand-file-name "new.txt" temp-dir)))
      (write-region "content" nil old-file)
      (grease--apply-changes `((:rename ,old-file ,new-file)))
      (should-not (file-exists-p old-file))
      (should (file-exists-p new-file))
      (should (equal (with-temp-buffer
                       (insert-file-contents new-file)
                       (buffer-string))
                     "content")))))

(ert-deftest grease-test-rename-directory ()
  "Test directory renaming."
  (grease-test-with-temp-dir
    (let ((old-dir (expand-file-name "olddir" temp-dir))
          (new-dir (expand-file-name "newdir" temp-dir)))
      (make-directory old-dir)
      (write-region "inside" nil (expand-file-name "file.txt" old-dir))
      (grease--apply-changes `((:rename ,old-dir ,new-dir)))
      (should-not (file-exists-p old-dir))
      (should (file-directory-p new-dir))
      (should (file-exists-p (expand-file-name "file.txt" new-dir))))))

(ert-deftest grease-test-copy-file ()
  "Test file copying via grease."
  (grease-test-with-temp-dir
    (let ((src-file (expand-file-name "source.txt" temp-dir))
          (dst-file (expand-file-name "dest.txt" temp-dir)))
      (write-region "original" nil src-file)
      (grease--apply-changes `((:copy ,src-file ,dst-file)))
      (should (file-exists-p src-file))
      (should (file-exists-p dst-file))
      (should (equal (with-temp-buffer
                       (insert-file-contents dst-file)
                       (buffer-string))
                     "original")))))

(ert-deftest grease-test-copy-directory ()
  "Test directory copying."
  (grease-test-with-temp-dir
    (let ((src-dir (expand-file-name "srcdir" temp-dir))
          (dst-dir (expand-file-name "dstdir" temp-dir)))
      (make-directory src-dir)
      (write-region "data" nil (expand-file-name "inner.txt" src-dir))
      (grease--apply-changes `((:copy ,src-dir ,dst-dir)))
      (should (file-directory-p src-dir))
      (should (file-directory-p dst-dir))
      (should (file-exists-p (expand-file-name "inner.txt" dst-dir))))))

(ert-deftest grease-test-move-file ()
  "Test file moving via grease."
  (grease-test-with-temp-dir
    (let* ((subdir (expand-file-name "subdir" temp-dir))
           (src-file (expand-file-name "moveme.txt" temp-dir))
           (dst-file (expand-file-name "moveme.txt" subdir)))
      (make-directory subdir)
      (write-region "moving" nil src-file)
      (grease--apply-changes `((:move ,src-file ,dst-file)))
      (should-not (file-exists-p src-file))
      (should (file-exists-p dst-file))
      (should (equal (with-temp-buffer
                       (insert-file-contents dst-file)
                       (buffer-string))
                     "moving")))))

(ert-deftest grease-test-move-directory ()
  "Test directory moving."
  (grease-test-with-temp-dir
    (let* ((src-dir (expand-file-name "movedir" temp-dir))
           (dst-parent (expand-file-name "dest" temp-dir))
           (dst-dir (expand-file-name "movedir" dst-parent)))
      (make-directory src-dir)
      (make-directory dst-parent)
      (write-region "x" nil (expand-file-name "f.txt" src-dir))
      (grease--apply-changes `((:move ,src-dir ,dst-dir)))
      (should-not (file-exists-p src-dir))
      (should (file-directory-p dst-dir))
      (should (file-exists-p (expand-file-name "f.txt" dst-dir))))))

;;;; Sorting Tests

(ert-deftest grease-test-sort-by-type ()
  "Test that directories are sorted before files."
  (grease-test-with-temp-dir
    (make-directory (expand-file-name "zdir" temp-dir))
    (write-region "" nil (expand-file-name "afile.txt" temp-dir))
    (make-directory (expand-file-name "adir" temp-dir))
    (write-region "" nil (expand-file-name "zfile.txt" temp-dir))
    (let* ((files '("afile.txt" "zfile.txt" "adir" "zdir"))
           (grease-sort-method 'type)
           (sorted (grease--sort-files files temp-dir)))
      (should (equal sorted '("adir" "zdir" "afile.txt" "zfile.txt"))))))

(ert-deftest grease-test-sort-by-name ()
  "Test alphabetical sorting."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "charlie.txt" temp-dir))
    (write-region "" nil (expand-file-name "alpha.txt" temp-dir))
    (write-region "" nil (expand-file-name "bravo.txt" temp-dir))
    (let* ((files '("charlie.txt" "alpha.txt" "bravo.txt"))
           (grease-sort-method 'name)
           (grease-sort-directories-first nil)
           (sorted (grease--sort-files files temp-dir)))
      (should (equal sorted '("alpha.txt" "bravo.txt" "charlie.txt"))))))

(ert-deftest grease-test-sort-by-extension ()
  "Test sorting by file extension."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "b.txt" temp-dir))
    (write-region "" nil (expand-file-name "a.el" temp-dir))
    (write-region "" nil (expand-file-name "c.md" temp-dir))
    (let* ((files '("b.txt" "a.el" "c.md"))
           (grease-sort-method 'extension)
           (grease-sort-directories-first nil)
           (sorted (grease--sort-files files temp-dir)))
      (should (equal sorted '("a.el" "c.md" "b.txt"))))))

(ert-deftest grease-test-sort-by-size ()
  "Test sorting by file size."
  (grease-test-with-temp-dir
    (write-region "aaa" nil (expand-file-name "medium.txt" temp-dir))
    (write-region "a" nil (expand-file-name "small.txt" temp-dir))
    (write-region "aaaaa" nil (expand-file-name "large.txt" temp-dir))
    (let* ((files '("medium.txt" "small.txt" "large.txt"))
           (grease-sort-method 'size)
           (grease-sort-directories-first nil)
           (sorted (grease--sort-files files temp-dir)))
      (should (equal sorted '("small.txt" "medium.txt" "large.txt"))))))

(ert-deftest grease-test-sort-by-size-desc ()
  "Test sorting by file size descending."
  (grease-test-with-temp-dir
    (write-region "aaa" nil (expand-file-name "medium.txt" temp-dir))
    (write-region "a" nil (expand-file-name "small.txt" temp-dir))
    (write-region "aaaaa" nil (expand-file-name "large.txt" temp-dir))
    (let* ((files '("medium.txt" "small.txt" "large.txt"))
           (grease-sort-method 'size-desc)
           (grease-sort-directories-first nil)
           (sorted (grease--sort-files files temp-dir)))
      (should (equal sorted '("large.txt" "medium.txt" "small.txt"))))))

;;;; Hidden Files Tests

(ert-deftest grease-test-filter-hidden-global ()
  "Test hidden file filtering with global setting."
  (let ((files '(".hidden" "visible" ".git" "normal.txt"))
        (grease--show-hidden-initialized nil))
    (let ((grease-show-hidden nil))
      (should (equal (grease--filter-hidden files) '("visible" "normal.txt"))))
    (let ((grease-show-hidden t))
      (should (equal (grease--filter-hidden files) files)))))

(ert-deftest grease-test-filter-hidden-buffer-local ()
  "Test hidden file filtering respects buffer-local override."
  (let ((files '(".hidden" "visible"))
        (grease-show-hidden nil)
        (grease--show-hidden-initialized t)
        (grease--current-show-hidden t))
    (should (equal (grease--filter-hidden files) files)))
  (let ((files '(".hidden" "visible"))
        (grease-show-hidden t)
        (grease--show-hidden-initialized t)
        (grease--current-show-hidden nil))
    (should (equal (grease--filter-hidden files) '("visible")))))

;;;; Conflict Detection Tests

(ert-deftest grease-test-detect-name-conflicts ()
  "Test duplicate filename detection."
  (let ((entries '((:name "foo.txt" :id 1)
                   (:name "bar.txt" :id 2)
                   (:name "foo.txt" :id 3))))
    (should (equal (grease--detect-name-conflicts entries) '("foo.txt")))))

(ert-deftest grease-test-no-conflicts ()
  "Test that unique names produce no conflicts."
  (let ((entries '((:name "foo.txt" :id 1)
                   (:name "bar.txt" :id 2)
                   (:name "baz.txt" :id 3))))
    (should (null (grease--detect-name-conflicts entries)))))

(ert-deftest grease-test-multiple-conflicts ()
  "Test detection of multiple conflicting names."
  (let ((entries '((:name "a.txt" :id 1)
                   (:name "b.txt" :id 2)
                   (:name "a.txt" :id 3)
                   (:name "b.txt" :id 4))))
    (should (= 2 (length (grease--detect-name-conflicts entries))))))

;;;; Registry Tests

(ert-deftest grease-test-register-file ()
  "Test file registration in registry."
  (grease-test-with-clean-state
    (grease-test-with-temp-dir
      (let* ((path (expand-file-name "test.txt" temp-dir))
             (id (grease--register-file path 'file)))
        (should (numberp id))
        (let ((info (grease--get-file-by-id id)))
          (should info)
          (should (equal (plist-get info :path) path))
          (should (eq (plist-get info :type) 'file)))))))

(ert-deftest grease-test-get-id-by-path ()
  "Test looking up file ID by path."
  (grease-test-with-clean-state
    (grease-test-with-temp-dir
      (let* ((path (expand-file-name "lookup.txt" temp-dir))
             (id (grease--register-file path 'file)))
        (should (equal (grease--get-id-by-path path) id))
        (should (null (grease--get-id-by-path "/nonexistent/path")))))))

(ert-deftest grease-test-mark-file-deleted ()
  "Test marking files as deleted."
  (grease-test-with-clean-state
    (grease-test-with-temp-dir
      (let* ((path (expand-file-name "todelete.txt" temp-dir))
             (id (grease--register-file path 'file)))
        (grease--mark-file-deleted id path)
        (should (equal (gethash id grease--deleted-file-ids) path))
        (let ((info (grease--get-file-by-id id)))
          (should (null (plist-get info :exists))))))))

;;;; Buffer Rendering Tests

(ert-deftest grease-test-render-creates-header ()
  "Test that rendering creates a header line."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (should (string-prefix-p " Grease" (buffer-substring (point) (line-end-position)))))))

(ert-deftest grease-test-render-shows-files ()
  "Test that rendering shows directory contents."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "visible.txt" temp-dir))
    (make-directory (expand-file-name "subdir" temp-dir))
    (grease-test-with-buffer temp-dir
      (should (string-match-p "visible.txt" (buffer-string)))
      (should (string-match-p "subdir" (buffer-string))))))

(ert-deftest grease-test-render-hides-dotfiles ()
  "Test that dotfiles are hidden by default."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name ".hidden" temp-dir))
    (write-region "" nil (expand-file-name "visible.txt" temp-dir))
    (let ((grease-show-hidden nil))
      (grease-test-with-buffer temp-dir
        (should (string-match-p "visible.txt" (buffer-string)))
        (should-not (string-match-p "\\.hidden" (buffer-string)))))))

(ert-deftest grease-test-get-line-data ()
  "Test extracting line data from buffer."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "test.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let ((data (grease--get-line-data)))
        (should data)
        (should (stringp (plist-get data :name)))
        (should (memq (plist-get data :type) '(file dir)))
        (should (numberp (plist-get data :id)))))))

(ert-deftest grease-test-insert-entry-properties ()
  "Test that inserted entries have correct text properties."
  (grease-test-with-clean-state
    (with-temp-buffer
      (grease-mode)
      (setq grease--root-dir "/tmp/")
      (grease--insert-entry 42 "testfile.txt" 'file nil nil)
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'grease-id) 42))
      (should (equal (get-text-property (point) 'grease-name) "testfile.txt"))
      (should (eq (get-text-property (point) 'grease-type) 'file)))))

;;;; Change Detection Tests

(ert-deftest grease-test-calculate-changes-empty ()
  "Test that unchanged buffer produces no changes."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "existing.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (let ((changes (grease--calculate-changes)))
        (should (null changes))))))

(ert-deftest grease-test-calculate-changes-deletion ()
  "Test detection of file deletion."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "todelete.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let ((inhibit-read-only t))
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      (let ((changes (grease--calculate-changes)))
        (should (= 1 (length changes)))
        (should (eq (caar changes) :delete))))))

(ert-deftest grease-test-calculate-changes-creation ()
  "Test detection of new file creation."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "newfile.txt\n"))
      (grease--format-plain-lines)
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if (lambda (c) (eq (car c) :create)) changes))))))

;;;; Rename Detection Tests

(ert-deftest grease-test-check-for-renames ()
  "Test rename detection logic."
  (grease-test-with-clean-state
    (grease-test-with-temp-dir
      (let* ((old-path (expand-file-name "oldname.txt" temp-dir))
             (new-name "newname.txt")
             (id (grease--register-file old-path 'file))
             (original-state (make-hash-table :test 'equal))
             (new-entries `((:name ,new-name :id ,id))))
        (setq grease--root-dir (file-name-as-directory temp-dir))
        (puthash "oldname.txt" 'file original-state)
        (let ((renames (grease--check-for-renames new-entries original-state)))
          (should (= 1 (length renames)))
          (should (eq (caar renames) :rename)))))))

;;;; Clipboard Tests

(ert-deftest grease-test-clipboard-copy ()
  "Test clipboard state after copy operation."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "tocopy.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (grease-copy)
      (should grease--clipboard)
      (should (eq (plist-get grease--clipboard :operation) 'copy))
      (should (equal (car (plist-get grease--clipboard :names)) "tocopy.txt")))))

(ert-deftest grease-test-clipboard-cut ()
  "Test clipboard state after cut operation."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "tocut.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (grease-cut)
      (should grease--clipboard)
      (should (eq (plist-get grease--clipboard :operation) 'cut))
      (should (equal (car (plist-get grease--clipboard :names)) "tocut.txt")))))

(ert-deftest grease-test-paste-after-copy ()
  "Test paste inserts entry after copy."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "original.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (grease-copy)
      (grease-paste)
      (let ((count 0))
        (goto-char (point-min))
        (while (search-forward "original" nil t)
          (cl-incf count))
        (should (>= count 2))))))

;;;; Full Path Tests

(ert-deftest grease-test-get-full-path ()
  "Test full path construction."
  (grease-test-with-clean-state
    (let ((grease--root-dir "/home/user/project/"))
      (should (equal (grease--get-full-path "file.txt")
                     "/home/user/project/file.txt"))
      (should (equal (grease--get-full-path "subdir/")
                     "/home/user/project/subdir")))))

;;;; Format Change Tests

(ert-deftest grease-test-format-change-create ()
  "Test formatting of create operations."
  (let ((grease--root-dir "/tmp/test/"))
    (should (string-match-p "\\[Create\\]"
                            (grease--format-change '(:create "/tmp/test/new.txt"))))))

(ert-deftest grease-test-format-change-delete ()
  "Test formatting of delete operations."
  (let ((grease--root-dir "/tmp/test/"))
    (should (string-match-p "\\[Delete\\]"
                            (grease--format-change '(:delete "/tmp/test/old.txt"))))))

(ert-deftest grease-test-format-change-rename ()
  "Test formatting of rename operations."
  (let ((grease--root-dir "/tmp/test/"))
    (should (string-match-p "\\[Rename\\].*->"
                            (grease--format-change '(:rename "/tmp/test/a.txt" "/tmp/test/b.txt"))))))

;;;; Cursor Constraint Tests

(ert-deftest grease-test-count-file-lines ()
  "Test counting file entry lines."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "a.txt" temp-dir))
    (write-region "" nil (expand-file-name "b.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (should (= 2 (grease--count-file-lines))))))

(ert-deftest grease-test-goto-line-clamped ()
  "Test clamped line navigation."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "only.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease--goto-line-clamped 100)
      (should (<= (line-number-at-pos) 3)))))

;;;; Scan Buffer Tests

(ert-deftest grease-test-scan-buffer-entries ()
  "Test that scan buffer returns all entries."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "one.txt" temp-dir))
    (write-region "" nil (expand-file-name "two.txt" temp-dir))
    (make-directory (expand-file-name "three" temp-dir))
    (grease-test-with-buffer temp-dir
      (let ((entries (grease--scan-buffer)))
        (should (= 3 (length entries)))))))

(provide 'grease-test)
;;; grease-test.el ends here
