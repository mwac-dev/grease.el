;;; grease-test.el --- Tests for grease.el -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'grease)


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

;;;; Filesystem Integration Tests

(defmacro grease-test-with-temp-dir (&rest body)
  "Execute BODY with a temporary directory, cleaning up afterward."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "grease-test-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

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
      ;; Directories should come first, both groups alphabetized
      (should (equal sorted '("adir" "zdir" "afile.txt" "zfile.txt"))))))

(ert-deftest grease-test-filter-hidden ()
  "Test hidden file filtering."
  (let ((files '(".hidden" "visible" ".git" "normal.txt")))
    (let ((grease-show-hidden nil))
      (should (equal (grease--filter-hidden files) '("visible" "normal.txt"))))
    (let ((grease-show-hidden t))
      (should (equal (grease--filter-hidden files) files)))))

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

(provide 'grease-test)
;;; grease-test.el ends here
