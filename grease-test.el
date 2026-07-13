;;; grease-test.el --- Tests for grease.el -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'grease)

;; Run with: emacs -batch -l grease.el -l grease-test.el -f ert-run-tests-batch-and-exit

;; or M-x ert

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

(defun grease-test-make-buffer (dir)
  "Create and render a Grease buffer for DIR without resetting global state."
  (let ((buffer (generate-new-buffer " *grease-test*")))
    (with-current-buffer buffer
      (grease-mode)
      (setq grease--root-dir
            (file-name-as-directory (expand-file-name dir)))
      (grease--render grease--root-dir))
    buffer))

(defmacro grease-test-with-buffers (bindings &rest body)
  "Create several Grease buffers and clean them up after BODY.

BINDINGS has the form ((VARIABLE DIRECTORY) ...).  All buffers share one
fresh set of Grease global state, making this suitable for transaction tests."
  (declare (indent 1))
  (let ((buffers (make-symbol "buffers")))
    `(grease-test-with-clean-state
       (let (,@(mapcar #'car bindings) ,buffers)
         (unwind-protect
             (progn
               ,@(mapcar (lambda (binding)
                           `(setq ,(car binding)
                                  (grease-test-make-buffer ,(cadr binding))
                                  ,buffers (cons ,(car binding) ,buffers)))
                         bindings)
               ,@body)
           (dolist (buffer ,buffers)
             (when (buffer-live-p buffer)
               (kill-buffer buffer))))))))

(defun grease-test-goto-entry (&optional id-or-name)
  "Move point to the entry matching ID-OR-NAME in the current buffer.
ID-OR-NAME may be a numeric file ID or a displayed filename.  Return the
entry data, or signal an error when no matching entry exists."
  (goto-char (point-min))
  (let (match)
    (while (and (not match) (not (eobp)))
      (let ((data (grease--get-line-data)))
        (when (and data
                   (if (numberp id-or-name)
                       (equal (plist-get data :id) id-or-name)
                     (equal (plist-get data :name) id-or-name)))
          (setq match data)))
      (unless match
        (forward-line 1)))
    (or match
        (error "No Grease entry matching %S" id-or-name))))

(defun grease-test-edit-entry (id-or-name new-name)
  "Rename ID-OR-NAME to NEW-NAME in the current Grease buffer."
  (let* ((data (grease-test-goto-entry id-or-name))
         (old-name (plist-get data :name))
         (line-end (line-end-position))
         (inhibit-read-only t))
    (unless (search-forward old-name line-end t)
      (error "Cannot find entry text %S" old-name))
    (replace-match new-name t t)
    (grease--update-line-metadata)
    (grease--get-line-data)))

(defun grease-test-cut-and-paste (source-buffer destination-buffer id-or-name)
  "Cut ID-OR-NAME from SOURCE-BUFFER and paste it in DESTINATION-BUFFER."
  (with-current-buffer source-buffer
    (grease-test-goto-entry id-or-name)
    (grease-cut))
  (with-current-buffer destination-buffer
    (goto-char (point-max))
    (grease-paste)))

(defun grease-test-read-file (path)
  "Return the literal contents of file PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun grease-test-create-fixture (root entries)
  "Create nested filesystem ENTRIES below ROOT.
Each entry is a plist with `:path' and `:type'.  Directory entries use type
`dir'; file entries use type `file' and may provide `:content'."
  (dolist (entry entries)
    (let ((path (expand-file-name (plist-get entry :path) root)))
      (pcase (plist-get entry :type)
        ('dir (make-directory path t))
        ('file
         (make-directory (file-name-directory path) t)
         (write-region (or (plist-get entry :content) "") nil path nil 'silent))
        (type (error "Unknown fixture type %S" type))))))

(defun grease-test--canonical-operation (operation)
  "Return a stable representation of OPERATION for test comparisons."
  (if (and (listp operation)
           (cl-evenp (length operation))
           (keywordp (car operation)))
      (let (pairs)
        (while operation
          (push (cons (pop operation) (pop operation)) pairs))
        (sort pairs (lambda (left right)
                      (string< (symbol-name (car left))
                               (symbol-name (car right))))))
    operation))

(defun grease-test-sort-operations (operations)
  "Canonicalize and sort OPERATIONS without relying on their input order."
  (sort (mapcar #'grease-test--canonical-operation (copy-tree operations))
        (lambda (left right)
          (string< (prin1-to-string left) (prin1-to-string right)))))

(defun grease-test-operations-equal-p (left right)
  "Return non-nil when operation collections LEFT and RIGHT are equivalent."
  (equal (grease-test-sort-operations left)
         (grease-test-sort-operations right)))

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

(ert-deftest grease-test-baseline-is-keyed-by-stable-id ()
  "Rendering should snapshot committed entries by ID and absolute path."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "stable.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (let* ((data (grease-test-goto-entry "stable.txt"))
             (id (plist-get data :id))
             (baseline (gethash id grease--baseline-by-id)))
        (should (equal (plist-get baseline :id) id))
        (should (equal (plist-get baseline :path)
                       (expand-file-name "stable.txt" temp-dir)))
        (should (eq (plist-get baseline :type) 'file))
        (should (plist-get baseline :committed-p))))))

(ert-deftest grease-test-file-id-survives-name-edit ()
  "Editing a filename should preserve its stable ID and committed path."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "before.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (let* ((before (grease-test-goto-entry "before.txt"))
             (id (plist-get before :id))
             (committed-path (plist-get (grease--get-file-by-id id) :path))
             (after (grease-test-edit-entry id "after.txt")))
        (should (equal (plist-get after :id) id))
        (should (equal (plist-get after :full-path)
                       (expand-file-name "after.txt" temp-dir)))
        (should (equal (plist-get (grease--get-file-by-id id) :path)
                       committed-path))))))

(ert-deftest grease-test-directory-id-survives-name-edit ()
  "Editing a directory name should preserve its stable ID."
  (grease-test-with-temp-dir
    (make-directory (expand-file-name "before" temp-dir))
    (grease-test-with-buffer temp-dir
      (let* ((before (grease-test-goto-entry "before"))
             (id (plist-get before :id))
             (after (grease-test-edit-entry id "after/")))
        (should (equal (plist-get after :id) id))
        (should (eq (plist-get after :type) 'dir))
        (should (equal (plist-get (gethash id grease--baseline-by-id) :path)
                       (expand-file-name "before" temp-dir)))))))

(ert-deftest grease-test-new-entry-is-uncommitted ()
  "A newly typed entry should have an ID distinct from committed entries."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "draft.txt\n"))
      (let* ((entry (car (grease--scan-buffer)))
             (id (plist-get entry :id))
             (registry-entry (grease--get-file-by-id id)))
        (should (numberp id))
        (should-not (plist-get entry :committed-p))
        (should-not (plist-get registry-entry :committed-p))
        (should-not (gethash id grease--baseline-by-id))))))

(ert-deftest grease-test-copy-entry-has-new-id-and-source-id ()
  "A copied entry should use a new ID linked to the committed source ID."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "source.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (let* ((source (grease-test-goto-entry "source.txt"))
             (source-id (plist-get source :id)))
        (grease-copy)
        (grease-paste)
        (let ((copy (cl-find-if
                     (lambda (entry)
                       (equal (plist-get entry :source-id) source-id))
                     (grease--scan-buffer))))
          (should copy)
          (should-not (equal (plist-get copy :id) source-id))
          (should-not (plist-get copy :committed-p))
          (should (equal (plist-get
                          (grease--get-file-by-id (plist-get copy :id))
                          :source-id)
                         source-id)))))))

(ert-deftest grease-test-cut-keeps-committed-registry-state ()
  "Staging a cut should not claim the on-disk entry has disappeared."
  (grease-test-with-temp-dir
    (let ((path (expand-file-name "move-me.txt" temp-dir)))
      (write-region "preserve me" nil path)
      (grease-test-with-buffer temp-dir
        (let* ((data (grease-test-goto-entry "move-me.txt"))
               (id (plist-get data :id)))
          (grease-cut)
          (let ((registry-entry (grease--get-file-by-id id)))
            (should (plist-get registry-entry :committed-p))
            (should (plist-get registry-entry :exists))
            (should (equal (plist-get registry-entry :path) path)))
          (should (file-exists-p path)))))))

;;;; Buffer Creation and Rendering Tests

(ert-deftest grease-test-create-buffer-returns-distinct-live-buffers ()
  "Creating the same directory twice should return two independent buffers."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((first (grease--create-buffer temp-dir))
            (second (grease--create-buffer temp-dir)))
        (unwind-protect
            (progn
              (should (buffer-live-p first))
              (should (buffer-live-p second))
              (should-not (eq first second))
              (should-not (equal (buffer-name first) (buffer-name second)))
              (dolist (buffer (list first second))
                (with-current-buffer buffer
                  (should (derived-mode-p 'grease-mode))
                  (should (equal grease--root-dir
                                 (file-name-as-directory
                                  (expand-file-name temp-dir)))))))
          (dolist (buffer (list first second))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-create-buffer-keeps-local-state-independent ()
  "Editing one created buffer should not alter the other buffer's local state."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "file.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((first (grease--create-buffer temp-dir))
            (second (grease--create-buffer temp-dir)))
        (unwind-protect
            (progn
              (with-current-buffer first
                (grease-test-edit-entry "file.txt" "renamed.txt")
                (setq grease--pending-changes '(:first-only))
                (should grease--buffer-dirty-p))
              (with-current-buffer second
                (should-not grease--buffer-dirty-p)
                (should-not grease--pending-changes)
                (should (string-match-p "file.txt" (buffer-string)))
                (should-not (string-match-p "renamed.txt" (buffer-string)))))
          (dolist (buffer (list first second))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-create-buffer-reuses-stable-file-ids ()
  "Existing files should retain their IDs across independently created buffers."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "stable.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((first (grease--create-buffer temp-dir))
            (second (grease--create-buffer temp-dir)))
        (unwind-protect
            (let ((first-id
                   (with-current-buffer first
                     (plist-get (grease-test-goto-entry "stable.txt") :id)))
                  (second-id
                   (with-current-buffer second
                     (plist-get (grease-test-goto-entry "stable.txt") :id))))
              (should (numberp first-id))
              (should (equal first-id second-id)))
          (dolist (buffer (list first second))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-direct-split-window-primitive-creates-new-buffer ()
  "Direct primitive splitting should be Grease-aware without command remapping."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)) new-buffer)
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (let ((source-window (selected-window))
                    (new-window (split-window nil nil 'right)))
                (setq new-buffer (window-buffer new-window))
                ;; Preserve the primitive's normal no-follow behavior.
                (should (eq (selected-window) source-window))
                (should-not (eq new-buffer source))
                (with-current-buffer new-buffer
                  (should (derived-mode-p 'grease-mode))
                  (should (equal grease--root-dir
                                 (file-name-as-directory
                                  (expand-file-name temp-dir)))))))
          (dolist (buffer (list source new-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-direct-split-wrapper-supports-zorg-follow-pattern ()
  "A direct split wrapper followed by `other-window' should select the new Grease buffer."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)) new-buffer)
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (split-window-right)
              (other-window 1)
              (setq new-buffer (current-buffer))
              (should-not (eq new-buffer source))
              (should (derived-mode-p 'grease-mode)))
          (dolist (buffer (list source new-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-direct-split-primitive-leaves-nongrease-behavior-unchanged ()
  "Primitive advice should leave ordinary buffers displayed in both split windows."
  (let ((buffer (generate-new-buffer " *nongrease-split-test*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buffer)
          (let ((new-window (split-window nil nil 'right)))
            (should (eq (window-buffer new-window) buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest grease-test-split-window-right-creates-new-grease-buffer ()
  "Splitting right should display and select an independent Grease buffer."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "file.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)) new-buffer)
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (let ((source-window (selected-window)))
                (grease-split-window-right)
                (setq new-buffer (current-buffer))
                (should (= 2 (length (window-list))))
                (should-not (eq (selected-window) source-window))
                (should (eq (window-buffer source-window) source))
                (should-not (eq new-buffer source))
                (should (derived-mode-p 'grease-mode))
                (should (equal grease--root-dir
                               (file-name-as-directory
                                (expand-file-name temp-dir))))))
          (dolist (buffer (list source new-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-split-window-below-creates-new-grease-buffer ()
  "Splitting below should display and select an independent Grease buffer."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)) new-buffer)
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (let ((source-window (selected-window)))
                (grease-split-window-below)
                (setq new-buffer (current-buffer))
                (should (= 2 (length (window-list))))
                (should-not (eq (selected-window) source-window))
                (should (eq (window-buffer source-window) source))
                (should-not (eq new-buffer source))
                (should (derived-mode-p 'grease-mode))))
          (dolist (buffer (list source new-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-direct-window-deletion-kills-hidden-grease-buffer ()
  "Direct `delete-window' callers should not bypass Grease buffer disposal."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)))
        (save-window-excursion
          (delete-other-windows)
          (let* ((source-window (split-window-right))
                 (other-window (selected-window)))
            (set-window-buffer source-window source)
            (set-window-buffer other-window (get-buffer-create "*scratch*"))
            ;; Delete the non-selected Grease window, as ace-window and other
            ;; window managers commonly do.
            (delete-window source-window)
            (should-not (buffer-live-p source))))))))

(ert-deftest grease-test-direct-window-deletion-cancel-keeps-window ()
  "Direct deletion should be vetoed when unified saving is cancelled."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "old.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (let ((source-window (split-window-right)))
                (set-window-buffer source-window source)
                (with-current-buffer source
                  (grease-test-edit-entry "old.txt" "new.txt"))
                (cl-letf (((symbol-function 'grease-save-all-buffers)
                           (lambda () nil)))
                  (delete-window source-window))
                (should (window-live-p source-window))
                (should (buffer-live-p source))))
          (when (buffer-live-p source)
            (kill-buffer source)))))))

(ert-deftest grease-test-direct-window-deletion-leaves-nongrease-buffer-live ()
  "Global deletion advice should preserve normal behavior for other buffers."
  (let ((buffer (generate-new-buffer " *nongrease-window-test*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((window (split-window-right)))
            (set-window-buffer window buffer)
            (delete-window window)
            (should (buffer-live-p buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest grease-test-close-commands-are-remapped-in-grease-mode ()
  "Emacs and Evil close-window commands should use Grease-aware cleanup."
  (with-temp-buffer
    (grease-mode)
    (dolist (command '(delete-window kill-buffer-and-window evil-window-delete))
      (should (eq (command-remapping command) 'grease-close-window)))))

(ert-deftest grease-test-close-last-displayed-window-kills-clean-buffer ()
  "Closing a clean buffer's last displayed window should kill the buffer."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((other-window
                 (let ((grease--splitting-window-p t))
                   (split-window-right))))
            (set-window-buffer other-window (get-buffer-create "*scratch*"))
            (should (grease-close-window))
            (should-not (buffer-live-p source))))))))

(ert-deftest grease-test-close-dirty-buffer-kills-only-after-save-success ()
  "A successful unified save should allow last-window buffer disposal."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "old.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir))
            save-called)
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((other-window
                 (let ((grease--splitting-window-p t))
                   (split-window-right))))
            (set-window-buffer other-window (get-buffer-create "*scratch*")))
          (grease-test-edit-entry "old.txt" "new.txt")
          (cl-letf (((symbol-function 'grease-save-all-buffers)
                     (lambda () (setq save-called t) t)))
            (should (grease-close-window)))
          (should save-called)
          (should-not (buffer-live-p source)))))))

(ert-deftest grease-test-close-one-of-multiple-views-keeps-buffer-live ()
  "Closing one window should not kill a Grease buffer still displayed elsewhere."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (let ((grease--splitting-window-p t))
                (split-window-right))
              (should (= 2 (length (get-buffer-window-list source nil t))))
              (should (grease-close-window))
              (should (buffer-live-p source))
              (should (= 1 (length (get-buffer-window-list source nil t)))))
          (when (buffer-live-p source)
            (kill-buffer source)))))))

(ert-deftest grease-test-close-cancel-keeps-window-and-dirty-buffer ()
  "Cancelling unified save should prevent window and buffer disposal."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "old.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (let ((other-window
                     (let ((grease--splitting-window-p t))
                       (split-window-right))))
                (set-window-buffer other-window (get-buffer-create "*scratch*")))
              (grease-test-edit-entry "old.txt" "new.txt")
              (cl-letf (((symbol-function 'grease-save-all-buffers)
                         (lambda () nil)))
                (should-not (grease-close-window)))
              (should (buffer-live-p source))
              (should (get-buffer-window source t))
              (with-current-buffer source
                (should grease--buffer-dirty-p)))
          (when (buffer-live-p source)
            (kill-buffer source)))))))

(ert-deftest grease-test-kill-hidden-buffers-removes-clean-grease-buffers ()
  "Explicit cleanup should kill every clean undisplayed Grease buffer."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((first (grease--create-buffer temp-dir))
            (second (grease--create-buffer temp-dir)))
        (should (= 2 (grease-kill-hidden-buffers)))
        (should-not (buffer-live-p first))
        (should-not (buffer-live-p second))))))

(ert-deftest grease-test-kill-hidden-buffers-cancel-keeps-dirty-buffers ()
  "Cancelled unified saving should leave hidden dirty Grease buffers alive."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "old.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((buffer (grease--create-buffer temp-dir)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (grease-test-edit-entry "old.txt" "new.txt"))
              (cl-letf (((symbol-function 'grease-save-all-buffers)
                         (lambda () nil)))
                (should (= 0 (grease-kill-hidden-buffers))))
              (should (buffer-live-p buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest grease-test-split-commands-are-remapped-in-grease-mode ()
  "Standard, Evil, and Doom split commands should use Grease-aware splits."
  (with-temp-buffer
    (grease-mode)
    (dolist (pair '((split-window-right . grease-split-window-right)
                    (split-window-below . grease-split-window-below)
                    (evil-window-vsplit . grease-split-window-right)
                    (evil-window-split . grease-split-window-below)
                    (+evil/window-vsplit-and-follow . grease-split-window-right)
                    (+evil/window-split-and-follow . grease-split-window-below)))
      (should (eq (command-remapping (car pair)) (cdr pair))))))

(ert-deftest grease-test-split-remapping-is-local-to-grease-mode ()
  "Non-Grease buffers should retain ordinary split command behavior."
  (with-temp-buffer
    (fundamental-mode)
    (should-not (command-remapping 'split-window-right))
    (should-not (command-remapping 'split-window-below))
    (should-not (command-remapping 'evil-window-vsplit))
    (should-not (command-remapping 'evil-window-split))))

(ert-deftest grease-test-standard-split-key-uses-remapped-command ()
  "A normal Emacs split key should resolve to the Grease-aware command."
  (with-temp-buffer
    (grease-mode)
    (should (eq (key-binding (kbd "C-x 2"))
                'grease-split-window-below))
    (should (eq (key-binding (kbd "C-x 3"))
                'grease-split-window-right))))

(ert-deftest grease-test-preview-window-does-not-create-grease-split-buffer ()
  "Opening a preview should remain separate from Grease-aware split commands."
  (grease-test-with-temp-dir
    (write-region "preview" nil (expand-file-name "file.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)) preview-buffer)
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (grease-test-goto-entry "file.txt")
              (cl-letf (((symbol-function 'grease--create-buffer)
                         (lambda (&rest _)
                           (error "Preview must not create a Grease buffer"))))
                (grease--open-preview))
              (setq preview-buffer grease--preview-buffer)
              (should (buffer-live-p preview-buffer))
              (should-not (with-current-buffer preview-buffer
                            (derived-mode-p 'grease-mode))))
          (when (buffer-live-p source)
            (with-current-buffer source
              (grease--close-preview)))
          (dolist (buffer (list source preview-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-split-renders-committed-not-dirty-source-state ()
  "A split should render disk state rather than clone uncommitted source text."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "disk.txt" temp-dir))
    (grease-test-with-clean-state
      (let ((source (grease--create-buffer temp-dir)) new-buffer)
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer source)
              (grease-test-edit-entry "disk.txt" "dirty.txt")
              (grease-split-window-right)
              (setq new-buffer (current-buffer))
              (should (string-match-p "disk.txt" (buffer-string)))
              (should-not (string-match-p "dirty.txt" (buffer-string)))
              (should-not grease--buffer-dirty-p)
              (with-current-buffer source
                (should grease--buffer-dirty-p)
                (should (string-match-p "dirty.txt" (buffer-string)))))
          (dolist (buffer (list source new-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest grease-test-create-buffer-does-not-display-buffer ()
  "Creating a Grease buffer should not change the selected window's buffer."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((original (current-buffer))
            (created (grease--create-buffer temp-dir)))
        (unwind-protect
            (progn
              (should (eq (current-buffer) original))
              (should-not (eq created original)))
          (when (buffer-live-p created)
            (kill-buffer created)))))))

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

;;;; ID-Based Diff Tests

(defun grease-test-baseline (&rest entries)
  "Return an ID-keyed baseline hash table containing ENTRIES."
  (let ((baseline (make-hash-table :test 'eql)))
    (dolist (entry entries baseline)
      (puthash (plist-get entry :id) entry baseline))))

(ert-deftest grease-test-diff-by-id-file-rename ()
  "A committed file at a new path should produce one relocation."
  (let* ((baseline (grease-test-baseline
                    '(:id 1 :path "/tmp/old.txt" :type file :committed-p t)))
         (current '((:id 1 :path "/tmp/new.txt" :type file :committed-p t))))
    (should (grease-test-operations-equal-p
             (grease--diff-by-id baseline current)
             '((:kind relocate :id 1 :src "/tmp/old.txt"
                      :dst "/tmp/new.txt" :type file))))))

(ert-deftest grease-test-diff-by-id-directory-rename ()
  "A committed directory at a new path should produce one relocation."
  (let* ((baseline (grease-test-baseline
                    '(:id 4 :path "/tmp/old" :type dir :committed-p t)))
         (current '((:id 4 :path "/tmp/new" :type dir :committed-p t))))
    (should (grease-test-operations-equal-p
             (grease--diff-by-id baseline current)
             '((:kind relocate :id 4 :src "/tmp/old"
                      :dst "/tmp/new" :type dir))))))

(ert-deftest grease-test-diff-by-id-two-way-swap ()
  "Two identities swapping occupied names should both relocate."
  (let* ((baseline (grease-test-baseline
                    '(:id 1 :path "/tmp/a" :type file :committed-p t)
                    '(:id 2 :path "/tmp/b" :type file :committed-p t)))
         (current '((:id 1 :path "/tmp/b" :type file :committed-p t)
                    (:id 2 :path "/tmp/a" :type file :committed-p t))))
    (should (grease-test-operations-equal-p
             (grease--diff-by-id baseline current)
             '((:kind relocate :id 1 :src "/tmp/a" :dst "/tmp/b" :type file)
               (:kind relocate :id 2 :src "/tmp/b" :dst "/tmp/a" :type file))))))

(ert-deftest grease-test-diff-by-id-three-way-rotation ()
  "Three identities rotating names should retain their destination mapping."
  (let* ((baseline (grease-test-baseline
                    '(:id 1 :path "/tmp/a" :type file :committed-p t)
                    '(:id 2 :path "/tmp/b" :type file :committed-p t)
                    '(:id 3 :path "/tmp/c" :type file :committed-p t)))
         (current '((:id 1 :path "/tmp/b" :type file :committed-p t)
                    (:id 2 :path "/tmp/c" :type file :committed-p t)
                    (:id 3 :path "/tmp/a" :type file :committed-p t))))
    (should (grease-test-operations-equal-p
             (grease--diff-by-id baseline current)
             '((:kind relocate :id 1 :src "/tmp/a" :dst "/tmp/b" :type file)
               (:kind relocate :id 2 :src "/tmp/b" :dst "/tmp/c" :type file)
               (:kind relocate :id 3 :src "/tmp/c" :dst "/tmp/a" :type file))))))

(ert-deftest grease-test-diff-by-id-create-copy-delete-and-unchanged ()
  "The identity diff should classify all non-relocation cases correctly."
  (let* ((baseline (grease-test-baseline
                    '(:id 1 :path "/tmp/source" :type file :committed-p t)
                    '(:id 2 :path "/tmp/delete" :type file :committed-p t)
                    '(:id 3 :path "/tmp/same" :type dir :committed-p t)))
         (current '((:id 1 :path "/tmp/source" :type file :committed-p t)
                    (:id 3 :path "/tmp/same" :type dir :committed-p t)
                    (:id 4 :path "/tmp/new" :type file :committed-p nil)
                    (:id 5 :path "/tmp/copied" :type file :committed-p nil
                         :source-id 1)))
         (expected '((:kind delete :id 2 :src "/tmp/delete" :type file)
                     (:kind create :id 4 :dst "/tmp/new" :type file)
                     (:kind copy :id 5 :source-id 1 :src "/tmp/source"
                            :dst "/tmp/copied" :type file))))
    (should (grease-test-operations-equal-p
             (grease--diff-by-id baseline current)
             expected))))

(ert-deftest grease-test-diff-by-id-does-not-consult-filename-occupancy ()
  "Relocation detection should depend on identity, not occupied names."
  (let* ((baseline (grease-test-baseline
                    '(:id 10 :path "/tmp/foo" :type dir :committed-p t)
                    '(:id 20 :path "/tmp/bar" :type dir :committed-p t)))
         (current '((:id 10 :path "/tmp/bar" :type dir :committed-p t)
                    (:id 20 :path "/tmp/foo" :type dir :committed-p t)))
         (operations (grease--diff-by-id baseline current)))
    (should (equal (plist-get
                    (cl-find 10 operations
                             :key (lambda (operation)
                                    (plist-get operation :id)))
                    :dst)
                   "/tmp/bar"))
    (should (equal (plist-get
                    (cl-find 20 operations
                             :key (lambda (operation)
                                    (plist-get operation :id)))
                    :dst)
                   "/tmp/foo"))))

;;;; Unified Transaction Collection Tests

(ert-deftest grease-test-build-transaction-cross-buffer-cut-is-relocation ()
  "Cutting in one buffer and pasting in another should produce one relocation."
  (grease-test-with-temp-dir
    (let ((source-dir (expand-file-name "source" temp-dir))
          (target-dir (expand-file-name "target" temp-dir)))
      (make-directory source-dir)
      (make-directory target-dir)
      (write-region "content" nil (expand-file-name "move.txt" source-dir))
      (grease-test-with-buffers ((source source-dir) (target target-dir))
        (grease-test-cut-and-paste source target "move.txt")
        (let ((operations (plist-get
                           (grease--build-transaction (list source target))
                           :operations)))
          (should (grease-test-operations-equal-p
                   operations
                   `((:kind relocate :id ,(with-current-buffer target
                                            (plist-get
                                             (grease-test-goto-entry "move.txt") :id))
                            :src ,(expand-file-name "move.txt" source-dir)
                            :dst ,(expand-file-name "move.txt" target-dir)
                            :type file)))))))))

(ert-deftest grease-test-build-transaction-is-independent-of-buffer-order ()
  "The initiating buffer order should not change transaction semantics."
  (grease-test-with-temp-dir
    (let ((source-dir (expand-file-name "source" temp-dir))
          (target-dir (expand-file-name "target" temp-dir)))
      (make-directory source-dir)
      (make-directory target-dir)
      (write-region "content" nil (expand-file-name "move.txt" source-dir))
      (grease-test-with-buffers ((source source-dir) (target target-dir))
        (grease-test-cut-and-paste source target "move.txt")
        (should (grease-test-operations-equal-p
                 (plist-get (grease--build-transaction (list source target))
                            :operations)
                 (plist-get (grease--build-transaction (list target source))
                            :operations)))))))

(ert-deftest grease-test-build-transaction-cross-buffer-copy ()
  "Cross-buffer copy should not delete its committed source."
  (grease-test-with-temp-dir
    (let ((source-dir (expand-file-name "source" temp-dir))
          (target-dir (expand-file-name "target" temp-dir)))
      (make-directory source-dir)
      (make-directory target-dir)
      (write-region "content" nil (expand-file-name "copy.txt" source-dir))
      (grease-test-with-buffers ((source source-dir) (target target-dir))
        (with-current-buffer source
          (grease-test-goto-entry "copy.txt")
          (grease-copy))
        (with-current-buffer target
          (goto-char (point-max))
          (grease-paste))
        (let ((operations (plist-get (grease--build-transaction) :operations)))
          (should (= 1 (length operations)))
          (should (eq (plist-get (car operations) :kind) 'copy)))))))

(ert-deftest grease-test-build-transaction-source-removal-is-delete ()
  "Removing a committed identity without placing it should remain a deletion."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "delete.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "delete.txt")
      (grease-cut)
      (let ((operations (plist-get
                         (grease--build-transaction (list (current-buffer)))
                         :operations)))
        (should (= 1 (length operations)))
        (should (eq (plist-get (car operations) :kind) 'delete))))))

(ert-deftest grease-test-build-transaction-includes-previous-directory ()
  "A snapshot staged before navigation should be included in the transaction."
  (grease-test-with-temp-dir
    (let ((first-dir (expand-file-name "first" temp-dir))
          (second-dir (expand-file-name "second" temp-dir)))
      (make-directory first-dir)
      (make-directory second-dir)
      (write-region "content" nil (expand-file-name "old.txt" first-dir))
      (grease-test-with-buffer first-dir
        (grease-test-edit-entry "old.txt" "new.txt")
        (grease--stage-current-directory)
        (grease--render second-dir t)
        (let ((operations (plist-get
                           (grease--build-transaction (list (current-buffer)))
                           :operations)))
          (should (= 1 (length operations)))
          (should (eq (plist-get (car operations) :kind) 'relocate))
          (should (equal (plist-get (car operations) :dst)
                         (expand-file-name "new.txt" first-dir))))))))

(ert-deftest grease-test-save-includes-previous-directory-snapshot ()
  "Saving should apply changes staged before navigating to another directory."
  (grease-test-with-temp-dir
    (let ((first-dir (expand-file-name "first" temp-dir))
          (second-dir (expand-file-name "second" temp-dir)))
      (make-directory first-dir)
      (make-directory second-dir)
      (write-region "content" nil (expand-file-name "old.txt" first-dir))
      (grease-test-with-buffer first-dir
        (grease-test-edit-entry "old.txt" "new.txt")
        (grease--stage-current-directory)
        (grease--render second-dir t)
        (let ((grease-skip-confirm-for-simple-edits t))
          (should (grease-save)))
        (should-not (file-exists-p (expand-file-name "old.txt" first-dir)))
        (should (equal (grease-test-read-file
                        (expand-file-name "new.txt" first-dir))
                       "content"))))))

(ert-deftest grease-test-build-transaction-rejects-conflicting-id-paths ()
  "Dirty buffers assigning one ID to different paths should abort."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "same.txt" temp-dir))
    (grease-test-with-buffers ((left temp-dir) (right temp-dir))
      (with-current-buffer left
        (grease-test-edit-entry "same.txt" "left.txt"))
      (with-current-buffer right
        (grease-test-edit-entry "same.txt" "right.txt"))
      (should-error (grease--build-transaction (list left right))
                    :type 'user-error))))

(ert-deftest grease-test-build-transaction-rejects-duplicate-destination ()
  "Different IDs claiming one destination should abort before mutation."
  (let* ((buffer (current-buffer))
         (baseline (grease-test-baseline
                    '(:id 1 :path "/tmp/a" :type file :committed-p t)
                    '(:id 2 :path "/tmp/b" :type file :committed-p t)))
         (state (list :root-dir "/tmp/" :baseline baseline
                      :entries '((:id 1 :path "/tmp/same" :type file)
                                 (:id 2 :path "/tmp/same" :type file)))))
    (cl-letf (((symbol-function 'grease--collect-buffer-state)
               (lambda (_buffer) (list :buffer buffer :states (list state)))))
      (should-error (grease--build-transaction (list buffer))
                    :type 'user-error))))

(ert-deftest grease-test-collect-buffer-state-ignores-whitespace-only-edit ()
  "A whitespace-only dirty buffer should contribute no operations."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "   \n"))
      (should grease--buffer-dirty-p)
      (should-not (grease--collect-buffer-state (current-buffer)))
      (should-not grease--buffer-dirty-p))))

;;;; Transaction Planning Tests

(ert-deftest grease-test-plan-orders-relocation-chain ()
  "A destination must be vacated before another relocation occupies it."
  (grease-test-with-temp-dir
    (let ((a (expand-file-name "a" temp-dir))
          (b (expand-file-name "b" temp-dir))
          (c (expand-file-name "c" temp-dir)))
      (write-region "a" nil a)
      (write-region "b" nil b)
      (let ((plan (grease--plan-transaction
                   `((:kind relocate :id 1 :src ,a :dst ,b :type file)
                     (:kind relocate :id 2 :src ,b :dst ,c :type file)))))
        (should (equal (mapcar (lambda (operation) (plist-get operation :id)) plan)
                       '(2 1)))))))

(ert-deftest grease-test-plan-orders-copy-before-source-relocation ()
  "Copies must run before their source is relocated."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (copy (expand-file-name "copy" temp-dir))
          (moved (expand-file-name "moved" temp-dir)))
      (write-region "content" nil source)
      (let ((plan (grease--plan-transaction
                   `((:kind relocate :id 1 :src ,source :dst ,moved :type file)
                     (:kind copy :id 2 :source-id 1 :src ,source
                            :dst ,copy :type file)))))
        (should (equal (mapcar (lambda (operation) (plist-get operation :kind)) plan)
                       '(copy relocate)))))))

(ert-deftest grease-test-plan-orders-delete-before-reuse ()
  "An explicit deletion should unblock its path before relocation."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name "target" temp-dir)))
      (write-region "source" nil source)
      (write-region "target" nil target)
      (let ((plan (grease--plan-transaction
                   `((:kind relocate :id 1 :src ,source :dst ,target :type file)
                     (:kind delete :id 2 :src ,target :type file)))))
        (should (equal (mapcar (lambda (operation) (plist-get operation :kind)) plan)
                       '(delete relocate)))))))

(ert-deftest grease-test-plan-rejects-external-occupied-destination ()
  "A destination outside the transaction must never be overwritten."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name "occupied" temp-dir)))
      (write-region "source" nil source)
      (write-region "external" nil target)
      (should-error
       (grease--plan-transaction
        `((:kind relocate :id 1 :src ,source :dst ,target :type file)))
       :type 'user-error))))

(ert-deftest grease-test-plan-detects-hidden-occupied-destination ()
  "Hidden destination entries should still block a transaction."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name ".hidden" temp-dir)))
      (write-region "source" nil source)
      (write-region "hidden" nil target)
      (should-error
       (grease--plan-transaction
        `((:kind relocate :id 1 :src ,source :dst ,target :type file)))
       :type 'user-error))))

(ert-deftest grease-test-plan-rejects-duplicate-destination ()
  "Planning should reject two operations with the same destination."
  (grease-test-with-temp-dir
    (let ((a (expand-file-name "a" temp-dir))
          (b (expand-file-name "b" temp-dir))
          (target (expand-file-name "target" temp-dir)))
      (write-region "a" nil a)
      (write-region "b" nil b)
      (should-error
       (grease--plan-transaction
        `((:kind relocate :id 1 :src ,a :dst ,target :type file)
          (:kind relocate :id 2 :src ,b :dst ,target :type file)))
       :type 'user-error))))

(ert-deftest grease-test-plan-rejects-directory-into-itself ()
  "A directory cannot be relocated beneath itself."
  (grease-test-with-temp-dir
    (let* ((source (expand-file-name "dir" temp-dir))
           (target (expand-file-name "dir/child/new" temp-dir)))
      (make-directory source)
      (should-error
       (grease--plan-transaction
        `((:kind relocate :id 1 :src ,source :dst ,target :type dir)))
       :type 'user-error))))

(ert-deftest grease-test-plan-normalizes-directory-destination-before-parent-check ()
  "A display-style trailing slash must not make a directory its own parent."
  (grease-test-with-temp-dir
    (let* ((destination (file-name-as-directory
                         (expand-file-name "test" temp-dir)))
           (plan (grease--plan-transaction
                  `((:kind create :id 1 :dst ,destination :type dir))))
           (operation (car plan)))
      (should (= 1 (length plan)))
      (should (equal (plist-get operation :dst)
                     (expand-file-name "test" temp-dir)))
      (should (eq (plist-get operation :type) 'dir)))))

(ert-deftest grease-test-plan-parent-check-is-consistent-for-files-and-directories ()
  "File and directory destinations should resolve to the same real parent."
  (grease-test-with-temp-dir
    (let ((file-plan
           (grease--plan-transaction
            `((:kind create :id 1
                     :dst ,(expand-file-name "new.txt" temp-dir) :type file))))
          (directory-plan
           (grease--plan-transaction
            `((:kind create :id 2
                     :dst ,(file-name-as-directory
                            (expand-file-name "new-dir" temp-dir))
                     :type dir)))))
      (should (= 1 (length file-plan)))
      (should (= 1 (length directory-plan))))))

(ert-deftest grease-test-plan-expands-missing-directory-parents ()
  "Missing parents should become explicit ordered directory creates."
  (grease-test-with-temp-dir
    (let* ((parent (expand-file-name "parent" temp-dir))
           (child (expand-file-name "parent/child" temp-dir))
           (plan (grease--plan-transaction
                  `((:kind create :id 10 :dst ,child :type dir)))))
      (should (= 2 (length plan)))
      (should (equal (mapcar (lambda (operation) (plist-get operation :dst)) plan)
                     (list parent child)))
      (should (plist-get (car plan) :implicit-parent-p))
      (should (eq (plist-get (car plan) :type) 'dir)))))

(ert-deftest grease-test-plan-expands-all-parents-for-nested-file ()
  "A nested file create should explicitly plan every missing ancestor."
  (grease-test-with-temp-dir
    (let* ((first (expand-file-name "one" temp-dir))
           (second (expand-file-name "one/two" temp-dir))
           (file (expand-file-name "one/two/file.txt" temp-dir))
           (plan (grease--plan-transaction
                  `((:kind create :id 20 :dst ,file :type file)))))
      (should (equal (mapcar (lambda (operation) (plist-get operation :dst)) plan)
                     (list first second file)))
      (should (equal (mapcar (lambda (operation) (plist-get operation :type)) plan)
                     '(dir dir file))))))

(ert-deftest grease-test-plan-does-not-duplicate-scheduled-parent ()
  "An explicit parent create should not gain a duplicate implicit operation."
  (grease-test-with-temp-dir
    (let* ((parent (expand-file-name "parent" temp-dir))
           (child (expand-file-name "parent/child.txt" temp-dir))
           (plan (grease--plan-transaction
                  `((:kind create :id 1 :dst ,parent :type dir)
                    (:kind create :id 2 :dst ,child :type file)))))
      (should (= 2 (length plan)))
      (should (= 1 (cl-count parent plan
                             :key (lambda (operation)
                                    (plist-get operation :dst))
                             :test #'equal))))))

(ert-deftest grease-test-plan-rejects-nondirectory-ancestor-before-mutation ()
  "An occupied file ancestor should abort without running the executor."
  (grease-test-with-temp-dir
    (let ((ancestor (expand-file-name "occupied" temp-dir))
          (executions 0))
      (write-region "not a directory" nil ancestor)
      (cl-letf (((symbol-function 'grease--execute-transaction)
                 (lambda (&rest _) (cl-incf executions))))
        (should-error
         (grease--plan-transaction
          `((:kind create :id 1
                   :dst ,(expand-file-name "occupied/child.txt" temp-dir)
                   :type file)))
         :type 'user-error))
      (should (= executions 0)))))

(ert-deftest grease-test-plan-orders-parent-directory-creation ()
  "A scheduled parent directory should be created before its child."
  (grease-test-with-temp-dir
    (let ((parent (expand-file-name "parent" temp-dir))
          (child (expand-file-name "parent/child.txt" temp-dir)))
      (let ((plan (grease--plan-transaction
                   `((:kind create :id 2 :dst ,child :type file)
                     (:kind create :id 1 :dst ,parent :type dir)))))
        (should (equal (mapcar (lambda (operation) (plist-get operation :id)) plan)
                       '(1 2)))))))

(ert-deftest grease-test-plan-validation-performs-zero-mutations ()
  "A validation failure must occur before any filesystem operation."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name "target" temp-dir))
          (mutations 0))
      (write-region "source" nil source)
      (write-region "occupied" nil target)
      (cl-letf (((symbol-function 'grease--apply-changes)
                 (lambda (&rest _) (cl-incf mutations))))
        (should-error
         (grease--plan-transaction
          `((:kind relocate :id 1 :src ,source :dst ,target :type file)))
         :type 'user-error))
      (should (= mutations 0)))))

;;;; Relocation Cycle Planning Tests

(defun grease-test-cycle-plan (root names &optional type)
  "Create and plan a cyclic rotation of NAMES below ROOT."
  (let ((entry-type (or type 'file))
        operations)
    (cl-loop for name in names
             for next-name in (append (cdr names) (list (car names)))
             for id from 1
             do (push (list :kind 'relocate :id id
                            :src (expand-file-name name root)
                            :dst (expand-file-name next-name root)
                            :type entry-type)
                      operations))
    (grease--plan-transaction (nreverse operations))))

(ert-deftest grease-test-plan-breaks-two-file-swap ()
  "A two-file swap should be ordered through one temporary relocation."
  (grease-test-with-temp-dir
    (write-region "a" nil (expand-file-name "a" temp-dir))
    (write-region "b" nil (expand-file-name "b" temp-dir))
    (let ((plan (grease-test-cycle-plan temp-dir '("a" "b"))))
      (should (= 3 (length plan)))
      (should (= 1 (cl-count-if (lambda (operation)
                                 (plist-get operation :temporary-p))
                               plan)))
      (should (equal (mapcar (lambda (operation) (plist-get operation :kind)) plan)
                     '(relocate relocate relocate))))))

(ert-deftest grease-test-plan-breaks-two-directory-swap ()
  "A directory swap with nested contents should use directory relocations."
  (grease-test-with-temp-dir
    (grease-test-create-fixture
     temp-dir
     '((:path "foo" :type dir)
       (:path "foo/nested/a.txt" :type file :content "a")
       (:path "bar" :type dir)
       (:path "bar/nested/b.txt" :type file :content "b")))
    (let ((plan (grease-test-cycle-plan temp-dir '("foo" "bar") 'dir)))
      (should (= 3 (length plan)))
      (should (cl-every (lambda (operation)
                          (and (eq (plist-get operation :kind) 'relocate)
                               (eq (plist-get operation :type) 'dir)))
                        plan)))))

(ert-deftest grease-test-plan-breaks-three-file-rotation ()
  "A three-file cycle should require only one temporary path."
  (grease-test-with-temp-dir
    (dolist (name '("a" "b" "c"))
      (write-region name nil (expand-file-name name temp-dir)))
    (let ((plan (grease-test-cycle-plan temp-dir '("a" "b" "c"))))
      (should (= 4 (length plan)))
      (should (= 1 (cl-count-if (lambda (operation)
                                 (plist-get operation :temporary-p))
                               plan))))))

(ert-deftest grease-test-plan-breaks-three-directory-rotation ()
  "A three-directory cycle should preserve relocation semantics."
  (grease-test-with-temp-dir
    (dolist (name '("a" "b" "c"))
      (make-directory (expand-file-name name temp-dir))
      (write-region name nil (expand-file-name (concat name "/inside") temp-dir)))
    (let ((plan (grease-test-cycle-plan temp-dir '("a" "b" "c") 'dir)))
      (should (= 4 (length plan)))
      (should-not (cl-find-if (lambda (operation)
                                (eq (plist-get operation :kind) 'copy))
                              plan)))))

(ert-deftest grease-test-plan-breaks-independent-cycles ()
  "Independent relocation cycles should each receive one temporary path."
  (grease-test-with-temp-dir
    (dolist (name '("a" "b" "c" "d"))
      (write-region name nil (expand-file-name name temp-dir)))
    (let* ((a (expand-file-name "a" temp-dir))
           (b (expand-file-name "b" temp-dir))
           (c (expand-file-name "c" temp-dir))
           (d (expand-file-name "d" temp-dir))
           (plan (grease--plan-transaction
                  `((:kind relocate :id 1 :src ,a :dst ,b :type file)
                    (:kind relocate :id 2 :src ,b :dst ,a :type file)
                    (:kind relocate :id 3 :src ,c :dst ,d :type file)
                    (:kind relocate :id 4 :src ,d :dst ,c :type file)))))
      (should (= 6 (length plan)))
      (should (= 2 (cl-count-if (lambda (operation)
                                 (plist-get operation :temporary-p))
                               plan))))))

(ert-deftest grease-test-plan-orders-copy-connected-to-cycle ()
  "A copy consuming a cycle source should run before that source is moved."
  (grease-test-with-temp-dir
    (let ((a (expand-file-name "a" temp-dir))
          (b (expand-file-name "b" temp-dir))
          (copy (expand-file-name "copy" temp-dir)))
      (write-region "a" nil a)
      (write-region "b" nil b)
      (let ((plan (grease--plan-transaction
                   `((:kind relocate :id 1 :src ,a :dst ,b :type file)
                     (:kind relocate :id 2 :src ,b :dst ,a :type file)
                     (:kind copy :id 3 :source-id 1 :src ,a :dst ,copy :type file)))))
        (should (eq (plist-get (car plan) :kind) 'copy))
        (should (= 4 (length plan)))))))

(ert-deftest grease-test-plan-avoids-temporary-name-collision ()
  "An occupied temporary candidate should cause another name to be chosen."
  (grease-test-with-temp-dir
    (let ((a (expand-file-name "a" temp-dir))
          (b (expand-file-name "b" temp-dir))
          (collision (expand-file-name ".grease-tmp-1-1" temp-dir)))
      (write-region "a" nil a)
      (write-region "b" nil b)
      (write-region "occupied" nil collision)
      (let* ((plan (grease-test-cycle-plan temp-dir '("a" "b")))
             (temporary (cl-find-if (lambda (operation)
                                      (plist-get operation :temporary-p))
                                    plan)))
        (should temporary)
        (should-not (equal (plist-get temporary :dst) collision))
        (should-not (file-exists-p (plist-get temporary :dst)))))))

(ert-deftest grease-test-cycle-plan-uses-no-recursive-copy ()
  "Cycle breaking should add relocations rather than copy operations."
  (grease-test-with-temp-dir
    (make-directory (expand-file-name "a" temp-dir))
    (make-directory (expand-file-name "b" temp-dir))
    (let ((plan (grease-test-cycle-plan temp-dir '("a" "b") 'dir)))
      (should (cl-every (lambda (operation)
                          (eq (plist-get operation :kind) 'relocate))
                        plan)))))

;;;; Transaction Executor Tests

(ert-deftest grease-test-executor-two-file-swap-preserves-contents ()
  "Executing a planned file swap should preserve both distinct contents."
  (grease-test-with-temp-dir
    (let ((a (expand-file-name "a" temp-dir))
          (b (expand-file-name "b" temp-dir)))
      (write-region "content-a" nil a)
      (write-region "content-b" nil b)
      (let* ((plan (grease-test-cycle-plan temp-dir '("a" "b")))
             (result (grease--execute-transaction plan)))
        (should (plist-get result :success-p))
        (should (equal (grease-test-read-file a) "content-b"))
        (should (equal (grease-test-read-file b) "content-a"))
        (should-not (plist-get result :temporary-paths))))))

(ert-deftest grease-test-executor-directory-swap-preserves-trees ()
  "Executing a directory swap should preserve every nested file."
  (grease-test-with-temp-dir
    (grease-test-create-fixture
     temp-dir
     '((:path "foo/nested/a.txt" :type file :content "foo-content")
       (:path "bar/deep/b.txt" :type file :content "bar-content")))
    (let ((result (grease--execute-transaction
                   (grease-test-cycle-plan temp-dir '("foo" "bar") 'dir))))
      (should (plist-get result :success-p))
      (should (equal (grease-test-read-file
                      (expand-file-name "foo/deep/b.txt" temp-dir))
                     "bar-content"))
      (should (equal (grease-test-read-file
                      (expand-file-name "bar/nested/a.txt" temp-dir))
                     "foo-content")))))

(ert-deftest grease-test-executor-three-way-rotation-preserves-identities ()
  "A three-way rotation should move contents with their stable identities."
  (grease-test-with-temp-dir
    (dolist (pair '(("a" . "A") ("b" . "B") ("c" . "C")))
      (write-region (cdr pair) nil (expand-file-name (car pair) temp-dir)))
    (let ((result (grease--execute-transaction
                   (grease-test-cycle-plan temp-dir '("a" "b" "c")))))
      (should (plist-get result :success-p))
      (should (equal (grease-test-read-file (expand-file-name "a" temp-dir)) "C"))
      (should (equal (grease-test-read-file (expand-file-name "b" temp-dir)) "A"))
      (should (equal (grease-test-read-file (expand-file-name "c" temp-dir)) "B")))))

(ert-deftest grease-test-executor-cross-directory-move-preserves-content ()
  "A semantic relocation should preserve file contents across directories."
  (grease-test-with-temp-dir
    (let* ((target-dir (expand-file-name "target" temp-dir))
           (source (expand-file-name "move.txt" temp-dir))
           (target (expand-file-name "move.txt" target-dir)))
      (make-directory target-dir)
      (write-region "moving-content" nil source)
      (let ((result (grease--execute-transaction
                     `((:kind relocate :id 1 :src ,source :dst ,target
                              :type file)))))
        (should (plist-get result :success-p))
        (should-not (file-exists-p source))
        (should (equal (grease-test-read-file target) "moving-content"))))))

(ert-deftest grease-test-executor-cross-filesystem-copy-failure-keeps-source ()
  "A failed cross-filesystem copy must not delete the relocation source."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name "target" temp-dir)))
      (write-region "source-content" nil source)
      (cl-letf (((symbol-function 'grease--same-filesystem-adapter-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'copy-file)
                 (lambda (&rest _) (error "injected copy failure"))))
        (let ((result (grease--execute-transaction
                       `((:kind relocate :id 1 :src ,source :dst ,target
                                :type file)))))
          (should-not (plist-get result :success-p))))
      (should (equal (grease-test-read-file source) "source-content"))
      (should-not (file-exists-p target)))))

(ert-deftest grease-test-executor-failure-preserves-staged-state ()
  "Executor failure should not clear buffer, clipboard, or staged state."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name "target" temp-dir))
          (grease--buffer-dirty-p t)
          (grease--pending-changes '(:staged))
          (grease--clipboard '(:operation cut)))
      (write-region "content" nil source)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (error "injected failure"))))
        (let ((result (grease--execute-transaction
                       `((:kind relocate :id 1 :src ,source :dst ,target
                                :type file)))))
          (should-not (plist-get result :success-p))
          (should (equal (plist-get (plist-get result :operation) :id) 1))
          (should (plist-get result :error))))
      (should grease--buffer-dirty-p)
      (should (equal grease--pending-changes '(:staged)))
      (should (equal grease--clipboard '(:operation cut)))
      (should (file-exists-p source)))))

(ert-deftest grease-test-executor-refuses-newly-appearing-destination ()
  "A destination appearing after planning should not be overwritten."
  (grease-test-with-temp-dir
    (let ((source (expand-file-name "source" temp-dir))
          (target (expand-file-name "target" temp-dir)))
      (write-region "source-content" nil source)
      (let ((plan (grease--plan-transaction
                   `((:kind relocate :id 1 :src ,source :dst ,target
                            :type file)))))
        (write-region "external-content" nil target)
        (let ((result (grease--execute-transaction plan)))
          (should-not (plist-get result :success-p))
          (should (equal (grease-test-read-file source) "source-content"))
          (should (equal (grease-test-read-file target) "external-content")))))))

(ert-deftest grease-test-executor-removes-temporary-paths-after-success ()
  "Successful cycle execution should leave no implementation temporary path."
  (grease-test-with-temp-dir
    (write-region "a" nil (expand-file-name "a" temp-dir))
    (write-region "b" nil (expand-file-name "b" temp-dir))
    (let* ((plan (grease-test-cycle-plan temp-dir '("a" "b")))
           (temporary-paths
            (delq nil (mapcar (lambda (operation)
                                (and (plist-get operation :temporary-p)
                                     (plist-get operation :dst)))
                              plan)))
           (result (grease--execute-transaction plan)))
      (should (plist-get result :success-p))
      (dolist (path temporary-paths)
        (should-not (file-exists-p path))))))

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

(ert-deftest grease-test-paste-directory-after-copy-no-double-slash ()
  "Test copied directories paste without an extra trailing slash."
  (grease-test-with-temp-dir
    (make-directory (expand-file-name "original-dir" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (grease-copy)
      (grease-paste)
      (goto-char (point-min))
      (should (search-forward "original-dir-copy/" nil t))
      (should-not (search-forward "original-dir-copy//" nil t)))))

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

(ert-deftest grease-test-format-semantic-directory-create-shows-slash ()
  "Directory creates should be visibly distinct in confirmation text."
  (let ((grease--root-dir "/tmp/test/"))
    (should (equal
             (grease--format-semantic-operation
              '(:kind create :id 1 :dst "/tmp/test/new-dir" :type dir))
             "  [Create] new-dir/"))))

(ert-deftest grease-test-format-semantic-file-create-has-no-slash ()
  "File creates should not receive a directory display slash."
  (let ((grease--root-dir "/tmp/test/"))
    (should (equal
             (grease--format-semantic-operation
              '(:kind create :id 1 :dst "/tmp/test/new-file" :type file))
             "  [Create] new-file"))))

(ert-deftest grease-test-format-semantic-directory-operations-show-slashes ()
  "Directory delete, rename, move, and copy paths should all show slashes."
  (let ((grease--root-dir "/tmp/test/"))
    (dolist (case
             '(((:kind delete :id 1 :src "/tmp/test/old" :type dir)
                . "  [Delete] old/")
               ((:kind relocate :id 1 :src "/tmp/test/old" :dst "/tmp/test/new"
                       :type dir)
                . "  [Rename] old/ -> new/")
               ((:kind relocate :id 1 :src "/tmp/test/old"
                       :dst "/tmp/elsewhere/new" :type dir)
                . "  [Move]   old/ -> /tmp/elsewhere/new/")
               ((:kind copy :id 2 :source-id 1 :src "/tmp/test/source"
                       :dst "/tmp/test/copy" :type dir)
                . "  [Copy]   source/ -> copy/")))
      (should (equal (grease--format-semantic-operation (car case))
                     (cdr case))))))

(ert-deftest grease-test-save-confirmation-shows-directory-slash ()
  "The unified save prompt should use type-aware directory presentation."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "test/\n"))
      (let (captured-prompt)
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (prompt &rest _)
                     (setq captured-prompt prompt)
                     ?n)))
          (should-not (grease-save)))
        (should (string-match-p "\\[Create\\] test/" captured-prompt))))))

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

(ert-deftest grease-test-scan-canonicalizes-typed-directory ()
  "Typing a directory should keep display slashes out of structured state."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "test/\n"))
      (let* ((entries (grease--scan-buffer))
             (entry (car entries)))
        (should (= 1 (length entries)))
        (should (equal (plist-get entry :name) "test"))
        (should (eq (plist-get entry :type) 'dir))
        (should (equal (plist-get entry :path)
                       (expand-file-name "test" temp-dir)))
        (should-not (string-suffix-p "/" (plist-get entry :path)))
        (should (string-match-p "test/" (buffer-string)))
        (should-not (string-match-p "test//" (buffer-string)))))))

(ert-deftest grease-test-insert-directory-adds-one-display-slash ()
  "Directory rendering should add exactly one display-only trailing slash."
  (grease-test-with-clean-state
    (with-temp-buffer
      (grease-mode)
      (setq grease--root-dir "/tmp/")
      (grease--insert-entry 42 "test///" 'dir)
      (should (string-match-p "test/" (buffer-string)))
      (should-not (string-match-p "test//" (buffer-string)))
      (goto-char (point-min))
      ;; The synthetic test buffer has no header, so inspect properties
      ;; directly rather than the header-aware line-data helper.
      (should (equal (get-text-property (point) 'grease-name) "test"))
      (should (equal (get-text-property (point) 'grease-full-path)
                     "/tmp/test")))))

(ert-deftest grease-test-directory-type-and-id-survive-rescan ()
  "Canonicalizing directory metadata should not change its type or identity."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "test/\n"))
      (let* ((first (car (grease--scan-buffer)))
             (id (plist-get first :id))
             (second (car (grease--scan-buffer))))
        (should (numberp id))
        (should (equal (plist-get second :id) id))
        (should (eq (plist-get second :type) 'dir))
        (should (equal (plist-get second :name) "test"))))))

(ert-deftest grease-test-scan-buffer-entries ()
  "Test that scan buffer returns all entries."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "one.txt" temp-dir))
    (write-region "" nil (expand-file-name "two.txt" temp-dir))
    (make-directory (expand-file-name "three" temp-dir))
    (grease-test-with-buffer temp-dir
      (let ((entries (grease--scan-buffer)))
        (should (= 3 (length entries)))))))

;;;; Standard Emacs Editing Tests

(ert-deftest grease-test-emacs-type-new-filename ()
  "Test typing a new filename on editable line."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (forward-line -1)
      (let ((inhibit-read-only t))
        (insert "brandnew.txt"))
      (grease--format-plain-lines)
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if (lambda (c)
                              (and (eq (car c) :create)
                                   (string-match-p "brandnew.txt" (cadr c))))
                            changes))))))

(ert-deftest grease-test-emacs-delete-whole-line ()
  "Test deleting a whole line with kill-whole-line behavior."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "deleteme.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let ((inhibit-read-only t))
        (delete-region (line-beginning-position) (min (1+ (line-end-position)) (point-max))))
      (let ((changes (grease--calculate-changes)))
        (should (= 1 (length changes)))
        (should (eq (caar changes) :delete))
        (should (string-match-p "deleteme.txt" (cadar changes)))))))

(ert-deftest grease-test-emacs-partial-delete-filename-only ()
  "Test deleting just the filename text (not the whole line)."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "partial.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let* ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (line-text (buffer-substring line-start line-end))
             (name-match (string-match "partial\\.txt" line-text))
             (inhibit-read-only t))
        (when name-match
          (goto-char (+ line-start name-match))
          (delete-region (point) line-end)))
      (let ((line-data (grease--get-line-data)))
        (should line-data)))))

(ert-deftest grease-test-emacs-backspace-partial ()
  "Test backspacing part of a filename - should be treated as rename."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "longname.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (let ((inhibit-read-only t))
        (delete-char -4))
      (grease--update-line-metadata)
      (let ((data (grease--get-line-data)))
        (should data)
        (should (string-match-p "longname" (plist-get data :name)))))))

(ert-deftest grease-test-emacs-rename-by-editing ()
  "Test renaming a file by editing its name in the buffer."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "oldname.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let* ((line-start (line-beginning-position))
             (line-text (buffer-substring line-start (line-end-position)))
             (name-start (string-match "oldname" line-text))
             (inhibit-read-only t))
        (when name-start
          (goto-char (+ line-start name-start))
          (delete-char 7)
          (insert "newname")))
      (grease--update-line-metadata)
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if (lambda (c) (eq (car c) :rename)) changes))))))

(ert-deftest grease-test-emacs-kill-line ()
  "Test C-k (kill-line) behavior."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "killme.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let ((inhibit-read-only t))
        (kill-line)
        (when (looking-at "\n") (delete-char 1)))
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if (lambda (c)
                              (and (eq (car c) :delete)
                                   (string-match-p "killme" (cadr c))))
                            changes))))))

(ert-deftest grease-test-emacs-yank-plain-text ()
  "Test yanking plain text from kill ring."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (kill-new "pasted-file.txt")
      (goto-char (point-max))
      (forward-line -1)
      (let ((inhibit-read-only t))
        (yank))
      (grease--format-plain-lines)
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if (lambda (c)
                              (and (eq (car c) :create)
                                   (string-match-p "pasted-file" (cadr c))))
                            changes))))))

(ert-deftest grease-test-emacs-insert-multiple-lines ()
  "Test inserting multiple filenames at once."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "file1.txt\nfile2.txt\nfile3.txt\n"))
      (grease--format-plain-lines)
      (let ((changes (grease--calculate-changes)))
        (should (>= (length (cl-remove-if-not
                             (lambda (c) (eq (car c) :create))
                             changes))
                    3))))))

(ert-deftest grease-test-emacs-create-directory-with-slash ()
  "Test creating a directory by typing name with trailing slash."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "newdir/\n"))
      (grease--format-plain-lines)
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if (lambda (c)
                              (and (eq (car c) :create)
                                   (string-suffix-p "newdir/" (cadr c))))
                            changes))))))

(ert-deftest grease-test-emacs-empty-line-no-change ()
  "Test that empty lines don't create spurious changes."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "existing.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "\n\n\n"))
      (let ((changes (grease--calculate-changes)))
        (should (null changes))))))

(ert-deftest grease-test-emacs-whitespace-only-no-change ()
  "Test that whitespace-only lines don't create files."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "   \n\t\t\n"))
      (let ((changes (grease--calculate-changes)))
        (should (null changes))))))

(ert-deftest grease-test-line-data-after-partial-edit ()
  "Test that line data is retrievable after partial filename edit."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "editme.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (goto-char (point-min))
      (forward-line 1)
      (let ((original-id (plist-get (grease--get-line-data) :id)))
        (end-of-line)
        (let ((inhibit-read-only t))
          (delete-char -4)
          (insert ".md"))
        (grease--update-line-metadata)
        (let ((new-data (grease--get-line-data)))
          (should new-data)
          (should (equal (plist-get new-data :id) original-id))
          (should (string-match-p "\\.md" (plist-get new-data :name))))))))

(ert-deftest grease-test-pending-copy-source-id-becomes-create ()
  "Copy metadata from a pending new line should create, not copy from disk."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((first-id (grease--get-next-id))
            (second-id (grease--get-next-id)))
        (grease--insert-entry first-id "draft.txt" 'file nil nil)
        (grease--insert-entry second-id "draft-2.txt" 'file first-id t)
        (let ((changes (grease--calculate-changes)))
          (should (= 2 (length (cl-remove-if-not
                                (lambda (c) (eq (car c) :create))
                                changes))))
          (should-not (cl-find-if (lambda (c) (eq (car c) :copy)) changes)))))))

(ert-deftest grease-test-newly-created-directory-copy-pastes-as-filesystem-copy ()
  "A directory created in-session should copy as a real directory after save/render."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "test/\n"))
      (let ((create-changes (grease--calculate-changes)))
        (grease--apply-changes create-changes))
      (grease--render grease--root-dir)
      (goto-char (point-min))
      (forward-line 1)
      (grease-copy)
      (grease-paste)
      (let ((copy-changes (grease--calculate-changes)))
        (should (cl-find-if
                 (lambda (c)
                   (and (eq (car c) :copy)
                        (string-suffix-p "test" (nth 1 c))
                        (string-suffix-p "test-copy" (nth 2 c))))
                 copy-changes))
        (should-not (cl-find-if
                     (lambda (c)
                       (and (eq (car c) :create)
                            (string-suffix-p "test-copy/" (cadr c))))
                     copy-changes))))))

(ert-deftest grease-test-renamed-pending-directory-creates-directory ()
  "A pending directory line with a changed name still creates a directory."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((id (grease--get-next-id)))
        ;; Simulate a duplicated pending directory that has been manually
        ;; renamed from test/ to test1/ before commit.
        (grease--insert-entry id "test1/" 'dir nil nil))
      (let ((changes (grease--calculate-changes)))
        (should (cl-find-if
                 (lambda (c)
                   (and (eq (car c) :create)
                        (string-suffix-p "test1/" (cadr c))))
                 changes))
        (grease--apply-changes changes))
      (should (file-directory-p (expand-file-name "test1" temp-dir))))))

(ert-deftest grease-test-copy-pending-new-line-pastes-as-text-create ()
  "Pasting a copied pending line should not keep a copy source-id."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (forward-line -1)
      (let ((inhibit-read-only t))
        (insert "draft.txt"))
      (grease--format-plain-lines)
      (goto-char (point-min))
      (forward-line 1)
      (should (eq (grease--line-data-source-kind (grease--get-line-data)) 'text))
      (grease-copy)
      (should (equal (plist-get grease--clipboard :source-kinds) '(text)))
      (grease-paste)
      (forward-line -1)
      (let ((pasted (grease--get-line-data)))
        (should pasted)
        (should (equal (plist-get pasted :name) "draft.txt"))
        (should-not (plist-get pasted :source-id))
        (should-not (plist-get pasted :is-duplicate))))))

;;;; Unified Save Tests

(defun grease-test-save-cross-buffer-move (initiator)
  "Run a cross-buffer move scenario, saving from INITIATOR buffer name."
  (grease-test-with-temp-dir
    (let ((source-dir (expand-file-name "source" temp-dir))
          (target-dir (expand-file-name "target" temp-dir)))
      (make-directory source-dir)
      (make-directory target-dir)
      (write-region "preserved-content" nil
                    (expand-file-name "move.txt" source-dir))
      (grease-test-with-buffers ((source source-dir) (target target-dir))
        (grease-test-cut-and-paste source target "move.txt")
        (let ((grease-skip-confirm-for-simple-edits t))
          (with-current-buffer (if (eq initiator 'source) source target)
            (should (grease-save))))
        (should-not (file-exists-p (expand-file-name "move.txt" source-dir)))
        (should (equal (grease-test-read-file
                        (expand-file-name "move.txt" target-dir))
                       "preserved-content"))
        (should-not grease--clipboard)
        (with-current-buffer source
          (should-not grease--buffer-dirty-p)
          (should-not (string-match-p "move.txt" (buffer-string))))
        (with-current-buffer target
          (should-not grease--buffer-dirty-p)
          (should (string-match-p "move.txt" (buffer-string))))))))

(ert-deftest grease-test-save-cross-buffer-move-from-source ()
  "Saving from the source should commit one content-preserving move."
  (grease-test-save-cross-buffer-move 'source))

(ert-deftest grease-test-save-cross-buffer-move-from-destination ()
  "Saving from the destination should produce the identical move result."
  (grease-test-save-cross-buffer-move 'target))

(ert-deftest grease-test-save-cross-buffer-move-has-no-later-delete ()
  "A committed move should not leave a later source deletion transaction."
  (grease-test-with-temp-dir
    (let ((source-dir (expand-file-name "source" temp-dir))
          (target-dir (expand-file-name "target" temp-dir)))
      (make-directory source-dir)
      (make-directory target-dir)
      (write-region "content" nil (expand-file-name "move.txt" source-dir))
      (grease-test-with-buffers ((source source-dir) (target target-dir))
        (grease-test-cut-and-paste source target "move.txt")
        (let ((grease-skip-confirm-for-simple-edits t))
          (with-current-buffer source (should (grease-save))))
        (should-not (plist-get (grease--build-transaction) :operations))
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (&rest _) (error "No later prompt expected"))))
          (with-current-buffer target
            (should (grease-save))))))))

(ert-deftest grease-test-save-cross-buffer-copy-preserves-source ()
  "Unified save should commit a copy without deleting its source."
  (grease-test-with-temp-dir
    (let ((source-dir (expand-file-name "source" temp-dir))
          (target-dir (expand-file-name "target" temp-dir)))
      (make-directory source-dir)
      (make-directory target-dir)
      (write-region "copy-content" nil (expand-file-name "copy.txt" source-dir))
      (grease-test-with-buffers ((source source-dir) (target target-dir))
        (with-current-buffer source
          (grease-test-goto-entry "copy.txt")
          (grease-copy))
        (with-current-buffer target
          (goto-char (point-max))
          (grease-paste)
          (let ((grease-skip-confirm-for-simple-edits t))
            (should (grease-save))))
        (should (equal (grease-test-read-file
                        (expand-file-name "copy.txt" source-dir))
                       "copy-content"))
        (should (equal (grease-test-read-file
                        (expand-file-name "copy.txt" target-dir))
                       "copy-content"))))))

(ert-deftest grease-test-save-conflict-keep-preserves-all-buffer-state ()
  "Choosing k after an identity conflict should keep every edit intact."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "same.txt" temp-dir))
    (grease-test-with-buffers ((left temp-dir) (right temp-dir))
      (with-current-buffer left
        (grease-test-edit-entry "same.txt" "left.txt"))
      (with-current-buffer right
        (grease-test-edit-entry "same.txt" "right.txt"))
      (let (prompt)
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (text &rest _)
                     (setq prompt text)
                     ?k)))
          (with-current-buffer left
            (should-not (grease-save))))
        (should (string-match-p "k = keep editing" prompt))
        (should (string-match-p "d = discard all" prompt))
        (should (string-match-p "left.txt" prompt))
        (should (string-match-p "right.txt" prompt)))
      (with-current-buffer left
        (should grease--buffer-dirty-p)
        (should (string-match-p "left.txt" (buffer-string))))
      (with-current-buffer right
        (should grease--buffer-dirty-p)
        (should (string-match-p "right.txt" (buffer-string))))
      (should (file-exists-p (expand-file-name "same.txt" temp-dir))))))

(ert-deftest grease-test-save-conflict-discard-clears-all-staged-state ()
  "Choosing d after a conflict should rerender every dirty buffer from disk."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "same.txt" temp-dir))
    (grease-test-with-buffers ((left temp-dir) (right temp-dir))
      (with-current-buffer left
        (grease-test-edit-entry "same.txt" "left.txt"))
      (with-current-buffer right
        (grease-test-edit-entry "same.txt" "right.txt"))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?d)))
        (with-current-buffer right
          (should (grease-save))))
      (dolist (buffer (list left right))
        (with-current-buffer buffer
          (should-not grease--buffer-dirty-p)
          (should-not grease--pending-changes)
          (should (string-match-p "same.txt" (buffer-string)))
          (should-not (string-match-p "left.txt\\|right.txt" (buffer-string)))))
      (should (file-exists-p (expand-file-name "same.txt" temp-dir))))))

(ert-deftest grease-test-save-cancel-keeps-all-participants-dirty ()
  "Cancelling a unified save should preserve all staged buffer state."
  (grease-test-with-temp-dir
    (let ((left-dir (expand-file-name "left" temp-dir))
          (right-dir (expand-file-name "right" temp-dir)))
      (make-directory left-dir)
      (make-directory right-dir)
      (write-region "left" nil (expand-file-name "left.txt" left-dir))
      (write-region "right" nil (expand-file-name "right.txt" right-dir))
      (grease-test-with-buffers ((left left-dir) (right right-dir))
        (with-current-buffer left
          (grease-test-edit-entry "left.txt" "left-new.txt"))
        (with-current-buffer right
          (grease-test-edit-entry "right.txt" "right-new.txt"))
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
          (with-current-buffer left
            (should-not (grease-save))))
        (with-current-buffer left (should grease--buffer-dirty-p))
        (with-current-buffer right (should grease--buffer-dirty-p))
        (should (file-exists-p (expand-file-name "left.txt" left-dir)))
        (should (file-exists-p (expand-file-name "right.txt" right-dir)))))))

(ert-deftest grease-test-save-discard-rerenders-all-participants ()
  "Discarding should clear and rerender every participating buffer."
  (grease-test-with-temp-dir
    (let ((left-dir (expand-file-name "left" temp-dir))
          (right-dir (expand-file-name "right" temp-dir)))
      (make-directory left-dir)
      (make-directory right-dir)
      (write-region "left" nil (expand-file-name "left.txt" left-dir))
      (write-region "right" nil (expand-file-name "right.txt" right-dir))
      (grease-test-with-buffers ((left left-dir) (right right-dir))
        (with-current-buffer left
          (grease-test-edit-entry "left.txt" "left-new.txt"))
        (with-current-buffer right
          (grease-test-edit-entry "right.txt" "right-new.txt"))
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?d)))
          (with-current-buffer right
            (should (grease-save))))
        (with-current-buffer left
          (should-not grease--buffer-dirty-p)
          (should (string-match-p "left.txt" (buffer-string))))
        (with-current-buffer right
          (should-not grease--buffer-dirty-p)
          (should (string-match-p "right.txt" (buffer-string))))))))

(ert-deftest grease-test-save-execution-failure-keeps-participants-dirty ()
  "A unified execution failure should preserve every participating state."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "old.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-edit-entry "old.txt" "new.txt")
      (cl-letf (((symbol-function 'grease--execute-transaction)
                 (lambda (plan)
                   (list :success-p nil :operation (car plan)
                         :error '(error "injected")))))
        (let ((grease-skip-confirm-for-simple-edits t))
          (should-not (grease-save))))
      (should grease--buffer-dirty-p)
      (should (string-match-p "new.txt" (buffer-string)))
      (should (file-exists-p (expand-file-name "old.txt" temp-dir))))))

(ert-deftest grease-test-save-simple-classification-uses-combined-transaction ()
  "Simple-edit confirmation skipping should inspect the combined transaction."
  (grease-test-with-temp-dir
    (let ((left-dir (expand-file-name "left" temp-dir))
          (right-dir (expand-file-name "right" temp-dir))
          prompted)
      (make-directory left-dir)
      (make-directory right-dir)
      (write-region "left" nil (expand-file-name "left.txt" left-dir))
      (write-region "right" nil (expand-file-name "right.txt" right-dir))
      (grease-test-with-buffers ((left left-dir) (right right-dir))
        (with-current-buffer left
          (grease-test-edit-entry "left.txt" "left-new.txt"))
        (with-current-buffer right
          (grease-test-edit-entry "right.txt" "right-new.txt"))
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (&rest _) (setq prompted t) ?n)))
          (let ((grease-skip-confirm-for-simple-edits t))
            (with-current-buffer left
              (should-not (grease-save)))))
        (should prompted)))))

(ert-deftest grease-test-save-nested-directory-through-full-transaction ()
  "Saving a nested directory should execute its explicit parent plan."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "parent/child/\n"))
      (let ((grease-skip-confirm-for-simple-edits t))
        (should (grease-save)))
      (should (file-directory-p
               (expand-file-name "parent/child" temp-dir)))
      (should-not (plist-get
                   (grease--build-transaction (list (current-buffer)))
                   :operations)))))

(ert-deftest grease-test-save-nested-file-through-full-transaction ()
  "Saving a nested file should explicitly create all parent directories."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "one/two/file.txt\n"))
      (let ((grease-skip-confirm-for-simple-edits t))
        (should (grease-save)))
      (should (file-exists-p
               (expand-file-name "one/two/file.txt" temp-dir)))
      (should-not (plist-get
                   (grease--build-transaction (list (current-buffer)))
                   :operations)))))

(ert-deftest grease-test-save-typed-directory-through-full-transaction ()
  "Typing test/ and saving should create the directory through the planner."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "test/\n"))
      (let ((grease-skip-confirm-for-simple-edits t))
        (should (grease-save)))
      (should (file-directory-p (expand-file-name "test" temp-dir)))
      (should-not grease--buffer-dirty-p)
      (should (string-match-p "test/" (buffer-string)))
      (should-not (string-match-p "test//" (buffer-string))))))

(ert-deftest grease-test-save-whitespace-only-is-clean ()
  "Whitespace-only dirty state should save as no-op success."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "   \n\t\t\n"))
      (should grease--buffer-dirty-p)
      (should (grease-save))
      (should-not grease--buffer-dirty-p))))

(ert-deftest grease-test-quit-only-closes-after-unified-save-success ()
  "A failed unified save should leave the requested Grease buffer open."
  (grease-test-with-temp-dir
    (write-region "content" nil (expand-file-name "old.txt" temp-dir))
    (grease-test-with-buffers ((buffer temp-dir))
      (with-current-buffer buffer
        (grease-test-edit-entry "old.txt" "new.txt")
        (cl-letf (((symbol-function 'grease--execute-transaction)
                   (lambda (plan)
                     (list :success-p nil :operation (car plan)
                           :error '(error "injected")))))
          (let ((grease-skip-confirm-for-simple-edits t))
            (grease-quit))))
      (should (buffer-live-p buffer))
      (with-current-buffer buffer
        (should grease--buffer-dirty-p)))))

(ert-deftest grease-test-quit-whitespace-only-does-not-prompt ()
  "Quitting after whitespace-only edits should not ask to save."
  (grease-test-with-temp-dir
    (grease-test-with-clean-state
      (let ((buf (generate-new-buffer " *grease-quit-test*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (grease-mode)
                (setq grease--root-dir (file-name-as-directory (expand-file-name temp-dir)))
                (grease--render grease--root-dir)
                (goto-char (point-max))
                (let ((inhibit-read-only t))
                  (insert "   \n"))
                (should grease--buffer-dirty-p)
                (cl-letf (((symbol-function 'read-char-choice)
                           (lambda (&rest _)
                             (error "Should not prompt for no-op changes"))))
                  (grease-quit)))
              (should-not (buffer-live-p buf)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(ert-deftest grease-test-simple-edit-rules ()
  "Test Oil-style simple edit classification."
  (let ((grease--root-dir "/tmp/grease-test/"))
    (should (grease--simple-edit-p '((:create "/tmp/grease-test/a")
                                     (:create "/tmp/grease-test/b")
                                     (:copy "/tmp/grease-test/a" "/tmp/grease-test/a-copy")
                                     (:rename "/tmp/grease-test/old" "/tmp/grease-test/new"))))
    (should-not (grease--simple-edit-p '((:delete "/tmp/grease-test/a"))))
    (should-not (grease--simple-edit-p '((:create "/tmp/grease-test/a")
                                         (:create "/tmp/grease-test/b")
                                         (:create "/tmp/grease-test/c")
                                         (:create "/tmp/grease-test/d")
                                         (:create "/tmp/grease-test/e")
                                         (:create "/tmp/grease-test/f"))))
    (should-not (grease--simple-edit-p '((:copy "/tmp/grease-test/a" "/tmp/grease-test/a-copy")
                                         (:copy "/tmp/grease-test/b" "/tmp/grease-test/b-copy"))))
    (should-not (grease--simple-edit-p '((:move "/tmp/grease-test/a" "/tmp/grease-test/b")
                                         (:rename "/tmp/grease-test/c" "/tmp/grease-test/d"))))))

(ert-deftest grease-test-save-simple-edit-skips-confirm-when-enabled ()
  "Simple edits should save without prompting when configured."
  (grease-test-with-temp-dir
    (grease-test-with-buffer temp-dir
      (let ((grease-skip-confirm-for-simple-edits t)
            (new-file (expand-file-name "quick.txt" temp-dir)))
        (goto-char (point-max))
        (forward-line -1)
        (let ((inhibit-read-only t))
          (insert "quick.txt"))
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (&rest _)
                     (error "Should not prompt for simple edits"))))
          (should (grease-save)))
        (should (file-exists-p new-file))))))

(ert-deftest grease-test-save-complex-edit-still-prompts-when-simple-skip-enabled ()
  "Deletes should still prompt even when simple edit confirmation is skipped."
  (grease-test-with-temp-dir
    (write-region "" nil (expand-file-name "deleteme.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (let ((grease-skip-confirm-for-simple-edits t)
            (prompted nil))
        (goto-char (point-min))
        (forward-line 1)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position)
                         (min (1+ (line-end-position)) (point-max))))
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (&rest _)
                     (setq prompted t)
                     ?n)))
          (should-not (grease-save)))
        (should prompted)))))

;;;; Symlink Display Tests

(defmacro grease-test-with-symlinks (dir symlinks &rest body)
  "Execute BODY in DIR after creating SYMLINKS.
SYMLINKS is a list of (LINK-NAME TARGET) pairs.
LINK-NAME is relative to DIR; TARGET may be absolute or relative.
Cleanup removes all created links."
  (declare (indent 2))
  `(let ((default-directory ,dir))
     (dolist (link-spec ,symlinks)
       (let ((link (expand-file-name (car link-spec) ,dir))
             (target (cadr link-spec)))
         (make-symbolic-link target link)))
     (unwind-protect
         (progn ,@body)
       (dolist (link-spec ,symlinks)
         (let ((link (expand-file-name (car link-spec) ,dir)))
           (when (file-symlink-p link)
             (delete-file link)))))))

(ert-deftest grease-test-symlink-display-produces-no-operations ()
  "Displaying a buffer with symlink entries must not create diff operations."
  (grease-test-with-temp-dir
    (write-region "real-content" nil (expand-file-name "target.txt" temp-dir))
    (make-symbolic-link "target.txt" (expand-file-name "link.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (should (grease-test-goto-entry "link.txt"))
      (let ((ops (grease--diff-by-id
                  grease--baseline-by-id
                  (grease--scan-buffer))))
        (should (zerop (length ops)))))))

(ert-deftest grease-test-symlink-extract-filename ()
  "Buffer text for symlinks must still extract to a clean filename."
  (grease-test-with-temp-dir
    (write-region "x" nil (expand-file-name "target.txt" temp-dir))
    (make-symbolic-link "target.txt" (expand-file-name "mylink.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "mylink.txt")
      (let ((visible-text (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
        (should (equal (grease--extract-filename visible-text) "mylink.txt"))
        (should-not (string-match-p "\\|target\\.txt" visible-text))))))

(ert-deftest grease-test-symlink-broken-display ()
  "A broken symlink should produce no diff operations and use the warning face."
  (grease-test-with-temp-dir
    (make-symbolic-link "nonexistent" (expand-file-name "broken" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "broken")
      (let ((ops (grease--diff-by-id
                  grease--baseline-by-id
                  (grease--scan-buffer))))
        (should (zerop (length ops))))
      (let* ((line-end (line-end-position))
             (ov (cl-find-if
                  (lambda (o) (overlay-get o 'grease-symlink-display))
                  (overlays-in (line-beginning-position) line-end))))
        (should ov)
        (let ((str (overlay-get ov 'after-string)))
          (should str)
          (should (eq (get-text-property 0 'face str)
                      'grease-symlink-broken))))
      (should-not (file-exists-p (expand-file-name "nonexistent" temp-dir))))))

(ert-deftest grease-test-symlink-valid-display ()
  "A valid symlink should use font-lock-comment-face on the after-string."
  (grease-test-with-temp-dir
    (write-region "real" nil (expand-file-name "real.txt" temp-dir))
    (make-symbolic-link "real.txt" (expand-file-name "good-link" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "good-link")
      (let* ((line-end (line-end-position))
             (ov (cl-find-if
                  (lambda (o) (overlay-get o 'grease-symlink-display))
                  (overlays-in (line-beginning-position) line-end))))
        (should ov)
        (let ((str (overlay-get ov 'after-string)))
          (should str)
          (should (eq (get-text-property 0 'face str)
                      'font-lock-comment-face)))))))

(ert-deftest grease-test-symlink-toggle-suppresses-display ()
  "Setting grease-show-symlink-targets to nil must remove the overlay."
  (grease-test-with-temp-dir
    (write-region "x" nil (expand-file-name "target" temp-dir))
    (make-symbolic-link "target" (expand-file-name "link" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "link")
      (let ((grease-show-symlink-targets nil))
        (grease--render grease--root-dir t)
        (grease-test-goto-entry "link")
        (should-not
         (cl-find-if
          (lambda (o) (overlay-get o 'grease-symlink-display))
          (overlays-in (line-beginning-position) (line-end-position))))))))

(ert-deftest grease-test-symlink-rename-extract ()
  "Renaming a symlink entry must produce a clean filename without suffix text."
  (grease-test-with-temp-dir
    (write-region "x" nil (expand-file-name "target.txt" temp-dir))
    (make-symbolic-link "target.txt" (expand-file-name "old-link.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-edit-entry "old-link.txt" "renamed-link.txt")
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
             (parsed (grease--extract-filename line)))
        (should (equal parsed "renamed-link.txt"))))))

(provide 'grease-test)
;;; grease-test.el ends here
