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

(ert-deftest grease-test-mark-file-deleted ()
  "Test staging deletion without changing committed registry state."
  (grease-test-with-clean-state
    (grease-test-with-temp-dir
      (let ((path (expand-file-name "todelete.txt" temp-dir)))
        (write-region "content" nil path)
        (let ((id (grease--register-file path 'file)))
          (grease--mark-file-deleted id path)
          (should (equal (gethash id grease--deleted-file-ids) path))
          (let ((info (grease--get-file-by-id id)))
            (should (plist-get info :committed-p))
            (should (plist-get info :exists))
            (should (equal (plist-get info :path) path))))))))

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

(provide 'grease-test)
;;; grease-test.el ends here
