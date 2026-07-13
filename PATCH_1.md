# PATCH_1 — Symlink target display: overlay, cleanup, and tests

## Overview

This patch addresses all three maintainer review points for the symlink target
display feature in commit `769a11b`.  The changes are:

1.  **Render symlink targets via an overlay `after-string`** anchored on the
    trailing newline, so the arrow and resolved path are purely display-only
    and never become part of the editable buffer text.

2.  **Remove debug logging and revert unrelated diff-engine formatting** to
    keep the PR focused on symlink support.

3.  **Add symlink-specific regression tests** that confirm display produces no
    filesystem operations, `grease--extract-filename` properly strips the
    suffix, and broken-symlink faces are applied without side effects.

---

## 1. Overlay with `after-string` (display-only)

### Problem

Currently `grease--insert-entry` inserts the arrow character `` and the
resolved target path as real buffer text (with text-property faces):

```elisp
(insert " ")
(let ((mark-start (point)))
  (insert " ")
  (put-text-property mark-start (point) 'face target-face))
(let ((target-start (point)))
  (insert resolved)
  (put-text-property target-start (point) 'face target-face))
```

Because this text lives in the buffer, the user can edit or yank it.  If the
arrow marker is removed, the target path can be misinterpreted as part of the
filename by `grease--extract-filename` and the diff engine, causing false
identity renames or trailing-path corruption.

### Fix

**Replace the direct insertion** in `grease--insert-entry` with an overlay
whose span is **the trailing newline** of the entry line, and which carries
the arrow + face-propertized target as its `after-string`.

**Anchor on the newline** because:
- The newline is present on every entry line, so the overlay always has an
  unambiguous span.
- The filename text itself remains overlay-free, so operations like
  `buffer-substring-no-properties`, `delete-region`, and `search-forward` work
  exactly as they do for non-symlink entries.
- When the user edits or deletes the filename, the newline (and its overlay)
  stays put until the whole line is replaced.

**Implementation in `grease--insert-entry`** — replace the symlink text block
(approximately lines 624–639):

```elisp
      ;; Optionally display symlink target as a display-only overlay
      (when (and grease-show-symlink-targets
                 (file-symlink-p full-path))
        (let* ((link-target (file-symlink-p full-path))
               (resolved (expand-file-name link-target
                                           (file-name-directory full-path)))
               (target-face (if (file-exists-p resolved)
                                'font-lock-comment-face
                              'grease-symlink-broken))
               (suffix (propertize (concat "  " resolved)
                                   'face target-face)))
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'after-string suffix)
            (overlay-put ov 'grease-symlink-display t))))
```

Key details:
- We create the overlay **before** inserting the newline, spanning `(point)` →
  `(point)` (zero-width at first).  The `(insert "\n")` that follows
  immediately extends the overlay to cover the newline.
- The `after-string` is displayed by Emacs **after** the overlay's covered
  region (the newline), so the arrow and target appear visually at the end of
  the line, but live outside the buffer text entirely.
- The overlay is tagged with the property `grease-symlink-display` (following
  Grease's existing lowercase-hyphenated property naming, e.g.
  `grease-prefix`, `grease-icon`, `grease-editable`) for identification and
  lifecycle management.
- No separate cleanup is needed on re-render: `grease--render` calls
  `erase-buffer`, which destroys all overlays automatically.

### `grease--extract-filename` — revert to original

Because the overlay `after-string` is never part of the buffer, the
`string-match ""` / `string-trim-right` stripping logic added in the
original commit is no longer necessary.  Revert `grease--extract-filename` to
its pre-symlink implementation:

```elisp
(defun grease--extract-filename (text)
  "Extract just the filename from TEXT, removing ID and icon."
  ;; First try to match with hidden ID format
  (if (string-match (concat "^" grease--id-prefix "[0-9]+\\s-+\\(.*\\)$") text)
      ;; Got the text after the ID, now extract just the filename (after any icon)
      (let ((content (match-string 1 text)))
        ;; Look for the first alphanumeric or allowed special char that starts the filename
        (if (string-match "\\(?:[^\n[:alnum:]/._+-]\\s-*\\)*\\([[:alnum:]/._+-].*\\)$" content)
            (match-string 1 content)
          content)) ;; Fallback to the whole content
    ;; No ID, try to extract filename directly
    (if (string-match "\\(?:[^\n[:alnum:]/._+-]\\s-*\\)*\\([[:alnum:]/._+-].*\\)$" text)
        (match-string 1 text)
      ;; Last resort fallback
      (string-trim text))))
```

Docstring reverts to `"Extract just the filename from TEXT, removing ID and
icon."`  (The `and symlink suffix` addition is removed.)

### Overlay refresh in `grease--update-line-metadata`

Add a new helper that refreshes the symlink overlay for a single line, and
call it from `grease--update-line-metadata` (which already iterates every
entry line).  This ensures overlays survive partial edits that strip text
properties but not the entry structure.

**New helper** (named following existing `grease--` prefixes):

```elisp
(defun grease--refresh-symlink-overlay (line-beg line-end full-path)
  "Refresh the symlink display overlay on the line between LINE-BEG and LINE-END.
If FULL-PATH is a symlink and `grease-show-symlink-targets' is non-nil,
create a display-only overlay anchored on the trailing newline.
Otherwise, remove any existing symlink overlay."
  ;; Remove any existing symlink overlay on this line
  (dolist (ov (overlays-in line-beg line-end))
    (when (overlay-get ov 'grease-symlink-display)
      (delete-overlay ov)))
  ;; Create new overlay if conditions are met
  (when (and grease-show-symlink-targets
             (file-symlink-p full-path))
    (let* ((link-target (file-symlink-p full-path))
           (resolved (expand-file-name link-target
                                       (file-name-directory full-path)))
           (target-face (if (file-exists-p resolved)
                            'font-lock-comment-face
                          'grease-symlink-broken))
           (suffix (propertize (concat "  " resolved)
                               'face target-face)))
      (let ((ov (make-overlay line-end (1+ line-end))))
        (overlay-put ov 'after-string suffix)
        (overlay-put ov 'grease-symlink-display t)))))
```

Note: the overlay is anchored on `line-end` to `(1+ line-end)`, i.e., the
newline character itself.  For the last line of the buffer (the editable
trailing line), there may not be a newline — but symlink entries are never
on that line, so this is safe.

**Integration into `grease--update-line-metadata`** — after the existing
`put-text-property` calls and before `(forward-line 1)`:

```elisp
              ;; Refresh symlink display overlay
              (grease--refresh-symlink-overlay line-beg line-end full-path)
```

This runs on every metadata update, so even if user edits (insertion/deletion
on the line) destroy an overlay, the next `grease--scan-buffer` call
(triggered before save or transaction build) will recreate it.

### Stale overlay cleanup on re-render

**Problem.**  `erase-buffer` deletes all buffer text but **does not delete
overlays**.  Since Grease had never used overlays before this patch, every
call site that relied on `erase-buffer` for a clean slate was unaware of
this distinction.  When the user navigated into a child directory
(e.g. pressing RET on a folder), the second `grease--render` call would
`erase-buffer` and then insert fresh content — but the old symlink overlay
from the first directory survived as a zero-width ghost at position 1,
its `after-string` still containing the previous directory's resolved path.
This caused the old symlink target to appear before the header text on
the new buffer, and on repeated navigations multiple ghosts would
accumulate.

**Fix.**  Insert an explicit overlay cleanup in `grease--render` immediately
after `erase-buffer`:

```elisp
    (erase-buffer)
    ;; erase-buffer does not delete overlays; remove stale ones now
    (dolist (ov (overlays-in (point-min) (point-max)))
      (delete-overlay ov))
```

Since overlays were not used prior to this patch, no existing code path
even considered overlay lifetime.  This defensive cleanup guarantees that
every render starts with a clean overlay slate regardless of what previous
renders or user edits may have left behind.

---

## 2. Remove debug logging and unrelated diff-engine formatting

### Problem

Commit `769a11b` introduced two categories of unrelated changes:

1. **Debug logging** in `grease--update-line-metadata` (lines ~1210–1211):
   ```elisp
   (unless (equal old-path full-path)
     (message "grease--update-line-metadata: line=%d old=%s new=%s path-changed"
              (line-number-at-pos) old-path full-path))
   ```
   This spams the `*Messages*` buffer during normal use.

2. **Formatting-only changes** in `grease--diff-by-id`:
   - Multi-line plist arguments collapsed onto single lines.
   - Docstring shortened (the "pure function" explanation removed).
   - Blank-line separators between logical sections removed.
   - The `;; Existing identities are either unchanged, relocated, or deleted.`
     and `;; Identities absent from the baseline are creates or copies.`
     section comments deleted.

### Fix

**Remove the debug `message` call** from `grease--update-line-metadata`.
Keep the rest of the metadata-update logic (including the useful `old-path`
tracking variable, which is now used to detect symlink changes).

**Restore `grease--diff-by-id`** to its pre-symlink-commit formatting: full
docstring, section comments, and multi-line plist layout for readability.
The logic remains identical — only formatting/comments are returned.

### Diff sketch for `grease--diff-by-id` restoration

```elisp
(defun grease--diff-by-id (baseline current-entries)
  "Return semantic operations between BASELINE and CURRENT-ENTRIES.
BASELINE is a hash table keyed by stable file ID.  This function is pure:
it reads only its arguments and never examines or mutates the filesystem."
  (let ((current-by-id (make-hash-table :test 'eql))
        operations)
    (dolist (entry current-entries)
      (when-let ((id (plist-get entry :id)))
        (puthash id entry current-by-id)))

    ;; Existing identities are either unchanged, relocated, or deleted.
    (maphash
     (lambda (id original)
       (let ((current (gethash id current-by-id)))
         (cond
          ((not current)
           (push (list :kind 'delete
                       :id id
                       :src (plist-get original :path)
                       :type (plist-get original :type))
                 operations))
          ((not (equal (plist-get original :path)
                       (plist-get current :path)))
           (push (list :kind 'relocate
                       :id id
                       :src (plist-get original :path)
                       :dst (plist-get current :path)
                       :type (plist-get current :type))
                 operations)))))
     baseline)

    ;; Identities absent from the baseline are creates or copies.
    (dolist (entry current-entries)
      (let* ((id (plist-get entry :id))
             (source-id (plist-get entry :source-id)))
        (unless (gethash id baseline)
          (if source-id
              (let* ((source-baseline (gethash source-id baseline))
                     (source-path (and source-baseline
                                       (plist-get source-baseline :path)))
                     (source-committed-p (and source-baseline
                                              (plist-get source-baseline
                                                         :committed-p)))
                     (entry-committed-p
                      (if source-baseline
                          (plist-get entry :source-committed-p)
                        (plist-get entry :source-committed-p))))
                (if (and source-id source-path source-committed-p)
                    (push (list :kind 'copy
                                :id id
                                :source-id source-id
                                :src source-path
                                :dst (plist-get entry :path)
                                :type (plist-get entry :type))
                          operations)
                  (push (list :kind 'create
                              :id id
                              :dst (plist-get entry :path)
                              :type (plist-get entry :type))
                        operations)))
            (push (list :kind 'create
                        :id id
                        :dst (plist-get entry :path)
                        :type (plist-get entry :type))
                  operations)))))
    operations))
```

---

## 3. Symlink-specific regression tests

### Goals

- Verify that **creating a buffer with symlinks produces zero filesystem
  operations** (i.e., display is purely cosmetic).
- Verify that `grease--extract-filename` returns the bare filename without any
  symlink suffix when called on visible buffer text (after the overlay change,
  the suffix is not in the buffer at all).
- Verify that broken-symlink entries use the `grease-symlink-broken` face
  while valid symlinks use `font-lock-comment-face`.
- Verify that the symlink display toggle (`grease-show-symlink-targets`)
  suppresses/reveals the overlay correctly.
- Verify that renaming a symlink entry does not pull overlay text into the
  parsed filename.

### New test helper (in `grease-test.el`)

```elisp
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
```

### Tests

```elisp
(ert-deftest grease-test-symlink-display-produces-no-operations ()
  "Displaying a buffer with symlink entries must not create diff operations."
  (grease-test-with-temp-dir
    (write-region "real-content" nil (expand-file-name "target.txt" temp-dir))
    (make-symbolic-link "target.txt" (expand-file-name "link.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      ;; The rendered buffer should contain the symlink entry
      (should (grease-test-goto-entry "link.txt"))
      ;; No operations should be derived from the display alone
      (let ((ops (grease--diff-by-id
                  (grease--build-baseline)
                  (grease--collect-entries))))
        (should (zerop (length ops)))
        "Displaying a symlink target must not register any diff operation"))))

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
        (should-not (string-match-p "\\|target\\.txt" visible-text))
        "The overlay after-string must not be part of the buffer text"))))

(ert-deftest grease-test-symlink-broken-display ()
  "A broken symlink should produce no diff operations and use the warning face."
  (grease-test-with-temp-dir
    (make-symbolic-link "nonexistent" (expand-file-name "broken" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "broken")
      (let ((ops (grease--diff-by-id
                  (grease--build-baseline)
                  (grease--collect-entries))))
        (should (zerop (length ops)))
        "A broken symlink must not create a diff operation")
      ;; After the overlay change, verify the overlay uses the broken face
      (let ((ov (cl-find-if
                 (lambda (o) (overlay-get o 'grease-symlink-display))
                 (overlays-in (line-beginning-position) (line-end-position)))))
        (should ov)
        (let ((str (overlay-get ov 'after-string)))
          (should str)
          (should (eq (get-text-property 0 'face str)
                      'grease-symlink-broken))
          "The after-string on a broken symlink must use grease-symlink-broken face"))
      (should-not (file-exists-p "nonexistent")))))

(ert-deftest grease-test-symlink-valid-display ()
  "A valid symlink should use font-lock-comment-face on the after-string."
  (grease-test-with-temp-dir
    (write-region "real" nil (expand-file-name "real.txt" temp-dir))
    (make-symbolic-link "real.txt" (expand-file-name "good-link" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "good-link")
      (let ((ov (cl-find-if
                 (lambda (o) (overlay-get o 'grease-symlink-display))
                 (overlays-in (line-beginning-position) (line-end-position)))))
        (should ov)
        (let ((str (overlay-get ov 'after-string)))
          (should str)
          (should (eq (get-text-property 0 'face str)
                      'font-lock-comment-face))
          "The after-string on a valid symlink must use font-lock-comment-face")))))

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
        (should-not (cl-find-if
                     (lambda (o) (overlay-get o 'grease-symlink-display))
                     (overlays-in (line-beginning-position) (line-end-position)))
                    "Setting grease-show-symlink-targets to nil must suppress the overlay")))))

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
        (should (equal parsed "renamed-link.txt"))
        "Renaming a symlink entry must produce a clean filename without suffix text"))))
```

---

## Summary of files to touch

| File | Change |
|---|---|
| `grease.el` | Replace symlink buffer-text insertion with overlay + `after-string` in `grease--insert-entry` (anchor on trailing newline) |
| `grease.el` | Add `grease--refresh-symlink-overlay` helper function |
| `grease.el` | Call `grease--refresh-symlink-overlay` from `grease--update-line-metadata` |
| `grease.el` | Revert `grease--extract-filename` to original (no symlink suffix stripping needed) |
| `grease.el` | Delete `message` debug call in `grease--update-line-metadata` |
| `grease.el` | Restore `grease--diff-by-id` docstring, section comments, and original plist formatting |
| `grease-test.el` | Add helper `grease-test-with-symlinks` macro |
| `grease-test.el` | Add the six regression tests above |
