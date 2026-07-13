# PATCH_1 — Symlink target display: overlay, cleanup, and tests

## Overview

This patch addresses all three maintainer review points for the symlink target
display feature in commit `769a11b`.  The changes are:

1.  **Render symlink targets via an overlay `after-string`** anchored on the
    **last character of the filename** (never on the newline), so the arrow
    and resolved path are purely display-only and never become part of the
    editable buffer text.

2.  **Remove debug logging and revert unrelated diff-engine formatting** to
    keep the PR focused on symlink support.

3.  **Add symlink-specific regression tests** that confirm display produces no
    filesystem operations, `grease--extract-filename` properly excludes
    overlay text, and broken-symlink faces are applied without side effects.

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

### Fix: overlay anchored on the last filename character

**Replace the direct insertion** in `grease--insert-entry` with an overlay
whose span is **the last character of the display-name** (e.g., the `/` in
`mydir/` or the `k` in `mylink`), carrying the arrow + face-propertized
target as its `after-string`.

**Why not anchor on the newline?** Early versions of this patch anchored on
the trailing newline (`\n`).  The `after-string` at a newline position caused
the overlay text to display at the **beginning of the next line**, corrupting
the header of the following render.  The maintainer's original suggestion of
"anchored on the newline" was refined to "anchored on the last visible
character" after testing.

**Implementation in `grease--insert-entry`** — the overlay is created AFTER
`(insert "\n")`, using `(line-end-position)` captured BEFORE the newline
insertion (since point moves to the next line after `insert`):

```elisp
    (let ((name-start (point)))
      (insert display-name)
      (add-text-properties start (point) ...)
      (put-text-property name-start (point) 'face 'font-lock-function-name-face))

    ;; Capture end-of-line before inserting the newline
    (let ((eol (line-end-position)))
      (insert "\n")

      ;; Optionally display symlink target via a display-only overlay
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
          (let ((ov (make-overlay (1- eol) eol)))
            (overlay-put ov 'after-string suffix)
            (overlay-put ov 'grease-symlink-display t)))))
```

Key details:
- `eol` is captured by `(line-end-position)` BEFORE `(insert "\n")` so it
  points to the position of the newline about to be inserted.
- The overlay covers `(1- eol)` to `eol`, i.e., the single character
  immediately before the newline — the last visible character of the filename.
- The `after-string` at the end of the overlay displays at position `eol`,
  which is the newline character.  The after-string appears BEFORE the
  newline's line break, so it shows on the same line as the filename.
- The overlay is tagged with `grease-symlink-display` (matching Grease's
  existing lowercase-hyphenated conventions: `grease-prefix`, `grease-icon`,
  `grease-editable`).

### `grease--extract-filename` — revert to original

Because the overlay `after-string` is never part of the buffer, the
`string-match ""` / `string-trim-right` stripping logic added in the
original commit is no longer necessary.  Revert `grease--extract-filename` to
its pre-symlink implementation.  Docstring reverts to `"Extract just the
filename from TEXT, removing ID and icon."`.

### Overlay refresh helper: `grease--refresh-symlink-overlay`

A new helper that refreshes the symlink overlay for a single line:

```elisp
(defun grease--refresh-symlink-overlay (line-beg line-end full-path)
  "Refresh the symlink display overlay on the line between LINE-BEG and LINE-END.
If FULL-PATH is a symlink and `grease-show-symlink-targets' is non-nil,
create a display-only overlay anchored on the last character of the line.
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
      (let ((ov (make-overlay (1- line-end) line-end)))
        (overlay-put ov 'after-string suffix)
        (overlay-put ov 'grease-symlink-display t)))))
```

Note: the overlay spans `(1- line-end)` to `line-end`, i.e., the last
visible character before the newline — the same anchoring as in
`grease--insert-entry`.  `line-end` is the position of the newline character
from `line-end-position`.

**Integration into `grease--update-line-metadata`** — after the existing
`put-text-property` calls:

```elisp
                ;; Refresh symlink display overlay
                (grease--refresh-symlink-overlay line-beg line-end full-path)
```

This runs on every metadata update (triggered before save or transaction
build), so overlays are always in sync with the current buffer content.

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
the new buffer, and on repeated navigations multiple ghosts would accumulate.

**Fix.**  Insert an explicit overlay cleanup in `grease--render` immediately
after `erase-buffer`:

```elisp
    (erase-buffer)
    ;; erase-buffer does not delete overlays; remove stale ones now
    (dolist (ov (overlays-in (point-min) (point-max)))
      (delete-overlay ov))
```

Since overlays were not used prior to this patch, no existing code path even
considered overlay lifetime.  This defensive cleanup guarantees that every
render starts with a clean overlay slate.

### Overlay cleanup on line delete (`dd`)

**Problem.**  `delete-region` (used internally by Evil's `dd`) also does
**not** delete overlays.  After deleting a symlink line, the overlay survived
as a ghost at the position where the line was, displaying its `after-string`
on whatever adjacent text shifted into that position.

**Fix.**  In `grease--before-evil-delete` (the `:before` advice on
`evil-delete` and `evil-delete-line`), remove symlink overlays from the
line(s) about to be deleted before Evil performs the deletion:

```elisp
;; Single-line dd
(dolist (ov (overlays-in (line-beginning-position) (1+ (line-end-position))))
  (when (overlay-get ov 'grease-symlink-display)
    (delete-overlay ov)))

;; Visual multi-line (Vjjdd) — same pattern inside the line iteration
```

This is done in both the single-line `(t)` branch and the visual multi-line
`(eq evil-state 'visual)` branch of `grease--before-evil-delete`.

### Overlay refresh after undo/redo

**Problem.**  Emacs undo restores buffer text and text properties but does
**not** restore overlays.  After the user presses `u` following a `dd`, the
deleted symlink line comes back with its `grease-name`, `grease-full-path`,
etc. intact — but the `grease-symlink-display` overlay is gone, so the
symlink target arrow and path do not appear.

**Fix.**  Expand `grease--on-change` (the `after-change-functions` hook) to
recreate overlays for affected lines:

```elisp
(defun grease--on-change (_beg _end _len)
  "Hook run after buffer changes to mark it dirty and refresh symlink overlays."
  (unless grease--change-hook-active
    (let ((grease--change-hook-active t))
      (setq grease--buffer-dirty-p t)
      (when (and (derived-mode-p 'grease-mode) grease--root-dir)
        (save-excursion
          (let* ((beg-line (progn (goto-char _beg) (line-beginning-position)))
                 (end-line (progn (goto-char (max _beg _end))
                                  (forward-line 1)
                                  (line-beginning-position))))
            (goto-char beg-line)
            (while (< (point) end-line)
              (let* ((lb (line-beginning-position))
                     (le (line-end-position))
                     (data (grease--get-line-data)))
                (when data
                  (let ((fp (plist-get data :full-path)))
                    (when fp
                      (grease--refresh-symlink-overlay lb le fp)))))
              (forward-line 1))))))))
```

The `grease--root-dir` guard prevents running during buffer setup, and the
`(when fp ...)` check avoids errors on non-entry lines (newly typed text that
hasn't been formatted yet).

### Overlay lifecycle summary

| Event | Mechanism | What happens |
|---|---|---|
| **Initial render** | `grease--insert-entry` | Overlay created on last filename char |
| **Refresh / metadata update** | `grease--update-line-metadata` → `grease--refresh-symlink-overlay` | Old overlay removed, new one created at `(1- line-end)` |
| **Re-render (navigate)** | `grease--render` after `erase-buffer` | All overlays explicitly deleted, then recreated by `grease--insert-entry` |
| **Line delete (dd)** | `grease--before-evil-delete` | Overlays removed from targeted lines before Evil deletes them |
| **Undo/redo** | `grease--on-change` (after-change-functions) | Overlays recreated on affected lines |

### Why not rely solely on `after-change-functions`?

A natural question is whether the hook-based `grease--on-change` alone could replace the
explicit cleanup points.  The answer is no — here is why each event cannot be handled by
the hook:

| Event | `after-change-functions` fires? | Why the hook alone is insufficient |
|---|---|---|
| `erase-buffer` in `grease--render` | **No** — `inhibit-modification-hooks t` suppresses all hooks during rendering | Hooks are explicitly inhibited during rendering to avoid performance costs and recursive triggers.  The hook cannot help. |
| Overlay creation on initial render | **No** — same inhibition | `grease--insert-entry` must create overlays directly during rendering. |
| Overlay refresh in `grease--update-line-metadata` | **Not applicable** — this runs during transaction building (save), not in response to a buffer change | It is a polling refresh, not event-driven.  The hook fires immediately on edit; the metadata update runs lazily when computing diffs. |
| `dd` (line deletion) | **Yes** — `delete-region` fires `after-change-functions` | The deletion has already happened by the time the hook runs.  The overlay that was on the deleted line survived `delete-region` and is now a ghost clamped to whatever position the deleted region mapped to (often position 1).  `grease--on-change` iterates over the now-empty deleted region and finds no entry lines there — the ghost is outside its search range. |
| Undo/redo | **Yes** | This is the only case where the hook is sufficient — the restored text arrives via undo, the hook fires, and `grease--refresh-symlink-overlay` correctly recreates the overlay. |

---

## 2. Remove debug logging and unrelated diff-engine formatting

### Problem

Commit `769a11b` introduced two categories of unrelated changes:

1. **Debug logging** in `grease--update-line-metadata`:
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

**Restore `grease--diff-by-id`** to its pre-symlink-commit formatting: full
docstring, section comments, and multi-line plist layout.  Logic unchanged
(note: the `nreverse` call and cross-buffer `source-committed-p` fallback
logic from the original commit `769a11b` are preserved for compatibility with
existing tests).

---

## 3. Symlink-specific regression tests

### Goals

- Render a buffer with symlinks → produce zero diff operations
- `grease--extract-filename` returns clean filename (no overlay text bleeding
  into buffer content)
- Broken symlinks use `grease-symlink-broken` face; valid symlinks use
  `font-lock-comment-face`
- Toggle `grease-show-symlink-targets` to `nil` → overlay suppressed
- Rename a symlink entry → clean extracted filename
- Delete a symlink line → no ghost overlays remain

### New test helper

```elisp
(defmacro grease-test-with-symlinks (dir symlinks &rest body)
  "Execute BODY in DIR after creating SYMLINKS.
SYMLINKS is a list of (LINK-NAME TARGET) pairs."
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

(ert-deftest grease-test-symlink-delete-removes-overlay ()
  "Deleting a symlink line must remove its overlay to prevent ghost displays."
  (grease-test-with-temp-dir
    (write-region "x" nil (expand-file-name "target.txt" temp-dir))
    (make-symbolic-link "target.txt" (expand-file-name "link.txt" temp-dir))
    (grease-test-with-buffer temp-dir
      (grease-test-goto-entry "link.txt")
      (let ((inhibit-read-only t))
        (dolist (ov (overlays-in (line-beginning-position) (1+ (line-end-position))))
          (when (overlay-get ov 'grease-symlink-display)
            (delete-overlay ov)))
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      (let ((ghosts (cl-remove-if-not
                     (lambda (o) (overlay-get o 'grease-symlink-display))
                     (overlays-in (point-min) (point-max)))))
        (should (zerop (length ghosts)))))))
```

---

## Summary of files touched

`grease.el`:

| Change | Description |
|---|---|
| `grease--insert-entry` | Replace text-based symlink insertion with overlay `after-string` anchored on last filename char (`(1- eol)` to `eol`). Overlay created AFTER `(insert "\n")`. |
| `grease--refresh-symlink-overlay` | New helper: remove stale overlay, create new one at `(1- line-end)` to `line-end`. |
| `grease--update-line-metadata` | Remove debug `message` call. Add `(grease--refresh-symlink-overlay ...)` call. |
| `grease--render` | Add explicit overlay cleanup after `erase-buffer()`. |
| `grease--before-evil-delete` | Add overlay cleanup in both single-line and visual-multi-line branches (removes overlays before Evil deletes). |
| `grease--on-change` | Expand to refresh symlink overlays on affected lines after buffer changes (handles undo/redo). |
| `grease--extract-filename` | Revert to original (no symlink-suffix stripping). |
| `grease--diff-by-id` | Restore docstring, section comments, and multi-line formatting. |

`grease-test.el`:

| Change | Description |
|---|---|
| `grease-test-with-symlinks` | New helper macro for creating symlink fixtures. |
| 7 new `ert-deftest` forms | Symlink display, extraction, faces, toggle, rename, and delete cleanup. |

## Test results

```
Ran 177 tests, 177 results as expected, 0 unexpected
```
