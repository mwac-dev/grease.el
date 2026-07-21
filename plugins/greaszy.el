;;; greaszy.el --- Zoxide-powered directory travel for Grease  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Greaszy is a self-contained Grease plugin that integrates the `zoxide'
;; directory-jumper with Emacs completion.  It provides a single entry point,
;; `greaszy-travel', which:
;;
;; 1. Queries the zoxide database (`zoxide query -ls`) for directories
;;    ranked by frecency (frequency + recency).
;; 2. Displays them in the minibuffer via consult's async completion,
;;    with the frecency score shown to the left of each path.
;; 3. On selection, opens the directory in Grease via `grease-open'.
;;
;; ── Keybindings (active in the greaszy minibuffer) ──
;;
;;   C-i  →  `greaszy-embark-add'       Boost the selected directory's
;;                                       frecency score by `greaszy-add-amount'.
;;                                       Runs `zoxide add --score N <path>'.
;;   C-d  →  `greaszy-embark-subtract'  Remove the selected directory from the
;;                                       zoxide database.  Runs `zoxide remove
;;                                       <path>'.
;;
;; Note: zoxide does not support a native "decrement" operation as of the
;; current version.  `greaszy-embark-subtract' fully removes the entry rather
;; than lowering its score.
;;
;; ── Requirements ──
;;
;; This plugin expects the following Emacs packages to be installed and
;; loaded.  If any are missing, `greaszy-travel' signals `user-error' with
;; instructions.
;;
;;   - consult  (provides `consult--read' and async pipeline)
;;   - embark   (provides embark keymap integration)
;;   - vertico  (provides the vertical completion UI; embark refresh depends
;;               on vertico internals)
;;
;; Additionally, the `zoxide' binary must be present on `exec-path'.
;;
;; ── Usage ──
;;
;; Place this file in Grease's `plugins/' directory and enable plugin
;; loading:
;;
;;   (setq grease-load-plugins t)
;;   (require 'grease)
;;
;; Bind `greaszy-travel' to a key of your choice:
;;
;;   (global-set-key (kbd "M-z") #'greaszy-travel)
;;
;; This plugin loads /after/ `(provide 'grease)', so all Grease symbols
;; (including `grease-open') are available at load time.
;;
;; ── Customization ──
;;
;;   greaszy-show-scores         Show/hide frecency scores (default t)
;;   greaszy-score-width         Width of the score field (default 6)
;;   greaszy-score-path-padding  Spaces between score and path (default 4)
;;   greaszy-add-amount          Score added by embark `+' (default 5)
;;   greaszy-subtract-amount     Score subtracted by embark `-' (default 5)
;;   greaszy-score-face          Face for the frecency score

;;; Code:

;; ════════════════════════════════════════════════════════════════════════════
;; ── Dependency guard ───────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defun greaszy--check-deps ()
  "Signal `user-error' if any required dependency is missing."
  (unless (executable-find "zoxide")
    (user-error "Greaszy: `zoxide' binary not found on exec-path"))
  (unless (fboundp 'consult--read)
    (user-error "Greaszy: consult is required — install it with `M-x package-install RET consult RET'"))
  (unless (featurep 'embark)
    (user-error "Greaszy: embark is required — install it with `M-x package-install RET embark RET'"))
  (unless (featurep 'vertico)
    (user-error "Greaszy: vertico is required — install it with `M-x package-install RET vertico RET'"))
  t)

;; ════════════════════════════════════════════════════════════════════════════
;; ── Customization ──────────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defcustom greaszy-executable (executable-find "zoxide")
  "Path to the `zoxide' binary."
  :type 'string
  :group 'grease)

(defcustom greaszy-show-scores t
  "When non-nil, display the frecency score to the left of each path.
When nil, only the path is shown (scores are still used for ranking
behind the scenes)."
  :type 'boolean
  :group 'grease)

(defcustom greaszy-score-width 6
  "Width of the score field when displaying greaszy results.
The score is right-justified within this width.
Has no effect when `greaszy-show-scores' is nil."
  :type 'integer
  :group 'grease)

(defcustom greaszy-score-path-padding 4
  "Spaces between the score and the path in greaszy results.
Has no effect when `greaszy-show-scores' is nil."
  :type 'integer
  :group 'grease)

(defcustom greaszy-add-amount 5
  "Amount added to a directory's score on `greaszy-embark-add'.
Passed as `--score' to `zoxide add'."
  :type 'integer
  :group 'grease)

(defcustom greaszy-subtract-amount 5
  "Fixed amount subtracted on `greaszy-embark-subtract'.
The directory is removed and re-added at `max(1, current - this)'."
  :type 'integer
  :group 'grease)

;; ════════════════════════════════════════════════════════════════════════════
;; ── Faces ──────────────────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defface greaszy-score-face
  '((t (:inherit font-lock-comment-face)))
  "Face for the frecency score in greaszy results."
  :group 'grease)

;; ════════════════════════════════════════════════════════════════════════════
;; ── Zoxide process ─────────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defun greaszy--run (async &rest args)
  "Run the `zoxide' command with ARGS.
If ASYNC is non-nil, launch asynchronously via `start-process' and return
the process object.  Otherwise run synchronously and return stdout as a
string, or signal a warning and return nil on failure."
  (if async
      (apply #'start-process "greaszy" "*greaszy*" greaszy-executable args)
    (with-temp-buffer
      (if (equal 0 (apply #'call-process greaszy-executable nil t nil args))
          (buffer-string)
        (append-to-buffer "*greaszy*" (point-min) (point-max))
        (warn "Greaszy: zoxide error (see buffer *greaszy* for details)")
        nil))))

;; ════════════════════════════════════════════════════════════════════════════
;; ── Score parsing & display ────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defun greaszy-parse-score-line (line)
  "Parse a single LINE from `zoxide query -ls' into (SCORE . PATH).
LINE format is \"<score> <path>\" where score may be right-justified
with leading whitespace.  Returns nil if LINE does not match."
  (let ((trimmed (string-trim line)))
    (when (string-match (rx (group (+ (or digit ?.)))  ;; score number
                            " "                        ;; separator
                            (group (+ any)))           ;; path
                        trimmed)
      (cons (string-to-number (match-string 1 trimmed))
            (string-trim (match-string 2 trimmed))))))

(defun greaszy-format-entry (score path &optional score-width padding)
  "Format a greaszy entry with SCORE right-justified before PATH.
When `greaszy-show-scores' is nil, only the path is returned.
SCORE-WIDTH controls the score field width (default `greaszy-score-width').
PADDING is the number of spaces between score and path."
  (if (not greaszy-show-scores)
      path
    (let* ((sw (or score-width greaszy-score-width))
           (pad (or padding greaszy-score-path-padding))
           (score-str (format (format "%%%d.1f" sw) score)))
      (concat
       (propertize score-str 'face 'greaszy-score-face)
       (make-string pad ?\s)
       path))))

(defun greaszy-consult-format (line)
  "Format a raw LINE from `zoxide query -ls' for consult display.
Returns a propertized string with `greaszy-score' and `greaszy-path'
text properties, or nil if LINE can't be parsed."
  (pcase (greaszy-parse-score-line line)
    (`(,score . ,path)
     (propertize (greaszy-format-entry score path)
                 'greaszy-score score
                 'greaszy-path path))
    (_ nil)))

(defun greaszy-consult-builder (input)
  "Build command line for `zoxide query -ls' from INPUT.
Returns a command list or nil.
Splits INPUT on whitespace for multi-keyword matching."
  (if (or (not input) (string-empty-p input))
      (list greaszy-executable "query" "-ls")
    (apply #'list greaszy-executable "query" "-ls" (split-string input))))

;; ════════════════════════════════════════════════════════════════════════════
;; ── Consult async pipeline ─────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defun greaszy--async-wrap (async)
  "Wrap ASYNC function for zoxide without the split/`#' prefix.
Adds an indicator spinner and refresh-on-input behaviour via
`consult--async-pipeline'."
  (consult--async-pipeline
   async
   (consult--async-indicator)
   (consult--async-refresh)))

;; ════════════════════════════════════════════════════════════════════════════
;; ── Keymap ─────────────────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defvar-keymap greaszy-consult-map
  :doc "Additional keybindings for the greaszy travel minibuffer.
C-i boosts the frecency score of the selected directory.
C-d removes the selected directory from zoxide."
  "C-i" #'greaszy-embark-add
  "C-d" #'greaszy-embark-subtract)

;; ════════════════════════════════════════════════════════════════════════════
;; ── Embark actions ─────────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

(defun greaszy--embark-extract-path (&optional candidate)
  "Extract the path from a vertico candidate.
If CANDIDATE is provided, strip its score prefix.  Otherwise read from
`vertico--candidate', tofu-strip, and parse."
  (unless candidate
    (setq candidate (vertico--candidate))
    (when (and candidate (fboundp 'consult--tofu-strip))
      (setq candidate (consult--tofu-strip candidate))))
  (or (cdr (greaszy-parse-score-line candidate)) candidate))

(defun greaszy--embark-refresh ()
  "Re-query zoxide and replace `vertico--candidates' in place.
Uses the current minibuffer input as the query filter."
  (let* ((input (minibuffer-contents-no-properties))
         (args (if (or (not input) (string-empty-p input))
                   '("query" "-ls")
                 `("query" "-ls" ,input)))
         (raw (apply #'greaszy--run nil args))
         (lines (and raw (remove "" (split-string raw "\n" t))))
         (new-candidates (delq nil (mapcar #'greaszy-consult-format lines))))
    (when (and (boundp 'vertico--candidates) vertico--candidates)
      (setq vertico--candidates new-candidates
            vertico--total (length vertico--candidates))
      (if (zerop vertico--total)
          (setq vertico--index -1)
        (when (>= vertico--index vertico--total)
          (setq vertico--index (max 0 (1- vertico--total)))))
      (vertico--prompt-selection)
      (vertico--display-count)
      (vertico--display-candidates (vertico--arrange-candidates)))))

(defun greaszy-embark-add (&optional candidate)
  "Boost the frecency score of the selected zoxide directory.
Runs `zoxide add --score N <path>' where N is `greaszy-add-amount'."
  (interactive)
  (setq candidate (greaszy--embark-extract-path candidate))
  (when candidate
    (greaszy--run nil "add" "--score"
                  (number-to-string greaszy-add-amount) candidate)
    (greaszy--embark-refresh)
    (message "Greaszy: %s +%d" candidate greaszy-add-amount))
  candidate)

(defun greaszy-embark-subtract (&optional candidate)
  "Remove the selected directory from the zoxide database.
Runs `zoxide remove <path>'.  Note: zoxide does not support a native
'decrement' operation, so the entry is fully removed rather than
having its score lowered."
  (interactive)
  (setq candidate (greaszy--embark-extract-path candidate))
  (when candidate
    (greaszy--run nil "remove" candidate)
    (greaszy--embark-refresh)
    (message "Greaszy: %s removed" candidate))
  candidate)

(defvar-keymap greaszy-embark-path-map
  :doc "Keymap for embark actions on greaszy-path candidates."
  :parent embark-general-map
  "+" #'greaszy-embark-add
  "-" #'greaszy-embark-subtract)

;; Register the embark keymap for our completion category.
(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(greaszy-path . greaszy-embark-path-map)))

;; ════════════════════════════════════════════════════════════════════════════
;; ── Main entry point ───────────────────────────────────────────────────────
;; ════════════════════════════════════════════════════════════════════════════

;;;###autoload
(defun greaszy-travel ()
  "Open a directory from zoxide, ranked by frecency with consult.
Shows the frecency score to the left of each path.
On selection, opens the directory in Grease via `grease-open'.

Requires consult, embark, and vertico.  The `zoxide' binary must be
on `exec-path'.  Signals `user-error' if any dependency is missing.

This function does not bind itself to any key — add your own binding,
e.g. (global-set-key (kbd \"M-z\") #'greaszy-travel)."
  (interactive)
  (greaszy--check-deps)
  (let* ((candidate
          (consult--read
           (consult--process-collection #'greaszy-consult-builder
             :transform (consult--async-map #'greaszy-consult-format))
           :async-wrap #'greaszy--async-wrap
           :keymap greaszy-consult-map
           :prompt "󰡦 : "
           :category 'greaszy-path
           :require-match t
           :sort nil
           :lookup (lambda (selected &rest _)
                     (when selected
                       (or (cdr (greaszy-parse-score-line selected))
                           selected))))))
    (when candidate
      (grease-open candidate))
    candidate))

(provide 'greaszy)
;;; greaszy.el ends here
