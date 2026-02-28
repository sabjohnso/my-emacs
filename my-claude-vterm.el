;;; my-claude-vterm.el --- Claude Code in vterm with reduced flicker -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Run Claude Code (and other LLMs) inside Emacs vterm with fixes for:
;; - Cursor disappearing due to DECSCUSR/DECTCEM escape sequences
;; - Flickering from unsupported DEC Mode 2026 (synchronized output)
;; - Broken copy-mode navigation (fake newlines, invisible cursor)
;; - No notification when Claude finishes
;; - C-g not mapped to ESC (Claude's cancel)
;; - Unnecessary terminal reflows on vertical-only resizes
;;
;; Techniques cherry-picked from claude-code.el and claudemacs.

(require 'cl-lib)

(use-package vterm :ensure t :commands (vterm vterm-mode))

(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-key "vterm")
(declare-function vterm--filter "vterm")


;;;; ---------------------------------------------------------------
;;;; Public API
;;;; ---------------------------------------------------------------

;;;###autoload
(defun my-claude ()
  "Run Claude Code in a vterm buffer named *claude-<directory>*."
  (interactive)
  (my--run-llm-in-vterm "claude"))

;;;###autoload
(defun my-codex ()
  "Run Codex in a vterm buffer named *codex-<directory>*."
  (interactive)
  (my--run-llm-in-vterm "codex"))


;;;; ---------------------------------------------------------------
;;;; Buffer setup
;;;; ---------------------------------------------------------------

(defun my--run-llm-in-vterm (command)
  "Run COMMAND in a vterm buffer named *COMMAND-<directory>*.
Create the buffer if it does not exist, then apply all vterm
configuration via `my--llm-vterm-configure'."
  (require 'vterm)
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name default-directory)))
         (buf-name (format "*%s-%s*" command dir-name)))
    (if (get-buffer buf-name)
        (pop-to-buffer buf-name)
      (let ((buf (generate-new-buffer buf-name)))
        (with-current-buffer buf
          (vterm-mode)
          (my--llm-vterm-configure)
          (vterm-send-string (concat command "\n")))
        (pop-to-buffer buf)))))

(defun my--llm-vterm-configure ()
  "Apply all Claude-specific vterm settings to the current buffer.
Must be called after `vterm-mode' has been activated."

  ;; -- Cursor fix --
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil)

  ;; -- Scroll stabilization --
  (setq-local scroll-conservatively 10000)
  (setq-local scroll-margin 0)
  (setq-local auto-window-vscroll nil)
  (setq-local scroll-step 1)
  (setq-local line-spacing 0)

  ;; -- Anti-flicker buffer-local variables --
  (setq-local my--vterm-output-buffer "")
  (setq-local my--vterm-flush-timer nil)
  (setq-local my--vterm-notify-timer nil)
  (setq-local my--vterm-cached-width nil)

  ;; -- Copy-mode: strip fake newlines for isearch --
  (when (boundp 'vterm-copy-mode-remove-fake-newlines)
    (setq-local vterm-copy-mode-remove-fake-newlines t))

  ;; -- Display table: replace U+23FA (⏺) with ✽ --
  (my--llm-vterm-setup-display-table)

  ;; -- Keybindings --
  (my--llm-vterm-setup-keymap)

  ;; -- Hooks (buffer-local) --
  (add-hook 'vterm-copy-mode-hook #'my--llm-vterm-copy-mode-hook nil t)
  (add-hook 'kill-buffer-hook #'my--llm-vterm-teardown nil t)

  ;; -- Install advice (buffer-aware) --
  (my--llm-vterm-install-advice))


;;;; ---------------------------------------------------------------
;;;; Anti-flicker filter
;;;; ---------------------------------------------------------------

(defvar-local my--vterm-output-buffer ""
  "Accumulated output waiting to be flushed as a single batch.")

(defvar-local my--vterm-flush-timer nil
  "Timer that flushes accumulated output.")

(defvar-local my--vterm-notify-timer nil
  "Debounce timer for desktop notifications.")

(defconst my--vterm-redraw-pattern
  "\033\\[[0-9]*;[0-9]*H\\|\033\\[[0-9]*K\\|\033\\[?25[lh]"
  "Regexp matching cursor-positioning, line-clearing, and cursor
visibility escape sequences that indicate a full redraw.")

(defconst my--vterm-escape-pattern "\033\\["
  "Regexp matching the start of any CSI escape sequence.")

(defconst my--vterm-min-escapes-for-batch 3
  "Minimum number of escape sequences in a chunk before we batch it.")

(defun my--vterm-looks-like-redraw-p (output)
  "Return non-nil if OUTPUT looks like a screen redraw.
Checks for at least `my--vterm-min-escapes-for-batch' escape
sequences and at least one cursor-positioning or line-clearing sequence."
  (and (>= (my--vterm-count-matches my--vterm-escape-pattern output)
            my--vterm-min-escapes-for-batch)
       (string-match-p my--vterm-redraw-pattern output)))

(defun my--vterm-count-matches (pattern string)
  "Count non-overlapping occurrences of PATTERN in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match pattern string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun my--vterm-flush-output (buf)
  "Flush accumulated output in BUF through the original vterm filter."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (> (length my--vterm-output-buffer) 0)
        (let ((output my--vterm-output-buffer)
              (proc (get-buffer-process buf)))
          (setq my--vterm-output-buffer "")
          (setq my--vterm-flush-timer nil)
          (when proc
            (let ((inhibit-redisplay t))
              (my--vterm-original-filter proc output))))))))

(defun my--vterm-filter-advice (orig-fn proc output)
  "Around advice for `vterm--filter'.
Buffers redraw-heavy output and flushes it in a single batch
after a short delay.  Normal text passes through immediately.
Also detects BEL character for notifications."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Bell detection
        (when (string-match-p "\a" output)
          (my--llm-vterm-notify buf))
        ;; Anti-flicker batching
        (if (my--vterm-looks-like-redraw-p output)
            (progn
              (setq my--vterm-output-buffer
                    (concat my--vterm-output-buffer output))
              (when my--vterm-flush-timer
                (cancel-timer my--vterm-flush-timer))
              (setq my--vterm-flush-timer
                    (run-at-time 0.01 nil
                                 #'my--vterm-flush-output buf)))
          ;; Not a redraw — flush any pending output first, then pass through
          (when (> (length my--vterm-output-buffer) 0)
            (let ((pending my--vterm-output-buffer))
              (setq my--vterm-output-buffer "")
              (when my--vterm-flush-timer
                (cancel-timer my--vterm-flush-timer)
                (setq my--vterm-flush-timer nil))
              (let ((inhibit-redisplay t))
                (funcall orig-fn proc pending))))
          (funcall orig-fn proc output))))))

(defun my--vterm-original-filter (proc output)
  "Call the original `vterm--filter' without our advice.
We remove advice temporarily to avoid recursion."
  (advice-remove 'vterm--filter #'my--vterm-filter-advice)
  (unwind-protect
      (vterm--filter proc output)
    (advice-add 'vterm--filter :around #'my--vterm-filter-advice)))


;;;; ---------------------------------------------------------------
;;;; Advice management (global, reference-counted)
;;;; ---------------------------------------------------------------

(defvar my--llm-vterm-buffer-count 0
  "Number of active LLM vterm buffers.
Advice is installed when this goes above 0, removed when it returns to 0.")

(defun my--llm-vterm-install-advice ()
  "Install vterm--filter advice if not already present."
  (cl-incf my--llm-vterm-buffer-count)
  (unless (advice-member-p #'my--vterm-filter-advice 'vterm--filter)
    (advice-add 'vterm--filter :around #'my--vterm-filter-advice))
  (unless (advice-member-p #'my--vterm-resize-advice
                           'vterm--window-adjust-process-window-size)
    (advice-add 'vterm--window-adjust-process-window-size
                :around #'my--vterm-resize-advice)))

(defun my--llm-vterm-teardown ()
  "Remove advice when no LLM vterm buffers remain.
Intended for `kill-buffer-hook'."
  (when my--vterm-flush-timer
    (cancel-timer my--vterm-flush-timer))
  (when my--vterm-notify-timer
    (cancel-timer my--vterm-notify-timer))
  (cl-decf my--llm-vterm-buffer-count)
  (when (<= my--llm-vterm-buffer-count 0)
    (setq my--llm-vterm-buffer-count 0)
    (advice-remove 'vterm--filter #'my--vterm-filter-advice)
    (advice-remove 'vterm--window-adjust-process-window-size
                   #'my--vterm-resize-advice)))


;;;; ---------------------------------------------------------------
;;;; Cursor fix (copy-mode)
;;;; ---------------------------------------------------------------

(defun my--llm-vterm-copy-mode-hook ()
  "Toggle cursor visibility when entering/leaving vterm copy mode."
  (if vterm-copy-mode
      (progn
        (setq-local cursor-type 'box)
        (my--llm-vterm-buttonize-files)
        ;; Override vterm-copy-mode-map's RET via minor-mode-overriding-map-alist
        ;; (minor mode maps outrank the local map, so use-local-map can't win)
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "RET") #'my--vterm-file-link-ret)
          (define-key map [return]    #'my--vterm-file-link-ret)
          (setq-local minor-mode-overriding-map-alist
                      (list (cons 'vterm-copy-mode map)))))
    (progn
      (setq-local cursor-type nil)
      (setq-local minor-mode-overriding-map-alist nil)
      ;; Restore our keymap — vterm replaces the local map on copy-mode exit
      (my--llm-vterm-setup-keymap))))


;;;; ---------------------------------------------------------------
;;;; File navigation (file:line buttons in copy-mode)
;;;; ---------------------------------------------------------------

(declare-function vterm-copy-mode-done "vterm")

(defun my--vterm-file-link-ret ()
  "Follow file link at point, or fall through to `vterm-copy-mode-done'."
  (interactive)
  (if (button-at (point))
      (push-button)
    (vterm-copy-mode-done nil)))

(define-button-type 'my-vterm-file-link
  'face 'compilation-info
  'follow-link t
  'action #'my--vterm-file-link-action
  'help-echo "RET or mouse-1: visit this file location")

(defun my--vterm-file-link-action (button)
  "Open the file at the location stored in BUTTON properties."
  (let ((file (button-get button 'my-file))
        (line (button-get button 'my-line))
        (col  (button-get button 'my-col)))
    (when (file-exists-p file)
      (pop-to-buffer (find-file-noselect file))
      (when line
        (goto-char (point-min))
        (forward-line (1- line))
        (when col (forward-char (1- col)))))))

(defconst my--vterm-file-extensions
  (concat "el\\|py\\|js\\|ts\\|tsx\\|jsx\\|json\\|rs\\|go\\|rb"
          "\\|c\\|h\\|cpp\\|hpp\\|cc\\|hh\\|java\\|kt\\|scala"
          "\\|ml\\|mli\\|re\\|rei\\|hs\\|lhs\\|clj\\|cljs\\|cljc"
          "\\|ex\\|exs\\|erl\\|hrl\\|sh\\|bash\\|zsh\\|fish"
          "\\|pl\\|pm\\|lua\\|vim\\|rkt\\|scm\\|lisp\\|cl\\|asd\\|fnl"
          "\\|zig\\|nim\\|v\\|sv\\|r\\|R\\|jl\\|dart\\|swift"
          "\\|m\\|mm\\|cs\\|fs\\|fsx\\|php"
          "\\|css\\|scss\\|sass\\|less\\|html\\|htm\\|xml\\|svg"
          "\\|json\\|yaml\\|yml\\|toml\\|ini\\|cfg\\|conf\\|lock"
          "\\|md\\|org\\|rst\\|txt\\|tex\\|bib\\|csv"
          "\\|sql\\|graphql\\|proto"
          "\\|cmake\\|mk\\|dockerfile\\|tf\\|hcl\\|nix\\|dhall")
  "Known file extensions for source, config, and text files.")

(defconst my--vterm-file-link-pattern
  (concat
   "\\(?:^\\|[[:space:]\"'(`]\\)"
   "\\([-a-zA-Z0-9_.~/]+\\.\\(?:" my--vterm-file-extensions "\\)\\>\\)"
   "\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?")
  "Regexp matching file paths with optional :line[:col].
Group 1 = file path, group 2 = line (may be nil), group 3 = column (may be nil).")

(defun my--vterm-resolve-file (raw-file project-files-cache)
  "Resolve RAW-FILE to an absolute path.
Try expand-file-name first.  For bare filenames (no directory
separator), fall back to searching PROJECT-FILES-CACHE for a
unique match by basename."
  (let ((expanded (expand-file-name raw-file)))
    (if (file-exists-p expanded)
        expanded
      (when (and (not (string-match-p "/" raw-file))
                 project-files-cache)
        (let ((matches (seq-filter
                        (lambda (f)
                          (string= (file-name-nondirectory f) raw-file))
                        project-files-cache)))
          (when (= (length matches) 1)
            (car matches)))))))

(defun my--llm-vterm-buttonize-files ()
  "Scan buffer for file path patterns and add buttons.
Only creates buttons for files that exist on disk."
  (remove-overlays (point-min) (point-max) 'category 'my-vterm-file-link)
  (let ((proj-files (ignore-errors
                      (require 'project)
                      (when-let ((proj (project-current nil)))
                        (project-files proj)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my--vterm-file-link-pattern nil t)
        (let* ((raw-file (match-string-no-properties 1))
               (file (my--vterm-resolve-file raw-file proj-files))
               (line (when (match-string 2)
                       (string-to-number (match-string-no-properties 2))))
               (col  (when (match-string 3)
                       (string-to-number (match-string-no-properties 3)))))
          (when file
            (make-button (match-beginning 1) (match-end 0)
                         'type 'my-vterm-file-link
                         'my-file file
                         'my-line line
                         'my-col col)))))))


;;;; ---------------------------------------------------------------
;;;; Display table
;;;; ---------------------------------------------------------------

(defun my--llm-vterm-setup-display-table ()
  "Replace U+23FA (⏺) with ✽ to prevent line-height fluctuation."
  (let ((dt (or buffer-display-table
               (make-display-table))))
    (aset dt #x23fa [?✽])
    (setq-local buffer-display-table dt)))


;;;; ---------------------------------------------------------------
;;;; Resize optimization
;;;; ---------------------------------------------------------------

(defvar-local my--vterm-cached-width nil
  "Cached terminal width for this buffer.
Used to suppress resize signals when only the height changed.")

(defun my--vterm-resize-advice (orig-fn &rest args)
  "Around advice for `vterm--window-adjust-process-window-size'.
Only forward the resize when the width has actually changed."
  (let* ((result (apply orig-fn args))
         (buf (current-buffer))
         (new-width (when result (car result))))
    (when (and new-width (buffer-local-value 'my--vterm-cached-width buf))
      (if (= new-width (buffer-local-value 'my--vterm-cached-width buf))
          nil  ; suppress — height-only change
        (with-current-buffer buf
          (setq my--vterm-cached-width new-width))
        result))
    (when (and new-width
               (null (buffer-local-value 'my--vterm-cached-width buf)))
      (with-current-buffer buf
        (setq my--vterm-cached-width new-width))
      result)))


;;;; ---------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------

(defun my--vterm-send-escape ()
  "Send ESC to the terminal process (maps C-g to Claude's cancel)."
  (interactive)
  (vterm-send-key "<escape>"))

(defun my--llm-vterm-setup-keymap ()
  "Install a sparse keymap with LLM-specific bindings.
Preserves the existing vterm keymap as parent."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "C-g") #'my--vterm-send-escape)
    (define-key map (kbd "<f5>") #'compile)
    (use-local-map map)))


;;;; ---------------------------------------------------------------
;;;; Bell / Notification
;;;; ---------------------------------------------------------------

(defconst my--vterm-notify-delay 3.0
  "Seconds of silence after last BEL before sending a notification.
Claude Code emits BEL frequently during streaming; we only want to
notify when output has actually stopped.")

(defun my--llm-vterm-notify (buf)
  "Schedule a desktop notification for BUF after a quiet period.
Resets the timer on each call so notifications only fire once
output has been idle for `my--vterm-notify-delay' seconds."
  (with-current-buffer buf
    (when my--vterm-notify-timer
      (cancel-timer my--vterm-notify-timer))
    (setq my--vterm-notify-timer
          (run-at-time my--vterm-notify-delay nil
                       #'my--llm-vterm-do-notify buf))))

(defun my--llm-vterm-do-notify (buf)
  "Send a desktop notification if BUF is not visible."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq my--vterm-notify-timer nil))
    (unless (get-buffer-window buf 'visible)
      (start-process "notify" nil "notify-send"
                     "Claude Code"
                     (format "%s: Awaiting input"
                             (buffer-name buf))))))


(provide 'my-claude-vterm)
;;; my-claude-vterm.el ends here
