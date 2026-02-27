;;; my-light-256-theme.el --- Light theme for 256-color terminals -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A light Emacs theme using xterm-256 color names for terminals without
;; true color support.  Structurally identical to my-light-theme but
;; pulls from the light-256 palette.

;;; Code:

(require 'my-theme-palette)

(deftheme my-light-256 "Light theme for 256-color terminals.")

(defgroup my-light-256-theme nil
  "Faces for my-light-256 theme."
  :group 'faces)

(defcustom my-light-256-scale-org-headlines t
  "Whether to scale Org headlines."
  :type 'boolean
  :group 'my-light-256-theme)

(let* ((palette (my-theme-palette 'light-256))
       (p (lambda (key) (alist-get key palette))))

  (custom-theme-set-faces
   'my-light-256

   ;; ---- Core ----
   `(default ((t (:foreground ,(funcall p 'fg) :background ,(funcall p 'bg)))))
   `(cursor ((t (:background ,(funcall p 'cursor)))))
   `(region ((t (:background ,(funcall p 'bg-region)))))
   `(highlight ((t (:background ,(funcall p 'bg-highlight)))))
   `(hl-line ((t (:background ,(funcall p 'bg-hl-line)))))
   `(isearch ((t (:background ,(funcall p 'bg-isearch) :foreground ,(funcall p 'fg)))))
   `(lazy-highlight ((t (:background ,(funcall p 'bg-lazy-isearch)))))
   `(isearch-fail ((t (:background ,(funcall p 'bg-paren-mismatch) :foreground ,(funcall p 'ml-active-fg)))))
   `(fringe ((t (:background ,(funcall p 'fringe)))))
   `(vertical-border ((t (:foreground ,(funcall p 'vertical-border)))))
   `(border ((t (:foreground ,(funcall p 'vertical-border)))))
   `(shadow ((t (:foreground ,(funcall p 'shadow)))))
   `(secondary-selection ((t (:background ,(funcall p 'bg-highlight)))))
   `(trailing-whitespace ((t (:background ,(funcall p 'trailing-ws)))))
   `(whitespace-trailing ((t (:background ,(funcall p 'trailing-ws)))))
   `(whitespace-space ((t (:foreground ,(funcall p 'whitespace)))))
   `(whitespace-tab ((t (:foreground ,(funcall p 'whitespace)))))
   `(whitespace-line ((t (:background ,(funcall p 'bg-highlight)))))
   `(whitespace-newline ((t (:foreground ,(funcall p 'whitespace)))))
   `(escape-glyph ((t (:foreground ,(funcall p 'escape-glyph)))))

   ;; ---- Mode Line ----
   `(mode-line ((t (:background ,(funcall p 'ml-active-bg)
                    :foreground ,(funcall p 'ml-active-fg)
                    :box (:line-width 1 :color ,(funcall p 'ml-active-bg))))))
   `(mode-line-inactive ((t (:background ,(funcall p 'ml-inactive-bg)
                             :foreground ,(funcall p 'ml-inactive-fg)
                             :box (:line-width 1 :color ,(funcall p 'ml-inactive-bg))))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-highlight ((t (:box (:line-width 1 :color ,(funcall p 'ml-active-fg))))))

   ;; ---- Prompts / Errors ----
   `(minibuffer-prompt ((t (:foreground ,(funcall p 'minibuffer-prompt) :weight bold))))
   `(error ((t (:foreground ,(funcall p 'error) :weight bold))))
   `(warning ((t (:foreground ,(funcall p 'warning) :weight bold))))
   `(success ((t (:foreground ,(funcall p 'success) :weight bold))))

   ;; ---- Font Lock ----
   `(font-lock-builtin-face ((t (:foreground ,(funcall p 'builtin)))))
   `(font-lock-comment-face ((t (:foreground ,(funcall p 'comment) :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,(funcall p 'comment) :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,(funcall p 'constant)))))
   `(font-lock-doc-face ((t (:foreground ,(funcall p 'doc) :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,(funcall p 'function-name) :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,(funcall p 'negation)))))
   `(font-lock-preprocessor-face ((t (:foreground ,(funcall p 'preprocessor)))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,(funcall p 'constant) :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,(funcall p 'constant) :weight bold))))
   `(font-lock-string-face ((t (:foreground ,(funcall p 'string)))))
   `(font-lock-type-face ((t (:foreground ,(funcall p 'type)))))
   `(font-lock-variable-name-face ((t (:foreground ,(funcall p 'variable)))))
   `(font-lock-warning-face ((t (:foreground ,(funcall p 'warning) :weight bold))))

   ;; ---- Links ----
   `(link ((t (:foreground ,(funcall p 'link) :underline t))))
   `(link-visited ((t (:foreground ,(funcall p 'link-visited) :underline t))))
   `(button ((t (:foreground ,(funcall p 'link) :underline t))))
   `(header-line ((t (:background ,(funcall p 'header-line-bg)
                      :foreground ,(funcall p 'header-line-fg)))))

   ;; ---- Diff / VCS ----
   `(diff-added ((t (:background ,(funcall p 'diff-added-bg) :foreground ,(funcall p 'diff-added-fg)))))
   `(diff-removed ((t (:background ,(funcall p 'diff-removed-bg) :foreground ,(funcall p 'diff-removed-fg)))))
   `(diff-changed ((t (:background ,(funcall p 'diff-changed-bg) :foreground ,(funcall p 'diff-changed-fg)))))
   `(diff-header ((t (:background ,(funcall p 'diff-hunk-bg) :foreground ,(funcall p 'diff-hunk-fg)))))
   `(diff-file-header ((t (:background ,(funcall p 'diff-hunk-bg) :foreground ,(funcall p 'diff-hunk-fg) :weight bold))))
   `(diff-hunk-header ((t (:background ,(funcall p 'diff-hunk-bg) :foreground ,(funcall p 'diff-hunk-fg)))))
   `(diff-refine-added ((t (:background ,(funcall p 'diff-fine-added)))))
   `(diff-refine-removed ((t (:background ,(funcall p 'diff-fine-removed)))))
   `(diff-indicator-added ((t (:foreground ,(funcall p 'diff-indicator-added)))))
   `(diff-indicator-removed ((t (:foreground ,(funcall p 'diff-indicator-removed)))))
   `(diff-hl-insert ((t (:foreground ,(funcall p 'diff-indicator-added) :background ,(funcall p 'diff-added-bg)))))
   `(diff-hl-delete ((t (:foreground ,(funcall p 'diff-indicator-removed) :background ,(funcall p 'diff-removed-bg)))))
   `(diff-hl-change ((t (:foreground ,(funcall p 'diff-changed-fg) :background ,(funcall p 'diff-changed-bg)))))
   `(smerge-upper ((t (:background ,(funcall p 'diff-removed-bg)))))
   `(smerge-lower ((t (:background ,(funcall p 'diff-added-bg)))))
   `(smerge-base ((t (:background ,(funcall p 'diff-changed-bg)))))
   `(smerge-markers ((t (:background ,(funcall p 'diff-hunk-bg)))))
   `(smerge-refined-added ((t (:background ,(funcall p 'diff-fine-added)))))
   `(smerge-refined-removed ((t (:background ,(funcall p 'diff-fine-removed)))))
   `(smerge-refined-changed ((t (:background ,(funcall p 'diff-changed-bg)))))

   ;; ---- Ediff ----
   `(ediff-current-diff-A ((t (:background ,(funcall p 'ediff-current-a)))))
   `(ediff-current-diff-B ((t (:background ,(funcall p 'ediff-current-b)))))
   `(ediff-current-diff-C ((t (:background ,(funcall p 'ediff-current-c)))))
   `(ediff-fine-diff-A ((t (:background ,(funcall p 'ediff-fine-a)))))
   `(ediff-fine-diff-B ((t (:background ,(funcall p 'ediff-fine-b)))))
   `(ediff-fine-diff-C ((t (:background ,(funcall p 'ediff-fine-c)))))
   `(ediff-even-diff-A ((t (:background ,(funcall p 'ediff-even)))))
   `(ediff-odd-diff-A ((t (:background ,(funcall p 'ediff-odd)))))

   ;; ---- Flycheck / Flyspell ----
   `(flycheck-error ((t (:underline (:style wave :color ,(funcall p 'error))))))
   `(flycheck-warning ((t (:underline (:style wave :color ,(funcall p 'warning))))))
   `(flycheck-info ((t (:underline (:style wave :color ,(funcall p 'info))))))
   `(flycheck-fringe-error ((t (:foreground ,(funcall p 'error)))))
   `(flycheck-fringe-warning ((t (:foreground ,(funcall p 'warning)))))
   `(flycheck-fringe-info ((t (:foreground ,(funcall p 'info)))))
   `(flycheck-error-list-error ((t (:foreground ,(funcall p 'error)))))
   `(flycheck-error-list-warning ((t (:foreground ,(funcall p 'warning)))))
   `(flycheck-error-list-info ((t (:foreground ,(funcall p 'info)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,(funcall p 'error))))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,(funcall p 'warning))))))

   ;; ---- Org Mode ----
   `(org-level-1 ((t (:foreground ,(funcall p 'ol1-fg) :background ,(funcall p 'ol1-bg) :weight bold
                       ,@(when my-light-256-scale-org-headlines '(:height 1.3))))))
   `(org-level-2 ((t (:foreground ,(funcall p 'ol2-fg) :background ,(funcall p 'ol2-bg) :weight bold
                       ,@(when my-light-256-scale-org-headlines '(:height 1.2))))))
   `(org-level-3 ((t (:foreground ,(funcall p 'ol3-fg) :background ,(funcall p 'ol3-bg) :weight bold
                       ,@(when my-light-256-scale-org-headlines '(:height 1.1))))))
   `(org-level-4 ((t (:foreground ,(funcall p 'ol4-fg) :weight bold))))
   `(org-level-5 ((t (:foreground ,(funcall p 'ol5-fg) :weight bold))))
   `(org-level-6 ((t (:foreground ,(funcall p 'ol6-fg) :weight bold))))
   `(org-level-7 ((t (:foreground ,(funcall p 'ol7-fg) :weight bold))))
   `(org-level-8 ((t (:foreground ,(funcall p 'ol8-fg) :weight bold))))
   `(org-todo ((t (:foreground ,(funcall p 'org-todo-fg) :weight bold))))
   `(org-done ((t (:foreground ,(funcall p 'org-done-fg) :weight bold))))
   `(org-headline-done ((t (:foreground ,(funcall p 'fg-dim)))))
   `(org-block ((t (:background ,(funcall p 'org-block-bg)))))
   `(org-block-begin-line ((t (:foreground ,(funcall p 'comment) :background ,(funcall p 'org-code-bg) :slant italic))))
   `(org-block-end-line ((t (:foreground ,(funcall p 'comment) :background ,(funcall p 'org-code-bg) :slant italic))))
   `(org-code ((t (:foreground ,(funcall p 'org-code-fg) :background ,(funcall p 'org-code-bg)))))
   `(org-verbatim ((t (:foreground ,(funcall p 'constant) :background ,(funcall p 'org-code-bg)))))
   `(org-table ((t (:foreground ,(funcall p 'fg) :background ,(funcall p 'org-table-bg)))))
   `(org-date ((t (:foreground ,(funcall p 'org-date-fg) :underline t))))
   `(org-tag ((t (:foreground ,(funcall p 'org-tag-fg)))))
   `(org-drawer ((t (:foreground ,(funcall p 'org-drawer-fg)))))
   `(org-special-keyword ((t (:foreground ,(funcall p 'comment)))))
   `(org-meta-line ((t (:foreground ,(funcall p 'comment)))))
   `(org-document-title ((t (:foreground ,(funcall p 'fg) :weight bold :height 1.4))))
   `(org-document-info ((t (:foreground ,(funcall p 'fg-alt)))))
   `(org-document-info-keyword ((t (:foreground ,(funcall p 'comment)))))
   `(org-link ((t (:foreground ,(funcall p 'link) :underline t))))
   `(org-footnote ((t (:foreground ,(funcall p 'link) :underline t))))
   `(org-ellipsis ((t (:foreground ,(funcall p 'fg-dim)))))
   `(org-agenda-structure ((t (:foreground ,(funcall p 'ml-active-bg) :weight bold))))
   `(org-agenda-date ((t (:foreground ,(funcall p 'ml-active-bg)))))
   `(org-agenda-date-today ((t (:foreground ,(funcall p 'ml-active-bg) :weight bold :slant italic))))
   `(org-agenda-date-weekend ((t (:foreground ,(funcall p 'fg-dim)))))
   `(org-agenda-done ((t (:foreground ,(funcall p 'org-done-fg)))))
   `(org-scheduled ((t (:foreground ,(funcall p 'fg)))))
   `(org-scheduled-today ((t (:foreground ,(funcall p 'fg) :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,(funcall p 'warning)))))
   `(org-upcoming-deadline ((t (:foreground ,(funcall p 'error)))))
   `(org-warning ((t (:foreground ,(funcall p 'warning) :weight bold))))
   `(org-checkbox ((t (:weight bold))))
   `(org-formula ((t (:foreground ,(funcall p 'constant)))))
   `(org-latex-and-related ((t (:foreground ,(funcall p 'type)))))

   ;; ---- Outline ----
   `(outline-1 ((t (:foreground ,(funcall p 'outline-1) :weight bold))))
   `(outline-2 ((t (:foreground ,(funcall p 'outline-2) :weight bold))))
   `(outline-3 ((t (:foreground ,(funcall p 'outline-3) :weight bold))))
   `(outline-4 ((t (:foreground ,(funcall p 'outline-4) :weight bold))))
   `(outline-5 ((t (:foreground ,(funcall p 'outline-5) :weight bold))))
   `(outline-6 ((t (:foreground ,(funcall p 'outline-6) :weight bold))))
   `(outline-7 ((t (:foreground ,(funcall p 'outline-7) :weight bold))))
   `(outline-8 ((t (:foreground ,(funcall p 'outline-8) :weight bold))))

   ;; ---- Markdown ----
   `(markdown-header-face-1 ((t (:foreground ,(funcall p 'outline-1) :weight bold :height 1.3))))
   `(markdown-header-face-2 ((t (:foreground ,(funcall p 'outline-2) :weight bold :height 1.2))))
   `(markdown-header-face-3 ((t (:foreground ,(funcall p 'outline-3) :weight bold :height 1.1))))
   `(markdown-header-face-4 ((t (:foreground ,(funcall p 'outline-4) :weight bold))))
   `(markdown-header-face-5 ((t (:foreground ,(funcall p 'outline-5) :weight bold))))
   `(markdown-header-face-6 ((t (:foreground ,(funcall p 'outline-6) :weight bold))))
   `(markdown-inline-code-face ((t (:foreground ,(funcall p 'org-code-fg) :background ,(funcall p 'org-code-bg)))))
   `(markdown-code-face ((t (:background ,(funcall p 'org-code-bg)))))
   `(markdown-link-face ((t (:foreground ,(funcall p 'link)))))
   `(markdown-url-face ((t (:foreground ,(funcall p 'link-visited)))))
   `(markdown-bold-face ((t (:weight bold))))
   `(markdown-italic-face ((t (:slant italic))))
   `(markdown-blockquote-face ((t (:foreground ,(funcall p 'fg-alt) :slant italic))))
   `(markdown-list-face ((t (:foreground ,(funcall p 'keyword)))))

   ;; ---- Company ----
   `(company-tooltip ((t (:background ,(funcall p 'company-bg) :foreground ,(funcall p 'company-fg)))))
   `(company-tooltip-selection ((t (:background ,(funcall p 'company-sel-bg) :foreground ,(funcall p 'company-sel-fg)))))
   `(company-tooltip-common ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,(funcall p 'fg-alt)))))
   `(company-scrollbar-bg ((t (:background ,(funcall p 'company-border)))))
   `(company-scrollbar-fg ((t (:background ,(funcall p 'fg-dim)))))
   `(company-preview ((t (:foreground ,(funcall p 'fg-dim)))))

   ;; ---- Magit ----
   `(magit-section-heading ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(magit-section-highlight ((t (:background ,(funcall p 'magit-section-bg)))))
   `(magit-diff-added ((t (:background ,(funcall p 'diff-added-bg) :foreground ,(funcall p 'diff-added-fg)))))
   `(magit-diff-added-highlight ((t (:background ,(funcall p 'diff-fine-added) :foreground ,(funcall p 'diff-added-fg)))))
   `(magit-diff-removed ((t (:background ,(funcall p 'diff-removed-bg) :foreground ,(funcall p 'diff-removed-fg)))))
   `(magit-diff-removed-highlight ((t (:background ,(funcall p 'diff-fine-removed) :foreground ,(funcall p 'diff-removed-fg)))))
   `(magit-diff-context ((t (:foreground ,(funcall p 'fg-alt)))))
   `(magit-diff-context-highlight ((t (:background ,(funcall p 'bg-alt) :foreground ,(funcall p 'fg-alt)))))
   `(magit-diff-hunk-heading ((t (:background ,(funcall p 'diff-hunk-bg) :foreground ,(funcall p 'diff-hunk-fg)))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,(funcall p 'diff-hunk-bg) :foreground ,(funcall p 'diff-hunk-fg) :weight bold))))
   `(magit-branch-local ((t (:foreground ,(funcall p 'magit-branch-local)))))
   `(magit-branch-remote ((t (:foreground ,(funcall p 'magit-branch-remote)))))
   `(magit-tag ((t (:foreground ,(funcall p 'magit-tag)))))
   `(magit-blame-heading ((t (:background ,(funcall p 'bg-alt) :foreground ,(funcall p 'fg-alt)))))

   ;; ---- Dired ----
   `(dired-directory ((t (:foreground ,(funcall p 'dired-directory) :weight bold))))
   `(dired-header ((t (:foreground ,(funcall p 'dired-header) :weight bold))))
   `(dired-symlink ((t (:foreground ,(funcall p 'dired-symlink)))))
   `(dired-mark ((t (:foreground ,(funcall p 'dired-mark) :weight bold))))
   `(dired-marked ((t (:foreground ,(funcall p 'dired-mark) :weight bold))))
   `(dired-flagged ((t (:foreground ,(funcall p 'error) :weight bold))))

   ;; ---- Compilation / Grep ----
   `(compilation-error ((t (:foreground ,(funcall p 'comp-error) :weight bold))))
   `(compilation-warning ((t (:foreground ,(funcall p 'comp-warning)))))
   `(compilation-info ((t (:foreground ,(funcall p 'comp-info)))))
   `(compilation-line-number ((t (:foreground ,(funcall p 'comp-line-number)))))
   `(compilation-column-number ((t (:foreground ,(funcall p 'comp-line-number)))))
   `(compilation-mode-line-exit ((t (:foreground ,(funcall p 'success) :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,(funcall p 'error) :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,(funcall p 'info)))))
   `(grep-match-face ((t (:background ,(funcall p 'grep-match)))))
   `(grep-hit-face ((t (:foreground ,(funcall p 'grep-file)))))
   `(grep-context-face ((t (:foreground ,(funcall p 'fg-alt)))))

   ;; ---- Paren / Rainbow ----
   `(show-paren-match ((t (:background ,(funcall p 'bg-paren-match) :weight bold))))
   `(show-paren-mismatch ((t (:background ,(funcall p 'bg-paren-mismatch) :foreground ,(funcall p 'ml-active-fg) :weight bold))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,(funcall p 'rainbow-1)))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,(funcall p 'rainbow-2)))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,(funcall p 'rainbow-3)))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,(funcall p 'rainbow-4)))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,(funcall p 'rainbow-5)))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,(funcall p 'rainbow-6)))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,(funcall p 'rainbow-7)))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,(funcall p 'rainbow-8)))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,(funcall p 'rainbow-9)))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,(funcall p 'error) :weight bold))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,(funcall p 'error) :weight bold :strike-through t))))

   ;; ---- ANSI Colors ----
   `(ansi-color-black ((t (:foreground ,(funcall p 'ansi-black) :background ,(funcall p 'ansi-black)))))
   `(ansi-color-red ((t (:foreground ,(funcall p 'ansi-red) :background ,(funcall p 'ansi-red)))))
   `(ansi-color-green ((t (:foreground ,(funcall p 'ansi-green) :background ,(funcall p 'ansi-green)))))
   `(ansi-color-yellow ((t (:foreground ,(funcall p 'ansi-yellow) :background ,(funcall p 'ansi-yellow)))))
   `(ansi-color-blue ((t (:foreground ,(funcall p 'ansi-blue) :background ,(funcall p 'ansi-blue)))))
   `(ansi-color-magenta ((t (:foreground ,(funcall p 'ansi-magenta) :background ,(funcall p 'ansi-magenta)))))
   `(ansi-color-cyan ((t (:foreground ,(funcall p 'ansi-cyan) :background ,(funcall p 'ansi-cyan)))))
   `(ansi-color-white ((t (:foreground ,(funcall p 'ansi-white) :background ,(funcall p 'ansi-white)))))
   `(ansi-color-bright-black ((t (:foreground ,(funcall p 'ansi-bright-black) :background ,(funcall p 'ansi-bright-black)))))
   `(ansi-color-bright-red ((t (:foreground ,(funcall p 'ansi-bright-red) :background ,(funcall p 'ansi-bright-red)))))
   `(ansi-color-bright-green ((t (:foreground ,(funcall p 'ansi-bright-green) :background ,(funcall p 'ansi-bright-green)))))
   `(ansi-color-bright-yellow ((t (:foreground ,(funcall p 'ansi-bright-yellow) :background ,(funcall p 'ansi-bright-yellow)))))
   `(ansi-color-bright-blue ((t (:foreground ,(funcall p 'ansi-bright-blue) :background ,(funcall p 'ansi-bright-blue)))))
   `(ansi-color-bright-magenta ((t (:foreground ,(funcall p 'ansi-bright-magenta) :background ,(funcall p 'ansi-bright-magenta)))))
   `(ansi-color-bright-cyan ((t (:foreground ,(funcall p 'ansi-bright-cyan) :background ,(funcall p 'ansi-bright-cyan)))))
   `(ansi-color-bright-white ((t (:foreground ,(funcall p 'ansi-bright-white) :background ,(funcall p 'ansi-bright-white)))))

   ;; ---- Term / Shell ----
   `(term ((t (:foreground ,(funcall p 'fg) :background ,(funcall p 'bg)))))
   `(term-color-black ((t (:foreground ,(funcall p 'ansi-black)))))
   `(term-color-red ((t (:foreground ,(funcall p 'ansi-red)))))
   `(term-color-green ((t (:foreground ,(funcall p 'ansi-green)))))
   `(term-color-yellow ((t (:foreground ,(funcall p 'ansi-yellow)))))
   `(term-color-blue ((t (:foreground ,(funcall p 'ansi-blue)))))
   `(term-color-magenta ((t (:foreground ,(funcall p 'ansi-magenta)))))
   `(term-color-cyan ((t (:foreground ,(funcall p 'ansi-cyan)))))
   `(term-color-white ((t (:foreground ,(funcall p 'ansi-white)))))
   `(comint-highlight-prompt ((t (:foreground ,(funcall p 'minibuffer-prompt) :weight bold))))
   `(comint-highlight-input ((t (:weight bold))))
   `(sh-heredoc ((t (:foreground ,(funcall p 'string)))))

   ;; ---- Line Numbers ----
   `(line-number ((t (:foreground ,(funcall p 'line-number) :background ,(funcall p 'bg-alt)))))
   `(line-number-current-line ((t (:foreground ,(funcall p 'line-number-current) :background ,(funcall p 'bg-hl-line) :weight bold))))

   ;; ---- Info / Widget / Misc ----
   `(info-title-1 ((t (:foreground ,(funcall p 'fg) :weight bold :height 1.4))))
   `(info-title-2 ((t (:foreground ,(funcall p 'fg) :weight bold :height 1.3))))
   `(info-title-3 ((t (:foreground ,(funcall p 'fg) :weight bold :height 1.2))))
   `(info-title-4 ((t (:foreground ,(funcall p 'fg) :weight bold :height 1.1))))
   `(info-header-node ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(info-header-xref ((t (:foreground ,(funcall p 'link) :underline t))))
   `(info-xref ((t (:foreground ,(funcall p 'link) :underline t))))
   `(info-xref-visited ((t (:foreground ,(funcall p 'link-visited) :underline t))))
   `(tooltip ((t (:background ,(funcall p 'tooltip-bg) :foreground ,(funcall p 'tooltip-fg)))))
   `(widget-field ((t (:background ,(funcall p 'bg-alt) :box (:line-width 1 :color ,(funcall p 'vertical-border))))))
   `(widget-button ((t (:foreground ,(funcall p 'link) :underline t :weight bold))))
   `(custom-variable-tag ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(custom-group-tag ((t (:foreground ,(funcall p 'keyword) :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,(funcall p 'success)))))
   `(custom-changed ((t (:foreground ,(funcall p 'warning)))))
   `(custom-set ((t (:foreground ,(funcall p 'success)))))
   `(custom-button ((t (:background ,(funcall p 'bg-alt) :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse ((t (:background ,(funcall p 'bg-highlight) :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((t (:background ,(funcall p 'bg-highlight) :box (:line-width 2 :style pressed-button)))))
   `(yas-field-highlight-face ((t (:background ,(funcall p 'yas-field)))))

   ;; ---- Tuareg (OCaml) ----
   `(tuareg-font-lock-governing-face ((t (:foreground ,(funcall p 'tuareg-governing) :weight bold))))
   `(tuareg-font-lock-multistage-face ((t (:foreground ,(funcall p 'tuareg-multistage) :weight bold))))
   `(tuareg-font-lock-line-number-face ((t (:foreground ,(funcall p 'tuareg-line-number)))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,(funcall p 'tuareg-operator)))))
   `(tuareg-font-lock-module-face ((t (:foreground ,(funcall p 'tuareg-module)))))
   `(tuareg-font-lock-constructor-face ((t (:inherit nil :foreground ,(funcall p 'tuareg-constructor)))))
   `(tuareg-font-lock-label-face ((t (:foreground ,(funcall p 'tuareg-label)))))
   `(tuareg-font-double-semicolon-face ((t (:foreground ,(funcall p 'tuareg-double-semi) :weight bold))))
   `(tuareg-font-lock-error-face ((t (:foreground ,(funcall p 'tuareg-error) :weight bold))))
   `(tuareg-font-lock-interactive-output-face ((t (:foreground ,(funcall p 'tuareg-interactive-output)))))
   `(tuareg-font-lock-interactive-error-face ((t (:foreground ,(funcall p 'tuareg-interactive-error)))))
   `(tuareg-font-lock-interactive-directive-face ((t (:foreground ,(funcall p 'tuareg-interactive-directive)))))
   `(tuareg-font-lock-attribute-face ((t (:foreground ,(funcall p 'tuareg-attribute)))))
   `(tuareg-font-lock-infix-extension-node-face ((t (:foreground ,(funcall p 'tuareg-extension-node)))))
   `(tuareg-font-lock-extension-node-face ((t (:foreground ,(funcall p 'tuareg-extension-node)))))
   `(tuareg-font-lock-doc-markup-face ((t (:foreground ,(funcall p 'tuareg-doc-markup)))))
   `(tuareg-font-lock-doc-verbatim-face ((t (:foreground ,(funcall p 'tuareg-doc-verbatim)))))
   `(tuareg-opam-error-face ((t (:foreground ,(funcall p 'tuareg-opam-error) :weight bold))))
   `(tuareg-opam-pkg-variable-name-face ((t (:foreground ,(funcall p 'tuareg-opam-pkg-variable)))))

   ;; ---- Merlin ----
   `(merlin-type-face ((t (:background ,(funcall p 'merlin-type)))))
   `(merlin-compilation-warning-face ((t (:background ,(funcall p 'merlin-warn)))))
   `(merlin-compilation-error-face ((t (:background ,(funcall p 'merlin-error)))))

   ;; ---- UTop ----
   `(utop-prompt ((t (:foreground ,(funcall p 'utop-prompt) :weight bold))))
   `(utop-stdout ((t (:foreground ,(funcall p 'utop-stdout)))))
   `(utop-stderr ((t (:foreground ,(funcall p 'utop-stderr)))))
   `(utop-frozen ((t (:foreground ,(funcall p 'utop-frozen)))))
   `(utop-error ((t (:foreground ,(funcall p 'utop-error) :weight bold))))

   ;; ---- Gnus / Message ----
   `(gnus-group-mail-1 ((t (:foreground ,(funcall p 'gnus-group-1) :weight bold))))
   `(gnus-group-mail-1-empty ((t (:foreground ,(funcall p 'gnus-group-1)))))
   `(gnus-group-mail-2 ((t (:foreground ,(funcall p 'gnus-group-2) :weight bold))))
   `(gnus-group-mail-2-empty ((t (:foreground ,(funcall p 'gnus-group-2)))))
   `(gnus-group-mail-3 ((t (:foreground ,(funcall p 'gnus-group-3) :weight bold))))
   `(gnus-group-mail-3-empty ((t (:foreground ,(funcall p 'gnus-group-3)))))
   `(gnus-group-news-1 ((t (:foreground ,(funcall p 'gnus-group-1) :weight bold))))
   `(gnus-group-news-1-empty ((t (:foreground ,(funcall p 'gnus-group-1)))))
   `(gnus-group-news-2 ((t (:foreground ,(funcall p 'gnus-group-2) :weight bold))))
   `(gnus-group-news-2-empty ((t (:foreground ,(funcall p 'gnus-group-2)))))
   `(gnus-group-news-3 ((t (:foreground ,(funcall p 'gnus-group-3) :weight bold))))
   `(gnus-group-news-3-empty ((t (:foreground ,(funcall p 'gnus-group-3)))))
   `(gnus-header-from ((t (:foreground ,(funcall p 'gnus-header-from)))))
   `(gnus-header-subject ((t (:foreground ,(funcall p 'gnus-header-subject) :weight bold))))
   `(gnus-header-name ((t (:foreground ,(funcall p 'gnus-header-name)))))
   `(gnus-header-content ((t (:foreground ,(funcall p 'gnus-header-content)))))
   `(gnus-summary-selected ((t (:foreground ,(funcall p 'gnus-summary-selected) :weight bold))))
   `(gnus-summary-normal-read ((t (:foreground ,(funcall p 'fg-dim)))))
   `(gnus-summary-normal-unread ((t (:foreground ,(funcall p 'gnus-summary-normal) :weight bold))))
   `(gnus-summary-normal-tstrstricked ((t (:foreground ,(funcall p 'fg-dim) :slant italic))))
   `(gnus-cite-1 ((t (:foreground ,(funcall p 'gnus-cite-1)))))
   `(gnus-cite-2 ((t (:foreground ,(funcall p 'gnus-cite-2)))))
   `(gnus-cite-3 ((t (:foreground ,(funcall p 'gnus-cite-3)))))
   `(message-header-name ((t (:foreground ,(funcall p 'message-header-name) :weight bold))))
   `(message-header-to ((t (:foreground ,(funcall p 'message-header-to)))))
   `(message-header-cc ((t (:foreground ,(funcall p 'message-header-cc)))))
   `(message-header-other ((t (:foreground ,(funcall p 'message-header-other)))))
   `(message-header-subject ((t (:foreground ,(funcall p 'message-header-subject) :weight bold))))
   `(message-separator ((t (:foreground ,(funcall p 'message-separator)))))
   `(message-cited-text ((t (:foreground ,(funcall p 'gnus-cite-1) :slant italic))))

   ;; ---- LSP ----
   `(lsp-face-highlight-textual ((t (:background ,(funcall p 'lsp-face-highlight)))))
   `(lsp-face-highlight-read ((t (:background ,(funcall p 'lsp-face-highlight)))))
   `(lsp-face-highlight-write ((t (:background ,(funcall p 'lsp-face-highlight) :weight bold))))
   `(lsp-ui-doc-background ((t (:background ,(funcall p 'bg-alt)))))
   `(lsp-ui-peek-peek ((t (:background ,(funcall p 'bg-alt)))))
   `(lsp-ui-peek-list ((t (:background ,(funcall p 'bg-alt)))))
   `(lsp-ui-peek-filename ((t (:foreground ,(funcall p 'function-name)))))
   `(lsp-ui-peek-line-number ((t (:foreground ,(funcall p 'fg-dim)))))
   `(lsp-ui-peek-highlight ((t (:background ,(funcall p 'bg-region)))))
   `(lsp-ui-peek-header ((t (:background ,(funcall p 'ml-active-bg) :foreground ,(funcall p 'ml-active-fg)))))
   `(lsp-ui-peek-selection ((t (:background ,(funcall p 'bg-region)))))
   `(lsp-ui-sideline-symbol ((t (:foreground ,(funcall p 'fg-dim)))))

   ;; ---- Helm ----
   `(helm-selection ((t (:background ,(funcall p 'bg-region)))))
   `(helm-match ((t (:foreground ,(funcall p 'keyword) :weight bold))))
   `(helm-source-header ((t (:foreground ,(funcall p 'ml-active-fg) :background ,(funcall p 'ml-active-bg) :weight bold))))
   `(helm-candidate-number ((t (:foreground ,(funcall p 'ml-active-fg) :background ,(funcall p 'ml-active-bg)))))

   ;; ---- Avy ----
   `(avy-lead-face ((t (:foreground ,(funcall p 'ml-active-fg) :background ,(funcall p 'error)))))
   `(avy-lead-face-0 ((t (:foreground ,(funcall p 'ml-active-fg) :background ,(funcall p 'keyword)))))
   `(avy-lead-face-1 ((t (:foreground ,(funcall p 'ml-active-fg) :background ,(funcall p 'success)))))
   `(avy-lead-face-2 ((t (:foreground ,(funcall p 'ml-active-fg) :background ,(funcall p 'warning)))))

   ;; ---- Calendar ----
   `(calendar-today ((t (:foreground ,(funcall p 'calendar-today) :weight bold))))
   `(holiday-face ((t (:foreground ,(funcall p 'calendar-holiday)))))

   ;; ---- Speedbar ----
   `(speedbar-file-face ((t (:foreground ,(funcall p 'speedbar-file)))))
   `(speedbar-directory-face ((t (:foreground ,(funcall p 'speedbar-dir) :weight bold))))
   `(speedbar-tag-face ((t (:foreground ,(funcall p 'speedbar-tag)))))
   `(speedbar-selected-face ((t (:foreground ,(funcall p 'speedbar-selected) :underline t))))
   `(speedbar-highlight-face ((t (:background ,(funcall p 'speedbar-highlight)))))
   `(speedbar-button-face ((t (:foreground ,(funcall p 'success)))))

   ;; ---- Woman ----
   `(woman-bold ((t (:weight bold))))
   `(woman-italic ((t (:slant italic)))))

  ;; ---- Variables ----
  (custom-theme-set-variables
   'my-light-256
   '(frame-background-mode 'light)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'my-light-256)
;;; my-light-256-theme.el ends here
