;;; my-theme-palette.el --- Shared color palettes for my-light and my-dark themes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides `my-theme-palette', which returns an alist of semantic color
;; names for a given variant (light or dark).  Both my-light-theme.el
;; and my-dark-theme.el require this module so that color data is
;; defined once and shared.

;;; Code:

(defun my-theme-palette--light ()
  "Return the light color palette alist."
  '(;; --- Base ---
    (bg              . "#FFFFFF")
    (bg-alt          . "#F7F7F7")
    (bg-highlight    . "#FFFFCC")
    (bg-region       . "#B4D5FE")
    (bg-paren-match  . "#99CCFF")
    (bg-paren-mismatch . "#FF6666")
    (bg-isearch      . "#FFFF00")
    (bg-lazy-isearch . "#CCFFCC")
    (bg-hl-line      . "#EFEFEF")
    (fg              . "#333333")
    (fg-alt          . "#555555")
    (fg-dim          . "#999999")
    (fg-faint        . "#BBBBBB")
    (cursor          . "#000000")

    ;; --- Syntax ---
    (keyword         . "#0000FF")
    (string          . "#008000")
    (comment         . "#8D8D84")
    (doc             . "#036A07")
    (variable        . "#BA36A5")
    (function-name   . "#006699")
    (type            . "#6434A3")
    (constant        . "#D0372D")
    (builtin         . "#006FE0")
    (preprocessor    . "#808080")
    (negation        . "#FF0000")
    (warning         . "#FF6600")
    (error           . "#FF0000")
    (success         . "#008000")
    (info            . "#006FE0")

    ;; --- Links ---
    (link            . "#006DAF")
    (link-visited    . "#8B008B")

    ;; --- Mode line ---
    (ml-active-bg    . "#335EA8")
    (ml-active-fg    . "#FFFFFF")
    (ml-inactive-bg  . "#9B9C97")
    (ml-inactive-fg  . "#F0F0EF")

    ;; --- Diff / VCS ---
    (diff-added-bg   . "#DDFFDD")
    (diff-added-fg   . "#22863A")
    (diff-removed-bg . "#FFDDDD")
    (diff-removed-fg . "#B31D28")
    (diff-changed-bg . "#FFF3CE")
    (diff-changed-fg . "#B08800")
    (diff-hunk-bg    . "#E0E7FF")
    (diff-hunk-fg    . "#335EA8")
    (diff-fine-added . "#AAFFAA")
    (diff-fine-removed . "#FFAAAA")
    (diff-indicator-added . "#22863A")
    (diff-indicator-removed . "#B31D28")

    ;; --- Org mode ---
    (ol1-bg          . "#E0E0E0")
    (ol1-fg          . "#3C3C3C")
    (ol2-bg          . "#DDEEFF")
    (ol2-fg          . "#123555")
    (ol3-bg          . "#DDFEDD")
    (ol3-fg          . "#005522")
    (ol4-fg          . "#E3258D")
    (ol5-fg          . "#0077CC")
    (ol6-fg          . "#2EAE2C")
    (ol7-fg          . "#FD8008")
    (ol8-fg          . "#8B8989")
    (org-todo-fg     . "#FF6347")
    (org-done-fg     . "#008000")
    (org-block-bg    . "#FFFFEA")
    (org-code-bg     . "#F5F5F5")
    (org-code-fg     . "#7F0055")
    (org-table-bg    . "#E8E5D5")
    (org-date-fg     . "#6434A3")
    (org-tag-fg      . "#AAAAAA")
    (org-drawer-fg   . "#0000FF")

    ;; --- Outline / Markdown ---
    (outline-1       . "#3C3C3C")
    (outline-2       . "#123555")
    (outline-3       . "#005522")
    (outline-4       . "#E3258D")
    (outline-5       . "#0077CC")
    (outline-6       . "#2EAE2C")
    (outline-7       . "#FD8008")
    (outline-8       . "#8B8989")

    ;; --- Company ---
    (company-bg      . "#F0F0F0")
    (company-fg      . "#333333")
    (company-sel-bg  . "#B4D5FE")
    (company-sel-fg  . "#000000")
    (company-border  . "#CCCCCC")

    ;; --- Flycheck / Flyspell ---
    (flycheck-error-bg . "#FFDDDD")
    (flycheck-warn-bg  . "#FFFFCC")
    (flycheck-info-bg  . "#DDFFDD")

    ;; --- Rainbow delimiters ---
    (rainbow-1       . "#707183")
    (rainbow-2       . "#7388D6")
    (rainbow-3       . "#909183")
    (rainbow-4       . "#709870")
    (rainbow-5       . "#907373")
    (rainbow-6       . "#6276BA")
    (rainbow-7       . "#858580")
    (rainbow-8       . "#80A880")
    (rainbow-9       . "#887070")

    ;; --- ANSI colors ---
    (ansi-black      . "#000000")
    (ansi-red        . "#CC0000")
    (ansi-green      . "#00CC00")
    (ansi-yellow     . "#CCCC00")
    (ansi-blue       . "#0000CC")
    (ansi-magenta    . "#CC00CC")
    (ansi-cyan       . "#00CCCC")
    (ansi-white      . "#CCCCCC")
    (ansi-bright-black   . "#555555")
    (ansi-bright-red     . "#FF0000")
    (ansi-bright-green   . "#00FF00")
    (ansi-bright-yellow  . "#FFFF00")
    (ansi-bright-blue    . "#0000FF")
    (ansi-bright-magenta . "#FF00FF")
    (ansi-bright-cyan    . "#00FFFF")
    (ansi-bright-white   . "#FFFFFF")

    ;; --- OCaml / Tuareg ---
    (tuareg-governing   . "#00688B")
    (tuareg-multistage  . "#7F007F")
    (tuareg-operator    . "#8B6508")
    (tuareg-module      . "#6434A3")
    (tuareg-constructor . "#2F4F4F")
    (tuareg-label       . "#8B4513")
    (tuareg-double-semi . "#CD3700")
    (tuareg-error       . "#FF0000")
    (tuareg-interactive-output    . "#006400")
    (tuareg-interactive-error     . "#8B0000")
    (tuareg-interactive-directive . "#0000CD")
    (tuareg-attribute        . "#808080")
    (tuareg-extension-node   . "#6B8E23")
    (tuareg-doc-markup       . "#8B008B")
    (tuareg-doc-verbatim     . "#005F5F")
    (tuareg-line-number      . "#999999")
    (tuareg-opam-error       . "#FF0000")
    (tuareg-opam-pkg-variable . "#BA36A5")

    ;; --- Merlin ---
    (merlin-type       . "#E8F0FF")
    (merlin-warn       . "#FFE0B2")
    (merlin-error      . "#FFCCCC")

    ;; --- UTop ---
    (utop-prompt       . "#0000FF")
    (utop-stdout       . "#333333")
    (utop-stderr       . "#CC0000")
    (utop-frozen       . "#808080")
    (utop-error        . "#FF0000")

    ;; --- Misc ---
    (shadow            . "#999999")
    (line-number       . "#999999")
    (line-number-current . "#333333")
    (tooltip-bg        . "#FFFFCC")
    (tooltip-fg        . "#333333")
    (header-line-bg    . "#F0F0F0")
    (header-line-fg    . "#333333")
    (whitespace        . "#E8E8E8")
    (trailing-ws       . "#FFBFBF")
    (escape-glyph      . "#FF6600")
    (minibuffer-prompt  . "#335EA8")
    (fringe            . "#F7F7F7")
    (vertical-border   . "#CCCCCC")
    (yas-field         . "#E5F4FF")

    ;; --- Ediff ---
    (ediff-current-a   . "#FFDDDD")
    (ediff-current-b   . "#DDFFDD")
    (ediff-current-c   . "#FFFFAA")
    (ediff-fine-a      . "#FFAAAA")
    (ediff-fine-b      . "#AAFFAA")
    (ediff-fine-c      . "#FFFF55")
    (ediff-even        . "#F0F0F0")
    (ediff-odd         . "#E0E0E0")

    ;; --- Magit ---
    (magit-section-bg  . "#F0F0F0")
    (magit-section-fg  . "#333333")
    (magit-branch-local  . "#006FE0")
    (magit-branch-remote . "#008000")
    (magit-tag         . "#D0372D")

    ;; --- Dired ---
    (dired-directory   . "#0000FF")
    (dired-header      . "#006FE0")
    (dired-symlink     . "#008B8B")
    (dired-mark        . "#FF0000")

    ;; --- Compilation / Grep ---
    (comp-error        . "#FF0000")
    (comp-warning      . "#FF6600")
    (comp-info         . "#008000")
    (comp-line-number  . "#6434A3")
    (grep-match        . "#FFFF00")
    (grep-file         . "#6434A3")
    (grep-line         . "#D0372D")

    ;; --- LSP ---
    (lsp-face-highlight . "#E8F0FF")

    ;; --- Calendar ---
    (calendar-today    . "#0000FF")
    (calendar-holiday  . "#FF0000")

    ;; --- Speedbar ---
    (speedbar-file     . "#006699")
    (speedbar-dir      . "#0000FF")
    (speedbar-tag      . "#8B4513")
    (speedbar-selected . "#FF0000")
    (speedbar-highlight . "#EEEEEE")

    ;; --- Gnus / Message ---
    (gnus-group-1      . "#006FE0")
    (gnus-group-2      . "#008000")
    (gnus-group-3      . "#6434A3")
    (gnus-header-from  . "#006699")
    (gnus-header-subject . "#0000FF")
    (gnus-header-name  . "#8B4513")
    (gnus-header-content . "#006400")
    (gnus-summary-selected . "#0000FF")
    (gnus-summary-normal . "#333333")
    (gnus-cite-1       . "#006699")
    (gnus-cite-2       . "#008000")
    (gnus-cite-3       . "#6434A3")
    (message-header-name . "#8B4513")
    (message-header-to   . "#006699")
    (message-header-cc   . "#006400")
    (message-header-other . "#006FE0")
    (message-header-subject . "#0000FF")
    (message-separator  . "#999999")))

(defun my-theme-palette--dark ()
  "Return the dark color palette alist."
  '(;; --- Base ---
    (bg              . "#1a1a2e")
    (bg-alt          . "#16213e")
    (bg-highlight    . "#2a2a4a")
    (bg-region       . "#3d4f7c")
    (bg-paren-match  . "#3d5f8c")
    (bg-paren-mismatch . "#6e2020")
    (bg-isearch      . "#5c5c00")
    (bg-lazy-isearch . "#1a3a1a")
    (bg-hl-line      . "#1e1e3a")
    (fg              . "#d0cdd3")
    (fg-alt          . "#b0adb3")
    (fg-dim          . "#7a7880")
    (fg-faint        . "#555560")
    (cursor          . "#e0af68")

    ;; --- Syntax ---
    (keyword         . "#7aa2f7")
    (string          . "#9ece6a")
    (comment         . "#565f89")
    (doc             . "#6a9955")
    (variable        . "#bb9af7")
    (function-name   . "#e0af68")
    (type            . "#2ac3de")
    (constant        . "#ff9e64")
    (builtin         . "#7dcfff")
    (preprocessor    . "#737aa2")
    (negation        . "#f7768e")
    (warning         . "#e0af68")
    (error           . "#f7768e")
    (success         . "#9ece6a")
    (info            . "#7dcfff")

    ;; --- Links ---
    (link            . "#7aa2f7")
    (link-visited    . "#bb9af7")

    ;; --- Mode line ---
    (ml-active-bg    . "#3d4f7c")
    (ml-active-fg    . "#d0cdd3")
    (ml-inactive-bg  . "#2a2a4a")
    (ml-inactive-fg  . "#565f89")

    ;; --- Diff / VCS ---
    (diff-added-bg   . "#1a2e1a")
    (diff-added-fg   . "#9ece6a")
    (diff-removed-bg . "#2e1a1a")
    (diff-removed-fg . "#f7768e")
    (diff-changed-bg . "#2e2a1a")
    (diff-changed-fg . "#e0af68")
    (diff-hunk-bg    . "#1a1a3e")
    (diff-hunk-fg    . "#7aa2f7")
    (diff-fine-added . "#2a4a2a")
    (diff-fine-removed . "#4a2a2a")
    (diff-indicator-added . "#9ece6a")
    (diff-indicator-removed . "#f7768e")

    ;; --- Org mode ---
    (ol1-bg          . "#2a2a4a")
    (ol1-fg          . "#d0cdd3")
    (ol2-bg          . "#1a2a4e")
    (ol2-fg          . "#7aa2f7")
    (ol3-bg          . "#1a2e2a")
    (ol3-fg          . "#9ece6a")
    (ol4-fg          . "#f7768e")
    (ol5-fg          . "#7dcfff")
    (ol6-fg          . "#9ece6a")
    (ol7-fg          . "#ff9e64")
    (ol8-fg          . "#737aa2")
    (org-todo-fg     . "#f7768e")
    (org-done-fg     . "#9ece6a")
    (org-block-bg    . "#16213e")
    (org-code-bg     . "#1e1e3a")
    (org-code-fg     . "#ff9e64")
    (org-table-bg    . "#1e1e3a")
    (org-date-fg     . "#bb9af7")
    (org-tag-fg      . "#565f89")
    (org-drawer-fg   . "#7aa2f7")

    ;; --- Outline / Markdown ---
    (outline-1       . "#d0cdd3")
    (outline-2       . "#7aa2f7")
    (outline-3       . "#9ece6a")
    (outline-4       . "#f7768e")
    (outline-5       . "#7dcfff")
    (outline-6       . "#9ece6a")
    (outline-7       . "#ff9e64")
    (outline-8       . "#737aa2")

    ;; --- Company ---
    (company-bg      . "#16213e")
    (company-fg      . "#d0cdd3")
    (company-sel-bg  . "#3d4f7c")
    (company-sel-fg  . "#FFFFFF")
    (company-border  . "#3d4f7c")

    ;; --- Flycheck / Flyspell ---
    (flycheck-error-bg . "#2e1a1a")
    (flycheck-warn-bg  . "#2e2a1a")
    (flycheck-info-bg  . "#1a2e1a")

    ;; --- Rainbow delimiters ---
    (rainbow-1       . "#9ca0b0")
    (rainbow-2       . "#7aa2f7")
    (rainbow-3       . "#9ece6a")
    (rainbow-4       . "#e0af68")
    (rainbow-5       . "#f7768e")
    (rainbow-6       . "#bb9af7")
    (rainbow-7       . "#2ac3de")
    (rainbow-8       . "#ff9e64")
    (rainbow-9       . "#7dcfff")

    ;; --- ANSI colors ---
    (ansi-black      . "#1a1a2e")
    (ansi-red        . "#f7768e")
    (ansi-green      . "#9ece6a")
    (ansi-yellow     . "#e0af68")
    (ansi-blue       . "#7aa2f7")
    (ansi-magenta    . "#bb9af7")
    (ansi-cyan       . "#7dcfff")
    (ansi-white      . "#d0cdd3")
    (ansi-bright-black   . "#565f89")
    (ansi-bright-red     . "#ff99a8")
    (ansi-bright-green   . "#b9e87a")
    (ansi-bright-yellow  . "#f0cf88")
    (ansi-bright-blue    . "#9ab8ff")
    (ansi-bright-magenta . "#d0b0ff")
    (ansi-bright-cyan    . "#99dfff")
    (ansi-bright-white   . "#FFFFFF")

    ;; --- OCaml / Tuareg ---
    (tuareg-governing   . "#7dcfff")
    (tuareg-multistage  . "#bb9af7")
    (tuareg-operator    . "#c0a36e")
    (tuareg-module      . "#bb9af7")
    (tuareg-constructor . "#73daca")
    (tuareg-label       . "#e0af68")
    (tuareg-double-semi . "#f08070")
    (tuareg-error       . "#f7768e")
    (tuareg-interactive-output    . "#9ece6a")
    (tuareg-interactive-error     . "#f7768e")
    (tuareg-interactive-directive . "#7aa2f7")
    (tuareg-attribute        . "#737aa2")
    (tuareg-extension-node   . "#9ece6a")
    (tuareg-doc-markup       . "#bb9af7")
    (tuareg-doc-verbatim     . "#2ac3de")
    (tuareg-line-number      . "#565f89")
    (tuareg-opam-error       . "#f7768e")
    (tuareg-opam-pkg-variable . "#bb9af7")

    ;; --- Merlin ---
    (merlin-type       . "#2a2a5e")
    (merlin-warn       . "#3e2e1a")
    (merlin-error      . "#3e1a1a")

    ;; --- UTop ---
    (utop-prompt       . "#7aa2f7")
    (utop-stdout       . "#d0cdd3")
    (utop-stderr       . "#f7768e")
    (utop-frozen       . "#565f89")
    (utop-error        . "#f7768e")

    ;; --- Misc ---
    (shadow            . "#565f89")
    (line-number       . "#3b3b55")
    (line-number-current . "#d0cdd3")
    (tooltip-bg        . "#16213e")
    (tooltip-fg        . "#d0cdd3")
    (header-line-bg    . "#16213e")
    (header-line-fg    . "#d0cdd3")
    (whitespace        . "#2a2a4a")
    (trailing-ws       . "#4a2020")
    (escape-glyph      . "#ff9e64")
    (minibuffer-prompt  . "#7aa2f7")
    (fringe            . "#1a1a2e")
    (vertical-border   . "#3d4f7c")
    (yas-field         . "#1a2a4e")

    ;; --- Ediff ---
    (ediff-current-a   . "#2e1a1a")
    (ediff-current-b   . "#1a2e1a")
    (ediff-current-c   . "#2e2a1a")
    (ediff-fine-a      . "#4a2a2a")
    (ediff-fine-b      . "#2a4a2a")
    (ediff-fine-c      . "#4a4a1a")
    (ediff-even        . "#1e1e3a")
    (ediff-odd         . "#2a2a4a")

    ;; --- Magit ---
    (magit-section-bg  . "#1e1e3a")
    (magit-section-fg  . "#d0cdd3")
    (magit-branch-local  . "#7aa2f7")
    (magit-branch-remote . "#9ece6a")
    (magit-tag         . "#ff9e64")

    ;; --- Dired ---
    (dired-directory   . "#7aa2f7")
    (dired-header      . "#7dcfff")
    (dired-symlink     . "#2ac3de")
    (dired-mark        . "#f7768e")

    ;; --- Compilation / Grep ---
    (comp-error        . "#f7768e")
    (comp-warning      . "#e0af68")
    (comp-info         . "#9ece6a")
    (comp-line-number  . "#bb9af7")
    (grep-match        . "#5c5c00")
    (grep-file         . "#bb9af7")
    (grep-line         . "#ff9e64")

    ;; --- LSP ---
    (lsp-face-highlight . "#2a2a5e")

    ;; --- Calendar ---
    (calendar-today    . "#7aa2f7")
    (calendar-holiday  . "#f7768e")

    ;; --- Speedbar ---
    (speedbar-file     . "#e0af68")
    (speedbar-dir      . "#7aa2f7")
    (speedbar-tag      . "#9ece6a")
    (speedbar-selected . "#f7768e")
    (speedbar-highlight . "#2a2a4a")

    ;; --- Gnus / Message ---
    (gnus-group-1      . "#7aa2f7")
    (gnus-group-2      . "#9ece6a")
    (gnus-group-3      . "#bb9af7")
    (gnus-header-from  . "#e0af68")
    (gnus-header-subject . "#7aa2f7")
    (gnus-header-name  . "#9ece6a")
    (gnus-header-content . "#2ac3de")
    (gnus-summary-selected . "#7aa2f7")
    (gnus-summary-normal . "#d0cdd3")
    (gnus-cite-1       . "#e0af68")
    (gnus-cite-2       . "#9ece6a")
    (gnus-cite-3       . "#bb9af7")
    (message-header-name . "#9ece6a")
    (message-header-to   . "#e0af68")
    (message-header-cc   . "#2ac3de")
    (message-header-other . "#7dcfff")
    (message-header-subject . "#7aa2f7")
    (message-separator  . "#565f89")))

(defun my-theme-palette (variant)
  "Return a color palette alist for VARIANT (symbol: `light' or `dark')."
  (pcase variant
    ('light (my-theme-palette--light))
    ('dark  (my-theme-palette--dark))
    (_ (error "Unknown theme variant: %s" variant))))

(defun my-theme-palette-keys ()
  "Return the list of semantic color names (keys) in the palette.
Both variants are guaranteed to share the same keys."
  (mapcar #'car (my-theme-palette--light)))

(provide 'my-theme-palette)
;;; my-theme-palette.el ends here
