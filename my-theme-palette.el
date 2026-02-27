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

(defun my-theme-palette--light-256 ()
  "Return the light 256-color palette alist.
Uses xterm-256 `color-NNN' names for terminals without true color."
  '(;; --- Base ---
    (bg              . "white")
    (bg-alt          . "color-255")
    (bg-highlight    . "color-229")
    (bg-region       . "color-153")
    (bg-paren-match  . "color-117")
    (bg-paren-mismatch . "color-196")
    (bg-isearch      . "yellow")
    (bg-lazy-isearch . "color-194")
    (bg-hl-line      . "color-254")
    (fg              . "color-236")
    (fg-alt          . "color-240")
    (fg-dim          . "color-246")
    (fg-faint        . "color-250")
    (cursor          . "black")

    ;; --- Syntax ---
    (keyword         . "blue")
    (string          . "color-28")
    (comment         . "color-245")
    (doc             . "color-22")
    (variable        . "color-133")
    (function-name   . "color-30")
    (type            . "color-56")
    (constant        . "color-160")
    (builtin         . "color-33")
    (preprocessor    . "color-244")
    (negation        . "red")
    (warning         . "color-208")
    (error           . "red")
    (success         . "color-28")
    (info            . "color-33")

    ;; --- Links ---
    (link            . "color-32")
    (link-visited    . "color-90")

    ;; --- Mode line ---
    (ml-active-bg    . "color-61")
    (ml-active-fg    . "white")
    (ml-inactive-bg  . "color-247")
    (ml-inactive-fg  . "color-255")

    ;; --- Diff / VCS ---
    (diff-added-bg   . "color-194")
    (diff-added-fg   . "color-28")
    (diff-removed-bg . "color-224")
    (diff-removed-fg . "color-124")
    (diff-changed-bg . "color-230")
    (diff-changed-fg . "color-136")
    (diff-hunk-bg    . "color-189")
    (diff-hunk-fg    . "color-61")
    (diff-fine-added . "color-157")
    (diff-fine-removed . "color-217")
    (diff-indicator-added . "color-28")
    (diff-indicator-removed . "color-124")

    ;; --- Org mode ---
    (ol1-bg          . "color-253")
    (ol1-fg          . "color-237")
    (ol2-bg          . "color-189")
    (ol2-fg          . "color-24")
    (ol3-bg          . "color-194")
    (ol3-fg          . "color-22")
    (ol4-fg          . "color-162")
    (ol5-fg          . "color-32")
    (ol6-fg          . "color-34")
    (ol7-fg          . "color-208")
    (ol8-fg          . "color-245")
    (org-todo-fg     . "color-203")
    (org-done-fg     . "color-28")
    (org-block-bg    . "color-230")
    (org-code-bg     . "color-255")
    (org-code-fg     . "color-89")
    (org-table-bg    . "color-223")
    (org-date-fg     . "color-56")
    (org-tag-fg      . "color-248")
    (org-drawer-fg   . "blue")

    ;; --- Outline / Markdown ---
    (outline-1       . "color-237")
    (outline-2       . "color-24")
    (outline-3       . "color-22")
    (outline-4       . "color-162")
    (outline-5       . "color-32")
    (outline-6       . "color-34")
    (outline-7       . "color-208")
    (outline-8       . "color-245")

    ;; --- Company ---
    (company-bg      . "color-254")
    (company-fg      . "color-236")
    (company-sel-bg  . "color-153")
    (company-sel-fg  . "black")
    (company-border  . "color-250")

    ;; --- Flycheck / Flyspell ---
    (flycheck-error-bg . "color-224")
    (flycheck-warn-bg  . "color-229")
    (flycheck-info-bg  . "color-194")

    ;; --- Rainbow delimiters ---
    (rainbow-1       . "color-243")
    (rainbow-2       . "color-68")
    (rainbow-3       . "color-247")
    (rainbow-4       . "color-65")
    (rainbow-5       . "color-95")
    (rainbow-6       . "color-62")
    (rainbow-7       . "color-246")
    (rainbow-8       . "color-71")
    (rainbow-9       . "color-94")

    ;; --- ANSI colors ---
    (ansi-black      . "black")
    (ansi-red        . "color-160")
    (ansi-green      . "color-34")
    (ansi-yellow     . "color-184")
    (ansi-blue       . "color-20")
    (ansi-magenta    . "color-164")
    (ansi-cyan       . "color-37")
    (ansi-white      . "color-250")
    (ansi-bright-black   . "color-240")
    (ansi-bright-red     . "red")
    (ansi-bright-green   . "color-46")
    (ansi-bright-yellow  . "yellow")
    (ansi-bright-blue    . "blue")
    (ansi-bright-magenta . "magenta")
    (ansi-bright-cyan    . "cyan")
    (ansi-bright-white   . "white")

    ;; --- OCaml / Tuareg ---
    (tuareg-governing   . "color-30")
    (tuareg-multistage  . "color-90")
    (tuareg-operator    . "color-136")
    (tuareg-module      . "color-56")
    (tuareg-constructor . "color-23")
    (tuareg-label       . "color-94")
    (tuareg-double-semi . "color-166")
    (tuareg-error       . "red")
    (tuareg-interactive-output    . "color-22")
    (tuareg-interactive-error     . "color-88")
    (tuareg-interactive-directive . "color-20")
    (tuareg-attribute        . "color-244")
    (tuareg-extension-node   . "color-64")
    (tuareg-doc-markup       . "color-90")
    (tuareg-doc-verbatim     . "color-23")
    (tuareg-line-number      . "color-246")
    (tuareg-opam-error       . "red")
    (tuareg-opam-pkg-variable . "color-133")

    ;; --- Merlin ---
    (merlin-type       . "color-189")
    (merlin-warn       . "color-223")
    (merlin-error      . "color-224")

    ;; --- UTop ---
    (utop-prompt       . "blue")
    (utop-stdout       . "color-236")
    (utop-stderr       . "color-160")
    (utop-frozen       . "color-244")
    (utop-error        . "red")

    ;; --- Misc ---
    (shadow            . "color-246")
    (line-number       . "color-246")
    (line-number-current . "color-236")
    (tooltip-bg        . "color-229")
    (tooltip-fg        . "color-236")
    (header-line-bg    . "color-254")
    (header-line-fg    . "color-236")
    (whitespace        . "color-253")
    (trailing-ws       . "color-217")
    (escape-glyph      . "color-208")
    (minibuffer-prompt  . "color-61")
    (fringe            . "color-255")
    (vertical-border   . "color-250")
    (yas-field         . "color-153")

    ;; --- Ediff ---
    (ediff-current-a   . "color-224")
    (ediff-current-b   . "color-194")
    (ediff-current-c   . "color-229")
    (ediff-fine-a      . "color-217")
    (ediff-fine-b      . "color-157")
    (ediff-fine-c      . "color-227")
    (ediff-even        . "color-254")
    (ediff-odd         . "color-253")

    ;; --- Magit ---
    (magit-section-bg  . "color-254")
    (magit-section-fg  . "color-236")
    (magit-branch-local  . "color-33")
    (magit-branch-remote . "color-28")
    (magit-tag         . "color-160")

    ;; --- Dired ---
    (dired-directory   . "blue")
    (dired-header      . "color-33")
    (dired-symlink     . "color-30")
    (dired-mark        . "red")

    ;; --- Compilation / Grep ---
    (comp-error        . "red")
    (comp-warning      . "color-208")
    (comp-info         . "color-28")
    (comp-line-number  . "color-56")
    (grep-match        . "yellow")
    (grep-file         . "color-56")
    (grep-line         . "color-160")

    ;; --- LSP ---
    (lsp-face-highlight . "color-189")

    ;; --- Calendar ---
    (calendar-today    . "blue")
    (calendar-holiday  . "red")

    ;; --- Speedbar ---
    (speedbar-file     . "color-30")
    (speedbar-dir      . "blue")
    (speedbar-tag      . "color-94")
    (speedbar-selected . "red")
    (speedbar-highlight . "color-254")

    ;; --- Gnus / Message ---
    (gnus-group-1      . "color-33")
    (gnus-group-2      . "color-28")
    (gnus-group-3      . "color-56")
    (gnus-header-from  . "color-30")
    (gnus-header-subject . "blue")
    (gnus-header-name  . "color-94")
    (gnus-header-content . "color-22")
    (gnus-summary-selected . "blue")
    (gnus-summary-normal . "color-236")
    (gnus-cite-1       . "color-30")
    (gnus-cite-2       . "color-28")
    (gnus-cite-3       . "color-56")
    (message-header-name . "color-94")
    (message-header-to   . "color-30")
    (message-header-cc   . "color-22")
    (message-header-other . "color-33")
    (message-header-subject . "blue")
    (message-separator  . "color-246")))

(defun my-theme-palette--dark-256 ()
  "Return the dark 256-color palette alist.
Uses xterm-256 `color-NNN' names for terminals without true color."
  '(;; --- Base ---
    (bg              . "color-234")
    (bg-alt          . "color-235")
    (bg-highlight    . "color-237")
    (bg-region       . "color-60")
    (bg-paren-match  . "color-61")
    (bg-paren-mismatch . "color-52")
    (bg-isearch      . "color-58")
    (bg-lazy-isearch . "color-22")
    (bg-hl-line      . "color-235")
    (fg              . "color-252")
    (fg-alt          . "color-249")
    (fg-dim          . "color-243")
    (fg-faint        . "color-240")
    (cursor          . "color-179")

    ;; --- Syntax ---
    (keyword         . "color-111")
    (string          . "color-149")
    (comment         . "color-60")
    (doc             . "color-107")
    (variable        . "color-141")
    (function-name   . "color-179")
    (type            . "color-44")
    (constant        . "color-209")
    (builtin         . "color-117")
    (preprocessor    . "color-103")
    (negation        . "color-204")
    (warning         . "color-179")
    (error           . "color-204")
    (success         . "color-149")
    (info            . "color-117")

    ;; --- Links ---
    (link            . "color-111")
    (link-visited    . "color-141")

    ;; --- Mode line ---
    (ml-active-bg    . "color-60")
    (ml-active-fg    . "color-252")
    (ml-inactive-bg  . "color-237")
    (ml-inactive-fg  . "color-60")

    ;; --- Diff / VCS ---
    (diff-added-bg   . "color-22")
    (diff-added-fg   . "color-149")
    (diff-removed-bg . "color-52")
    (diff-removed-fg . "color-204")
    (diff-changed-bg . "color-58")
    (diff-changed-fg . "color-179")
    (diff-hunk-bg    . "color-17")
    (diff-hunk-fg    . "color-111")
    (diff-fine-added . "color-28")
    (diff-fine-removed . "color-88")
    (diff-indicator-added . "color-149")
    (diff-indicator-removed . "color-204")

    ;; --- Org mode ---
    (ol1-bg          . "color-237")
    (ol1-fg          . "color-252")
    (ol2-bg          . "color-17")
    (ol2-fg          . "color-111")
    (ol3-bg          . "color-22")
    (ol3-fg          . "color-149")
    (ol4-fg          . "color-204")
    (ol5-fg          . "color-117")
    (ol6-fg          . "color-149")
    (ol7-fg          . "color-209")
    (ol8-fg          . "color-103")
    (org-todo-fg     . "color-204")
    (org-done-fg     . "color-149")
    (org-block-bg    . "color-235")
    (org-code-bg     . "color-236")
    (org-code-fg     . "color-209")
    (org-table-bg    . "color-236")
    (org-date-fg     . "color-141")
    (org-tag-fg      . "color-60")
    (org-drawer-fg   . "color-111")

    ;; --- Outline / Markdown ---
    (outline-1       . "color-252")
    (outline-2       . "color-111")
    (outline-3       . "color-149")
    (outline-4       . "color-204")
    (outline-5       . "color-117")
    (outline-6       . "color-149")
    (outline-7       . "color-209")
    (outline-8       . "color-103")

    ;; --- Company ---
    (company-bg      . "color-235")
    (company-fg      . "color-252")
    (company-sel-bg  . "color-60")
    (company-sel-fg  . "white")
    (company-border  . "color-60")

    ;; --- Flycheck / Flyspell ---
    (flycheck-error-bg . "color-52")
    (flycheck-warn-bg  . "color-58")
    (flycheck-info-bg  . "color-22")

    ;; --- Rainbow delimiters ---
    (rainbow-1       . "color-248")
    (rainbow-2       . "color-111")
    (rainbow-3       . "color-149")
    (rainbow-4       . "color-179")
    (rainbow-5       . "color-204")
    (rainbow-6       . "color-141")
    (rainbow-7       . "color-44")
    (rainbow-8       . "color-209")
    (rainbow-9       . "color-117")

    ;; --- ANSI colors ---
    (ansi-black      . "color-234")
    (ansi-red        . "color-204")
    (ansi-green      . "color-149")
    (ansi-yellow     . "color-179")
    (ansi-blue       . "color-111")
    (ansi-magenta    . "color-141")
    (ansi-cyan       . "color-117")
    (ansi-white      . "color-252")
    (ansi-bright-black   . "color-60")
    (ansi-bright-red     . "color-210")
    (ansi-bright-green   . "color-156")
    (ansi-bright-yellow  . "color-186")
    (ansi-bright-blue    . "color-147")
    (ansi-bright-magenta . "color-183")
    (ansi-bright-cyan    . "color-153")
    (ansi-bright-white   . "white")

    ;; --- OCaml / Tuareg ---
    (tuareg-governing   . "color-117")
    (tuareg-multistage  . "color-141")
    (tuareg-operator    . "color-179")
    (tuareg-module      . "color-141")
    (tuareg-constructor . "color-79")
    (tuareg-label       . "color-179")
    (tuareg-double-semi . "color-209")
    (tuareg-error       . "color-204")
    (tuareg-interactive-output    . "color-149")
    (tuareg-interactive-error     . "color-204")
    (tuareg-interactive-directive . "color-111")
    (tuareg-attribute        . "color-103")
    (tuareg-extension-node   . "color-149")
    (tuareg-doc-markup       . "color-141")
    (tuareg-doc-verbatim     . "color-44")
    (tuareg-line-number      . "color-60")
    (tuareg-opam-error       . "color-204")
    (tuareg-opam-pkg-variable . "color-141")

    ;; --- Merlin ---
    (merlin-type       . "color-17")
    (merlin-warn       . "color-58")
    (merlin-error      . "color-52")

    ;; --- UTop ---
    (utop-prompt       . "color-111")
    (utop-stdout       . "color-252")
    (utop-stderr       . "color-204")
    (utop-frozen       . "color-60")
    (utop-error        . "color-204")

    ;; --- Misc ---
    (shadow            . "color-60")
    (line-number       . "color-239")
    (line-number-current . "color-252")
    (tooltip-bg        . "color-235")
    (tooltip-fg        . "color-252")
    (header-line-bg    . "color-235")
    (header-line-fg    . "color-252")
    (whitespace        . "color-237")
    (trailing-ws       . "color-52")
    (escape-glyph      . "color-209")
    (minibuffer-prompt  . "color-111")
    (fringe            . "color-234")
    (vertical-border   . "color-60")
    (yas-field         . "color-17")

    ;; --- Ediff ---
    (ediff-current-a   . "color-52")
    (ediff-current-b   . "color-22")
    (ediff-current-c   . "color-58")
    (ediff-fine-a      . "color-88")
    (ediff-fine-b      . "color-28")
    (ediff-fine-c      . "color-100")
    (ediff-even        . "color-236")
    (ediff-odd         . "color-237")

    ;; --- Magit ---
    (magit-section-bg  . "color-236")
    (magit-section-fg  . "color-252")
    (magit-branch-local  . "color-111")
    (magit-branch-remote . "color-149")
    (magit-tag         . "color-209")

    ;; --- Dired ---
    (dired-directory   . "color-111")
    (dired-header      . "color-117")
    (dired-symlink     . "color-44")
    (dired-mark        . "color-204")

    ;; --- Compilation / Grep ---
    (comp-error        . "color-204")
    (comp-warning      . "color-179")
    (comp-info         . "color-149")
    (comp-line-number  . "color-141")
    (grep-match        . "color-58")
    (grep-file         . "color-141")
    (grep-line         . "color-209")

    ;; --- LSP ---
    (lsp-face-highlight . "color-17")

    ;; --- Calendar ---
    (calendar-today    . "color-111")
    (calendar-holiday  . "color-204")

    ;; --- Speedbar ---
    (speedbar-file     . "color-179")
    (speedbar-dir      . "color-111")
    (speedbar-tag      . "color-149")
    (speedbar-selected . "color-204")
    (speedbar-highlight . "color-237")

    ;; --- Gnus / Message ---
    (gnus-group-1      . "color-111")
    (gnus-group-2      . "color-149")
    (gnus-group-3      . "color-141")
    (gnus-header-from  . "color-179")
    (gnus-header-subject . "color-111")
    (gnus-header-name  . "color-149")
    (gnus-header-content . "color-44")
    (gnus-summary-selected . "color-111")
    (gnus-summary-normal . "color-252")
    (gnus-cite-1       . "color-179")
    (gnus-cite-2       . "color-149")
    (gnus-cite-3       . "color-141")
    (message-header-name . "color-149")
    (message-header-to   . "color-179")
    (message-header-cc   . "color-44")
    (message-header-other . "color-117")
    (message-header-subject . "color-111")
    (message-separator  . "color-60")))

(defun my-theme-palette (variant)
  "Return a color palette alist for VARIANT.
VARIANT is a symbol: `light', `dark', `light-256', or `dark-256'."
  (pcase variant
    ('light     (my-theme-palette--light))
    ('dark      (my-theme-palette--dark))
    ('light-256 (my-theme-palette--light-256))
    ('dark-256  (my-theme-palette--dark-256))
    (_ (error "Unknown theme variant: %s" variant))))

(defun my-theme-palette-keys ()
  "Return the list of semantic color names (keys) in the palette.
Both variants are guaranteed to share the same keys."
  (mapcar #'car (my-theme-palette--light)))

(provide 'my-theme-palette)
;;; my-theme-palette.el ends here
