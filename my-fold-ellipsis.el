;;; my-fold-ellipsis.el --- A visible marker for folded text -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Folded text shows a marker in place of the hidden text.  By default
;; Emacs draws this marker as plain dots in the face of the text around
;; it, so the marker is hard to see.  This module gives the marker the
;; face `my-fold-ellipsis' in three locations:
;;
;;   - The `selective-display' slot of `standard-display-table'.
;;     Hideshow, `outline-minor-mode', and selective display use it.
;;   - The variable `org-ellipsis'.  Org uses the face `org-ellipsis'
;;     only when this variable is a string.
;;   - `hs-set-up-overlay'.  A hidden code block shows the marker and
;;     the number of hidden lines.
;;
;; The themes set the color of the faces.  This module sets only the
;; glyphs.  Call `my-fold-ellipsis-setup' one time from the init file.
;; This module owns the values of `hs-set-up-overlay' and `org-ellipsis'.
;; It sets them with `setq', so Org and hideshow do not load at startup.

;;; Code:

(defvar org-ellipsis)
(defvar hs-set-up-overlay)

(defgroup my-fold-ellipsis nil
  "A visible marker for folded text."
  :group 'display)

(defface my-fold-ellipsis
  '((t :inherit warning))
  "Face of the marker that shows folded text."
  :group 'my-fold-ellipsis)

(defconst my-fold-ellipsis-string " ▸…"
  "The text of the marker that shows folded text.")

;; --- Entry point -------------------------------------------------------------

(defun my-fold-ellipsis-setup ()
  "Show the fold marker in hideshow, outline, selective display, and Org."
  (my-fold-ellipsis-install)
  (setq hs-set-up-overlay #'my-fold-ellipsis-hs-overlay)
  (setq org-ellipsis my-fold-ellipsis-string))

;; --- Display table -----------------------------------------------------------

(defun my-fold-ellipsis-install (&optional table)
  "Put the fold marker into the `selective-display' slot of TABLE.
When TABLE is nil, use `standard-display-table' and make it if it is nil."
  (let ((table (or table (my-fold-ellipsis--standard-table))))
    (set-display-table-slot
     table 'selective-display
     (my-fold-ellipsis-glyphs my-fold-ellipsis-string 'my-fold-ellipsis))))

(defun my-fold-ellipsis--standard-table ()
  "Return `standard-display-table'.  Make it first if it is nil."
  (or standard-display-table
      (setq standard-display-table (make-display-table))))

(defun my-fold-ellipsis-glyphs (string face)
  "Return a vector of glyphs that show STRING in FACE.
Law: the characters of the glyphs give back STRING, and each glyph
has FACE."
  (vconcat (mapcar (lambda (char) (make-glyph-code char face)) string)))

;; --- Hideshow line counts ----------------------------------------------------

(defun my-fold-ellipsis-hs-overlay (overlay)
  "Show the fold marker and the hidden line count on a code OVERLAY.
Use this function as the value of `hs-set-up-overlay'.  A hidden
comment keeps the marker of the display table."
  (when (eq 'code (overlay-get overlay 'hs))
    (overlay-put overlay 'display
                 (my-fold-ellipsis-label (my-fold-ellipsis-hidden-lines overlay)))))

(defun my-fold-ellipsis-hidden-lines (overlay)
  "Return the number of lines that OVERLAY removes from the display.
The overlay starts after the text of the first line, so that line
stays visible and does not count.
Law: the result is the number of newlines in the text of OVERLAY."
  (- (line-number-at-pos (overlay-end overlay))
     (line-number-at-pos (overlay-start overlay))))

(defun my-fold-ellipsis-label (lines)
  "Return the fold marker followed by the count of LINES, in the marker face.
Law: the label starts with `my-fold-ellipsis-string', shows LINES in
decimal, uses the word \"line\" only when LINES is 1, and has the face
`my-fold-ellipsis' on each character."
  (propertize (format "%s [+%d %s]" my-fold-ellipsis-string lines
                      (if (= lines 1) "line" "lines"))
              'face 'my-fold-ellipsis))

(provide 'my-fold-ellipsis)
;;; my-fold-ellipsis.el ends here
