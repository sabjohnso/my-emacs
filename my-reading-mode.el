;;; my-reading-mode.el --- Read-only buffer with org-style code folding -*- lexical-binding: t -*-

;;; Commentary:

;; `my-reading-mode' is a minor mode to read code.  It makes the buffer
;; read-only and binds the folding commands of hideshow to the keys
;; that fold an org-mode outline:
;;
;;   TAB    cycle the block that starts on the current line:
;;          folded -> children -> all -> folded
;;   S-TAB  cycle the buffer: overview -> all -> overview
;;
;; The keys are in the keymap of the minor mode, and the mode does not
;; change the local map of the buffer.  When the mode turns off, Emacs
;; stops using that keymap, and the old bindings work again.
;;
;; When the mode turns off, it restores the read-only state and the
;; hideshow state that the buffer had before the mode turned on.
;;
;; The cycle commands keep no memory of earlier key presses.  They get
;; the present fold state from the buffer, so a fold that another
;; command made does not confuse them.

;;; Code:

(require 'hideshow)
(require 'seq)

;; --- The mode ---------------------------------------------------------------

(defvar-keymap my-reading-mode-map
  :doc "Keymap for `my-reading-mode'."
  "TAB" #'my-reading-mode-cycle-block
  "<backtab>" #'my-reading-mode-cycle-buffer)

;;;###autoload
(define-minor-mode my-reading-mode
  "Make the buffer read-only and fold code with org-style keys.

\\{my-reading-mode-map}"
  :lighter " Read"
  :keymap my-reading-mode-map
  (if my-reading-mode
      (my-reading-mode--enter)
    (my-reading-mode--leave)))

;; --- Save and restore the buffer state --------------------------------------

(defvar-local my-reading-mode--saved nil
  "The buffer state before the mode turned on, or nil when the mode is off.
The value is a plist with the keys :read-only and :started-hideshow.")

(defun my-reading-mode--enter ()
  "Record the buffer state, then make the buffer ready to read.
When the mode is already on, do nothing."
  (unless my-reading-mode--saved
    (setq my-reading-mode--saved
          (list :read-only buffer-read-only
                :started-hideshow (not hs-minor-mode)))
    (setq buffer-read-only t)
    (unless hs-minor-mode
      (hs-minor-mode 1))))

(defun my-reading-mode--leave ()
  "Restore the buffer state that `my-reading-mode--enter' recorded.
When the mode is already off, do nothing.  Turning `hs-minor-mode'
off also shows all of its blocks."
  (when my-reading-mode--saved
    (setq buffer-read-only (plist-get my-reading-mode--saved :read-only))
    (when (plist-get my-reading-mode--saved :started-hideshow)
      (hs-minor-mode -1))
    (setq my-reading-mode--saved nil)))

;; --- Block cycle: folded -> children -> all ---------------------------------

(defun my-reading-mode-cycle-block ()
  "Cycle the block that starts on the current line.
The states are folded, then children only, then all.  On a line
where no block starts, do nothing."
  (interactive)
  (when-let* ((start (my-reading-mode--block-start-on-line)))
    (my-reading-mode--show-block
     start
     (my-reading-mode--next-block-state (my-reading-mode--block-state start)))))

(defun my-reading-mode--next-block-state (state)
  "Return the block state that follows STATE."
  (pcase state
    ('folded 'children)
    ('children 'all)
    ('all 'folded)))

(defun my-reading-mode--block-start-on-line ()
  "Return the start of the block that starts on the current line, or nil."
  (save-excursion
    (end-of-line)
    (let ((line-start (line-beginning-position))
          (start (funcall hs-find-block-beginning-func)))
      (and start (>= start line-start) start))))

(defun my-reading-mode--block-state (start)
  "Return the present state of the block at START.
A hidden block is folded.  A visible block that holds a fold is in
the children state.  Any other block is in the all state."
  (save-excursion
    (goto-char start)
    (cond ((hs-already-hidden-p) 'folded)
          ((my-reading-mode--holds-fold-p start (my-reading-mode--block-end start))
           'children)
          (t 'all))))

(defun my-reading-mode--block-end (start)
  "Return the end of the block at START."
  (save-excursion
    (goto-char start)
    (funcall hs-forward-sexp-func 1)
    (point)))

(defun my-reading-mode--show-block (start state)
  "Make the block at START look like STATE."
  (save-excursion
    (goto-char start)
    (pcase state
      ('folded (hs-hide-block))
      ('children (hs-show-block) (hs-hide-level 1))
      ('all (hs-show-block)))))

;; --- Buffer cycle: overview -> all ------------------------------------------

(defun my-reading-mode-cycle-buffer ()
  "Cycle the buffer between the overview and all blocks visible."
  (interactive)
  (my-reading-mode--show-buffer
   (my-reading-mode--next-buffer-state (my-reading-mode--buffer-state))))

(defun my-reading-mode--next-buffer-state (state)
  "Return the buffer state that follows STATE."
  (pcase state
    ('overview 'all)
    ('all 'overview)))

(defun my-reading-mode--buffer-state ()
  "Return the present state of the buffer.
A buffer that holds a fold is in the overview state."
  (if (my-reading-mode--holds-fold-p (point-min) (point-max))
      'overview
    'all))

(defun my-reading-mode--show-buffer (state)
  "Make the buffer look like STATE."
  (pcase state
    ('overview (hs-hide-all))
    ('all (hs-show-all))))

;; --- Folds ------------------------------------------------------------------

(defun my-reading-mode--holds-fold-p (beg end)
  "Return non-nil if a hideshow fold starts between BEG and END.
Hideshow gives each of its overlays the property `hs'."
  (seq-some (lambda (ov) (overlay-get ov 'hs))
            (overlays-in (1+ beg) end)))

(provide 'my-reading-mode)
;;; my-reading-mode.el ends here
