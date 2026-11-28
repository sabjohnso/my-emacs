;;; test-my-fold-ellipsis.el --- ERT tests for my-fold-ellipsis -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the visible fold marker in my-fold-ellipsis.el.
;; Run with: emacs --batch -L . -l test-my-fold-ellipsis.el -f ert-run-tests-batch-and-exit
;;
;; The property tests use a seeded random state, so each run checks the
;; same cases.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hideshow)
(require 'my-fold-ellipsis)

(defvar org-ellipsis)

;;; --- Fixtures ---

(defmacro test-my-fold-ellipsis--with-setup (&rest body)
  "Run BODY after `my-fold-ellipsis-setup', with the global changes undone after."
  (declare (indent 0) (debug t))
  `(let ((standard-display-table (make-display-table))
         (hs-set-up-overlay #'ignore)
         (org-ellipsis nil))
     (my-fold-ellipsis-setup)
     ,@body))

(defmacro test-my-fold-ellipsis--with-folded (source &rest body)
  "Insert SOURCE, fold the block at its start with hideshow, then run BODY."
  (declare (indent 1) (debug t))
  `(test-my-fold-ellipsis--with-setup
     (with-temp-buffer
       (insert ,source)
       (emacs-lisp-mode)
       (hs-minor-mode 1)
       (goto-char (point-min))
       (hs-hide-block)
       ,@body)))

(defun test-my-fold-ellipsis--hidden-display (kind)
  "Return the display property of the hideshow overlay of KIND."
  (cl-some (lambda (ov) (and (eq kind (overlay-get ov 'hs))
                             (overlay-get ov 'display)))
           (overlays-in (point-min) (point-max))))

(defun test-my-fold-ellipsis--random-string (state)
  "Return a random string of up to 8 characters from random STATE.
The characters come from the range from space to U+2FFF."
  (apply #'string
         (cl-loop repeat (cl-random 9 state)
                  collect (+ ?\s (cl-random (- #x3000 ?\s) state)))))

(defun test-my-fold-ellipsis--random-counts (state)
  "Return 0, 1, 2, and 200 random line counts below one million from STATE."
  (append '(0 1 2) (cl-loop repeat 200 collect (cl-random 1000000 state))))

(defun test-my-fold-ellipsis--newlines (string)
  "Return the number of newlines in STRING."
  (cl-count ?\n string))

;;; --- Glyphs ---

(ert-deftest my-fold-ellipsis-glyphs-round-trip ()
  "The characters of the glyphs give back the string, each with the face."
  (let ((state (cl-make-random-state 42))
        (faces '(bold italic my-fold-ellipsis)))
    (dotimes (_ 200)
      (let* ((string (test-my-fold-ellipsis--random-string state))
             (face (nth (cl-random (length faces) state) faces))
             (glyphs (my-fold-ellipsis-glyphs string face)))
        (should (equal string (apply #'string (mapcar #'glyph-char glyphs))))
        (should (cl-every (lambda (g) (eq face (glyph-face g))) glyphs))))))

;;; --- Display table ---

(ert-deftest my-fold-ellipsis-setup-puts-marker-in-display-table ()
  "Setup shows the marker text in the marker face for selective display."
  (test-my-fold-ellipsis--with-setup
    (let ((glyphs (display-table-slot standard-display-table
                                      'selective-display)))
      (should (equal " ▸…" (apply #'string (mapcar #'glyph-char glyphs))))
      (should (cl-every (lambda (g) (eq 'my-fold-ellipsis (glyph-face g)))
                        glyphs)))))

(ert-deftest my-fold-ellipsis-install-makes-standard-table ()
  "Install with no table makes the standard display table when it is nil."
  (let ((standard-display-table nil))
    (my-fold-ellipsis-install)
    (should (display-table-slot standard-display-table 'selective-display))))

;;; --- Org ---

(ert-deftest my-fold-ellipsis-setup-sets-org-ellipsis ()
  "Setup gives Org a string ellipsis, so Org uses the face `org-ellipsis'."
  (test-my-fold-ellipsis--with-setup
    (should (equal " ▸…" org-ellipsis))))

;;; --- Labels ---

(ert-deftest my-fold-ellipsis-label-examples ()
  "The label shows the marker and the count, singular only for one line."
  (should (equal " ▸… [+12 lines]" (my-fold-ellipsis-label 12)))
  (should (equal " ▸… [+1 line]" (my-fold-ellipsis-label 1)))
  (should (equal " ▸… [+0 lines]" (my-fold-ellipsis-label 0))))

(ert-deftest my-fold-ellipsis-label-laws ()
  "Each label obeys the laws in the docstring of `my-fold-ellipsis-label'."
  (dolist (n (test-my-fold-ellipsis--random-counts (cl-make-random-state 7)))
    (let ((label (my-fold-ellipsis-label n)))
      (should (string-prefix-p my-fold-ellipsis-string label))
      (should (string-match "\\[\\+\\([0-9]+\\) \\(lines?\\)\\]\\'" label))
      (should (= n (string-to-number (match-string 1 label))))
      (should (eq (= n 1) (equal "line" (match-string 2 label))))
      (dotimes (i (length label))
        (should (eq 'my-fold-ellipsis (get-text-property i 'face label)))))))

;;; --- Hideshow line counts ---

(ert-deftest my-fold-ellipsis-hideshow-shows-line-count ()
  "After setup, a hidden code block shows the marker and its hidden line count."
  (test-my-fold-ellipsis--with-folded
      "(defun foo ()\n  (let ((x 1))\n    (bar x)\n    (baz x)))\n"
    (should (equal " ▸… [+3 lines]"
                   (test-my-fold-ellipsis--hidden-display 'code)))))

(ert-deftest my-fold-ellipsis-hideshow-keeps-comment-marker ()
  "After setup, a hidden comment gets no label, so it keeps the table marker."
  (test-my-fold-ellipsis--with-folded ";; one\n;; two\n;; three\n(foo)\n"
    (should-not (test-my-fold-ellipsis--hidden-display 'comment))))

(ert-deftest my-fold-ellipsis-hidden-lines-counts-newlines ()
  "The count is the number of newlines in the overlay, also when narrowed."
  (let ((state (cl-make-random-state 3)))
    (dotimes (_ 200)
      (with-temp-buffer
        (dotimes (_ (1+ (cl-random 30 state)))
          (insert (make-string (cl-random 10 state) ?x) "\n"))
        (let* ((a (1+ (cl-random (buffer-size) state)))
               (b (1+ (cl-random (buffer-size) state)))
               (start (min a b))
               (end (max a b))
               (overlay (make-overlay start end)))
          (when (zerop (cl-random 2 state))
            (narrow-to-region (1+ (cl-random start state))
                              (+ end (cl-random (- (point-max) end -1) state))))
          (should (= (test-my-fold-ellipsis--newlines
                      (buffer-substring-no-properties start end))
                     (my-fold-ellipsis-hidden-lines overlay))))))))

(provide 'test-my-fold-ellipsis)
;;; test-my-fold-ellipsis.el ends here
