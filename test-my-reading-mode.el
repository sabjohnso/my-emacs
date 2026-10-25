;;; test-my-reading-mode.el --- ERT tests for my-reading-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the read-only folding minor mode in my-reading-mode.el.
;; Run with: emacs --batch -L . -l test-my-reading-mode.el -f ert-run-tests-batch-and-exit
;;
;; The tests observe folds through the lines that stay visible, so they
;; depend on what the user sees and not on the overlays of hideshow.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'my-reading-mode)

;;; --- Fixtures ---

(defconst test-my-reading-mode--source
  "(defun foo ()\n  (let ((x 1))\n    (bar x)\n    (baz x)))\n\n(defun qux ()\n  1)\n"
  "Elisp source with one nested block, one flat block, and a blank line.
Line 1 starts `foo', line 2 starts the nested `let', line 5 is blank,
and line 6 starts `qux'.")

(defconst test-my-reading-mode--all-lines '(1 2 3 4 5 6 7)
  "The lines of the fixture source.")

(defmacro test-my-reading-mode--with-buffer (&rest body)
  "Run BODY in an `emacs-lisp-mode' buffer that holds the fixture source."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert test-my-reading-mode--source)
     (emacs-lisp-mode)
     (goto-char (point-min))
     ,@body))

(defun test-my-reading-mode--line (n)
  "Move point to the start of line N."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun test-my-reading-mode--visible-lines ()
  "Return the numbers of the fixture lines whose start is visible."
  (save-excursion
    (cl-remove-if (lambda (n)
                    (test-my-reading-mode--line n)
                    (invisible-p (point)))
                  test-my-reading-mode--all-lines)))

(defun test-my-reading-mode--snapshot ()
  "Return the buffer state that `my-reading-mode' must restore."
  (list buffer-read-only
        (and (bound-and-true-p hs-minor-mode) t)
        (key-binding (kbd "TAB"))
        (key-binding [backtab])
        (test-my-reading-mode--visible-lines)))

(defun test-my-reading-mode--prepare (read-only hideshow)
  "Set the start state: READ-ONLY, and HIDESHOW on with `foo' folded."
  (setq buffer-read-only read-only)
  (when hideshow
    (hs-minor-mode 1)
    (save-excursion (test-my-reading-mode--line 1) (hs-hide-block))))

(defmacro test-my-reading-mode--for-each-start-state (&rest body)
  "Run BODY in a fresh fixture buffer for each of the four start states."
  (declare (indent 0) (debug t))
  `(dolist (read-only '(nil t))
     (dolist (hideshow '(nil t))
       (test-my-reading-mode--with-buffer
         (test-my-reading-mode--prepare read-only hideshow)
         ,@body))))

(defun test-my-reading-mode--press (command n)
  "Run COMMAND N times."
  (dotimes (_ n) (funcall command)))

;;; --- Round-trip laws: off after on restores the buffer state ---

(ert-deftest my-reading-mode-off-after-on-restores-every-start-state ()
  "For all four start states, toggling on then off is the identity."
  (test-my-reading-mode--for-each-start-state
    (let ((before (test-my-reading-mode--snapshot)))
      (my-reading-mode 1)
      (my-reading-mode -1)
      (should (equal (test-my-reading-mode--snapshot) before)))))

(ert-deftest my-reading-mode-on-is-idempotent ()
  "For all four start states, on, on, then off is the identity."
  (test-my-reading-mode--for-each-start-state
    (let ((before (test-my-reading-mode--snapshot)))
      (my-reading-mode 1)
      (my-reading-mode 1)
      (my-reading-mode -1)
      (should (equal (test-my-reading-mode--snapshot) before)))))

(ert-deftest my-reading-mode-off-when-off-changes-nothing ()
  "For all four start states, off when the mode is off is the identity."
  (test-my-reading-mode--for-each-start-state
    (let ((before (test-my-reading-mode--snapshot)))
      (my-reading-mode -1)
      (should (equal (test-my-reading-mode--snapshot) before)))))

;;; --- While on ---

(ert-deftest my-reading-mode-makes-buffer-read-only ()
  "The buffer is read-only while the mode is on."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (should buffer-read-only)))

(ert-deftest my-reading-mode-binds-folding-keys ()
  "TAB and S-TAB run the cycle commands while the mode is on."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (should (eq (key-binding (kbd "TAB")) #'my-reading-mode-cycle-block))
    (should (eq (key-binding [backtab]) #'my-reading-mode-cycle-buffer))))

(ert-deftest my-reading-mode-off-shows-blocks-it-folded ()
  "When the mode turned on hideshow, turning the mode off shows all blocks."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (my-reading-mode-cycle-buffer)
    (my-reading-mode -1)
    (should (equal (test-my-reading-mode--visible-lines)
                   test-my-reading-mode--all-lines))))

;;; --- Block cycle: folded, children, all ---

(ert-deftest my-reading-mode-cycle-block-goes-through-three-states ()
  "TAB on a block start line folds, then shows children, then shows all."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (my-reading-mode-cycle-block)
    (should (equal (test-my-reading-mode--visible-lines) '(1 5 6 7)))
    (my-reading-mode-cycle-block)
    (should (equal (test-my-reading-mode--visible-lines) '(1 2 5 6 7)))
    (my-reading-mode-cycle-block)
    (should (equal (test-my-reading-mode--visible-lines)
                   test-my-reading-mode--all-lines))))

(defun test-my-reading-mode--assert-block-period (line period)
  "Assert that TAB on LINE restores the visible lines after PERIOD presses."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (test-my-reading-mode--line line)
    (let ((before (test-my-reading-mode--visible-lines)))
      (my-reading-mode-cycle-block)
      (should-not (equal (test-my-reading-mode--visible-lines) before))
      (test-my-reading-mode--press #'my-reading-mode-cycle-block (1- period))
      (should (equal (test-my-reading-mode--visible-lines) before)))))

(ert-deftest my-reading-mode-cycle-block-with-children-has-period-three ()
  "On a block that holds a block of more than one line, three TABs
restore the visible lines."
  (test-my-reading-mode--assert-block-period 1 3))

(ert-deftest my-reading-mode-cycle-block-without-children-has-period-two ()
  "On a block that holds no block of more than one line, the children
state looks like the all state, so two TABs restore the visible lines.
The `let' on line 2 holds only blocks of one line, which do not fold."
  (dolist (line '(2 6))
    (test-my-reading-mode--assert-block-period line 2)))

(ert-deftest my-reading-mode-cycle-block-acts-on-block-of-current-line ()
  "TAB on a nested start line folds the nested block, not its parent."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (test-my-reading-mode--line 2)
    (my-reading-mode-cycle-block)
    (should (equal (test-my-reading-mode--visible-lines) '(1 2 5 6 7)))))

(ert-deftest my-reading-mode-cycle-block-ignores-line-with-no-block ()
  "TAB on a line where no block starts does nothing."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (dolist (line '(3 5))
      (test-my-reading-mode--line line)
      (my-reading-mode-cycle-block)
      (should (equal (test-my-reading-mode--visible-lines)
                     test-my-reading-mode--all-lines)))))

(ert-deftest my-reading-mode-cycle-block-reads-state-from-buffer ()
  "After another command shows a block in the children state, TAB folds it."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (test-my-reading-mode--press #'my-reading-mode-cycle-block 2)
    (hs-show-all)
    (my-reading-mode-cycle-block)
    (should (equal (test-my-reading-mode--visible-lines) '(1 5 6 7)))))

(ert-deftest my-reading-mode-cycle-block-keeps-point ()
  "TAB does not move point."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (forward-char 3)
    (dotimes (_ 3)
      (my-reading-mode-cycle-block)
      (should (= (point) 4)))))

;;; --- Buffer cycle: overview, all ---

(ert-deftest my-reading-mode-cycle-buffer-goes-through-two-states ()
  "S-TAB folds every top-level block, then shows all."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (my-reading-mode-cycle-buffer)
    (should (equal (test-my-reading-mode--visible-lines) '(1 5 6)))
    (my-reading-mode-cycle-buffer)
    (should (equal (test-my-reading-mode--visible-lines)
                   test-my-reading-mode--all-lines))))

(ert-deftest my-reading-mode-cycle-buffer-has-period-two ()
  "For n from 1 to 3, 2n S-TABs restore the visible lines."
  (dolist (n '(1 2 3))
    (test-my-reading-mode--with-buffer
      (my-reading-mode 1)
      (let ((before (test-my-reading-mode--visible-lines)))
        (test-my-reading-mode--press #'my-reading-mode-cycle-buffer (* 2 n))
        (should (equal (test-my-reading-mode--visible-lines) before))))))

(ert-deftest my-reading-mode-cycle-buffer-shows-all-after-any-fold ()
  "When TAB folded a block, the next S-TAB shows all blocks."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (my-reading-mode-cycle-block)
    (my-reading-mode-cycle-buffer)
    (should (equal (test-my-reading-mode--visible-lines)
                   test-my-reading-mode--all-lines))))

(ert-deftest my-reading-mode-cycle-buffer-keeps-point ()
  "S-TAB does not move point."
  (test-my-reading-mode--with-buffer
    (my-reading-mode 1)
    (test-my-reading-mode--line 6)
    (dotimes (_ 2)
      (my-reading-mode-cycle-buffer)
      (should (= (point) 57)))))

(provide 'test-my-reading-mode)
;;; test-my-reading-mode.el ends here
