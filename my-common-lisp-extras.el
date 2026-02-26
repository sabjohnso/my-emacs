;;; my-common-lisp-extras.el --- Extra Support for Common Lisp -*- lexical-binding: t -*-

(use-package pcre2el :ensure t :pin melpa)
(require 'pcre2el)

(defun my-common-lisp-definition-keywords ()
    '(defknown
      defun
      defsystem
      defmacro
      defunion
      defenum
      defstruct
      def-suite
      def-fixture))

(defun my-common-lisp-binding-form-keywords ()
    '(let-fun
      let-app
      let-mon
      let
      let*
      flet
      labels))

(defun my-common-lisp-testing-keywords ()
  '(is-true
    is-false
    is
    test
    in-suite))

(defun my-common-lisp-misc-introducer-keywords ()
  '(make-instance))

(defun my-common-lisp-introducer-pattern ()
  (concat
   "(\\("
   (regexp-opt
    (mapcar #'symbol-name
            (append
             (my-common-lisp-definition-keywords)
             (my-common-lisp-binding-form-keywords)
             (my-common-lisp-testing-keywords)
             (my-common-lisp-misc-introducer-keywords))))
   "\\)[)[:space:]\n]"))

(defun my-common-lisp-introducer-rule ()
  (list (my-common-lisp-introducer-pattern) 1 font-lock-keyword-face))


(defun my-add-lisp-keywords ()
  (interactive)
  (font-lock-add-keywords
   nil
   (list (my-common-lisp-introducer-rule))))

(defun my-lisp-indentations ()
  '((test . 1)
    (let-fun . 1)
    (let-app . 1)
    (let-mon . 1)
    (ctx-run . 1)
    (let-fun/list . 1)
    (let-app/list . 1)
    (let-mon/list . 1)
    (let-fun/list . 1)
    (let-app/list . 1)
    (let-mon/list . 1)
    (make-instance . 1)
    (derive-monad-interface . 1)
    (derive-state-monad-interface . 1)
    (progn-mon . 0)))

(defun my-add-lisp-indentation ()
  (interactive)
  (mapcar
   (lambda (item)
     (cl-destructuring-bind (sym . num) item
         (put sym 'common-lisp-indent-function num)))
   (my-lisp-indentations)))

;; (defun my-add-lisp-keywords ()
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil
;;    '(("\\(defknown\\)"
;;       1 font-lock-keyword-face)

;;      ("\\(single-float\\|double-float\\|long-float\\|unsigned-byte\\|integer\\|simd-pack\\(?:-single\\|-double\\)?\\)"
;;       1 font-lock-type-face))
;;    '(("(\\(\\(?:defunion\\|defknown\\|defenum\\|define\\|deflist\\|fn\\|defsystem\\|def-suite[*]?\\|in-suite\\|def-fixture\\|test\\|is\\|is-true\\|is-false\\|setf\\|make-instance\\|coalton\\(?:-toplevel\\)?\\|labels\\|flet\\)\\)[ \t\n]"
;;       1 font-lock-keyword-face)
;;      ("(\\(?:defunion\\|defknown\\|defenum\\(?:-class\\|-struct\\)?\\|deflist\\|defsystem\\|def-suite[*]?\\|in-suite\\|def-fixture\\|test\\)\\s-+\\([^[:space:])\n]+\\)"
;;       1 font-lock-type-face)
;;      ("([[:space:]\n]*define[[:space:]\n]+([[:space:]\n]*\\([^[:space:]\n)]*\\)"
;;       1 font-lock-function-name-face)
;;      ("([[:space:]\n]*\\(in-readtable\\)[[:space:]\n]+\\([^[:space:])]+\\)[[:space:]\n]*)"
;;       1 font-lock-keyword-face)

;;      ("(the\\s-+\\([^[:space:]\n)]+\\)" 1 font-lock-type-face)
;;      ("\\b\\([^[:space:]\n:]+\\):" 1 font-lock-constant-face))))

(provide 'my-common-lisp-extras)
