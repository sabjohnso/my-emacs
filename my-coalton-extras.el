;;; my-coalton-extras.el --- Extra Support for Coalton -*- lexical-binding: t -*-

(use-package pcre2el :ensure t :pin melpa)

(defun my-coalton-common-lisp-definition-keywords ()
  "Return a list of common lisp definition keywords"
  '())

(defun my-coalton-definition-kewords ()
  "Return a list of coalton keywods introducing `DEFINE' style bindings."
  '(define-struct
    declare
    define-class
    define-instance))

(defun my-coalton-let-keywords
    "Return a list of keywords introducing `LET' style bindings."
  '(let))

(defun my-coalton-misc-introducer-keywors ()
  "Return a list of other coalton introducer keywords"
  nil)
