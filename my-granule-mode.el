;;; my-granule-mode.el --- Major mode for the Granule language -*- lexical-binding: t -*-

;;; Commentary:

;; A simple major mode for editing Granule source code (`.gr' files).
;;
;; Granule is a functional programming language with graded and linear
;; types.  Its concrete syntax is Haskell-like: `--' line comments,
;; nestable `{- -}' block comments, top-level type signatures of the
;; form `name : Type', and equations of the form `name args = body'.
;;
;; This mode provides a syntax table and font-lock highlighting only.
;; Indentation is left to `prog-mode' defaults: Granule's layout rules
;; are subtle, and a naive indenter tends to fight the user.

;;; Code:

;; --- Lexical categories ------------------------------------------------------
;; Each category is a small, self-contained function returning the raw
;; tokens for that category.  Keeping them separate makes the highlighted
;; vocabulary easy to read, test, and extend without touching the
;; font-lock machinery that consumes them.

(defun my-granule-keywords ()
  "Reserved words that introduce or structure Granule forms."
  '("import" "data" "where" "case" "of" "let" "in"
    "if" "then" "else" "forall" "language" "module"
    "hiding" "as" "interface" "instance" "spec"))

(defun my-granule-constants ()
  "Literal value constructors that read as constants."
  '("True" "False"))

(defun my-granule-builtin-types ()
  "Types and coeffect/grade constructors shipped with Granule."
  '("Int" "Float" "Char" "String" "Bool"
    "Nat" "Q" "Level" "Set" "Interval" "Ext"
    "Cartesian" "IO" "Handle" "Cap"))

;; --- Font lock ---------------------------------------------------------------

(defvar my-granule-font-lock-keywords
  `(;; Top-level type signatures and equation heads: a lowercase name in
    ;; the leftmost column.  Matched first so the name itself wins over
    ;; the generic identifier rules below.
    ("^[ \t]*\\([a-z_][A-Za-z0-9_']*\\)[ \t]*:" 1 font-lock-function-name-face)
    ;; Reserved words.
    (,(regexp-opt (my-granule-keywords) 'symbols) . font-lock-keyword-face)
    ;; Boolean / nullary value constructors.
    (,(regexp-opt (my-granule-constants) 'symbols) . font-lock-constant-face)
    ;; Built-in types named explicitly (kept distinct for clarity even
    ;; though the capitalized-identifier rule would also catch them).
    (,(regexp-opt (my-granule-builtin-types) 'symbols) . font-lock-type-face)
    ;; Any other capitalized identifier is a type or data constructor.
    ("\\_<\\([A-Z][A-Za-z0-9_']*\\)\\_>" 1 font-lock-type-face)
    ;; Typed holes: `?' or `?name'.  Highlighted loudly so they stand out
    ;; as deliberate gaps to be filled in.
    ("\\?[A-Za-z0-9_']*" . font-lock-warning-face)
    ;; Type-signature and pattern operators.
    ("->\\|<-\\|=>\\|\\.\\." . font-lock-builtin-face))
  "Font-lock highlighting rules for `my-granule-mode'.")

;; --- Syntax table ------------------------------------------------------------

(defvar my-granule-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `--' line comments and nestable `{- -}' block comments, using the
    ;; standard Haskell-style multi-character comment encoding.
    (modify-syntax-entry ?\{ "(}1nb" table)
    (modify-syntax-entry ?\} "){4nb" table)
    (modify-syntax-entry ?-  ". 123" table)
    (modify-syntax-entry ?\n ">"     table)
    ;; Underscore and prime are word-internal in Granule identifiers.
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?'  "_" table)
    table)
  "Syntax table for `my-granule-mode'.")

;; --- Mode definition ---------------------------------------------------------

;;;###autoload
(define-derived-mode my-granule-mode prog-mode "Granule"
  "Major mode for editing Granule source code."
  :syntax-table my-granule-mode-syntax-table
  (setq-local font-lock-defaults '(my-granule-font-lock-keywords))
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?:--+\\|{-+\\)[ \t]*")
  (setq-local comment-padding " ")
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gr\\'" . my-granule-mode))

(provide 'my-granule-mode)

;;; my-granule-mode.el ends here
