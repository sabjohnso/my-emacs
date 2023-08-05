;;; my-racket-extras.el --- Extra Support for Racket -*- lexical-binding: t -*-

(use-package pcre2el :ensure t :pin melpa)
(require 'pcre2el)

(defun my-regex-opt-symbols (symbols)
  (regexp-opt (mapcar #'symbol-name symbols)))

(defun my-racket-extras-definition-keywords ()  
  "Return a list of Racket keywords introducing definitions in Racket"
  '(define/contract
    define-tag
    define-curried
    define/public
    define/pubment
    define/public-final
    define/override
    define/overment
    define/override-final
    define/augment
    define/augride
    define/augment-final
    define/private
    define-generics
    define/generic
    define/rule
    define-syntax-rule
    define-syntax-class
    define-splicing-syntax-class
    define-match-expander
    define-tokens
    define-empty-tokens
    define-runtime-path
    define-runtime-paths
    define-runtime-path-list
    define-match-predicate
    define-pattern-predicate
    define-rule
    define-token
    define-lexical-rule
    define-type
    define-predicate
    define-ffi-definer
    define-wsfeed
    define-jsonschema
    define-cpointer-type
    define-optional-predicate
    define-either-predicate
    define-jsonschema-predicate
    define/lexeme
    define/pattern
    define/transform
    define/subexpression-pos-prop))

(defun my-racket-extras-module-introducers ()
  "Return a list of Racket keywords introducing modules"
  '(module
    module+
    module*
    struct
    message
    union
    enum
    data
    describe
    context
    it
    test-case
    test-begin
    thread
    thread-run))

(defun my-racket-let-introducers ()
  "Return a list of Racket let binding introducers"
  '(let
    let*
    letrec
    let-values
    let*-values
    letrec-values
    let-syntax
    letrec-syntax
    let-syntaxes
    letrec-syntaxes+values
    let/m
    let/a
    let/f
    let/state
    let/env
    let/list
    with-syntax
    match-lambda**
    match*
    process
    let/arrow
    place
    parameterize
    parameterize*
    λ/arrow
    lambda/arrow
    let/arrow
    call-with-input-file
    call-with-input-file*
    with-input-from-file
    call-with-output-file
    call-with-output-file*
    with-output-to-file))

(defun my-racket-require-spec-introducers ()
  "Return a list of Racket require spec introducers"
  '(local-require
    only-in
    except-in
    prefix-in
    rename-in
    combind-in
    relative-in
    only-meta-in
    only-space-in
    for-syntax
    for-template
    for-label
    for-meta
    for-space
    submod
    thunk
    delay
    future))

(defun my-racket-provide-spec-introducers ()
  "Return a list of Racket require spec introducers"
  '(all-defined-out
    all-from-out
    contract-out
    rename-out
    except-out
    prefix-out
    struct-out
    combind-out
    protect-out
    for-meta
    for-syntax
    for-template
    for-label
    for-space))

(defun my-racket-introducer-keywords ()
  "Return a list of Racket introducer keywords"
  '(quasisyntax
    unsyntax
    unsyntax-splicing
    syntax
    quasisyntax/loc
    syntax/loc
    quote-syntax/prune
    begin
    begin0
    begin-for-syntax
    begin/m
    syntax-parser
    syntax-parse
    interface
    interface*
    class
    class*
    super-new
    new
    init
    init-field
    place-channel-put
    place-channel-get
    place-channel-put/get))


(defun my-racket-introducer-keyword-pattern ()
  "Return a pattern recognizing introducer keywords"
  (concat
   "(\\("
   (regexp-opt
    (mapcar #'symbol-name
	    (append (my-racket-extras-definition-keywords)
		    (my-racket-extras-module-introducers)
		    (my-racket-let-introducers)
		    (my-racket-require-spec-introducers)
		    (my-racket-provide-spec-introducers)
		    (my-racket-introducer-keywords))))
   "\\)[)[:space:]\n]"))

(defun my-racket-infix-operators ()
  '(: <- -<))

(defun my-racket-infix-pattern ()
  (concat "[[:space:]\n]\\("
	  (regexp-opt (mapcar #'symbol-name (my-racket-infix-operators)))
	  "\\)[[:space:]\n]"))

(defun my-racket-infix-rule ()
  (list (my-racket-infix-pattern) 1 'font-lock-keyword-face))

(defun my-racket-type-prefixes ()
  '(gen prop exn))

(defun my-racket-type-suffixes ()
  '(% <%>))

(defun my-racket-type-name-pattern ()
  (concat "(?\\("
	  (my-regex-opt-symbols (my-racket-type-prefixes))
	  ":"
	  "[^[:space:]\n)]+"
	  "\\|"
	  "[^[[:space:]\n)]+" (my-regex-opt-symbols (my-racket-type-suffixes))
	  "\\)[[:space:]\n)]"))

(defun my-racket-type-name-rule ()
  (list (my-racket-type-name-pattern) 1 'font-lock-type-face))

(defun my-racket-ctype-name-rule ()
  (list
   "[[([:space:]\n]\\(_[^[:space:])\n)]*\\)\\b"
   1
   'font-lock-type-face))

(defun my-racket-lang-rule ()
  (list "#lang[[:space:]]+\\(.*\\)" 1 font-lock-type-face))

(defun my-racket-introducer-rule ()
  (list (my-racket-introducer-keyword-pattern) 1 'font-lock-keyword-face))

(defun my-module-struct-name-rule ()
  (list
   (concat "([[:space:]\n]*"
	   (regexp-opt (mapcar #'symbol-name (my-racket-extras-module-introducers)))
	   "[[:space:]\n]+\\([^[:space:]\n)]+\\)[[:space:]\n)]")
   1
   'font-lock-type-face))

(defun my-racket-module-language-rule ()
  (list
   "([[:space:]\n]*module[[:space:]\n]+[^[:space:]\n]+[[:space:]\n]+\\([^[:space:]\n]+\\)"
   1
   font-lock-type-face))

(defun my-racket-struct-super-rule ()
  (list
   "([[:space:]\n]*struct[[:space:]\n]+[^([:space:]\n]+[[:space:]\n]+\\([^([:space:]\n]+\\)"
   1
   font-lock-type-face))

(defun my-racket-variable-name-rule ()
  (list
   (concat "([[:space:]\n]*"
	   (regexp-opt (mapcar #'symbol-name (my-racket-extras-definition-keywords)))
	   "[[:space:]\n]+(\\([^[:space:]\n()]+\\)")
   1
   font-lock-function-name-face))

(defun my-racket-expanded-rule ()
  (list "[([:space:]\n]\\(#%[^[:space:]\n)]+\\)[[:space:]\n)]" 1
	font-lock-warning-face))

(defun my-racket-infix-notation-rule ()  
  (list "[[:space:]\n]\\(\`[^[:space:]\n(]+\\)[[:space:]\n]" 1
	font-lock-builtin-face))

(defun my-add-racket-highlights ()
  (interactive)
  (font-lock-add-keywords
   nil
   (list
    (my-racket-lang-rule)    
    (my-racket-introducer-rule)
    (my-racket-variable-name-rule)
    (my-module-struct-name-rule)
    (my-racket-module-language-rule)
    (my-racket-type-name-rule)
    (my-racket-infix-rule)
    (my-racket-expanded-rule)
    (my-racket-infix-notation-rule)
    (my-racket-ctype-name-rule)))
  (font-lock-ensure))

(defun my-add-racket-indentation ()
  (interactive)
  (put 'new 'scheme-indent-function 1)
  (put 'data 'scheme-indent-function 1)
  (put 'syntax-parse 'scheme-indent-function 1)
  (put 'when/m 'scheme-indent-function 1)
  (put 'unless/m 'scheme-indent-function 1)
  (put 'let/m 'scheme-indent-function 1)
  (put 'let/a 'scheme-indent-function 1)
  (put 'let/f 'scheme-indent-function 1)
  (put 'lambda/arrow 'scheme-indent-function 1)
  (put 'λ/arrow 'scheme-indent-function 1)  
  (put 'let/arrow 'scheme-indent-function 1)
  (put 'match* 'scheme-indent-function 1)
  (put 'process 'scheme-indent-function 1)
  (put 'rename-in 'scheme-indent-function 1)
  (put 'enum 'scheme-indent-function 1)
  (put 'union 'scheme-indent-function 1)
  (put 'describe 'scheme-indent-function 1)
  (put 'context 'scheme-indent-function 1)
  (put 'it 'scheme-indent-function 1)
  (put 'format 'scheme-indent-function 1)
  (put 'struct-copy 'schme-indent-function 2)
  (put 'if 'scheme-indent-function 2)
  (put 'stream-iterate 'scheme-indent-function 1)
  (put 'pipe 'scheme-indent-function 1)
  (put 'test-case 'scheme-indent-function 1)
  (put 'test-begin 'scheme-indent-function 0)
  (put 'place 'scheme-indent-function 1)
  (put 'place-channel-put 'scheme-indent-function 1)
  (put 'class* 'scheme-indent-function 2)
  (put 'call-with-input-file 'scheme-indent-function 1)
  (put 'call-with-input-file* 'scheme-indent-function 1)
  (put 'with-input-from-file 'scheme-indent-function 1)
  (put 'call-with-output-file 'scheme-indent-function 1)
  (put 'call-with-output-file* 'scheme-indent-function 1)
  (put 'only-in 'scheme-indent-function 1)
  (put 'printf 'scheme-indent-function 1)
  (put 'with-output-to-file 'scheme-indent-function 1)
  (put 'query-exec 'scheme-indent-function 1)
  (put 'query-row 'scheme-indent-function 1)
  (put 'query-rows 'scheme-indent-function 1)
  (put 'query-list 'scheme-indent-function 1)
  (put 'query-maybe-row 'scheme-indet-function 1)
  (put 'query-value 'scheme-indent-function 1)
  (put 'query-maybe-value 'scheme-indent-function 1)
  (put 'in-query 'scheme-indent-function 1))

(add-hook 'scheme-mode-hook 'my-add-racket-highlights)
(add-hook 'scheme-mode-hook 'my-add-racket-indentation)

(provide 'my-racket-extras)
