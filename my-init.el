;;; my-init.el

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Packages and paths
;;;
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Org
;;
;; (add-to-list 'load-path (expand-file-name "~/Sandbox/org-mode/lisp"))
(require 'org)
(use-package org-tempo :demand t)
(add-hook 'org-mode-hook 'auto-fill-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Tweaks
;;;
(set-frame-font
 '(:family "DejaVu Sans Mono"
	   :foundry "PfEd"
	   :slant normal
	   :weight normal
	   :height 105
	   :width normal))

(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq visible-bell t)
(global-set-key (kbd "<f5>") #'compile)

(setq save-abbrevs 'silently)



(defun my-get-monitor-pixels (&optional frame)
  "Return a two element list containing the number of
pixels for the frame's monitor."
  (let* ((monitor (frame-monitor-attributes frame))
	 (geometry (alist-get 'geometry monitor))
	 (x0 (car geometry))
	 (x1 (caddr geometry))
	 (y0 (cadr geometry))
	 (y1 (cadddr geometry)))
    (list (- x1 x0)
	  (- y1 y0))))

(defun my-get-monitor-inches (&optional frame)
  "Return the moitor size in inches"
  (let* ((monitor (frame-monitor-attributes frame))
	 (mm-size (alist-get 'mm-size monitor)))
    (list (/ (car mm-size) 25.4)
	  (/ (cadr mm-size) 25.4))))

(defun my-get-monitor-resolution (&optional frame)
  "Return the resolution in pixels per milimeter
for the current frame or the optinally provide frame"
  (let ((pixels (my-get-monitor-pixels frame))
	(inches (my-get-monitor-inches frame)))
    (list (/ (car pixels) (car inches))
	  (/ (cadr pixels) (cadr inches)))))

(defun my-adapt-font-size (&optional frame)
  (let* ((ppi (cadr (my-get-monitor-resolution frame))))
    (message "PPI: %s" ppi)
    (if (> ppi 120)
        (set-face-attribute 'default frame :height 100)
      (set-face-attribute 'default frame :height 50))))


(add-function :after after-focus-change-function #'my-adapt-font-size)
(add-hook 'after-make-frame-functions #'my-adapt-font-size)

(setq compilation-environment '("TERM=xterm-256color"))
(add-hook 'compilation-mode-hook (lambda () (toggle-truncate-lines 1)))
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))


(setq split-height-threshold 10000)

(defvar my-delete-trailing-whitespace-mode-list
  '(c-mode
    c++-mode
    cmake-mode
    elisp-mode
    scheme-mode
    shell-script-mode
    lua-mode
    lisp-mode
    org-mode
    scad-mode
    racket-mode)
  "For modes in this list run `delete-trailing-whitespace` brefore
saving the buffer.")
(global-display-line-numbers-mode)
(add-hook 'before-save-hook
   (lambda ()
     (when (member major-mode my-delete-trailing-whitespace-mode-list)
       (delete-trailing-whitespace))))

;; prevent scaling with the mouse wheel and add
;; scaling with the up and down arrows
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

(defun my-increase-scale ()
  "Increase the text scale by 1 increment"
  (interactive)
  (text-scale-adjust 1))

(defun my-decrease-scale ()
  "Descrease the text scale by 1 increment"
  (interactive)
  (text-scale-adjust -1))

(global-set-key (kbd "C-<up>") 'my-increase-scale)
(global-set-key (kbd "C-<down>") 'my-decrease-scale)

;; hide emacs backup files
(require 'dired-x)
(setq dired-omit-files ".~$")
(setq dired-omit-extensions nil)
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package pcre2el :ensure t :pin melpa)
(use-package sr-speedbar :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... General Development
;;
(use-package rtags :ensure t)
(use-package company-rtags :ensure t)
(use-package highlight-thing :ensure t)
;; (use-package highlight-unique-symbol :ensure t)
(use-package paredit-everywhere :ensure t :pin melpa)
(use-package paredit-menu :ensure t :pin melpa)
(use-package flycheck-projectile :ensure t :pin melpa)
(use-package lsp-mode :ensure t :pin melpa)
(use-package lsp-ui :ensure t :pin melpa)
(use-package magit :ensure t :pin melpa)
(use-package git-modes :ensure t :pin melpa)
(use-package projectile
  :ensure t
  :pin melpa
  :bind-keymap
  ("C-c p" . projectile-command-map))
(use-package projectile-speedbar :ensure t :pin melpa)
(use-package plantuml-mode :ensure t :pin melpa)
(use-package mermaid-mode :ensure t :pin melpa)

(defun my-add-fixme-highlights ()
  (interactive)
  (font-lock-add-keywords
   nil
   "//.*\\(FIXME\\|TODO\\):"
   '(("\\s<.*?\\(TODO\\|FIXME\\):" 1 font-lock-warning-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Haskell
;;
(use-package haskell-mode :ensure t :pin melpa)
;; (use-package dante :ensure t :pin melpa) Development mode for Haskell
(use-package  ac-haskell-process :ensure t :pin melpa) ;; Haskell auto-complete source which uses the current haskell process
(use-package flycheck-haskell :ensure t :pin melpa) ;; not sure what this uses but it says automatic
;; (use-package flycheck-liquidhs :ensure t :pin melpa) ;; it uses liquidhaskell (what is that)
;;(use-package flymake-haskell-multi :ensure t :pin melpa) ;; it uses hlint and ghc
;; (use-package flymake-hlint :ensure t :pin melpa) ;; it uses hlint

;; (use-package ghc-imported-from :ensure t :pin melpa ) ;; Haskell documentation lookup with ghc-imported-from
;; (use-package ghci-completion :ensure t :pin melpa) ;; Completion for GHCi commands in inferior-haskell buffers

;; (use-package haskell-tng-mode :ensure t :pin nongnu) ;; Major mode for editing Haskell
;; (use-package hcel :ensure t :pin gnu) ;; Haskell codebase explorer
;; (use-package hi2 :ensure t :pin melpa) ;; indentation module for Haskell Mode
(use-package hindent :ensure t :pin melpa) ;; Indent haskell code using the "hindent" program
;; (use-package hyai :ensure t :pin melpa) ;; Haskell Yet Another Indentation
(use-package lsp-haskell :ensure t :pin melpa) ;; Haskell support for lsp-mode
;; (use-package nix-haskell-mode :ensure t :pin melpa) ;; haskell-mode integrations for Nix
(use-package ormolu :ensure t :pin melpa) ;;Format Haskell source code using the "ormolu" program
;; (use-package retrie :ensure t :pin melpa) ;; Refactoring Haskell code with retrie
;; (use-package shm :ensure t :pin melpa)  ;; Structured Haskell Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Julia
;;;
(use-package julia-repl :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Lisp Development
;;;

;; slime/lisp
(use-package slime :ensure t :pin melpa)



(add-hook 'slime-repl-mode-hook
	  (lambda () (local-set-key (kbd "<f12>") 'slime-repl-clear-buffer)))
(use-package slime-company :ensure t :pin melpa)
(use-package company :ensure t :pin melpa)
(use-package racket-mode :ensure t :pin melpa)
(use-package scribble-mode :ensure t :pin melpa)
(require 'slime-autoloads)

(use-package clojure-mode :ensure t :pin melpa)
(use-package cider :ensure t :pin melpa)
(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

;; scheme
(use-package geiser-racket :ensure t :pin melpa)
(use-package geiser-chibi :ensure t :pin melpa)
(use-package geiser-chez :ensure t :pin melpa)
(use-package geiser-chicken :ensure t :pin melpa)
(use-package geiser-gambit :ensure t :pin melpa)
(use-package geiser-gauche :ensure t :pin melpa)
(use-package geiser-guile :ensure t :pin melpa)
(use-package geiser-kawa :ensure t :pin melpa)

;; (use-package dr-racket-like-unicode :ensure t :pin melpa
;;   :hook (scheme-mode . dr-racket-like-unicode))

(add-hook 'geiser-repl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-a") 'move-beginning-of-line)
	    (local-set-key (kbd "<f12>") 'geiser-repl-clear-buffer)))


;; paredit
(use-package paredit :ensure t :pin melpa)

(defun my-fix-paredit ()
  (interactive)
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))

(with-eval-after-load 'paredit
  (my-fix-paredit))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'inferior-emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'racket-mode 'paredit-mode)




(defun my-override-slime-repl-bindings-with-paredit ()
  (define-key 'slime-repl-mode-map
	      (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'my-override-slime-repl-bindings-with-paredit)


(defun my-add-lisp-keywords ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("(\\(\\(?:defenum\\|deflist\\|defsystem\\|def-suite[*]?\\|in-suite\\|def-fixture\\|test\\|is\\)\\)[ \t\n]"
      1 font-lock-keyword-face)
     ("(\\(?:defenum\\|deflist\\|defsystem\\|def-suite[*]\\|in-suite\\|def-fixture\\|test\\)\\s-+\\([^[:space:])\n]+\\)"
      1 font-lock-type-face)
     ("(the\\s-+\\([^[:space:]\n)]+\\)" 1 font-lock-type-face))))

(add-hook 'lisp-mode-hook 'my-add-lisp-keywords)
(add-hook 'lisp-mode-hook (lambda () (local-set-key (kbd "C-c I") 'slime-inspect)))

;; emacs-lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (local-set-key (kbd "<f5>") 'emacs-lisp-byte-compile-and-load)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Erlang
;;
(use-package edts :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... C++ Development
;;;
(use-package doxy-graph-mode :ensure t)
(require 'doxy-graph-mode)

(add-hook 'c-mode-hook 'doxy-graph-mode)
(add-hook 'c++-mode-hook 'doxy-graph-mode)
(add-hook 'python-mode-hook 'doxy-graph-mode)
(use-package modern-cpp-font-lock :ensure t)
;; (use-package company-c-headers :ensure t)
;; (use-package cpp-auto-include :ensure t)
;; (use-package cpputils-cmake :ensure t) ; this probably doesn't work
;; (use-package demangle-mode :endsure t) ;
;; (use-package disaster :ensure t) ; this shoud dissasemble code at point
;; (use-package function-args :ensure t) ; C++ completion for GNU Emacs
(use-package irony :ensure t) ; C/C++ minor mode powered by libclang
;; (use-package irony-eldoc :ensure t) ; irony-mode support for eldoc-mode
(use-package doxy-graph-mode :ensure t
    :hook ((c++-mode . doxy-graph-mode)
	   (c-mode . doxy-graph-mode)
	   (python-mode . doxy-graph-mode)))

(use-package highlight-doxygen :ensure t)
(require 'cc-styles)
(add-to-list
 'c-style-alist
 '("my"
   (c-basic-offset . 3)
   (c-comment-only-line-offset . 0)
   (c-offsets-alist
    (statement-block-intro . +)
    (knr-argdecl-intro . +)
    (substatement-open . +)
    (substatement-label . 0)
    (label . 0)
    (statement-cont . +)
    (inline-open . 0)
    (inexpr-class . 0))))
(use-package compiler-explorer :ensure t :pin melpa)
(use-package pickle :ensure t :pin melpa)
(use-package cmake-mode :ensure t :pin melpa)
(use-package cmake-font-lock :ensure t :pin melpa)

(defface compilation-pass
  '((t :inherit compilation-info))
  "Face used to highlight passed tests executed during compilation.")

(defface compilation-fail
  '((t :inherit compilation-error))
  "Face used to highlight failed tests executed during compilation.")

(defvar compilation-pass-face 'compilation-pass
  "Face name to use for pass indicators.")

(defvar compilation-fail-face 'compilation-fail
  "Face name to use for failure indicators.")

(defun my-add-compilation-keywords ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\([*][*][*]Failed\\)" 1 compilation-fail-face)
     ("\\(Passed\\)" 1 compilation-pass-face)))
  (font-lock-ensure))
(add-hook 'compilation-mode-hook 'my-add-compilation-keywords)
;; (use-package cmake-ide :ensure t :pin melpa)
;; (use-package rtags :ensure t :pin melpa)
;; (use-package flycheck-rtags :ensure t :pin melpa)
;; (use-package company-rtags :ensure t :pin melpa)
(use-package clang-format :ensure t :pin melpa)
(use-package clang-format+ :ensure t :pin melpa)

(add-to-list 'cmake-font-lock-function-keywords-alist
	     '("find_package_handle_standard_args" .
	       ("FOUND_VAR" "REQUIRED_VARS" "VERSION_VAR" "HANDLE_VERSION_RANGE"
		"CONFIG_MODE" "REASON_FAILURE_MESSAGE" "FAIL_MESSAGE")))
(add-to-list 'cmake-font-lock-function-keywords-alist
	     '("parse_args" .
	       ("PREFIX" "BOOLEAN_KEYWORDS" "SINGLE_VALUE_KEYWORDS"
		"MULTI_VALUE_KEYWORDS" "REQUIRED" "ARGV")))
(add-to-list 'cmake-font-lock-function-keywords-alist
	     '("racket_add_executable" .
	       ("NAME" "SOURCE")))
(add-to-list 'cmake-font-lock-function-keywords-alist
	     '("raco_add_package" .
	       ("PACKAGE_PATH" "DEPENDS")))

;; (require 'rtags)
;; (cmake-ide-setup)

(defun my-add-c++-fixme-highlights ()
  (interactive)
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
      ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face))))

(defun my-add-c++-macro-highlights ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\b\\([$]\\w+\\)" 1 font-lock-warning-face prepend)
     ("\\b\\([A-Z_][A-Z_]+\\)\\b" 1 font-lock-warning-face))))

(defun my-c++-catch2-keywords ()
  "Return a list of keywords to add for Catch2 highlighting"
  '(TEST_CASE
    SECTION
    GIVEN
    THEN
    WHEN
    AND_GIVEN
    AND_THEN
    AND_WHEN
    TEMPLATE_TEST_CASE
    TEMPLATE_PRODUCT_TEST_CASE
    TEMPLATE_TEST_CASE_SIG
    TEMPLATE_PRODUCT_TEST_CASE_SIG
    TEST_CASE_METHOD
    CATCH_REGISTER_LISTENER
    GENERATE
    CHECKED_IF
    CHECK_NOFAIL
    SUCEED
    STATIC_REQUIRE
    STATIC_REQUIRE_FALSE
    STATIC_CHECK
    STATIC_CHECK_FALSE
    DYNAMIC_SECTION
    REQUIRE_STATIC
    REQUIRE
    CHECK
    REQUIRE_FALSE
    CHECK_FALSE
    REQUIRE_NOTHROW
    CHECK_NOTHROW
    REQUIRE_THROWS
    CHECK_THROWS
    REQUIRE_THROWS_AS
    CHECK_THROWS_AS
    REQUIRE_THROWS_WITH
    CHECK_THROWS_WITH
    REQUIRE_THAT
    CHECK_THAT
    REQUIRE_THROWS_THAT
    CHECK_THROWS_THAT
    INFO
    UNSCOPED_INFO
    WARN
    FAIL
    FAIL_CHECK
    CAPTURE
    BENCHMARK
    BENCHMARK_ADVANCED))
(use-package pcre2el :ensure t :pin melpa)
(require 'pcre2el)

(defun my-c++-catch2-keywords-pattern ()
  "Return a pattern recognizing chatch2 macros"
  (concat "\\b\\("
	  (regexp-opt (mapcar #'symbol-name (my-c++-catch2-keywords)))
	  "\\)\\b"))
(defun my-add-c++-catch2-keywords ()
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(my-c++-catch2-keywords-pattern) 1 font-lock-warning-face))))


(defvar my-clang-format-on-save-enabled t)
(defun my-clang-format-on-save ()
  (when (and my-clang-format-on-save-enabled (eq major-mode 'c++-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'my-clang-format-on-save)
(add-hook 'c++-mode-hook 'clang-format+-mode)
(add-hook 'c++-mode-hook
	  (lambda ()
	    (c-set-style "my")
	    (my-add-c++-fixme-highlights)
	    (my-add-c++-catch2-keywords)
	    (local-set-key (kbd "<f12>") 'clang-format-buffer)
	    ;; (my-add-c++-macro-highlights)
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Python
(use-package pipenv :ensure t :pin melpa)
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package jedi :ensure t :pin melpa)
(use-package company-jedi :ensure t :pin melpa)
(use-package elpy :ensure t :pin melpa)
(use-package python-black :ensure t :pin melpa
  :hook (python-mode . python-black-on-save-mode))
;; (use-package jedi-direx :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... OCaml
;;;
(use-package dune :ensure t :pin melpa)
(use-package dune-format :ensure t :pin melpa)
(use-package merlin :ensure t :pin melpa)
(use-package merlin-company :ensure t :pin melpa                )
(use-package merlin-eldoc :ensure t :pin melpa)
(use-package merlin-iedit :ensure t :pin melpa)
;; (use-package ocaml-format :ensure t :pin melpa)
(use-package opam-switch-mode :ensure t :pin melpa)
(use-package tuareg :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Shells
;;
(use-package powershell :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Text Formats
;;;
(use-package json-mode :ensure t :pin melpa)
(use-package json-par :ensure t :pin melpa)
(use-package toml-mode :ensure t :pin melpa)
(use-package yaml-mode :ensure t :pin melpa)
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist `("Pipfile" . toml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
(require 'rng-loc)
(add-hook 'nxml-mode-hook
	  (lambda ()
	     (make-local-variable 'indent-tabs-mode)
	     (setq indent-tabs-mode nil)
	     (add-to-list 'rng-schema-locating-files
			  (expand-file-name "~/.emacs.d/nxml-schemas/schemas.xml"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Grammars
;;;
(use-package bnf-mode :ensure t :pin melpa)
(use-package ebnf-mode :ensure t :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Text and documentation
;;;
(use-package markdown-mode :ensure t :pin melpa)
(use-package markdown-preview-eww :ensure t :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Solid Modeling
;;;
(use-package scad-mode :ensure t :pin melpa)
(add-hook 'scad-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Environment Modules
;;;
(load "/usr/local/Modules/init/lisp")
(add-to-list 'magic-mode-alist '("#%Module.*" . tcl-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Bullshit
;;;

(use-package chatgpt-shell :ensure t :pin melpa)
(setq chatgpt-shell-openai-key "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")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; ... My Stuff
;;
(require 'my-closet)
(require 'my-racket-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-init)
