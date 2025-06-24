;;; my-init.el -*- lexical-binding: t -*-
(require 'package)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Packages and paths
;;

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(use-package el-get :ensure t :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Org
;;

;; (add-to-list 'load-path (expand-file-name "~/Sandbox/org-mode/lisp"))
(require 'org)
(use-package org-tempo :demand t)
(add-hook 'org-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Tweaks
;;

;; down with tabs
(setq-default indent-tabs-mode nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight regular :height 110 :width normal))))
 '(dir-treeview-directory-face ((t (:inherit dir-treeview-default-filename-face :foreground "cyan"))))
 '(tuareg-font-lock-governing-face ((t (:weight bold :foreground "dark cyan" :inherit bold)))))


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
        (set-face-attribute 'default frame :height 200)
      (set-face-attribute 'default frame :height 100))))


(defvar my-face-scale-factor 0.1
  "The factor by which the default font is
scaled when increasing or decreasing the the
height of the font.")

(defun my-get-default-font-height (&optional frame)
  "Return the default font height"
  (face-attribute 'default :height))

(defun my-next-bigger-font-height (&optional frame)
  "Return the next bigger font height"
  (round (* (+ 1 my-face-scale-factor)
	    (my-get-default-font-height frame))))

(defun my-next-smaller-font-height (&optional frame)
  "Return the next smaller default font height"
  (round (* (- 1 my-face-scale-factor)
	    (my-get-default-font-height frame))))

(defun my-increase-font-size (&optional frame)
  "Increase the default font size"
  (interactive)
  (set-face-attribute
   'default frame
   :height (my-next-bigger-font-height)))

(defun my-decrease-font-size (&optional frame)
  "Decrease the default font size"
  (interactive)
  (set-face-attribute
   'default frame
   :height (my-next-smaller-font-height)))

(defun my-set-global-font-size-keys ()
  (interactive)
  (global-set-key (kbd "C-=") 'my-increase-font-size)
  (global-set-key (kbd "C-M-=") 'my-decrease-font-size))

(my-set-global-font-size-keys)



;;(add-function :after after-focus-change-function #'my-adapt-font-size)
;; (add-hook 'after-make-frame-functions #'my-adapt-font-size)

(with-eval-after-load 'compile
  (setq compilation-environment '("TERM=xterm-256color")))


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
    racket-mode
    dockerfile-mode
    scribble-mode
    tuareg-mode
    json-mode
    python-mode
    fortran-mode
    f90-mode
    haskell-mode
    yaml-mode
    markdown-mode
    emacs-lisp-mode
    nxml-mode)
  "For modes in this list run `delete-trailing-whitespace` brefore
saving the buffer.")

(add-hook 'before-save-hook
   (lambda ()
     (when (member major-mode my-delete-trailing-whitespace-mode-list)
       (delete-trailing-whitespace)
       (font-lock-fontify-buffer))))


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


(add-to-list 'auto-mode-alist '("\\.cheat\\'" . shell-script-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... Graphviz
;;;
(use-package graphviz-dot-mode :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ... General Development
;;;
(use-package dockerfile-mode :ensure t :pin melpa)
(use-package direnv :ensure t :pin melpa)
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
;; (use-package flymake-haskell-multi :ensure t :pin melpa) ;; it uses hlint and ghc
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
;;
;; ... Julia
;;

(use-package julia-repl :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Lisp Development
;;

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'comment-region)))

;; slime/lisp
;; (use-package slime :ensure t :pin melpa)
;; (require 'slime)
;; (defun my-override-slime-repl-bindings-with-paredit ()
;;   (interactive)
;;   (keymap-unset slime-repl-mode-map
;; 	      (read-kbd-macro paredit-backward-delete-key)))

;; (add-hook 'slime-repl-mode-hook (lambda () (local-set-key (kbd "<f12>") 'slime-repl-clear-buffer)))
;; (add-hook 'slime-repl-mode
;;           (lambda ()
;;             (paredit-mode)
;;             (local-set-key (kbd "<f12>") 'slime-repl-clear-buffer)))

;; (use-package slime-company :ensure t :pin melpa)
(use-package company :ensure t :pin melpa)
(use-package racket-mode :ensure t :pin melpa)
(use-package scribble-mode :ensure t :pin melpa)
(use-package hy-mode :ensure t :pin melpa)

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.eclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.ecl\\'" . lisp-mode))

(use-package clojure-mode :ensure t :pin melpa)
(use-package cider :ensure t :pin melpa)

(with-eval-after-load
    'slime
  (setq slime-lisp-implementations
        '((ros ("ros" "run") )
          (sbcl ("sbcl") :coding-system utf-8-unix)
          (clisp ("clisp"))
          (abcl ("/home/sbj/.roswell/impls/x86-64/linux/abcl-bin/1.9.3/abcl"))
          (ccl ("/home/sbj/.roswell/impls/x86-64/linux/ccl-bin/1.13/scripts/ccl"))
          (cmu ("/home/sbj/.roswell/impls/x86-64/linux/cmu-bin/21e/bin/lisp"))
          (ecl ("/home/sbj/.roswell/impls/x86-64/linux/ecl/24.5.10/bin/ecl")))))

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
(require 'paredit)
(define-key paredit-mode-map (kbd "RET") nil)


;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
;; (defun my-override-slime-repl-bindings-with-paredit ()
;;   (interactive)
;;   (define-key slime-repl-mode-map
;;               (read-kbd-macro paredit-backward-delete-key) nil))
;; (add-hook 'slime-repl-mode-hook 'my-override-slime-repl-bindings-with-paredit)


(define-key paredit-mode-map (kbd "RET") nil)
(define-key paredit-mode-map (kbd "C-j") 'paredit-newline)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'inferior-emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'racket-mode-hook 'paredit-mode)
(add-hook 'hy-mode-hook 'paredit-mode)

;;  (add-hook 'slime-repl-mode-hook 'my-override-slime-repl-bindings-with-paredit)


(defun my-fix-paredit ()
  (interactive)
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))

(add-hook 'lisp-mode-hook 'my-add-lisp-keywords)
(add-hook 'lisp-mode-hook 'company-mode)
(add-hook 'lisp-mode-hook (lambda () (local-set-key (kbd "C-c I") 'slime-inspect)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Erlang
;;

(use-package edts :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... C++ Development
;;

;; (use-package doxy-graph-mode :ensure t)
;; (require 'doxy-graph-mode)

;; (add-hook 'c-mode-hook 'doxy-graph-mode)
;; (add-hook 'c++-mode-hook 'doxy-graph-mode)
;; (add-hook 'python-mode-hook 'doxy-graph-mode)

(use-package modern-cpp-font-lock :ensure t)
;; (use-package company-c-headers :ensure t)
;; (use-package cpp-auto-include :ensure t)
;; (use-package cpputils-cmake :ensure t) ; this probably doesn't work
;; (use-package demangle-mode :ensure t) ;
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
   (c-basic-offset . 2)
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

(require 'el-get-bundle)
(el-get-bundle cmake-cache-mode
  :url "https://github.com/sabjohnso/cmake-cache-mode.git"
  :features cmake-cache-mode)

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
;; (add-hook 'compilation-mode-hook 'my-add-compilation-keywords)
;; (use-package cmake-ide :ensure t :pin melpa)
;; (use-package rtags :ensure t :pin melpa)
;; (use-package flycheck-rtags :ensure t :pin melpa)
;; (use-package company-rtags :ensure t :pin melpa)
(use-package clang-format :ensure t :pin melpa)
(add-to-list 'auto-mode-alist '("\\.clang-format" . yaml-mode))

(with-eval-after-load 'cmake-font-lock
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
  (add-to-list 'cmake-font-lock-function-keywords-alist
	       '("parameter"  .
		 ("NAME" "DEFAULT" "TYPE" "DOC")))
  (add-to-list 'cmake-font-lock-function-keywords-alist
	       '("git_resolvable_dependency" .
		 ("NAME" "GIT_REPOSITORY" "GIT_TAG")))
  (add-to-list 'cmake-font-lock-function-keywords-alist
               '("FetchContent_Declare" .
                 ("EXCLUDE_FROM_ALL"
                  "SYSTEM"
                  "GIT_REPOSITORY"
                  "GIT_TAG"))))

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
   '(("\\b\\([0-9A-Z_]+\\)\\b" 1 font-lock-constant-face))))

;; Add Highlights and Links in `compilation-mode'
;; ==============================================
;;
;; `compilation-mode' has a somewhat two allists that are used to control highlighting and linking
;; to errors in compilation mode (`compilation-error-regexp-alist' and `compilation-error-regexp-alist-alist').
;; `compilation-error-regexp-alist' is primarily a list of symbols (not an alist).  Those symbols
;; are keys for `compilation-error-regexp-alist-alist', which is actually an alist.
;;
;; Presumably the intent is to allow one to store patterns in `compilation-error-regexp-alist-alist' and
;; select them easily in `compilation-error-regexp-alist', where and initial implementation probably
;; had them in `compilation-error-regexp-alist'.  Note that the patterns can still be put in
;; `compilation-error-regexp-alist'.
;;
;; Each element in `compilation-error-regexp-alist-alist' has the form
;; (NAME REGEXP FILE [LINE COLUMN TYPE HYPERLINK] [HIGHLIGHTS ...]) see the documentation for
;; an explanation, but `FILE', `LINE' and `COLUMN' are the indices of the match of the regular expression.
;; Remember that 0 is for the whole pattern and the first group starts at 1.
;;
;; Add a pattern for assertion failures that matches for g++ and clang++
(add-to-list
 'compilation-error-regexp-alist-alist
 '(clang-address-sanitizer
   "#[0-9]+ 0x\\(?:[0-9a-f]+\\) in .*? \\(/.*?\\):\\([0-9]+\\):\\([0-9]+\\)$"
   1 2 3 1))

(add-to-list 'compilation-error-regexp-alist-alist
 '(gnu-assertion
   "^\\b[^:]+:\\s +\\([^:]*\\):\\([0-9]+\\):\\s +\\(.*+?\\):\\([0-9]*\\) Assertion `\\(.*?\\)' failed."
   1 2 3 2 1))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(rackunit
   "location:\s +\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   1 2 3 2 1))

(add-to-list 'compilation-error-regexp-alist-alist
             '(path-and-line-number
               "[\n][\t ]+\\([^: ]+\\):\\([0-9]+\\):"
   1 2 nil 0 1))

(add-to-list 'compilation-error-regexp-alist 'gnu-assertion )
(add-to-list 'compilation-error-regexp-alist 'path-and-line-number)
(add-to-list 'compilation-error-regexp-alist 'rackunit)
(add-to-list 'compilation-error-regexp-alist 'clang-address-sanitizer)

;; (defun my-add-c++-macro-highlights ()
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil
;;    '(("\\b\\([$]\\w+\\)" 1 font-lock-warning-face prepend)
;;      ("\\b\\([A-Z_][A-Z_]+\\)\\b" 1 font-lock-warning-face))))

(defun my-c++-catch2-keywords ()
  "Return a list of keywords to add for Catch2 highlighting"
  '(TEST_CASE
    SCENARIO
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
    SKIP
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
(require 'clang-format)
(defun my-clang-format-on-save ()
  (when (and my-clang-format-on-save-enabled (eq major-mode 'c++-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'my-clang-format-on-save)
;; (add-hook 'c++-mode-hook 'clang-format+-mode)
(add-hook 'c++-mode-hook 'auto-complete-mode)



(defun my-c++-mode-hooks ()
  "Hook the following functions when c++-mode is started"
  (c-set-style "my")
  (display-line-numbers-mode)
  (my-add-c++-fixme-highlights)
  (my-add-c++-macro-highlights)
  (my-add-c++-catch2-keywords)
  (local-set-key (kbd "<f12>") 'clang-format-buffer))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "my")
            (display-line-numbers-mode)
            (my-add-c++-fixme-highlights)
            (my-add-c++-macro-highlights)
            (my-add-c++-catch2-keywords)
            (local-set-key (kbd "<f12>") 'clang-format-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Go
;;
(use-package go-mode :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Rust
;;
(use-package cargo :ensure t :pin melpa)
(use-package cargo-mode :ensure t :pin melpa)
(use-package rust-mode :ensure t :pin melpa)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Python
;;
(add-to-list 'auto-mode-alist
             '("poetry.lock" . toml-mode))

(use-package pipenv :ensure t :pin melpa)
(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package jedi :ensure t :pin melpa)
(use-package company-jedi :ensure t :pin melpa)
(use-package elpy :ensure t :pin melpa)
(use-package python-black :ensure t :pin melpa
  :hook (python-mode . python-black-on-save-mode))
(use-package flymake-python-pyflakes :ensure t :pin melpa)
;; (use-package jedi-direx :ensure t :pin melpa)
(add-hook 'python-mode-hook 'flycheck-mode)



(defun ipython ()
  "Start an ipython shell in a terminal-emulator in a new buffer
The buffer is in Term mode; see `term-mode' for the
commands to use in that buffer."
  (interactive)
  (let ((prog "ipython"))
    (set-buffer (apply #'make-term prog prog nil nil))
    (pop-to-buffer-same-window (concat "*" prog "*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... OCaml
;;

(use-package utop :ensure t :pin melpa)
(use-package dune :ensure t :pin melpa)
(use-package dune-format :ensure t :pin melpa)
(use-package merlin :ensure t :pin melpa)
(use-package merlin-company :ensure t :pin melpa)
(use-package merlin-eldoc :ensure t :pin melpa)
(use-package merlin-iedit :ensure t :pin melpa)
;; (use-package ocaml-format :ensure t :pin melpa)
(use-package opam-switch-mode :ensure t :pin melpa)
(use-package tuareg :ensure t :pin melpa)

(require 'merlin)

(add-hook 'tuareg-mode-hook (lambda () (merlin-mode)))
(add-hook 'caml-mode-hook (lambda () (merlin-mode)))
(add-hook 'tuareg-mode-hook (lambda () (paredit-mode)))
(add-hook 'dune-mode-hook (lambda () (paredit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Shells
;;
(use-package powershell :ensure t :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Text Formats
;;

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
;;
;; ... Grammars
;;

(use-package bnf-mode :ensure t :pin melpa)
(use-package ebnf-mode :ensure t :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Text and documentation
;;

(use-package markdown-mode :ensure t :pin melpa)
(use-package markdown-preview-eww :ensure t :pin melpa)
(use-package markdown-preview-mode :ensure t :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Solid Modeling
;;

(use-package scad-mode :ensure t :pin melpa)
(add-hook 'scad-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Environment Modules
;;

(load "/usr/local/Modules/init/lisp")
(add-to-list 'magic-mode-alist '("#%Module.*" . tcl-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... Bullshit
;;

(use-package chatgpt-shell :ensure t :pin melpa)
(with-eval-after-load 'chatgpt-shell
  (setq chatgpt-shell-openai-key "xxxx"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... My Stuff
;;

(require 'my-closet)
(require 'my-racket-extras)
(require 'my-common-lisp-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-init)
