;;; setup-emacs.ele
;;

(require 'package)
(package-initialize)


(setq package-archivese
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name "~/my-emacs"))

;; (use-package el-get :ensure t)
(require 'org)
(use-package org-tempo :demand t)
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;;(use-package pcre2el :ensure t)
(use-package sr-speedbar :ensure t )
(use-package graphviz-dot-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package direnv :ensure t)
(use-package rtags :ensure t)
(use-package company-rtags :ensure t)
(use-package highlight-thing :ensure t)
(use-package paredit-everywhere :ensure t)
(use-package paredit-menu :ensure t)
(use-package flycheck-projectile :ensure t)
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)
(use-package magit :ensure t)
(use-package git-modes :ensure t)
(use-package projectile
  :ensure t

  :bind-keymap
  ("C-c p" . projectile-command-map))
(use-package projectile-speedbar :ensure t)
(use-package plantuml-mode :ensure t)
(use-package mermaid-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package ac-haskell-process :ensure t)
(use-package flycheck-haskell :ensure t)
(use-package hindent :ensure t)
(use-package lsp-haskell :ensure t)
(use-package ormolu :ensure t)
;; (use-package julia-repl :ensure t)
(use-package company :ensure t)
(use-package racket-mode :ensure t)
(use-package scribble-mode :ensure t)
(use-package hy-mode :ensure t)

(use-package clojure-mode :ensure t)
(use-package cider :ensure t)

(use-package geiser-racket :ensure t)
(use-package geiser-chibi :ensure t)
(use-package geiser-chez :ensure t)
(use-package geiser-chicken :ensure t)
(use-package geiser-gambit :ensure t)
(use-package geiser-gauche :ensure t)
(use-package geiser-guile :ensure t)
(use-package geiser-kawa :ensure t)

(use-package paredit :ensure t)
(use-package edts :ensure t)
(use-package modern-cpp-font-lock :ensure t)
(use-package irony :ensure t)
(use-package highlight-doxygen :ensure t)
(use-package compiler-explorer :ensure t)
(use-package pickle :ensure t)
(use-package cmake-mode :ensure t)
(use-package cmake-font-lock :ensure t)
(use-package clang-format :ensure t)

(use-package go-mode :ensure t)

(use-package cargo :ensure t)
(use-package cargo-mode :ensure t)
(use-package rust-mode :ensure t)

(use-package pipenv :ensure t)
(use-package pipenv :hook (python-mode . pipenv-mode))

(use-package jedi :ensure t)
(use-package company-jedi :ensure t)
(use-package elpy :ensure t)
(use-package python-black :ensure t
  :hook (python-mode . python-black-on-save-mode))
(use-package flymake-python-pyflakes :ensure t)
;; (use-package jedi-direx :ensure t)
(add-hook 'python-mode-hook 'flycheck-mode)
(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(use-package lazy-ruff
  :ensure t
  :bind (("<f12>" . lazy-ruff-lint-format-dwim)) ;; keybinding
  :config (lazy-ruff-global-mode t))

(use-package utop :ensure t)
(use-package dune :ensure t)
(use-package dune-format :ensure t)
(use-package merlin :ensure t)
(use-package merlin-company :ensure t)
(use-package merlin-eldoc :ensure t)
(use-package merlin-iedit :ensure t)
;; (use-package ocaml-format :ensure t)
(use-package opam-switch-mode :ensure t)
(use-package tuareg :ensure t)
(use-package powershell :ensure t)

(use-package json-mode :ensure t)
(use-package json-par :ensure t)
(use-package toml-mode :ensure t)
(use-package yaml-mode :ensure t)


(use-package bnf-mode :ensure t)
(use-package ebnf-mode :ensure t)

(use-package markdown-mode :ensure t)

;; (use-package markdown-preview-eww :ensure t)
;; (use-package markdown-preview-mode :ensure t)

;; (use-package scad-mode :ensure t)

(use-package gptel :ensure t)
