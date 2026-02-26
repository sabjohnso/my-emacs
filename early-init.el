;;; early-init.el --- Pre-frame startup optimizations -*- lexical-binding: t -*-

;; Raise GC threshold during init to avoid collections.
;; Restored to a reasonable value after startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Temporarily disable file-name-handler-alist for faster file loading.
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore defaults after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)) ; 16 MB
            (setq file-name-handler-alist my--file-name-handler-alist)))

;; We manage packages ourselves via use-package; prevent double init.
(setq package-enable-at-startup nil)

;; Suppress frame decorations early so Emacs never draws then removes them.
(tool-bar-mode 0)
(scroll-bar-mode 0)

(provide 'early-init)
;;; early-init.el ends here
