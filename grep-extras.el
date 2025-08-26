;;; my-utilities.el -*- lexical-binding: t -*-
(require 'grep)

(defvar my-grep-cmake-history nil "History list for grep-cmake")
(defvar my-grep-cxx-history nil "History list for grep-cxx")

(defun my-grep-cmake ()
  "Run grep via find for files matching the names of cmake files"
  (interactive
   (progn
     (grep-compute-defaults)
     (if grep-find-command
         (list (read-shell-command
                "Run find (list this): "
                grep-find-command 'grep-cmake-history))
       ;; No Default was set
       (read-string "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when command-args
    (let ((null-device nil))
      (grep command-args))))

(provide 'my-utilities)
