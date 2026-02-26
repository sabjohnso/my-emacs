;;; test-my-themes.el --- ERT tests for my-light and my-dark themes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Structural tests for my-light-theme and my-dark-theme.
;; Run with: emacs --batch -L . -l test-my-themes.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'my-theme-palette)

;; Ensure the current directory is on custom-theme-load-path so
;; load-theme can discover my-light and my-dark in batch mode.
(add-to-list 'custom-theme-load-path
             (file-name-directory (or load-file-name buffer-file-name
                                     default-directory)))

;;; --- Palette tests ---

(ert-deftest my-theme-palette-light-returns-alist ()
  "Light palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'light)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-palette-dark-returns-alist ()
  "Dark palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'dark)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-palette-symmetry ()
  "Light and dark palettes have identical key sets."
  (let ((light-keys (sort (mapcar #'car (my-theme-palette 'light)) #'string<))
        (dark-keys  (sort (mapcar #'car (my-theme-palette 'dark))  #'string<)))
    (should (equal light-keys dark-keys))))

(ert-deftest my-theme-palette-all-values-are-strings ()
  "Every palette value is a color string."
  (dolist (variant '(light dark))
    (dolist (entry (my-theme-palette variant))
      (should (stringp (cdr entry))))))

(ert-deftest my-theme-palette-invalid-variant-errors ()
  "Requesting an unknown variant signals an error."
  (should-error (my-theme-palette 'sepia)))

;;; --- OCaml face key coverage ---

(defvar my-theme-test--ocaml-face-keys
  '(tuareg-governing tuareg-multistage tuareg-operator tuareg-module
    tuareg-constructor tuareg-label tuareg-double-semi tuareg-error
    tuareg-interactive-output tuareg-interactive-error
    tuareg-interactive-directive tuareg-attribute tuareg-extension-node
    tuareg-doc-markup tuareg-doc-verbatim tuareg-line-number
    tuareg-opam-error tuareg-opam-pkg-variable
    merlin-type merlin-warn merlin-error
    utop-prompt utop-stdout utop-stderr utop-frozen utop-error)
  "All OCaml-related palette keys that must be present.")

(ert-deftest my-theme-palette-ocaml-keys-present ()
  "Both palettes contain all OCaml face keys."
  (dolist (variant '(light dark))
    (let ((keys (mapcar #'car (my-theme-palette variant))))
      (dolist (k my-theme-test--ocaml-face-keys)
        (should (memq k keys))))))

;;; --- Theme loading tests ---

(ert-deftest my-theme-light-loads-without-error ()
  "my-light theme loads without error."
  (load-theme 'my-light t)
  (should (memq 'my-light custom-enabled-themes))
  (disable-theme 'my-light))

(ert-deftest my-theme-dark-loads-without-error ()
  "my-dark theme loads without error."
  (load-theme 'my-dark t)
  (should (memq 'my-dark custom-enabled-themes))
  (disable-theme 'my-dark))

(ert-deftest my-theme-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading a theme works cleanly."
  (load-theme 'my-light t)
  (should (memq 'my-light custom-enabled-themes))
  (disable-theme 'my-light)
  (should-not (memq 'my-light custom-enabled-themes))
  (load-theme 'my-light t)
  (should (memq 'my-light custom-enabled-themes))
  (disable-theme 'my-light))

(ert-deftest my-theme-dark-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading dark theme works cleanly."
  (load-theme 'my-dark t)
  (should (memq 'my-dark custom-enabled-themes))
  (disable-theme 'my-dark)
  (should-not (memq 'my-dark custom-enabled-themes))
  (load-theme 'my-dark t)
  (should (memq 'my-dark custom-enabled-themes))
  (disable-theme 'my-dark))

;;; --- Theme face existence tests ---

(defvar my-theme-test--tuareg-faces
  '(tuareg-font-lock-governing-face
    tuareg-font-lock-multistage-face
    tuareg-font-lock-line-number-face
    tuareg-font-lock-operator-face
    tuareg-font-lock-module-face
    tuareg-font-lock-constructor-face
    tuareg-font-lock-label-face
    tuareg-font-double-semicolon-face
    tuareg-font-lock-error-face
    tuareg-font-lock-interactive-output-face
    tuareg-font-lock-interactive-error-face
    tuareg-font-lock-interactive-directive-face
    tuareg-font-lock-attribute-face
    tuareg-font-lock-infix-extension-node-face
    tuareg-font-lock-extension-node-face
    tuareg-font-lock-doc-markup-face
    tuareg-font-lock-doc-verbatim-face
    tuareg-opam-error-face
    tuareg-opam-pkg-variable-name-face)
  "All tuareg faces that both themes must define.")

(defvar my-theme-test--merlin-faces
  '(merlin-type-face
    merlin-compilation-warning-face
    merlin-compilation-error-face)
  "All merlin faces that both themes must define.")

(defvar my-theme-test--utop-faces
  '(utop-prompt utop-stdout utop-stderr utop-frozen utop-error)
  "All utop faces that both themes must define.")

(defun my-theme-test--face-themed-p (face theme)
  "Return non-nil if FACE has a spec from THEME."
  (let ((spec (get face 'theme-face)))
    (and spec (assq theme spec))))

(ert-deftest my-theme-light-defines-tuareg-faces ()
  "my-light defines all tuareg faces."
  (load-theme 'my-light t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-light)))
    (disable-theme 'my-light)))

(ert-deftest my-theme-dark-defines-tuareg-faces ()
  "my-dark defines all tuareg faces."
  (load-theme 'my-dark t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-dark)))
    (disable-theme 'my-dark)))

(ert-deftest my-theme-light-defines-merlin-faces ()
  "my-light defines all merlin faces."
  (load-theme 'my-light t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-light)))
    (disable-theme 'my-light)))

(ert-deftest my-theme-dark-defines-merlin-faces ()
  "my-dark defines all merlin faces."
  (load-theme 'my-dark t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-dark)))
    (disable-theme 'my-dark)))

(ert-deftest my-theme-light-defines-utop-faces ()
  "my-light defines all utop faces."
  (load-theme 'my-light t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-light)))
    (disable-theme 'my-light)))

(ert-deftest my-theme-dark-defines-utop-faces ()
  "my-dark defines all utop faces."
  (load-theme 'my-dark t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-dark)))
    (disable-theme 'my-dark)))

(provide 'test-my-themes)
;;; test-my-themes.el ends here
