;;; test-my-themes.el --- ERT tests for my-light and my-dark themes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Structural tests for my-light, my-dark, my-light-256, and my-dark-256 themes.
;; Run with: emacs --batch -L . -l test-my-themes.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'my-theme-palette)

;; Ensure the current directory is on custom-theme-load-path so
;; load-theme can discover my-light and my-dark in batch mode.
(add-to-list 'custom-theme-load-path
             (file-name-directory (or load-file-name buffer-file-name
                                     default-directory)))

;;; --- All known variants ---

(defvar my-theme-test--all-variants
  '(light dark light-256 dark-256 solarized-dark gruvbox-dark dracula nord)
  "All palette variants that must be supported.")

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

(ert-deftest my-theme-palette-light-256-returns-alist ()
  "Light-256 palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'light-256)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-palette-dark-256-returns-alist ()
  "Dark-256 palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'dark-256)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-palette-symmetry ()
  "All palette variants have identical key sets."
  (let ((reference-keys (sort (mapcar #'car (my-theme-palette 'light)) #'string<)))
    (dolist (variant (cdr my-theme-test--all-variants))
      (let ((keys (sort (mapcar #'car (my-theme-palette variant)) #'string<)))
        (should (equal reference-keys keys))))))

(ert-deftest my-theme-palette-all-values-are-strings ()
  "Every palette value is a color string."
  (dolist (variant my-theme-test--all-variants)
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
  "All palettes contain all OCaml face keys."
  (dolist (variant my-theme-test--all-variants)
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

(ert-deftest my-theme-light-256-loads-without-error ()
  "my-light-256 theme loads without error."
  (load-theme 'my-light-256 t)
  (should (memq 'my-light-256 custom-enabled-themes))
  (disable-theme 'my-light-256))

(ert-deftest my-theme-dark-256-loads-without-error ()
  "my-dark-256 theme loads without error."
  (load-theme 'my-dark-256 t)
  (should (memq 'my-dark-256 custom-enabled-themes))
  (disable-theme 'my-dark-256))

(ert-deftest my-theme-light-256-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading light-256 theme works cleanly."
  (load-theme 'my-light-256 t)
  (should (memq 'my-light-256 custom-enabled-themes))
  (disable-theme 'my-light-256)
  (should-not (memq 'my-light-256 custom-enabled-themes))
  (load-theme 'my-light-256 t)
  (should (memq 'my-light-256 custom-enabled-themes))
  (disable-theme 'my-light-256))

(ert-deftest my-theme-dark-256-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading dark-256 theme works cleanly."
  (load-theme 'my-dark-256 t)
  (should (memq 'my-dark-256 custom-enabled-themes))
  (disable-theme 'my-dark-256)
  (should-not (memq 'my-dark-256 custom-enabled-themes))
  (load-theme 'my-dark-256 t)
  (should (memq 'my-dark-256 custom-enabled-themes))
  (disable-theme 'my-dark-256))

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

;;; --- 256-color theme face existence tests ---

(ert-deftest my-theme-light-256-defines-tuareg-faces ()
  "my-light-256 defines all tuareg faces."
  (load-theme 'my-light-256 t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-light-256)))
    (disable-theme 'my-light-256)))

(ert-deftest my-theme-dark-256-defines-tuareg-faces ()
  "my-dark-256 defines all tuareg faces."
  (load-theme 'my-dark-256 t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-dark-256)))
    (disable-theme 'my-dark-256)))

(ert-deftest my-theme-light-256-defines-merlin-faces ()
  "my-light-256 defines all merlin faces."
  (load-theme 'my-light-256 t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-light-256)))
    (disable-theme 'my-light-256)))

(ert-deftest my-theme-dark-256-defines-merlin-faces ()
  "my-dark-256 defines all merlin faces."
  (load-theme 'my-dark-256 t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-dark-256)))
    (disable-theme 'my-dark-256)))

(ert-deftest my-theme-light-256-defines-utop-faces ()
  "my-light-256 defines all utop faces."
  (load-theme 'my-light-256 t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-light-256)))
    (disable-theme 'my-light-256)))

(ert-deftest my-theme-dark-256-defines-utop-faces ()
  "my-dark-256 defines all utop faces."
  (load-theme 'my-dark-256 t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-dark-256)))
    (disable-theme 'my-dark-256)))

;;; --- Solarized Dark theme tests ---

(ert-deftest my-theme-palette-solarized-dark-returns-alist ()
  "Solarized-dark palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'solarized-dark)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-solarized-dark-loads-without-error ()
  "my-solarized-dark theme loads without error."
  (load-theme 'my-solarized-dark t)
  (should (memq 'my-solarized-dark custom-enabled-themes))
  (disable-theme 'my-solarized-dark))

(ert-deftest my-theme-solarized-dark-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading solarized-dark theme works cleanly."
  (load-theme 'my-solarized-dark t)
  (should (memq 'my-solarized-dark custom-enabled-themes))
  (disable-theme 'my-solarized-dark)
  (should-not (memq 'my-solarized-dark custom-enabled-themes))
  (load-theme 'my-solarized-dark t)
  (should (memq 'my-solarized-dark custom-enabled-themes))
  (disable-theme 'my-solarized-dark))

(ert-deftest my-theme-solarized-dark-defines-tuareg-faces ()
  "my-solarized-dark defines all tuareg faces."
  (load-theme 'my-solarized-dark t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-solarized-dark)))
    (disable-theme 'my-solarized-dark)))

(ert-deftest my-theme-solarized-dark-defines-merlin-faces ()
  "my-solarized-dark defines all merlin faces."
  (load-theme 'my-solarized-dark t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-solarized-dark)))
    (disable-theme 'my-solarized-dark)))

(ert-deftest my-theme-solarized-dark-defines-utop-faces ()
  "my-solarized-dark defines all utop faces."
  (load-theme 'my-solarized-dark t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-solarized-dark)))
    (disable-theme 'my-solarized-dark)))

;;; --- Gruvbox Dark theme tests ---

(ert-deftest my-theme-palette-gruvbox-dark-returns-alist ()
  "Gruvbox-dark palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'gruvbox-dark)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-gruvbox-dark-loads-without-error ()
  "my-gruvbox-dark theme loads without error."
  (load-theme 'my-gruvbox-dark t)
  (should (memq 'my-gruvbox-dark custom-enabled-themes))
  (disable-theme 'my-gruvbox-dark))

(ert-deftest my-theme-gruvbox-dark-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading gruvbox-dark theme works cleanly."
  (load-theme 'my-gruvbox-dark t)
  (should (memq 'my-gruvbox-dark custom-enabled-themes))
  (disable-theme 'my-gruvbox-dark)
  (should-not (memq 'my-gruvbox-dark custom-enabled-themes))
  (load-theme 'my-gruvbox-dark t)
  (should (memq 'my-gruvbox-dark custom-enabled-themes))
  (disable-theme 'my-gruvbox-dark))

(ert-deftest my-theme-gruvbox-dark-defines-tuareg-faces ()
  "my-gruvbox-dark defines all tuareg faces."
  (load-theme 'my-gruvbox-dark t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-gruvbox-dark)))
    (disable-theme 'my-gruvbox-dark)))

(ert-deftest my-theme-gruvbox-dark-defines-merlin-faces ()
  "my-gruvbox-dark defines all merlin faces."
  (load-theme 'my-gruvbox-dark t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-gruvbox-dark)))
    (disable-theme 'my-gruvbox-dark)))

(ert-deftest my-theme-gruvbox-dark-defines-utop-faces ()
  "my-gruvbox-dark defines all utop faces."
  (load-theme 'my-gruvbox-dark t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-gruvbox-dark)))
    (disable-theme 'my-gruvbox-dark)))

;;; --- Dracula theme tests ---

(ert-deftest my-theme-palette-dracula-returns-alist ()
  "Dracula palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'dracula)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-dracula-loads-without-error ()
  "my-dracula theme loads without error."
  (load-theme 'my-dracula t)
  (should (memq 'my-dracula custom-enabled-themes))
  (disable-theme 'my-dracula))

(ert-deftest my-theme-dracula-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading dracula theme works cleanly."
  (load-theme 'my-dracula t)
  (should (memq 'my-dracula custom-enabled-themes))
  (disable-theme 'my-dracula)
  (should-not (memq 'my-dracula custom-enabled-themes))
  (load-theme 'my-dracula t)
  (should (memq 'my-dracula custom-enabled-themes))
  (disable-theme 'my-dracula))

(ert-deftest my-theme-dracula-defines-tuareg-faces ()
  "my-dracula defines all tuareg faces."
  (load-theme 'my-dracula t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-dracula)))
    (disable-theme 'my-dracula)))

(ert-deftest my-theme-dracula-defines-merlin-faces ()
  "my-dracula defines all merlin faces."
  (load-theme 'my-dracula t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-dracula)))
    (disable-theme 'my-dracula)))

(ert-deftest my-theme-dracula-defines-utop-faces ()
  "my-dracula defines all utop faces."
  (load-theme 'my-dracula t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-dracula)))
    (disable-theme 'my-dracula)))

;;; --- Nord theme tests ---

(ert-deftest my-theme-palette-nord-returns-alist ()
  "Nord palette returns a non-empty alist."
  (let ((palette (my-theme-palette 'nord)))
    (should (listp palette))
    (should (> (length palette) 0))
    (should (consp (car palette)))))

(ert-deftest my-theme-nord-loads-without-error ()
  "my-nord theme loads without error."
  (load-theme 'my-nord t)
  (should (memq 'my-nord custom-enabled-themes))
  (disable-theme 'my-nord))

(ert-deftest my-theme-nord-load-disable-reload-roundtrip ()
  "Loading, disabling, and reloading nord theme works cleanly."
  (load-theme 'my-nord t)
  (should (memq 'my-nord custom-enabled-themes))
  (disable-theme 'my-nord)
  (should-not (memq 'my-nord custom-enabled-themes))
  (load-theme 'my-nord t)
  (should (memq 'my-nord custom-enabled-themes))
  (disable-theme 'my-nord))

(ert-deftest my-theme-nord-defines-tuareg-faces ()
  "my-nord defines all tuareg faces."
  (load-theme 'my-nord t)
  (unwind-protect
      (dolist (face my-theme-test--tuareg-faces)
        (should (my-theme-test--face-themed-p face 'my-nord)))
    (disable-theme 'my-nord)))

(ert-deftest my-theme-nord-defines-merlin-faces ()
  "my-nord defines all merlin faces."
  (load-theme 'my-nord t)
  (unwind-protect
      (dolist (face my-theme-test--merlin-faces)
        (should (my-theme-test--face-themed-p face 'my-nord)))
    (disable-theme 'my-nord)))

(ert-deftest my-theme-nord-defines-utop-faces ()
  "my-nord defines all utop faces."
  (load-theme 'my-nord t)
  (unwind-protect
      (dolist (face my-theme-test--utop-faces)
        (should (my-theme-test--face-themed-p face 'my-nord)))
    (disable-theme 'my-nord)))

(provide 'test-my-themes)
;;; test-my-themes.el ends here
