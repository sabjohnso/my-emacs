;;; my-dune-extras.el --- Extra Support for Dune -*- lexical-binding: t -*-
(use-package pcre2el :ensure t :pin melpa)
(require 'pcre2el)



(defun my-regex-opt-symbols (symbols)
  (regex-opt (mapcar #'symbol-name symbols)))

(defun my-dune-extras-project-introducers ()
  '(name
    generate-opam-files
    source
    authors
    maintainers
    license
    documentation
    description
    depends
    tags))

(defun my-dune-extras-stanza-introducers ()
  '(library
    executable
    executables
    rule))


(defun my-dune-extras-library ()
  '(name
    public_name
    package
    synopsis
    modules
    libraries
    wrapped
    preprocess
    preprocessor_deps
    optional
    foreign_stubs
    foreign_archives
    install_c_headers
    public_headers
    modes
    no_dynlink
    kind
    ppx_runtime_libraries
    virtual_deps
    implements
    parameters
    js_of_ocaml
    wasm_of_ocaml
    flags
    ocamlc_flags
    ocamlopt_flags
    library_flags
    c_library_flags
    modules_without_implementation
    private_modules
    allow_overlapping_dependencies
    enable_if
    inline_tests
    root_module
    ctypes
    empty_module_interface_if_absent))
