;;; test-my-gptel.el --- ERT tests for my-gptel -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for my-gptel.el: naming helpers, keyword conversion, arg-spec
;; predicates, partitioning, doc-string generation, buffer predicates,
;; tool registry, and macro expansion.
;;
;; Run with: make test-gptel

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'my-gptel)

;;; --- Naming helpers ---

(ert-deftest my-gptel-test-make-tool-fn-name ()
  "my/gptel--make-tool-fn-name produces my/gptel-<name>."
  (should (eq (my/gptel--make-tool-fn-name 'foo) 'my/gptel-foo))
  (should (eq (my/gptel--make-tool-fn-name 'buffer-read) 'my/gptel-buffer-read)))

(ert-deftest my-gptel-test-make-tool-fn-name-is-symbol ()
  "my/gptel--make-tool-fn-name always returns a symbol."
  (should (symbolp (my/gptel--make-tool-fn-name 'x))))

(ert-deftest my-gptel-test-make-tool-fn-name-rejects-non-symbol ()
  "my/gptel--make-tool-fn-name signals on non-symbol input."
  (should-error (my/gptel--make-tool-fn-name "foo")))

(ert-deftest my-gptel-test-make-tool-obj-name ()
  "my/gptel--make-tool-obj-name produces my/gptel-<name>-tool."
  (should (eq (my/gptel--make-tool-obj-name 'foo) 'my/gptel-foo-tool))
  (should (eq (my/gptel--make-tool-obj-name 'buffer-read) 'my/gptel-buffer-read-tool)))

(ert-deftest my-gptel-test-make-tool-obj-name-is-symbol ()
  "my/gptel--make-tool-obj-name always returns a symbol."
  (should (symbolp (my/gptel--make-tool-obj-name 'x))))

(ert-deftest my-gptel-test-make-tool-obj-name-rejects-non-symbol ()
  "my/gptel--make-tool-obj-name signals on non-symbol input."
  (should-error (my/gptel--make-tool-obj-name "foo")))

(ert-deftest my-gptel-test-fn-and-obj-names-differ ()
  "Function name and object name for the same tool are distinct."
  (should-not (eq (my/gptel--make-tool-fn-name 'foo)
                  (my/gptel--make-tool-obj-name 'foo))))

;;; --- Keyword conversion ---

(ert-deftest my-gptel-test-symbol-to-keyword ()
  "my/gptel--symbol-to-keyword converts symbol to keyword."
  (should (eq (my/gptel--symbol-to-keyword 'foo) :foo))
  (should (eq (my/gptel--symbol-to-keyword 'bar) :bar)))

(ert-deftest my-gptel-test-symbol-to-keyword-rejects-keyword ()
  "my/gptel--symbol-to-keyword rejects keyword input."
  (should-error (my/gptel--symbol-to-keyword :foo)))

(ert-deftest my-gptel-test-string-to-keyword ()
  "my/gptel--string-to-keyword converts string to keyword."
  (should (eq (my/gptel--string-to-keyword "foo") :foo))
  (should (eq (my/gptel--string-to-keyword "buffer-name") :buffer-name)))

(ert-deftest my-gptel-test-string-to-keyword-rejects-non-string ()
  "my/gptel--string-to-keyword rejects non-string input."
  (should-error (my/gptel--string-to-keyword 'foo)))

;;; --- Arg-spec predicates ---

(ert-deftest my-gptel-test-optional-arg-spec-p-true ()
  "Arg-spec with :optional non-nil is optional."
  (should (my/gptel--optional-arg-spec-p
           '(:name "x" :type "string" :description "desc" :optional t))))

(ert-deftest my-gptel-test-optional-arg-spec-p-false ()
  "Arg-spec without :optional is not optional."
  (should-not (my/gptel--optional-arg-spec-p
               '(:name "x" :type "string" :description "desc"))))

(ert-deftest my-gptel-test-optional-arg-spec-p-explicit-nil ()
  "Arg-spec with :optional nil is not optional."
  (should-not (my/gptel--optional-arg-spec-p
               '(:name "x" :type "string" :description "desc" :optional nil))))

(ert-deftest my-gptel-test-required-before-optional-all-required ()
  "All required args satisfies required-before-optional."
  (should (my/gptel--required-before-optional-p
           '((:name "a" :type "string" :description "d")
             (:name "b" :type "string" :description "d")))))

(ert-deftest my-gptel-test-required-before-optional-all-optional ()
  "All optional args satisfies required-before-optional."
  (should (my/gptel--required-before-optional-p
           '((:name "a" :type "string" :description "d" :optional t)
             (:name "b" :type "string" :description "d" :optional t)))))

(ert-deftest my-gptel-test-required-before-optional-correct-order ()
  "Required then optional satisfies required-before-optional."
  (should (my/gptel--required-before-optional-p
           '((:name "a" :type "string" :description "d")
             (:name "b" :type "string" :description "d" :optional t)))))

(ert-deftest my-gptel-test-required-before-optional-wrong-order ()
  "Optional then required violates required-before-optional."
  (should-not (my/gptel--required-before-optional-p
               '((:name "a" :type "string" :description "d" :optional t)
                 (:name "b" :type "string" :description "d")))))

(ert-deftest my-gptel-test-required-before-optional-empty ()
  "Empty arg list satisfies required-before-optional."
  (should (my/gptel--required-before-optional-p nil)))

;;; --- Lambda list predicate ---

(ert-deftest my-gptel-test-lambda-list-p-plist ()
  "A plist is a valid lambda list."
  (should (my/gptel--lambda-list-p '(:name "x" :type "string"))))

(ert-deftest my-gptel-test-lambda-list-p-nil ()
  "nil is a valid lambda list (empty plist)."
  (should (my/gptel--lambda-list-p nil)))

;;; --- Arg-spec partitioning ---

(ert-deftest my-gptel-test-partition-all-required ()
  "Partitioning all-required args puts them in the first list."
  (let ((specs '((:name "a" :type "string" :description "d")
                 (:name "b" :type "string" :description "d"))))
    (cl-destructuring-bind (req opt) (my/gptel--partition-arg-specs specs)
      (should (equal req '(a b)))
      (should (null opt)))))

(ert-deftest my-gptel-test-partition-all-optional ()
  "Partitioning all-optional args puts them in the second list."
  (let ((specs '((:name "a" :type "string" :description "d" :optional t)
                 (:name "b" :type "string" :description "d" :optional t))))
    (cl-destructuring-bind (req opt) (my/gptel--partition-arg-specs specs)
      (should (null req))
      (should (equal opt '(a b))))))

(ert-deftest my-gptel-test-partition-mixed ()
  "Partitioning mixed args splits required from optional."
  (let ((specs '((:name "x" :type "string" :description "d")
                 (:name "y" :type "integer" :description "d")
                 (:name "z" :type "string" :description "d" :optional t))))
    (cl-destructuring-bind (req opt) (my/gptel--partition-arg-specs specs)
      (should (equal req '(x y)))
      (should (equal opt '(z))))))

(ert-deftest my-gptel-test-partition-empty ()
  "Partitioning empty arg list returns two empty lists."
  (cl-destructuring-bind (req opt) (my/gptel--partition-arg-specs nil)
    (should (null req))
    (should (null opt))))

(ert-deftest my-gptel-test-partition-rejects-bad-order ()
  "Partitioning with optional before required signals an error."
  (let ((specs '((:name "a" :type "string" :description "d" :optional t)
                 (:name "b" :type "string" :description "d"))))
    (should-error (my/gptel--partition-arg-specs specs))))

;;; --- Doc string generation ---

(ert-deftest my-gptel-test-arg-spec-to-doc-string-contains-name ()
  "Doc string for an arg-spec contains the argument name."
  (let* ((spec '(:name "path" :type "string" :description "File path" :optional nil))
         (doc (my/gptel--arg-spec-to-doc-string spec)))
    (should (stringp doc))
    (should (string-match-p "path" doc))))

(ert-deftest my-gptel-test-arg-spec-to-doc-string-contains-type ()
  "Doc string for an arg-spec contains the argument type."
  (let* ((spec '(:name "count" :type "integer" :description "Number of items"))
         (doc (my/gptel--arg-spec-to-doc-string spec)))
    (should (string-match-p "integer" doc))))

(ert-deftest my-gptel-test-arg-spec-to-doc-string-contains-description ()
  "Doc string for an arg-spec contains the description."
  (let* ((spec '(:name "x" :type "string" :description "The input value"))
         (doc (my/gptel--arg-spec-to-doc-string spec)))
    (should (string-match-p "The input value" doc))))

(ert-deftest my-gptel-test-make-arg-specs-doc-combines-specs ()
  "make-arg-specs-doc joins multiple arg-spec docs."
  (let* ((specs '((:name "a" :type "string" :description "first")
                  (:name "b" :type "integer" :description "second")))
         (doc (my/gptel--make-arg-specs-doc specs)))
    (should (stringp doc))
    (should (string-match-p "a" doc))
    (should (string-match-p "b" doc))
    (should (string-match-p "first" doc))
    (should (string-match-p "second" doc))))

(ert-deftest my-gptel-test-make-arg-specs-doc-empty ()
  "make-arg-specs-doc on empty list returns empty string."
  (should (string-empty-p (my/gptel--make-arg-specs-doc nil))))

;;; --- rappend ---

(ert-deftest my-gptel-test-rappend-both-empty ()
  "rappend of two empty lists is empty."
  (should (null (my/gptel--rappend nil nil))))

(ert-deftest my-gptel-test-rappend-first-empty ()
  "rappend with empty first list returns second list."
  (should (equal (my/gptel--rappend nil '(1 2 3)) '(1 2 3))))

(ert-deftest my-gptel-test-rappend-second-empty ()
  "rappend with empty second list returns reversed first."
  (should (equal (my/gptel--rappend '(3 2 1) nil) '(1 2 3))))

(ert-deftest my-gptel-test-rappend-both-nonempty ()
  "rappend prepends reversed first onto second."
  (should (equal (my/gptel--rappend '(a b c) '(d e)) '(c b a d e))))

(ert-deftest my-gptel-test-rappend-single-element ()
  "rappend with single-element lists."
  (should (equal (my/gptel--rappend '(x) '(y)) '(x y))))

(ert-deftest my-gptel-test-rappend-rejects-non-list ()
  "rappend rejects non-list arguments."
  (should-error (my/gptel--rappend 42 nil))
  (should-error (my/gptel--rappend nil "foo")))

;;; --- Buffer predicates ---

(ert-deftest my-gptel-test-live-buffer-name-p-existing ()
  "A live buffer's name returns t."
  (let ((buf (generate-new-buffer " *test-live*")))
    (unwind-protect
        (should (my/gptel--live-buffer-name-p (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest my-gptel-test-live-buffer-name-p-killed ()
  "A killed buffer's name returns nil."
  (let* ((buf (generate-new-buffer " *test-killed*"))
         (name (buffer-name buf)))
    (kill-buffer buf)
    (should-not (my/gptel--live-buffer-name-p name))))

(ert-deftest my-gptel-test-live-buffer-name-p-nonexistent ()
  "A name with no corresponding buffer returns nil."
  (should-not (my/gptel--live-buffer-name-p " *no-such-buffer-exists-12345*")))

(ert-deftest my-gptel-test-live-buffer-name-p-non-string ()
  "A non-string returns nil."
  (should-not (my/gptel--live-buffer-name-p 42))
  (should-not (my/gptel--live-buffer-name-p nil)))

(ert-deftest my-gptel-test-live-file-buffer-name-p-file-buffer ()
  "A buffer visiting a file returns t."
  (let* ((tmp (make-temp-file "test-gptel"))
         (buf (find-file-noselect tmp t)))
    (unwind-protect
        (should (my/gptel--live-file-buffer-name-p (buffer-name buf)))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest my-gptel-test-live-file-buffer-name-p-non-file-buffer ()
  "A buffer not visiting a file returns nil."
  (let ((buf (generate-new-buffer " *test-no-file*")))
    (unwind-protect
        (should-not (my/gptel--live-file-buffer-name-p (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest my-gptel-test-buffer-read-only-p ()
  "Read-only buffer returns t, writable returns nil."
  (let ((buf (generate-new-buffer " *test-ro*")))
    (unwind-protect
        (progn
          (should-not (my/gptel--buffer-read-only-p buf))
          (with-current-buffer buf (setq buffer-read-only t))
          (should (my/gptel--buffer-read-only-p buf)))
      (kill-buffer buf))))

(ert-deftest my-gptel-test-buffer-writable-p ()
  "Writable buffer returns t, read-only returns nil."
  (let ((buf (generate-new-buffer " *test-wr*")))
    (unwind-protect
        (progn
          (should (my/gptel--buffer-writable-p buf))
          (with-current-buffer buf (setq buffer-read-only t))
          (should-not (my/gptel--buffer-writable-p buf)))
      (kill-buffer buf))))

(ert-deftest my-gptel-test-writable-live-buffer-name-p-writable ()
  "A live writable buffer returns t."
  (let ((buf (generate-new-buffer " *test-wl*")))
    (unwind-protect
        (should (my/gptel--writable-live-buffer-name-p (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest my-gptel-test-writable-live-buffer-name-p-read-only ()
  "A live read-only buffer returns nil."
  (let ((buf (generate-new-buffer " *test-wl-ro*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (setq buffer-read-only t))
          (should-not (my/gptel--writable-live-buffer-name-p (buffer-name buf))))
      (kill-buffer buf))))

(ert-deftest my-gptel-test-writable-live-buffer-name-p-dead ()
  "A nonexistent buffer name returns nil."
  (should-not (my/gptel--writable-live-buffer-name-p " *no-such-buf-99999*")))

;;; --- Tool registry ---

(ert-deftest my-gptel-test-make-tool-list-unique-by-name-no-dupes ()
  "Unique tools pass through unchanged."
  (let* ((t1 (gptel-make-tool :name "alpha" :function #'identity :description "a" :args nil))
         (t2 (gptel-make-tool :name "beta" :function #'identity :description "b" :args nil))
         (result (my/gptel--make-tool-list-unique-by-name (list t1 t2))))
    (should (= (length result) 2))
    (should (equal (mapcar #'gptel-tool-name result) '("alpha" "beta")))))

(ert-deftest my-gptel-test-make-tool-list-unique-by-name-first-wins ()
  "When names collide, the first occurrence wins."
  (let* ((t1 (gptel-make-tool :name "dup" :function #'identity :description "first" :args nil))
         (t2 (gptel-make-tool :name "dup" :function #'ignore :description "second" :args nil))
         (result (my/gptel--make-tool-list-unique-by-name (list t1 t2))))
    (should (= (length result) 1))
    (should (equal (gptel-tool-description (car result)) "first"))))

(ert-deftest my-gptel-test-make-tool-list-unique-by-name-empty ()
  "Empty list returns empty list."
  (should (null (my/gptel--make-tool-list-unique-by-name nil))))

(ert-deftest my-gptel-test-register-tool-adds-to-all-tools ()
  "my/gptel-register-tool adds the tool to my/gptel-all-tools."
  (let ((my/gptel-all-tools nil)
        (tool (gptel-make-tool :name "test-reg"
                               :function #'identity
                               :description "test registration"
                               :args nil)))
    (my/gptel-register-tool tool)
    (should (= (length my/gptel-all-tools) 1))
    (should (equal (gptel-tool-name (car my/gptel-all-tools)) "test-reg"))))

(ert-deftest my-gptel-test-register-tool-returns-name ()
  "my/gptel-register-tool returns the tool name."
  (let ((my/gptel-all-tools nil)
        (tool (gptel-make-tool :name "test-ret"
                               :function #'identity
                               :description "d"
                               :args nil)))
    (should (equal (my/gptel-register-tool tool) "test-ret"))))

(ert-deftest my-gptel-test-register-tool-deduplicates ()
  "Registering a tool with existing name keeps the first."
  (let* ((my/gptel-all-tools nil)
         (t1 (gptel-make-tool :name "same" :function #'identity :description "first" :args nil))
         (t2 (gptel-make-tool :name "same" :function #'ignore :description "second" :args nil)))
    (my/gptel-register-tool t1)
    (my/gptel-register-tool t2)
    (should (= (length my/gptel-all-tools) 1))
    (should (equal (gptel-tool-description (car my/gptel-all-tools)) "second"))))

(ert-deftest my-gptel-test-register-global-tool ()
  "my/gptel-register-global-tool adds to my/gptel-global-tools."
  (let ((my/gptel-global-tools nil)
        (tool (gptel-make-tool :name "test-glob"
                               :function #'identity
                               :description "d"
                               :args nil)))
    (my/gptel-register-global-tool tool)
    (should (= (length my/gptel-global-tools) 1))
    (should (equal (gptel-tool-name (car my/gptel-global-tools)) "test-glob"))))

(ert-deftest my-gptel-test-deregister-tool ()
  "my/gptel-deregister-tool removes by name."
  (let* ((t1 (gptel-make-tool :name "keep" :function #'identity :description "k" :args nil))
         (t2 (gptel-make-tool :name "drop" :function #'identity :description "d" :args nil))
         (my/gptel-all-tools (list t1 t2)))
    (my/gptel-deregister-tool "drop")
    (should (= (length my/gptel-all-tools) 1))
    (should (equal (gptel-tool-name (car my/gptel-all-tools)) "keep"))))

(ert-deftest my-gptel-test-deregister-tool-nonexistent ()
  "Deregistering a name that doesn't exist leaves the list unchanged."
  (let* ((t1 (gptel-make-tool :name "stay" :function #'identity :description "s" :args nil))
         (my/gptel-all-tools (list t1)))
    (my/gptel-deregister-tool "ghost")
    (should (= (length my/gptel-all-tools) 1))))

(ert-deftest my-gptel-test-get-tool-by-name-found ()
  "my/gptel--get-tool-by-name returns the tool when found."
  (let* ((tool (gptel-make-tool :name "findme" :function #'identity :description "d" :args nil))
         (my/gptel-all-tools (list tool)))
    (should (eq (my/gptel--get-tool-by-name "findme") tool))))

(ert-deftest my-gptel-test-get-tool-by-name-not-found ()
  "my/gptel--get-tool-by-name signals error when not found."
  (let ((my/gptel-all-tools nil))
    (should-error (my/gptel--get-tool-by-name "missing"))))

;;; --- Macro expansion ---

(ert-deftest my-gptel-test-defun-expands-to-progn ()
  "my/gptel-defun expands to a progn form."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-tool
                       ((:name "x" :type "string" :description "An arg"))
                       "A test tool."
                       (:category "test" :confirm nil)
                       (format "got %s" x)))))
    (should (eq (car expansion) 'progn))))

(ert-deftest my-gptel-test-defun-generates-correct-fn-name ()
  "my/gptel-defun generates my/gptel-<name> function."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-tool
                       ((:name "x" :type "string" :description "An arg"))
                       "A test tool."
                       (:category "test" :confirm nil)
                       (format "got %s" x)))))
    ;; The cl-defun form should reference my/gptel-test-tool
    (let ((defun-form (nth 1 expansion)))
      (should (memq (car defun-form) '(cl-defun defun)))
      (should (eq (cadr defun-form) 'my/gptel-test-tool)))))

(ert-deftest my-gptel-test-defun-generates-correct-obj-name ()
  "my/gptel-defun generates my/gptel-<name>-tool defconst."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-tool
                       ((:name "x" :type "string" :description "An arg"))
                       "A test tool."
                       (:category "test" :confirm nil)
                       (format "got %s" x)))))
    ;; The defconst form should define my/gptel-test-tool-tool
    (let ((defconst-form (nth 2 expansion)))
      (should (eq (car defconst-form) 'defconst))
      (should (eq (cadr defconst-form) 'my/gptel-test-tool-tool)))))

(ert-deftest my-gptel-test-defun-includes-register-call ()
  "my/gptel-defun expansion includes a register-tool call."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-tool
                       ((:name "x" :type "string" :description "An arg"))
                       "A test tool."
                       (:category "test" :confirm nil)
                       (format "got %s" x)))))
    ;; The last form should be (my/gptel-register-tool ...)
    (let ((register-form (nth 3 expansion)))
      (should (eq (car register-form) 'my/gptel-register-tool)))))

(ert-deftest my-gptel-test-defun-no-args ()
  "my/gptel-defun with no args produces a zero-arity function."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-no-args ()
                       "No args tool."
                       (:category "test" :confirm nil)
                       "done"))))
    (let ((defun-form (nth 1 expansion)))
      ;; formals list should be empty
      (should (null (caddr defun-form))))))

(ert-deftest my-gptel-test-defun-optional-args-produce-optional-keyword ()
  "my/gptel-defun with optional args produces &optional in lambda list."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-opt
                       ((:name "req" :type "string" :description "required")
                        (:name "opt" :type "string" :description "optional" :optional t))
                       "Tool with optional."
                       (:category "test" :confirm nil)
                       (format "%s %s" req opt)))))
    (let* ((defun-form (nth 1 expansion))
           (formals (caddr defun-form)))
      (should (memq '&optional formals))
      (should (memq 'req formals))
      (should (memq 'opt formals)))))

(ert-deftest my-gptel-test-defun-docstring-includes-param-doc ()
  "my/gptel-defun generated docstring includes parameter documentation."
  (let ((expansion (macroexpand-1
                    '(my/gptel-defun test-doc
                       ((:name "path" :type "string" :description "A file path"))
                       "Read a file."
                       (:category "test" :confirm nil)
                       (format "%s" path)))))
    (let* ((defun-form (nth 1 expansion))
           ;; docstring is the 4th element (after name and formals)
           (docstring (cadddr defun-form)))
      (should (stringp docstring))
      (should (string-match-p "Read a file" docstring))
      (should (string-match-p "path" docstring))
      (should (string-match-p "string" docstring)))))

;;; --- Integration: existing tools loaded correctly ---

(ert-deftest my-gptel-test-all-tools-nonempty ()
  "my/gptel-all-tools is populated after loading my-gptel."
  (should (> (length my/gptel-all-tools) 0)))

(ert-deftest my-gptel-test-global-tools-nonempty ()
  "my/gptel-global-tools is populated after loading my-gptel."
  (should (> (length my/gptel-global-tools) 0)))

(ert-deftest my-gptel-test-all-tools-are-gptel-tools ()
  "Every entry in my/gptel-all-tools is a gptel-tool struct."
  (dolist (tool my/gptel-all-tools)
    (should (gptel-tool-p tool))))

(ert-deftest my-gptel-test-buffer-read-tool-exists ()
  "The buffer-read tool is registered."
  (should (my/gptel--get-tool-by-name "buffer-read")))

(ert-deftest my-gptel-test-calc-tool-exists ()
  "The calc tool is registered."
  (should (my/gptel--get-tool-by-name "calc")))

(provide 'test-my-gptel)
;;; test-my-gptel.el ends here
