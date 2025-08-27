;;; my-gptel.el -*- lexical-binding: t -*-
;; Copyright (C) 2025 Samuel B. Johnson

;;; Commentary
;; This file primarily contains the definition of gptel-tools and
;; functions/macros to help define them. Tools are defined with the
;; macro `my/gptel-defun', which combines the definition of the tool's
;; function and the tool object used by gptel to call the function.
;;
;; Tools should be defined with a very specific job, and only one
;; job. Optional arguments should be used very sparingly where the
;; additional complexity that comes with there use can be justified by
;; the additional value they add.

;; Unnecessary arguments should be avoided. For example, a tool to
;; append content to a buffer should not have an optional argument
;; controlling whether a new line is added at the end of the content.
;; Instead, the LLM should be instructed to add a new line when to
;; content that it appeds to the  end of the file.
;;
;; Tools should not include extra code to handle errors. The gptel
;; package already handles errors and doing it here just adds
;; unnecessary complexity. In the body of the tool function.
;; The macro `cl-assert' should be used to validate that input matches
;; its specification as early in the function definition as early as
;; is reasonable, rather than using elaborate logic intertwined in the
;; primary logic of the function to ferret out  errors. The assertions
;; provide both validation of the functions contract, and
;; documentation for human beings and LLMs that can read the
;; definition.  Further, any constraints on a function argument,
;; beyond the  type, should be documented in the  arguments
;; description and  verified with assertions.

(require 'gptel)
(require 'cl-lib)



(defgroup my/gptel nil
  "Customization for my/gptel tool helpers."
  :group 'applications)


(defvar my/gptel-all-tools nil
  "A list of all gptel tools that could be made available for gptel")


(defvar my/gptel-global-tools nil
  "A list of gptel tools that are enabled by default with the function .")


(cl-defun my/gptel--make-tool-list-unique-by-name (tools)
  "Given a list of gptel tools, return a list of gptel tools with
unique names, where the tool with the first occurance of a name is
kept and subsequent tools with the same name are discarded."
  (cl-assert (cl-every #'gptel-tool-p tools))
  (cl-labels ((recur (remaining-tools names-encountered accum)
                (if (null remaining-tools) (reverse accum)
                  (let ((current-name (gptel-tool-name (car remaining-tools))))
                    (if (member current-name names-encountered)
                        (recur (cdr remaining-tools) names-encountered accum)
                      (recur (cdr remaining-tools)
                             (cons current-name names-encountered)
                             (cons (car remaining-tools) accum)))))))
    (recur tools nil nil)))


(cl-defun my/gptel-register-tool (tool-object)
  "Add a tool to the registry of defined tools.  Note that this does not make
the tool available for use, only available to be enabled for use."
  (cl-assert (gptel-tool-p tool-object))
  (setq my/gptel-all-tools
        (my/gptel--make-tool-list-unique-by-name
         (cons tool-object my/gptel-all-tools)))
  (gptel-tool-name tool-object))


(cl-defun my/gptel--rappend (xs ys)
  (cl-assert (listp xs))
  (cl-assert (listp ys))
  (named-let recur ((xs xs)
                   (ys ys))
    (if (null xs) ys
      (recur (cdr xs)
             (cons (car xs) ys)))))


(cl-defun my/gptel-register-global-tool (tool-object)
  "Add a gptel tool to the list global gptel tools"
  (setq my/gptel-global-tools
        (my/gptel--make-tool-list-unique-by-name
         (cons tool-object my/gptel-global-tools))))


(cl-defun my/gptel-deregister-tool (tool-name)
  "Remove a tool from the list of all gptel tools"
  (interactive)
  (setq my/gptel-all-tools
        (named-let recur ((tools my/gptel-all-tools)
                         (accum nil))
          (if (null tools) my/gptel-all-tools
            (let ((tool (car tools)))
              (if (equal tool-name (gptel-tool-name tool))
                  (my/gptel--rappend accum (cdr tools))
                (recur (cdr tools) (cons tool accum))))))))


(cl-defun my/gptel--make-tool-fn-name (tool-name)
  "Return the name of a function corresponding to the input `TOOL-NAME'"
  (cl-assert (symbolp tool-name))
  (intern (format "my/gptel-%s" tool-name)))


(cl-defun my/gptel--make-tool-obj-name (tool-name)
  "Return the name of a tool object corresponding to the input `TOOL-NAME'"
  (cl-assert (symbolp tool-name))
  (intern (format "my/gptel-%s-tool" tool-name)))


(cl-defun my/gptel--optional-arg-spec-p (arg-spec)
  "Return `T' if `ARG-SPEC' specifies an optional argument.
Otherwise, return `NIL'."
  (cl-assert (plistp arg-spec))
  (plist-get arg-spec :optional))


(cl-defun my/gptel--required-before-optional-p (arg-specs)
  "Return `t' if the required arguments proceed the optional arguments.
Otherwise, return `nil'"
  (let ((encountered-optional-argument nil)
        (result t)
        (remaining-arg-specs arg-specs))
    (while (and remaining-arg-specs result)
      (let ((arg-spec (car remaining-arg-specs)))
        (if (my/gptel--optional-arg-spec-p arg-spec)
            (setq encountered-optional-argument t)
          (if encountered-optional-argument
              (setq result nil)))
        (setq remaining-arg-specs (cdr remaining-arg-specs))))
    result))


(cl-defun my/gptel--symbol-to-keyword (name)
  "Return a keyword corresponding to the non-keyword input symbol"
  (cl-assert (not (keywordp name)))
  (intern (format ":%s" name)))


(cl-defun my/gptel--string-to-keyword (name)
  "Return a keyword symbol corresponding to the input string"
  (cl-assert (stringp name))
  (my/gptel--symbol-to-keyword (intern name)))


(cl-defun my/gptel--partition-arg-specs (arg-specs)
  "Return the names from the argument specs, separated into two list,
where the first list contains the required arguments and the second
list contains the optional arguments."
  (cl-assert (my/gptel--required-before-optional-p arg-specs))
  (cl-labels ((recur (remaining-specs required-specs)
                (cond
                 ((null remaining-specs) (list (reverse required-specs) nil))
                 ((my/gptel--optional-arg-spec-p (car remaining-specs)) (list (reverse required-specs) (mapcar (lambda (spec) (intern (plist-get spec :name))) remaining-specs)))
                 (t (recur (cdr remaining-specs) (cons (intern (plist-get (car remaining-specs) :name)) required-specs))))))
    (recur arg-specs nil)))


(cl-defun my/gptel--arg-spec-to-doc-string (arg-spec)
  "Given an argspec, return documentation"
  (concat (plist-get arg-spec :name) " : " (plist-get arg-spec :type) " (optional: " (format "%s" (plist-get arg-spec :optional)) ")\n    "
          (string-join (string-split (plist-get arg-spec :description) "\n") "\n    ")))


(cl-defun my/gptel--make-arg-specs-doc (arg-specs)
  "Return a string with the arg-specs appended to each other"
  (cl-flet ((format-spec (arg-spec)
              (format "\t%s" arg-spec)))
    (string-join (mapcar #'my/gptel--arg-spec-to-doc-string arg-specs) "\n")))


(defun my/gptel--lambda-list-p (arg)
  "Return `t' if the input is a list of proper argument specifications
for a gptel tool. Otherwise, return `nil'"
  (plistp arg))


(cl-defmacro my/gptel-defun (tool-name (&rest arg-specs) doc (&rest attributes) &rest body)
  "Define a gptel tool and its runtime function in a single form.

`TOOL-NAME' is a symbol used as the tool's external name (e.g., buffer-read).
This macro defines:
- Function: `my/gptel-<TOOL-NAME>'
- Tool OBJECT: `my/gptel-<TOOL-NAME>-tool'
- Tool name (seen by the model): \"TOOL-NAME\"

`ARG-SPECS' is a list of plists describing parameters (used for
the tool :args praameter and the function argument names).
Each plist should contain:
  :name        string
  :description string
  :type        string (e.g., \"string\", \"integer\")
  :optional    non-nil means the parameter is optional

`DOC' is the human-facing description and the LLM facing description.
It is used for the tool :description and as the first part of the function
docstring. The docstring is automatically augmented with a
\"Parameters:\" section built from ARG-SPECS.

`ATTRIBUTES' is a plist containing additional attributes for the tool object.
These would include attributes like `:category', `:confirm', `:async',
`:include'.

`BODY' is the implementation of the `my/gptel-<TOOL-NAME>' function. It must
return a string (what gptel will send back to the model)."
  (declare
   (doc-string 3) (indent defun))
  (cl-assert (symbolp tool-name))
  (cl-assert (stringp doc))
  (cl-assert (cl-every #'plistp arg-specs))
  (cl-assert (plistp attributes))
  (let* ((function-name   (my/gptel--make-tool-fn-name tool-name))
         (object-name (my/gptel--make-tool-obj-name tool-name))
         (param-doc (my/gptel--make-arg-specs-doc arg-specs)))
    (cl-destructuring-bind (required-args optional-args) (my/gptel--partition-arg-specs arg-specs)
      (let* ((formals (if optional-args `(,@required-args &optional ,@optional-args)
                        `(,@required-args))))
        `(progn
           (cl-defun ,function-name (,@formals)
             ,(concat doc "\n\n" param-doc)
             ,@body)
           (defconst ,object-name
             (gptel-make-tool
              :name ,(symbol-name tool-name)
              :description ,doc
              :args '(,@arg-specs)
              :function #',function-name
              ,@attributes))
           (my/gptel-register-tool  ,object-name))))))


(my/gptel-defun calc
  ((:name "expr" :type "string"
    :description "The arithmetic expression that is to be evaluated" ))
  "Evaluate a basic arithmetic expression and return the result as a string."
  (:category "utility"
   :confirm nil)
  (condition-case err
      (calc-eval (or expr ""))
    (error (format "Error: %s" (error-message-string err)))))


(my/gptel-defun open-url
  ((:name "url" :description "The URL that is to be opened in a browser" :type "string"))
  "Open URL in the user's browser and confirm."
  (:category "utility"
   :confirm nil)
  (require 'browse-url)
  (if (and url (string-match-p "\\'https?://" url))
      (progn (browse-url url)
             (format "Opened: %s" url))
    "Error: Provide a valid http(s) URL."))


(my/gptel-defun list-enabled-tools ()
  "Show a list of gptel tools that are active in this buffer"
  (:category "introspection"
   :confirm nil)
  (interactive)
  (string-join
   (cl-loop for tool in gptel-tools
            collecting (format "%s - %s" (gptel-tool-name tool) (gptel-tool-description tool)))
   "\n"))


(my/gptel-register-global-tool my/gptel-list-enabled-tools-tool)


(my/gptel-defun buffer-read
  ((:name "buffer-name"
    :description "Name of the buffer to read. The buffer name
must be the name of an active buffer."
    :type "string"))
  "Return the content of an Emacs buffer with name `BUFFER-NAME' as a
plain string."
  (:category "utility" :confirm nil)
  (cl-assert (my/gptel--live-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (buffer-string))))


(my/gptel-defun buffer-mode
  ((:name "buffer-name"
    :type "string"
    :description "Name of the buffer to inspect.
The name must be the name of an active buffer."))
  "Return the major mode of a buffer as a readable string."
  (:category "utility" :confirm nil)
  (cl-assert (my/gptel--live-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (symbol-name major-mode))))


(my/gptel-defun buffer-list ()
  "Return the names of all open buffers as a newline-separated string."
  (:category "introspection"
   :confirm nil)
  ;; Collect live buffer names (including special/internal like *Messages*).
  (let ((names
         (cl-loop for buf in (buffer-list)
                  for name = (buffer-name buf)
                  when name
                  collect name)))
    (mapconcat #'identity names "\n")))


(my/gptel-defun buffer-replace
  ((:name "content" :type "string"
    :description "New contents for the buffer.")
   (:name "buffer-name"  :type "string"
    :description "Name of the buffer to modify. The buffer name
 must be the name of an active, writable buffer."))
  "Replace the entire contents of a buffer with CONTENT, returning a
confirmation message."
  (:category "editing" :confirm t)
  (cl-assert (my/gptel--writable-live-buffer-name-p buffer-name))
  (let ((buffer  (get-buffer buffer-name)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (erase-buffer)
        (insert content)))
   (format "Replaced the content of buffer %s" buffer-name)))


(my/gptel-defun buffer-append
  ((:name "content"     :type "string"  :description "Text to append to the buffer.")
   (:name "buffer-name"  :type "string"
    :description "Name of the buffer to modify. The buffer name
 must be the name of an active, writable buffer."))
  "Append `CONTENT' to the end of `BUFFER', returning a confirmation
 message."
  (:category "editing" :confirm t)
  (cl-assert (my/gptel--writable-live-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((original-content (buffer-string)))
          (erase-buffer)
          (insert (concat original-content content))))))
  (format "Appended the content to buffer %s" buffer-name))


(my/gptel-defun buffer-prepend
  ((:name "content"     :type "string"
          :description "Text to prepend to the buffer.")

   (:name "buffer-name" :type "string"
    :description
    "Name of the buffer to modify"))
  "Prepend `CONTENT' to the beginning of `BUFFER-NAME', returning a
confirmation message."
  (:category "editing" :confirm t)
  (cl-assert (my/gptel--writable-live-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((original-content (buffer-string)))
          (erase-buffer)
          (insert (concat content original-content))))))
  (format "Appended the content to buffer %s" buffer-name))

(cl-defun my/gptel--live-buffer-name-p (buffer-name)
  "Return `t' if `BUFFER-NAME' is the name of a live buffer.
Otherwise return `nil'"
  (and (stringp buffer-name) (buffer-live-p (get-buffer buffer-name))))

(cl-defun my/gptel--live-file-buffer-name-p (buffer-name)
  "Return true if `BUFFER-NAME' is the name of a live buffer visiting a
file. Otherwise return `nil'"
  (and (my/gptel--live-buffer-name-p buffer-name)
       (if (buffer-file-name (get-buffer buffer-name)) t nil)))

(cl-defun my/gptel--buffer-read-only-p (buffer)
  "Return `t' if the buffer is read only. Otherwise return `nil'"
  (cl-assert (bufferp buffer))
  (with-current-buffer buffer
    buffer-read-only))

(cl-defun my/gptel--buffer-writable-p (buffer)
  "Return `t' if the buffer is writable.  Otherwise return `nil'."
  (cl-assert (bufferp buffer))
  (not (my/gptel--buffer-read-only-p buffer)))

(cl-defun my/gptel--writable-live-buffer-name-p (buffer-name)
  "Return `t' if the buffer name corresponds to an active buffer that
is writable."
  (and (my/gptel--live-buffer-name-p buffer-name)
       (my/gptel--buffer-writable-p (get-buffer buffer-name))))


(my/gptel-defun buffer-insert
  ((:name "content"     :type "string"  :description "Text to insert.")
   (:name "position"    :type "integer"
          :description "1-based index at which to insert.
1 inserts at beginning of the buffer;
buffer length+1 appends at the end of the buffer.")
   (:name "buffer-name"  :type "string"
    :description "Name of the buffer to modify. The buffer name
 must be the name of an active, writable buffer."))
  "Insert `CONTENT' at `POSITION' in `BUFFER-NAME' and report a
confirmation message. `BUFFER-NAME' must be the name of a live
buffer that is  writable"
  (:category "editing" :confirm t)
  (cl-assert (stringp content))
  (cl-assert (integerp position))
  (cl-assert (my/gptel--live-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (goto-char position)
        (insert content))))
  (format "Inserted the content into buffer %s at position %d" buffer-name position))

(my/gptel-defun file-exists
  ((:name "path" :type "string" :description "File path to check (may be relative to `default-directory')."))
  "Determine whether the input path is to an existing file. Returns a
string indicating whether the path is to an existing file."
  (:category "utility"
   :confirm t)
  (cl-assert (stringp path))
  (let ((absolute-path (expand-file-name path)))
    (cond ((and (file-exists-p absolute-path) (not (file-directory-p absolute-path))) (format "True, the path %s is to an existing file" absolute-path))
          ((file-exists-p absolute-path) (format "False, the path %s is to a directory, not a file" absolute-path))
          (t (format "False, the path % does not exist" absolute-path)))))

(my/gptel-defun file-open
  ((:name "path"    :type "string"  :description "Existing file path to open (absolute or relative to `default-directory')."))
  "Open an existing file into a buffer and return a confirmation message."
  (:category "filesystem"
   :confirm t)
  (cl-assert (stringp path))
  (let ((absolute-path (expand-file-name path)))
    (cl-assert (file-exists-p absolute-path))
    (cl-assert (not (file-directory-p absolute-path)))
    (let ((buffer (find-file-noselect absolute-path t)))
      (format "Opened %s in buffer %s" absolute-path (buffer-name buffer)))))

(my/gptel-defun buffer-save
  ((:name "buffer-name" :type "string" :description "Name of the buffer to save. Defaults to the current buffer."))
  "Save BUFFER to its visited file, where BUFFER must be and existing
buffer that is visiting a file"
  (:category "filesystem" :confirm t)
  (cl-assert (my/gptel--live-file-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (cl-assert (buffer-file-name buffer))
    (with-current-buffer buffer
      (save-buffer))
    (format "Saved buffer %s to file %s" buffer-name (buffer-file-name buffer))))

(my/gptel-defun buffer-close
  ((:name "buffer-name" :type "string" :description "Name of the buffer to close"))
  "Close BUFFER, where BUFFER is an existing buffer, and if buffer is
visiting a file, it must not have any unsaved changes."
  (:category "filesystem" :confirm nil)
  (cl-assert (my/gptel--live-file-buffer-name-p buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (cl-assert (or (not (buffer-file-name buffer)) (not (buffer-modified-p buffer))))
    (kill-buffer buffer)))


(my/gptel-defun run-shell-command
  ((:name "command" :type "string"
    :description "A command to be executed in the bash shell"))
  "Execute `COMMAND' in a shell and return the text output as a string"
  (:category "utility" :confirm t)
  (shell-command-to-string command))

(my/gptel-defun get-compile-command ()
  "Return the current value of `compile-command' as a string"
  (:category "utility" :confirm nil)
  compile-command)

(my/gptel-defun compile ()
  "Execute `compile-command' and return the output as a string"
  (:category "utility" :confirm t)
  (concat (format "Executed the command \"%s\" a which produced the following output:\n\n" compile-command)
          (shell-command-to-string compile-command)))

(my/gptel-defun list-all-tools ()
  "Return a list with the name and description of all available tools"
  (:category "introspection" :confirm nil)
  (string-join
   (cl-loop for tool in my/gptel-all-tools
            collecting (format "%s - %s" (gptel-tool-name tool) (gptel-tool-description tool)))
   "\n"))

(my/gptel-register-global-tool my/gptel-list-all-tools-tool)

(cl-defun my/gptel--get-tool-by-name (name)
  "Return the gptel tool with the name NAME"
  (cl-assert (stringp name))
  (cl-labels ((recur (remaining-tools)
                (if remaining-tools
                    (let ((tool (car remaining-tools)))
                      (if (equal name (gptel-tool-name tool)) tool
                        (recur (cdr remaining-tools))))
                  (error "Could not find a tool with the provided name: %s" name))))
    (recur my/gptel-all-tools)))


(my/gptel-defun enable-tool-by-name
  ((:name "tool-name" :description "The name of the to to enable" :type "string"))
  "Enable a gptel tool for use in the current buffer"
  (:category "introspection" :confirm t)
  (setq-local gptel-tools (my/gptel--make-tool-list-unique-by-name (cons (my/gptel--get-tool-by-name tool-name) gptel-tools)))
  (format "The tool %s is now enabled" tool-name))

(my/gptel-register-global-tool my/gptel-enable-tool-by-name-tool)

(setq gptel-tools my/gptel-global-tools)

(defun my/gptel-reset-local-tools ()
  "Locally set the enabled gptel tools to the global gptel tools"
  (interactive)
  (setq-local gptel-tools my/gptel-global-tools))

(provide 'my-gptel)
