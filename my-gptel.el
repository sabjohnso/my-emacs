;; gptel basic setup (assuming your OPENAI_API_KEY env var is set) -*- lexical-binding: t -*-


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


(cl-defmacro my/gptel-defun (tool-name (&rest arg-specs) doc (&rest attributes) &rest body)
  "Define a gptel tool and its runtime function in a single form.

`NAME' is a symbol used as the tool's external name (e.g., buffer-read).
This macro defines:
- Function: `my/gptel-NAME'
- Tool var: `my/gptel-NAME-tool'
- Tool name (seen by the model): \"NAME\"

`ARG-SPECS' is a list of plists describing parameters (mirrors the tool :args).
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
These would include attributes like :category and :confirm.

`BODY' is the implementation of the my/gptel-NAME function. It must
return a string (what gptel will send back to the model)."
  ;; (declare (doc-string 3) (indent defun))
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
  ((:name "expr" :description "The arithmetic expression that is to be evaluated" :type "string"))
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
  ((:name "buffer"    :description "Name of the buffer to read. Default: current buffer." :type "string" :optional t)
   (:name "start"     :description "1-based start position. Default: beginning of buffer." :type "integer" :optional t)
   (:name "end"       :description "1-based end position. Default: end of buffer." :type "integer" :optional t))
  "Return the content of an Emacs buffer as a plain string (`BUFFER').
Optionally, restrict reading to a region defined by the positions
`START' and `END'."
  (:category "utility"
   :confirm nil)
  (let* ((buf (if (and buffer (not (equal buffer "")))
                  (get-buffer buffer)
                (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer not found: %s" (or buffer "<current>")))
    (with-current-buffer buf
      (save-restriction
        (widen)
        ;; Normalize and clamp region to valid bounds.
        (let* ((beg (if (and (integerp start) (> start 0))
                        (min (max start (point-min)) (point-max))
                      (point-min)))
               (fin (if (and (integerp end) (> end 0))
                        (min (max end (point-min)) (point-max))
                      (point-max))))
          (cl-assert (<= beg fin))
          (let ((text (buffer-substring-no-properties beg fin)))
            text))))))

(my/gptel-defun buffer-mode
  ((:name "buffer"
    :type "string"
    :description "Name of the buffer to inspect. Defaults to the current buffer."
    :optional t))
  "Return the major mode of a buffer as a readable string."
  (:category "utility"
   :confirm nil)
  (condition-case err
      (let* ((buf (if (and buffer (not (equal buffer "")))
                      (get-buffer buffer)
                    (current-buffer))))
        ;; Validate buffer existence.
        (unless (buffer-live-p buf)
          (user-error "Buffer not found: %s" (or buffer "<current>")))
        (with-current-buffer buf
          ;; major-mode is the symbol; mode-name is the user-facing name.
          ;; format-mode-line ensures a robust string for mode-name.
          (let* ((mode-sym major-mode)
                 (mode-str (format-mode-line mode-name))
                 (buf-name (buffer-name)))
            (format "Buffer: %s\nMajor mode: %s\nMode name: %s"
                    buf-name mode-sym mode-str))))
    ;; Return a readable error message rather than signaling upstream.
    (error (format "Error: %s" (error-message-string err)))))

(my/gptel-defun buffer-list
  ()
  "Return the names of all open buffers as a newline-separated string."
  (:category "introspection"
   :confirm nil)
  ;; Collect live buffer names (including special/internal like *Messages*).
  (let ((names (cl-loop for buf in (buffer-list)
                        for name = (buffer-name buf)
                        when name
                        collect name)))
    (mapconcat #'identity names "\n")))

(my/gptel-defun buffer-replace
  ((:name "content" :type "string" :description "New contents for the buffer.")
   (:name "buffer"  :type "string" :description "Name of the buffer to modify. Defaults to the current buffer." :optional t))
  "Replace the entire contents of a buffer with CONTENT, returning a
confirmation message."
  (:category "editing"
   :confirm t)
  (condition-case err
      (let* ((buf (if (and buffer (not (equal buffer "")))
                      (get-buffer buffer)
                    (current-buffer))))
        (unless (buffer-live-p buf)
          (user-error "Buffer not found: %s" (or buffer "<current>")))
        (unless (stringp content)
          (user-error "CONTENT must be a string"))
        (with-current-buffer buf
          (save-restriction
            (widen)
            (let* ((inhibit-read-only t)
                   (old-len (max 0 (- (point-max) (point-min)))))
              (erase-buffer)
              (insert content)
              (format "Replaced content of %s (%d -> %d chars)."
                      (buffer-name buf) old-len (length content))))))
    (error (format "Error: %s" (error-message-string err)))))

(my/gptel-defun buffer-append
  ((:name "content"         :type "string"  :description "Text to append to the buffer.")
   (:name "buffer"          :type "string"  :description "Name of the buffer to modify. Defaults to the current buffer." :optional t)
   (:name "ensure-newline"  :type "boolean" :description "If non-nil, insert a newline before CONTENT when
the buffer does not end with one." :optional t))
  "Append `CONTENT' to the end of `BUFFER', returning a confirmation
 message."
  (:category "editing"
   :confirm t)
  (condition-case err
      (let* ((buf (if (and buffer (not (equal buffer "")))
                      (get-buffer buffer)
                    (current-buffer))))
        ;; Validate target buffer and input type.
        (unless (buffer-live-p buf)
          (user-error "Buffer not found: %s" (or buffer "<current>")))
        (unless (stringp content)
          (user-error "CONTENT must be a string"))
        (with-current-buffer buf
          (save-restriction
            (widen)
            (let ((inhibit-read-only t)
                  (before-size (buffer-size)))
              (goto-char (point-max))
              ;; Optionally ensure a trailing newline before appending.
              (when ensure-newline
                (unless (or (= (point) (point-min))
                            (eq (char-before) ?\n))
                  (insert "\n")))
              ;; Append the content and report.
              (insert content)
              (format "Appended %d chars to %s (size %d -> %d)."
                      (length content)
                      (buffer-name buf)
                      before-size
                      (buffer-size))))))
    (error (format "Error: %s" (error-message-string err)))))

(my/gptel-defun buffer-prepend
  ((:name "content"         :type "string"  :description "Text to prepend to the buffer.")
   (:name "buffer"          :type "string"  :description
          "Name of the buffer to modify. Defaults to the current buffer." :optional t)
   (:name "ensure-newline"  :type "boolean"
          :description
          "If non-nil and the buffer is non-empty, ensure there is a newline
 after CONTENT so the original text starts on a new line." :optional t))
  "Prepend `CONTENT' to the beginning of `BUFFER', returning a
confirmation message."
  (:category "editing"
   :confirm t)
  (condition-case err
      (let* ((buf (if (and buffer (not (equal buffer "")))
                      (get-buffer buffer)
                    (current-buffer))))
        ;; Validate target buffer and input type.
        (unless (buffer-live-p buf)
          (user-error "Buffer not found: %s" (or buffer "<current>")))
        (unless (stringp content)
          (user-error "CONTENT must be a string"))
        (with-current-buffer buf
          (save-restriction
            (widen)
            (let ((inhibit-read-only t)
                  (before-size (buffer-size))
                  (was-empty (= (point-min) (point-max))))
              (goto-char (point-min))
              ;; Insert the new content.
              (insert content)
              ;; Optionally ensure a newline boundary before the original text.
              (when (and ensure-newline
                         (not was-empty)
                         (not (eq (char-before) ?\n)))
                (insert "\n"))
              (format "Prepended %d chars to %s (size %d -> %d)."
                      (length content)
                      (buffer-name buf)
                      before-size
                      (buffer-size))))))
    (error (format "Error: %s" (error-message-string err)))))

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
  (with-current-buffer
      buffer-read-only buffer))

(cl-defun my/gptel--buffer-writable-p (buffer)
  "Return `t' if the buffer is writable.  Otherwise return `nil'."
  (cl-assert (bufferp buffer))
  (not (my/gptel--buffer-read-only-p buffer)))


(my/gptel-defun buffer-insert
  ((:name "content"     :type "string"  :description "Text to insert.")
   (:name "position"    :type "integer"
          :description "1-based index at which to insert. 1 inserts at BOB;
 buffer length+1 appends at EOB.")
   (:name "buffer-name" :type "string"  :description "Buffer name. Defaults to current buffer."))
  "Insert `CONTENT' at `POSITION' in `BUFFER-NAME' and report a
confirmation message. `BUFFER-NAME' must be the name of a live
buffer that is  writable"
  (:category "editing"
   :confirm t)

  (cl-assert (stringp content))
  (cl-assert (integerp position))
  (cl-assert (my/gptel--live-buffer-name-p buffer-name))

  (condition-case err
      (let* ((buf (if (and buffer-name (not (equal buffer-name "")))
                      (get-buffer buffer-name)
                    (current-buffer))))
        ;; Validate target buffer and inputs.
        (unless (buffer-live-p buf)
          (user-error "Buffer not found: %s" (or buffer-name "<current>")))
        (unless (stringp content)
          (user-error "CONTENT must be a string but received %s " content))
        (unless (and (integerp position) (> position 0))
          (user-error "POSITION must be a positive integer (1-based)"))
        (with-current-buffer buf
          (save-restriction
            (widen)
            (let* ((inhibit-read-only t)
                   (before-size (buffer-size))
                   ;; Clamp 1-based POSITION to [1, length+1].
                   (len before-size)
                   (pos-1-based (min (max position 1) (1+ len)))
                   ;; Convert to buffer point. point-min is 1 when widened.
                   (ipos (+ (point-min) (1- pos-1-based))))
              (goto-char ipos)
              (insert content)

              (format "Inserted %d chars at position %d in %s (size %d -> %d)."
                      (length content)
                      pos-1-based
                      (buffer-name buf)
                      before-size
                      (buffer-size))))))
    (error (format "Error: %s" (error-message-string err)))))

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
  (:category "filesystem"
   :confirm t)
  (cl-assert (stringp buffer-name))
  (cl-assert (not (equal buffer-name "")))
  (let ((buffer (get-buffer buffer-name)))
    (cl-assert (buffer-live-p buffer))
    (cl-assert (buffer-file-name buffer))
    (with-current-buffer buffer
      (save-buffer))
    (format "Saved buffer %s to file %s" buffer-name (buffer-file-name buffer))))

(my/gptel-defun buffer-close
  ((:name "buffer-name" :type "string" :description "Name of the buffer to close"))
  "Close BUFFER, where BUFFER is an existing buffer, and if buffer is
visiting a file, it must not have any unsaved changes."
  (:category "filesystem"
   :confirm t)
  (cl-assert (stringp buffer-name))
  (cl-assert (not (equal buffer-name "")))
  (let ((buffer (get-buffer buffer-name)))
    (cl-assert (buffer-live-p buffer))
    (cl-assert (or (not (buffer-file-name buffer)) (not (buffer-modified-p buffer))))
    (kill-buffer buffer)))


(my/gptel-defun compile
  ((:name "command"   :type "string"  :description "Shell command to run. Default: value of `compile-command'." :optional t)
   (:name "directory" :type "string"  :description "Run in this directory. Default: current buffer's `default-directory'." :optional t)
   (:name "display"   :type "boolean" :description "If non-nil, display the compilation buffer after starting." :optional t))
  "Start an asynchronous compilation and report the buffer name,
directory, and command."
  (:category "software development"
   :confirm t)
  (condition-case err
      (let* ((cmd (or command compile-command))
             (_   (unless (and (stringp cmd) (not (string-empty-p cmd)))
                    (user-error "Compilation command must be a non-empty string")))
             (dir (if (and directory (not (equal directory "")))
                      (expand-file-name directory)
                    default-directory))
             (_   (unless (file-directory-p dir)
                    (user-error "Not a directory: %s" dir)))
             ;; Start compilation in DIR using Emacs' standard machinery.
             (buf (let ((default-directory dir))
                    (compilation-start cmd t (lambda (_m) "*Compilation*")))))
        ;; Optionally display the buffer; compilation-start usually shows it already,
        ;; but we make it explicit when DISPLAY is requested.
        (when (and display (buffer-live-p buf))
          (pop-to-buffer buf))
        (format "Started compilation\nBuffer: %s\nDirectory: %s\nCommand: %s"
                (buffer-name buf) dir cmd))
    (error (format "Error: %s" (error-message-string err)))))


(my/gptel-defun file-list
  ((:name "directory"      :type "string"  :description "Root directory to list files under.")
   (:name "pattern"        :type "string"  :description "Regexp matched against each file's relative path." :optional t)
   (:name "include-hidden" :type "boolean" :description "Include dotfiles and files under dot-directories when non-nil." :optional t)
   (:name "absolute"       :type "boolean" :description "Return absolute paths when non-nil (default). Otherwise relative." :optional t)
   (:name "limit"          :type "integer" :description "Maximum number of results to return." :optional t))
  "List all regular files below DIRECTORY and return them as a
newline-separated string."
  (:category "filesystem"
   :confirm t)
  (condition-case err
      (let* ((root (expand-file-name directory)))
        ;; Validate directory
        (unless (and (stringp directory) (file-directory-p root))
          (user-error "Not a directory: %s" (or directory "")))
        (let ((abs-default (if (boundp 'absolute) absolute t)))
          (cl-labels
              ;; Hidden path check: any component starting with "."?
              ((hidden-path-p (rel)
                 (catch 'hidden
                   (dolist (comp (split-string rel "/" t))
                     (when (and (> (length comp) 0)
                                (eq (aref comp 0) ?.))
                       (throw 'hidden t)))
                   nil))
               ;; Depth-first traversal that collects regular files; does not follow symlinked dirs.
               (walk (dir acc)
                 (let* ((entries (directory-files dir t nil t))) ; absolute names
                   (dolist (e entries acc)
                     (let ((base (file-name-nondirectory e)))
                       (unless (member base '("." ".."))
                         (cond
                          ((and (file-directory-p e)
                                (not (file-symlink-p e)))
                           (setq acc (walk e acc)))
                          ((file-regular-p e)
                           (push e acc)))))))))
            (let* ((all (walk root nil))
                   ;; Sort deterministically
                   (all (sort all #'string<))
                   ;; Convert to relative for filtering and for final mapping if needed
                   (rels (mapcar (lambda (p) (file-relative-name p root)) all))
                   ;; Apply hidden filter unless include-hidden
                   (rels (if include-hidden
                             rels
                           (cl-remove-if #'hidden-path-p rels)))
                   ;; Apply optional regexp filter on relative paths
                   (rels (if (and (stringp pattern) (not (equal pattern "")))
                             (cl-remove-if-not (lambda (r) (string-match-p pattern r)) rels)
                           rels))
                   ;; Map to output paths: absolute or relative
                   (outs (mapcar (lambda (r) (if abs-default
                                                 (expand-file-name r root)
                                               r))
                                 rels))
                   ;; Apply optional limit
                   (outs (if (and (integerp limit) (> limit 0))
                             (cl-subseq outs 0 (min limit (length outs)))
                           outs)))
              (if outs
                  (mapconcat #'identity outs "\n")
                "No files found.")))))
    (error (format "Error: %s" (error-message-string err)))))



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
