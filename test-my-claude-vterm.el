;;; test-my-claude-vterm.el --- ERT tests for claude-vterm server management -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the Emacs server management functions in my-claude-vterm.el.
;; Run with: emacs --batch -L . -f package-initialize -l test-my-claude-vterm.el -f ert-run-tests-batch-and-exit
;; Without `-f package-initialize', vterm is not found and the vterm
;; key binding tests are skipped.

;;; Code:

(require 'ert)
(require 'server)
(require 'my-claude-vterm)

;;; --- my--claude-server-name ---

(ert-deftest my-claude-server-name-starts-with-claude ()
  "Server name starts with \"claude-\"."
  (let ((name (my--claude-server-name)))
    (should (string-prefix-p "claude-" name))))

(ert-deftest my-claude-server-name-contains-directory ()
  "Server name contains the directory component."
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name default-directory)))
         (name (my--claude-server-name)))
    (should (string-match-p (regexp-quote dir-name) name))))

(ert-deftest my-claude-server-name-ends-with-pid ()
  "Server name ends with the Emacs PID."
  (let ((name (my--claude-server-name))
        (pid  (number-to-string (emacs-pid))))
    (should (string-suffix-p pid name))))

(ert-deftest my-claude-server-name-format ()
  "Server name has format claude-<dir>-<pid>."
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name default-directory)))
         (expected (format "claude-%s-%d" dir-name (emacs-pid)))
         (actual   (my--claude-server-name)))
    (should (string= expected actual))))

;;; --- my--claude-pid-alive-p ---

(ert-deftest my-claude-pid-alive-p-pid-1 ()
  "PID 1 (init/systemd) is always alive on Linux."
  (should (my--claude-pid-alive-p 1)))

(ert-deftest my-claude-pid-alive-p-bogus-pid ()
  "An absurdly large PID is not alive."
  (should-not (my--claude-pid-alive-p 999999999)))

(ert-deftest my-claude-pid-alive-p-self ()
  "Our own PID is alive."
  (should (my--claude-pid-alive-p (emacs-pid))))

;;; --- my--claude-make-command ---

(ert-deftest my-claude-make-command-contains-env-var ()
  "Output contains EMACS_SOCKET_NAME assignment."
  (let ((result (my--claude-make-command "claude" "claude-foo-123")))
    (should (string-match-p "EMACS_SOCKET_NAME=" result))))

(ert-deftest my-claude-make-command-contains-server-name ()
  "Output contains the server name."
  (let ((result (my--claude-make-command "claude" "claude-foo-123")))
    (should (string-match-p "claude-foo-123" result))))

(ert-deftest my-claude-make-command-contains-command ()
  "Output ends with the command."
  (let ((result (my--claude-make-command "claude" "claude-foo-123")))
    (should (string-match-p "claude$" result))))

(ert-deftest my-claude-make-command-quotes-special-chars ()
  "Server name with special characters is properly quoted."
  (let ((result (my--claude-make-command "claude" "claude-my dir-123")))
    (should (string-match-p "EMACS_SOCKET_NAME=" result))))

;;; --- my--claude-stale-sockets ---

(ert-deftest my-claude-stale-sockets-empty-dir ()
  "Returns nil when socket directory has no matching sockets."
  (let ((tmpdir (make-temp-file "server-test" t)))
    (unwind-protect
        (let ((server-socket-dir tmpdir))
          (should (null (my--claude-stale-sockets "myproject"))))
      (delete-directory tmpdir t))))

(ert-deftest my-claude-stale-sockets-finds-other-pid ()
  "Finds sockets from a different PID for the same project."
  (let ((tmpdir (make-temp-file "server-test" t)))
    (unwind-protect
        (let* ((server-socket-dir tmpdir)
               (other-pid 999999999)
               (socket-name (format "claude-myproject-%d" other-pid)))
          ;; Create a fake socket file
          (write-region "" nil (expand-file-name socket-name tmpdir))
          (let ((stale (my--claude-stale-sockets "myproject")))
            (should (= (length stale) 1))
            (should (string= (caar stale) socket-name))
            (should (= (cdar stale) other-pid))))
      (delete-directory tmpdir t))))

(ert-deftest my-claude-stale-sockets-ignores-own-pid ()
  "Does not report our own PID's socket as stale."
  (let ((tmpdir (make-temp-file "server-test" t)))
    (unwind-protect
        (let* ((server-socket-dir tmpdir)
               (socket-name (format "claude-myproject-%d" (emacs-pid))))
          (write-region "" nil (expand-file-name socket-name tmpdir))
          (should (null (my--claude-stale-sockets "myproject"))))
      (delete-directory tmpdir t))))

(ert-deftest my-claude-stale-sockets-ignores-other-projects ()
  "Does not report sockets for different projects."
  (let ((tmpdir (make-temp-file "server-test" t)))
    (unwind-protect
        (let* ((server-socket-dir tmpdir)
               (socket-name "claude-otherproject-999999999"))
          (write-region "" nil (expand-file-name socket-name tmpdir))
          (should (null (my--claude-stale-sockets "myproject"))))
      (delete-directory tmpdir t))))

;;; --- my--claude-ensure-server ---

(ert-deftest my-claude-ensure-server-existing-server ()
  "When server-process is non-nil, returns existing server-name."
  (let ((server-process t)
        (server-name "existing-server"))
    (should (string= (my--claude-ensure-server) "existing-server"))))

;;; --- my--llm-resume-args ---

(ert-deftest my-llm-resume-args-claude ()
  "Claude resumes its most recent session with --continue."
  (should (equal (my--llm-resume-args "claude") '("--continue"))))

(ert-deftest my-llm-resume-args-codex ()
  "Codex resumes its most recent session with resume --last."
  (should (equal (my--llm-resume-args "codex") '("resume" "--last"))))

(ert-deftest my-llm-resume-args-unknown-command ()
  "An unrecognized command has no resume arguments."
  (should (null (my--llm-resume-args "foo"))))

(ert-deftest my-llm-resume-args-need-no-shell-quoting ()
  "Resume arguments survive `my--llm-vterm-full-command', which does not quote.
The joined command is interpolated into a shell command line, so
an argument holding whitespace or a shell metacharacter would be
split or interpreted there."
  (pcase-dolist (`(,_ . ,args) my--llm-resume-args-alist)
    (dolist (arg args)
      (should (equal (shell-quote-argument arg) arg)))))

;;; --- my--llm-vterm-full-command ---

(ert-deftest my-llm-vterm-full-command-no-extra-args ()
  "With no extra args, the full command is just the command itself."
  (should (string= (my--llm-vterm-full-command "claude" nil) "claude")))

(ert-deftest my-llm-vterm-full-command-single-extra-arg ()
  "A single extra arg is appended with a space."
  (should (string= (my--llm-vterm-full-command "claude" '("--continue"))
                   "claude --continue")))

(ert-deftest my-llm-vterm-full-command-multiple-extra-args ()
  "Multiple extra args are each appended with a space."
  (should (string= (my--llm-vterm-full-command "codex" '("resume" "--last"))
                   "codex resume --last")))

;;; --- vterm scrollback ---

(ert-deftest my-claude-vterm-max-scrollback ()
  "vterm keeps the largest scrollback that its module allows.
The value takes effect when vterm loads, so the test skips
without vterm."
  (skip-unless (require 'vterm nil t))
  (should (= vterm-max-scrollback 100000)))

;;; --- vterm-directory ---

(ert-deftest my-vterm-directory-buffer-name-example ()
  "The buffer name holds the last component of the directory."
  (should (string= (my--vterm-directory-buffer-name "/home/user/project/")
                   "*vterm-project*")))

(ert-deftest my-vterm-directory-buffer-name-root ()
  "The root directory gives its own name, \"/\"."
  (should (string= (my--vterm-directory-buffer-name "/") "*vterm-/*")))

(ert-deftest my-vterm-directory-buffer-name-remote-root ()
  "The root of a remote host gives the full remote root as its name."
  (should (string= (my--vterm-directory-buffer-name "/ssh:host:/")
                   "*vterm-/ssh:host:/*")))

(ert-deftest my-vterm-directory-buffer-name-shared-last-component ()
  "Two directories with the same last component share one name."
  (should (string= (my--vterm-directory-buffer-name "/a/project/")
                   (my--vterm-directory-buffer-name "/b/project/"))))

(ert-deftest my-vterm-directory-starts-vterm-with-directory-name ()
  "Starts vterm in a buffer named for `default-directory'.
The shell starts in that same directory."
  (let ((default-directory "/tmp/some-project/")
        (requested nil)
        (start-directory nil))
    (cl-letf (((symbol-function 'vterm)
               (lambda (&optional arg)
                 (setq requested arg
                       start-directory default-directory))))
      (vterm-directory))
    (should (equal requested "*vterm-some-project*"))
    (should (equal start-directory "/tmp/some-project/"))))

(ert-deftest my-vterm-directory-reuses-existing-buffer ()
  "Selects the buffer for the directory when it exists already.
No second vterm starts."
  (let ((default-directory "/tmp/some-project/")
        (started nil)
        (buf (get-buffer-create "*vterm-some-project*")))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'vterm)
                     (lambda (&optional _) (setq started t))))
            (vterm-directory)
            (should-not started)
            (should (eq (window-buffer (selected-window)) buf))))
      (kill-buffer buf))))

;;; --- my--llm-env ---

(ert-deftest my-llm-env-claude-disables-alternate-screen ()
  "Claude runs on the main screen, so vterm keeps its output in scrollback."
  (should (member "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1"
                  (my--llm-env "claude"))))

(ert-deftest my-llm-env-codex ()
  "Codex needs no extra environment variables."
  (should (null (my--llm-env "codex"))))

(ert-deftest my-llm-env-unknown-command ()
  "An unrecognized command has no extra environment variables."
  (should (null (my--llm-env "foo"))))

(ert-deftest my-llm-env-quotes-values ()
  "A value that holds shell metacharacters is quoted in its assignment."
  (let ((my--llm-env-alist '(("tool" . (("FOO" . "a b;c"))))))
    (should (equal (my--llm-env "tool")
                   (list (concat "FOO=" (shell-quote-argument "a b;c")))))))

;;; --- Generators for the property tests ---

(defconst test-my-claude--seed "test-my-claude-vterm"
  "Seed for `random', so that a failed property test can be replayed.")

(defconst test-my-claude--cases 200
  "Number of random cases that each property test checks.")

(defun test-my-claude--random-string ()
  "Return a random string of 0 to 6 characters, some of them special."
  (let ((chars "ab =;'\"$x-"))
    (apply #'string
           (cl-loop repeat (random 7)
                    collect (aref chars (random (length chars)))))))

(defun test-my-claude--random-command ()
  "Return a known LLM command name or a random string."
  (let ((known (append (mapcar #'car my--llm-env-alist) '("codex" "foo"))))
    (if (zerop (random 4))
        (test-my-claude--random-string)
      (nth (random (length known)) known))))

(defun test-my-claude--random-args ()
  "Return a list of 0 to 5 random argument strings."
  (cl-loop repeat (random 6) collect (test-my-claude--random-string)))

(defmacro test-my-claude--for-random-cases (bindings &rest body)
  "Run BODY for each random case, with BINDINGS as in `let*'."
  (declare (indent 1))
  `(progn
     (random test-my-claude--seed)
     (dotimes (_ test-my-claude--cases)
       (let* ,bindings ,@body))))

;;; --- my--llm-vterm-launch-command ---

(ert-deftest my-llm-vterm-launch-command-claude ()
  "The Claude launch command puts its environment before the command."
  (should (string= (my--llm-vterm-launch-command "claude" '("--continue"))
                   "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 claude --continue")))

(ert-deftest my-llm-vterm-launch-command-without-env ()
  "A command with no environment launches as its full command."
  (should (string= (my--llm-vterm-launch-command "codex" '("resume" "--last"))
                   (my--llm-vterm-full-command "codex" '("resume" "--last")))))

(ert-deftest my-llm-vterm-launch-command-laws ()
  "The environment comes first and the full command follows it unchanged."
  (test-my-claude--for-random-cases
      ((command (test-my-claude--random-command))
       (args    (test-my-claude--random-args))
       (env     (my--llm-env command))
       (full    (my--llm-vterm-full-command command args))
       (launch  (my--llm-vterm-launch-command command args)))
    (should (string-suffix-p full launch))
    (if env
        (should (string-prefix-p (concat (string-join env " ") " ") launch))
      (should (string= launch full)))))

;;; --- my--llm-vterm-shell-line ---

(ert-deftest my-llm-vterm-shell-line-claude ()
  "The shell line sets the server socket, then the Claude environment."
  (should (string= (my--llm-vterm-shell-line "claude" nil "srv")
                   (concat "EMACS_SOCKET_NAME=srv "
                           "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 claude"))))

(ert-deftest my-llm-vterm-shell-line-laws ()
  "The shell line is the server assignment in front of the launch command."
  (test-my-claude--for-random-cases
      ((command (test-my-claude--random-command))
       (args    (test-my-claude--random-args))
       (server  (concat "claude-" (test-my-claude--random-string)))
       (line    (my--llm-vterm-shell-line command args server)))
    (should (string= line
                     (concat "EMACS_SOCKET_NAME="
                             (shell-quote-argument server) " "
                             (my--llm-vterm-launch-command command args))))))

;;; --- my--vterm-directory-buffer-name ---

(defun test-my-claude--random-component ()
  "Return a directory component of 1 to 6 characters, some of them special."
  (let ((chars "ab .-_é~%"))
    (apply #'string
           (cl-loop repeat (1+ (random 6))
                    collect (aref chars (random (length chars)))))))

(defun test-my-claude--random-directory (base)
  "Return a random directory path whose last component is BASE.
The path is relative, absolute, or remote.  It has 0 to 5 parent
components, some of them \".\" or \"..\", with 1 or 2 slashes
after each.  It ends in 0 to 3 slashes."
  (concat (nth (random 3) '("" "/" "/ssh:host:/"))
          (mapconcat (lambda (part)
                       (concat part (make-string (1+ (random 2)) ?/)))
                     (cl-loop repeat (random 6)
                              collect (test-my-claude--random-parent))
                     "")
          base
          (make-string (random 4) ?/)))

(defun test-my-claude--random-parent ()
  "Return a parent directory component: \".\", \"..\", or a random one."
  (pcase (random 4)
    (0 ".")
    (1 "..")
    (_ (test-my-claude--random-component))))

(ert-deftest my-vterm-directory-buffer-name-laws ()
  "Only the last directory component sets the name.
Trailing slashes and the parent directories have no effect."
  (test-my-claude--for-random-cases
      ((base (test-my-claude--random-component))
       (dir  (test-my-claude--random-directory base)))
    (should (string= (my--vterm-directory-buffer-name dir)
                     (concat "*vterm-" base "*")))))

;;; --- Key forwarding (keys Emacs intercepts before vterm) ---

(defmacro test-my-claude--recording-sends (var &rest body)
  "Run BODY with vterm's key sender stubbed, binding VAR to what was sent.
VAR collects the `kbd' key descriptions handed to the terminal,
in the order they were sent."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'vterm-send)
                (lambda (keys) (setq ,var (append ,var (list keys))))))
       ,@body)))

(ert-deftest my-vterm-send-C-x-forwards-C-x ()
  "Forwards the single key C-x, which Emacs reserves as its prefix key."
  (test-my-claude--recording-sends sent
    (my-vterm-send-C-x)
    (should (equal sent '("C-x")))))

(ert-deftest my-claude-kill-agents-forwards-the-chord ()
  "Forwards Claude Code's `chat:killAgents' chord, whatever it is set to."
  (test-my-claude--recording-sends sent
    (my-claude-kill-agents)
    (should (equal sent (list my-claude-kill-agents-chord)))))

(ert-deftest my-claude-kill-agents-chord-is-canonical ()
  "The chord is spelled the way Emacs itself spells it."
  (should (equal (key-description (kbd my-claude-kill-agents-chord))
                 my-claude-kill-agents-chord)))

(ert-deftest my-claude-kill-agents-chord-defaults-to-C-x-C-k ()
  "The default follows Claude Code's own default for `chat:killAgents'."
  (should (equal (default-value 'my-claude-kill-agents-chord) "C-x C-k")))

(defconst test-my-claude--key-descriptions '("C-x C-k" "<f5>" "M-g M-g")
  "Strings that name a key and must be accepted everywhere.")

(defconst test-my-claude--non-key-descriptions
  '("ctrl-x" "C-x C-K" "" " C-x C-k" "C-x C-k " "ESC g" 5 nil left [?\C-x])
  "Values that every key check must reject.
None is a key description in the form that `key-description' prints.
Some name a real key in another spelling.  To accept a second
spelling of the same key is a failure, not a convenience.")

(defun test-my-claude--key-widget-matches (symbol key)
  "Return non-nil when SYMBOL's customization type accepts KEY."
  (widget-apply (widget-convert (get symbol 'custom-type)) :match key))

(defun test-my-claude--widget-agrees-with-predicate (matcher key)
  "Return non-nil when MATCHER and the predicate agree about KEY.
MATCHER is a function of one key description."
  (eq (not (funcall matcher key))
      (not (my--llm-vterm-key-description-p key))))

(ert-deftest my-claude-kill-agents-chord-type-follows-the-predicate ()
  "The chord's customization type accepts exactly what the predicate does."
  (let ((matcher (lambda (key)
                   (test-my-claude--key-widget-matches
                    'my-claude-kill-agents-chord key))))
    (dolist (key (append test-my-claude--key-descriptions
                         test-my-claude--non-key-descriptions))
      (should (test-my-claude--widget-agrees-with-predicate matcher key)))))

(ert-deftest my-llm-vterm-key-bindings-type-follows-the-predicate ()
  "The table's key type accepts exactly what the predicate does."
  (let* ((key-type (plist-get (cdr (get 'my-llm-vterm-key-bindings
                                        'custom-type))
                              :key-type))
         (matcher  (lambda (key)
                     (widget-apply (widget-convert key-type) :match key))))
    (dolist (key (append test-my-claude--key-descriptions
                         test-my-claude--non-key-descriptions))
      (should (test-my-claude--widget-agrees-with-predicate matcher key)))))

;;; --- my--llm-vterm-key-description-p ---

(ert-deftest my-llm-vterm-key-description-p-accepts-canonical-keys ()
  "Every key description in the shared corpus is accepted."
  (dolist (key test-my-claude--key-descriptions)
    (should (my--llm-vterm-key-description-p key))))

(ert-deftest my-llm-vterm-key-description-p-rejects-other-values ()
  "Prose, non-canonical spellings and values that are not strings
are rejected."
  (dolist (key test-my-claude--non-key-descriptions)
    (should-not (my--llm-vterm-key-description-p key))))

(ert-deftest my-llm-vterm-key-description-p-accepts-what-emacs-prints ()
  "Whatever `key-description' prints is accepted, so no real key is
rejected as unsettable."
  (dolist (event '(?a ?Z ?0 ?\s ?\C-a ?\M-a ?\C-\M-a
                   f1 f5 return tab backspace home left))
    (dolist (keys (list (vector event) (vector ?\C-x event)))
      (should (my--llm-vterm-key-description-p (key-description keys))))))

(ert-deftest my-claude-kill-agents-is-reachable-from-the-keymap ()
  "Pressing C-c C-k forwards the chord, keymap and command together.
The buffer must be the selected window's, because key lookup
during `execute-kbd-macro' consults that window, not the buffer
merely made current."
  (let ((buf      (generate-new-buffer "*claude-keymap-test*"))
        (original (window-buffer (selected-window))))
    (unwind-protect
        (test-my-claude--recording-sends sent
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (my--llm-vterm-setup-keymap)
            (execute-kbd-macro (kbd "C-c C-k")))
          (should (equal sent (list my-claude-kill-agents-chord))))
      (set-window-buffer (selected-window) original)
      (kill-buffer buf))))

(ert-deftest my-vterm-send-next-key-delegates-to-vterm ()
  "Forwarding the next key is vterm's own command, not a reimplementation."
  (let ((called nil))
    (cl-letf (((symbol-function 'vterm-send-next-key)
               (lambda () (setq called t))))
      (my-vterm-send-next-key))
    (should called)))

;;; --- my-llm-vterm-key-bindings ---

(ert-deftest my-llm-vterm-key-bindings-are-distinct ()
  "No key sequence appears twice, where the later entry would win silently."
  (let ((keys (mapcar #'car my-llm-vterm-key-bindings)))
    (should (equal keys (delete-dups (copy-sequence keys))))))

(ert-deftest my-llm-vterm-key-bindings-are-canonical ()
  "Every key string is spelled the way Emacs itself spells it."
  (pcase-dolist (`(,key . ,_) my-llm-vterm-key-bindings)
    (should (equal (key-description (kbd key)) key))))

(ert-deftest my-llm-vterm-key-bindings-name-commands ()
  "Every entry names a command, not an arbitrary symbol `define-key' accepts."
  (pcase-dolist (`(,_ . ,command) my-llm-vterm-key-bindings)
    (should (commandp command))))

(ert-deftest my-llm-vterm-key-bindings-are-customizable ()
  "The table is a customization option, not a constant to edit in place."
  (should (custom-variable-p 'my-llm-vterm-key-bindings)))

(ert-deftest my-llm-vterm-key-bindings-reinstall-on-customization ()
  "Customizing the table reaches buffers that already have our keymap."
  (let ((buf (generate-new-buffer "*claude-custom-test*"))
        (original my-llm-vterm-key-bindings))
    (unwind-protect
        (with-current-buffer buf
          (my--llm-vterm-setup-keymap)
          (customize-set-variable 'my-llm-vterm-key-bindings
                                  '(("C-c C-9" . ignore)))
          (should (eq (lookup-key (current-local-map) (kbd "C-c C-9"))
                      #'ignore))
          (should-not (lookup-key (current-local-map) (kbd "C-c C-k"))))
      (customize-set-variable 'my-llm-vterm-key-bindings original)
      (kill-buffer buf))))

(ert-deftest my-llm-vterm-key-bindings-spare-buffers-with-no-local-map ()
  "A buffer we never touched keeps its empty local map.
Nil is what `current-local-map' returns there, so it cannot double
as the record of a keymap we installed."
  (let ((buf      (generate-new-buffer "*untouched-test*"))
        (original my-llm-vterm-key-bindings))
    (unwind-protect
        (with-current-buffer buf
          (customize-set-variable 'my-llm-vterm-key-bindings
                                  '(("C-c C-9" . ignore)))
          (should (null (current-local-map))))
      (customize-set-variable 'my-llm-vterm-key-bindings original)
      (kill-buffer buf))))

(ert-deftest my-llm-vterm-key-bindings-spare-buffers-we-no-longer-own ()
  "A buffer whose local map was replaced by something else is left alone."
  (let ((buf      (generate-new-buffer "*claude-foreign-test*"))
        (foreign  (make-sparse-keymap))
        (original my-llm-vterm-key-bindings))
    (unwind-protect
        (with-current-buffer buf
          (my--llm-vterm-setup-keymap)
          (use-local-map foreign)
          (customize-set-variable 'my-llm-vterm-key-bindings
                                  '(("C-c C-9" . ignore)))
          (should (eq (current-local-map) foreign)))
      (customize-set-variable 'my-llm-vterm-key-bindings original)
      (kill-buffer buf))))

;;; --- my--llm-vterm-setup-keymap ---

(defun test-my-claude--local-binding (key)
  "Return the command KEY is bound to in a freshly configured keymap."
  (with-temp-buffer
    (my--llm-vterm-setup-keymap)
    (lookup-key (current-local-map) (kbd key))))

(ert-deftest my-llm-vterm-keymap-binds-every-table-entry ()
  "Every entry of `my-llm-vterm-key-bindings' is installed."
  (with-temp-buffer
    (my--llm-vterm-setup-keymap)
    (pcase-dolist (`(,key . ,command) my-llm-vterm-key-bindings)
      (should (eq (lookup-key (current-local-map) (kbd key)) command)))))

(ert-deftest my-llm-vterm-keymap-follows-the-table ()
  "Installation is driven by the table, not by the keys it happens to hold."
  (with-temp-buffer
    (let ((my-llm-vterm-key-bindings '(("C-c C-9" . ignore)
                                       ("<f9>"    . undefined))))
      (my--llm-vterm-setup-keymap)
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-9")) #'ignore))
      (should (eq (lookup-key (current-local-map) (kbd "<f9>")) #'undefined))
      (should-not (lookup-key (current-local-map) (kbd "C-c C-k"))))))

(ert-deftest my-llm-vterm-keymap-install-is-idempotent-without-a-parent ()
  "A buffer that had no local map never adopts our own keymap as its base.
Nil is a legal value of `current-local-map', so it cannot double
as the marker for \"nothing recorded yet\"."
  (with-temp-buffer
    (my--llm-vterm-setup-keymap)
    (let ((first (current-local-map)))
      (my--llm-vterm-setup-keymap)
      (should-not (eq (keymap-parent (current-local-map)) first))
      (should (null (keymap-parent (current-local-map))))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-k"))
                  #'my-claude-kill-agents)))))

(ert-deftest my-llm-vterm-keymap-install-adopts-a-replaced-map ()
  "A map installed by someone else becomes the base of the next install.
Otherwise a later install — on vterm's copy-mode exit, say —
would rebuild over a base map recorded before the replacement and
silently drop the replacement's bindings."
  (with-temp-buffer
    (my--llm-vterm-setup-keymap)
    (let ((foreign (make-sparse-keymap)))
      (define-key foreign (kbd "C-c C-t") #'ignore)
      (use-local-map foreign)
      (my--llm-vterm-setup-keymap)
      (should (eq (keymap-parent (current-local-map)) foreign))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-t")) #'ignore))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-k"))
                  #'my-claude-kill-agents)))))

(defun test-my-claude--keymap-chain (map)
  "Return the list of keymaps reachable from MAP through its parents."
  (let (chain)
    (while (keymapp map)
      (push map chain)
      (setq map (keymap-parent map)))
    (nreverse chain)))

(defun test-my-claude--should-be-same-keymaps (actual expected)
  "Assert that ACTUAL and EXPECTED hold the same keymap objects in order.
`equal' is not enough: it compares keymaps by their contents, so two
different empty keymaps are `equal'."
  (should (= (length actual) (length expected)))
  (should (cl-every #'eq actual expected)))

(defun test-my-claude--derive-keymap (key)
  "Install a keymap derived from the current local map, binding KEY.
Models a package that keeps what it found by making the current
local map the parent of the one it installs."
  (let ((derived (make-sparse-keymap)))
    (set-keymap-parent derived (current-local-map))
    (define-key derived (kbd key) #'ignore)
    (use-local-map derived)))

(ert-deftest my-llm-vterm-keymap-install-unlinks-our-earlier-keymap ()
  "Our earlier keymap is spliced out wherever in the chain it sits.
A package that derives its keymap from the current local map
captures ours as a parent.  Left there, a key dropped from
`my-llm-vterm-key-bindings' would still resolve through it, and
an implementation that checked only the immediate parent would
miss it as soon as a second package derived from the first."
  (dolist (above '(0 1 2 3 5))
    (dolist (below '(0 1 2 3))
      (ert-info ((format "%d maps above ours, %d below" above below))
        (with-temp-buffer
          (use-local-map (make-sparse-keymap))
          (dotimes (i below)
            (test-my-claude--derive-keymap (format "C-c %d" i)))
          (my--llm-vterm-setup-keymap)
          (let ((ours (current-local-map)))
            (dotimes (i above)
              (test-my-claude--derive-keymap (format "C-c C-%d" i)))
            (let ((chain (test-my-claude--keymap-chain (current-local-map)))
                  ;; <f9> is outside the C-c keys that the derived maps bind.
                  (my-llm-vterm-key-bindings '(("<f9>" . ignore))))
              (my--llm-vterm-setup-keymap)
              ;; Our keymap leaves the chain; everything else keeps its place.
              (test-my-claude--should-be-same-keymaps
               (test-my-claude--keymap-chain (current-local-map))
               (cons (current-local-map) (remq ours chain))))
            ;; C-c C-k is unbound: `lookup-key' gives nil, or 1 when no
            ;; map makes C-c a prefix.
            (let ((dropped (lookup-key (current-local-map) (kbd "C-c C-k"))))
              (should (or (null dropped) (numberp dropped))))
            (should (eq (lookup-key (current-local-map) (kbd "<f9>"))
                        #'ignore))
            (dotimes (i below)
              (should (eq (lookup-key (current-local-map)
                                      (kbd (format "C-c %d" i)))
                          #'ignore)))
            (dotimes (i above)
              (should (eq (lookup-key (current-local-map)
                                      (kbd (format "C-c C-%d" i)))
                          #'ignore)))))))))

(ert-deftest my-llm-vterm-keymap-install-bounds-the-parent-chain ()
  "Each derive-and-install cycle leaves one map behind, not two.
The map the other package installed stays; the keymap we had
installed before it does not."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (my--llm-vterm-setup-keymap)
    (let ((depth (length (test-my-claude--keymap-chain (current-local-map)))))
      (dotimes (i 5)
        (test-my-claude--derive-keymap (format "C-c %d" i))
        (my--llm-vterm-setup-keymap))
      (should (= (length (test-my-claude--keymap-chain (current-local-map)))
                 (+ depth 5))))))

(ert-deftest my-llm-vterm-keymap-install-is-idempotent ()
  "A second install replaces our keymap instead of stacking another on it.
The base map carries a parent of its own, the shape in which a
stacked install would be hardest to see."
  (with-temp-buffer
    (let ((parent      (make-sparse-keymap))
          (grandparent (make-sparse-keymap)))
      (define-key grandparent (kbd "C-c C-r") #'ignore)
      (set-keymap-parent parent grandparent)
      (define-key parent (kbd "C-c C-t") #'ignore)
      (use-local-map parent)
      (my--llm-vterm-setup-keymap)
      (my--llm-vterm-setup-keymap)
      (should (eq (keymap-parent (current-local-map)) parent))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-t")) #'ignore))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-r")) #'ignore))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-k"))
                  #'my-claude-kill-agents)))))

(ert-deftest my-llm-vterm-keymap-preserves-the-parent-map ()
  "Keys absent from the table keep their inherited binding; the table wins."
  (with-temp-buffer
    (let ((parent (make-sparse-keymap)))
      (define-key parent (kbd "C-c C-t") #'ignore)
      (define-key parent (kbd "C-c C-c") #'undefined)
      (use-local-map parent))
    (my--llm-vterm-setup-keymap)
    (should (eq (lookup-key (current-local-map) (kbd "C-c C-t")) #'ignore))
    (should (eq (lookup-key (current-local-map) (kbd "C-c C-c"))
                #'my--vterm-send-escape))))

(ert-deftest my-llm-vterm-keymap-preserves-C-c-C-c ()
  "C-c C-c still sends ESC."
  (should (eq (test-my-claude--local-binding "C-c C-c")
              #'my--vterm-send-escape)))

(ert-deftest my-llm-vterm-keymap-preserves-f5 ()
  "<f5> still runs `compile'."
  (should (eq (test-my-claude--local-binding "<f5>") #'compile)))

;;; --- my--llm-vterm-kill-current ---

(ert-deftest my-llm-vterm-kill-current-silences-the-exit-query ()
  "The running process is cleared of its exit query before the kill."
  (let* ((buf  (generate-new-buffer "*claude-process-test*"))
         (proc (start-process "test-sleep" buf "sleep" "60")))
    (with-current-buffer buf
      (setq-local my--llm-vterm-command "claude")
      (my--llm-vterm-kill-current))
    (should-not (process-query-on-exit-flag proc))
    (should-not (buffer-live-p buf))))

(ert-deftest my-llm-vterm-kill-current-returns-the-session ()
  "The session is captured before the buffer that holds it is destroyed."
  (let* ((buf (generate-new-buffer "*claude-session-test*"))
         (session (with-current-buffer buf
                    (setq-local my--llm-vterm-command "claude")
                    (setq-local default-directory "/tmp/")
                    (my--llm-vterm-kill-current))))
    (should (equal (plist-get session :command) "claude"))
    (should (equal (plist-get session :name) "*claude-session-test*"))
    (should (equal (plist-get session :directory) "/tmp/"))))

;;; --- my-claude-quit ---

(ert-deftest my-claude-quit-rejects-a-foreign-buffer ()
  "Quitting a buffer that runs no LLM signals a user error."
  (with-temp-buffer
    (should-error (my-claude-quit) :type 'user-error)))

(ert-deftest my-claude-quit-kills-the-buffer ()
  "Quitting kills the LLM vterm buffer."
  (let ((buf (generate-new-buffer "*claude-test*")))
    (with-current-buffer buf
      (setq-local my--llm-vterm-command "claude")
      (my-claude-quit))
    (should-not (buffer-live-p buf))))

;;; --- my-claude-restart ---

(ert-deftest my-claude-restart-rejects-a-foreign-buffer ()
  "Restarting a buffer that runs no LLM signals a user error."
  (with-temp-buffer
    (should-error (my-claude-restart) :type 'user-error)))

(ert-deftest my-claude-restart-recreates-every-known-session ()
  "For every known command, restart reuses the name and directory of the
buffer it killed and passes that command's resume arguments."
  (pcase-dolist (`(,command . ,resume-args) my--llm-resume-args-alist)
    (let* ((buf   (generate-new-buffer (format "*%s-restart test*" command)))
           (name  (buffer-name buf))
           (dir   "/tmp/a directory/")
           (calls nil))
      (cl-letf (((symbol-function 'my--llm-vterm-create-buffer)
                 (lambda (&rest args)
                   (setq calls (append calls (list (cons default-directory
                                                         args)))))))
        (with-current-buffer buf
          (setq-local my--llm-vterm-command command)
          (setq-local default-directory dir)
          (my-claude-restart)))
      (should-not (buffer-live-p buf))
      (should (equal calls (list (list dir name command resume-args)))))))

(ert-deftest my-claude-restart-relaunches-an-unknown-command ()
  "A command with no resume arguments still keeps its name and directory."
  (let* ((buf   (generate-new-buffer "*mystery-restart-test*"))
         (name  (buffer-name buf))
         (calls nil))
    (cl-letf (((symbol-function 'my--llm-vterm-create-buffer)
               (lambda (&rest args) (setq calls (append calls (list args))))))
      (with-current-buffer buf
        (setq-local my--llm-vterm-command "mystery")
        (my-claude-restart)))
    (should (equal calls (list (list name "mystery" nil))))))

(ert-deftest my-claude-restart-aborts-when-the-kill-is-vetoed ()
  "A surviving buffer is not shadowed by a second one."
  (let ((buf     (generate-new-buffer "*claude-veto-test*"))
        (created nil))
    (unwind-protect
        (cl-letf (((symbol-function 'my--llm-vterm-create-buffer)
                   (lambda (&rest _) (setq created t))))
          (with-current-buffer buf
            (setq-local my--llm-vterm-command "claude")
            (add-hook 'kill-buffer-query-functions #'ignore nil t)
            (should-error (my-claude-restart) :type 'user-error)))
      (with-current-buffer buf
        (setq-local kill-buffer-query-functions nil))
      (kill-buffer buf))
    (should-not created)))

;;; --- Prompt visibility (follow the prompt when a window shows vterm) ---

(defmacro test-my-claude--with-terminal (lines cursor &rest body)
  "Run BODY in the selected window, in a buffer that stands for a terminal.
The buffer holds LINES numbered lines, and it ends with a newline
as a vterm buffer does.  The terminal cursor is on the line
CURSOR.  The window shows the top of the buffer."
  (declare (indent 2) (debug t))
  `(save-window-excursion
     (delete-other-windows)
     (with-temp-buffer
       (dotimes (i ,lines) (insert (format "%d\n" (1+ i))))
       (goto-char (point-min))
       (test-my-claude--show-top (selected-window) (current-buffer))
       (cl-letf (((symbol-function 'vterm-reset-cursor-point)
                  (lambda ()
                    (goto-char (point-min))
                    (forward-line (1- ,cursor)))))
         ,@body))))

(defun test-my-claude--show-top (window buffer)
  "Show the top of BUFFER in WINDOW."
  (set-window-buffer window buffer)
  (set-window-start window (with-current-buffer buffer (point-min)))
  (set-window-point window (with-current-buffer buffer (point-min))))

(defun test-my-claude--window-lines (window)
  "Return the start line and the point line of WINDOW."
  (with-current-buffer (window-buffer window)
    (list (line-number-at-pos (window-start window))
          (line-number-at-pos (window-point window)))))

(defun test-my-claude--prompt-lines (window lines cursor)
  "Return the lines that WINDOW must have when it shows the prompt.
The terminal has LINES lines and its cursor is on the line CURSOR."
  (list (max 1 (- (1+ lines) (window-body-height window))) cursor))

(defun test-my-claude--split-window ()
  "Split the selected window at a random height and return the new window."
  (let ((spare (- (window-total-height) (* 2 window-min-height))))
    (split-window nil (- (+ window-min-height (random (1+ spare)))))))

(ert-deftest my-vterm-last-rows-start-laws ()
  "The last rows start at the start of a line.
The text from there to the end holds ROWS lines, or the whole
buffer if the buffer is shorter.  A final newline does not start
another row.  The point does not move."
  (test-my-claude--for-random-cases
      ((lines (1+ (random 100)))
       (rows  (1+ (random 60)))
       (final (nth (random 2) '("" "\n"))))
    (with-temp-buffer
      (insert (string-join (make-list lines "x") "\n") final)
      (goto-char (1+ (random (point-max))))
      (let* ((point (point))
             (start (my--vterm-last-rows-start rows)))
        (should (= (point) point))
        (should (= (count-lines start (point-max)) (min lines rows)))
        (goto-char start)
        (should (bolp))))))

(ert-deftest my-vterm-show-prompt-laws ()
  "The window shows the cursor and the last rows, and no other window moves.
This is true for a selected window and for another window, from
any current buffer.  A second call changes nothing."
  (test-my-claude--for-random-cases
      ((lines  (1+ (random 100)))
       (cursor (1+ (random lines))))
    (test-my-claude--with-terminal lines cursor
      (let* ((buffer  (current-buffer))
             (windows (list (selected-window) (test-my-claude--split-window)))
             (shown   (nth (random 2) windows))
             (other   (car (remq shown windows))))
        (test-my-claude--show-top (cadr windows) buffer)
        (dotimes (_ 2)
          (with-temp-buffer (my--vterm-show-prompt shown))
          (should (equal (test-my-claude--window-lines shown)
                         (test-my-claude--prompt-lines shown lines cursor)))
          (should (equal (test-my-claude--window-lines other) '(1 1))))))))

(ert-deftest my-vterm-show-prompt-does-nothing-in-copy-mode ()
  "Copy mode does not change the window position."
  (test-my-claude--with-terminal 300 294
    (setq-local vterm-copy-mode t)
    (my--vterm-show-prompt (selected-window))
    (should (equal (test-my-claude--window-lines (selected-window)) '(1 1)))))

(defmacro test-my-claude--with-followed-terminal (window &rest body)
  "Run BODY with WINDOW bound to a window that shows a vterm buffer.
The buffer has 300 lines and the cursor is on the line 294.  Only
`vterm-mode-hook' prepares the buffer, as it does for each vterm
buffer."
  (declare (indent 1) (debug (symbolp body)))
  `(test-my-claude--with-terminal 300 294
     (let ((,window (selected-window))
           (my--vterm-minibuffer-opener nil))
       (run-hooks 'vterm-mode-hook)
       ,@body)))

(defun test-my-claude--shows-prompt-p (window)
  "Return non-nil if WINDOW shows the prompt of a followed terminal."
  (equal (test-my-claude--window-lines window)
         (test-my-claude--prompt-lines window 300 294)))

(defun test-my-claude--selection-change (window old-selected)
  "Run `window-selection-change-functions' for WINDOW in its buffer.
Make `old-selected-window' return OLD-SELECTED."
  (cl-letf (((symbol-function 'old-selected-window) (lambda () old-selected)))
    (with-current-buffer (window-buffer window)
      (run-hook-with-args 'window-selection-change-functions window))))

(defun test-my-claude--open-minibuffer (window)
  "Run `minibuffer-setup-hook' as if WINDOW opened the minibuffer."
  (cl-letf (((symbol-function 'minibuffer-selected-window)
             (lambda () window)))
    (run-hooks 'minibuffer-setup-hook)))

(ert-deftest my-vterm-prompt-shows-when-a-window-gets-the-buffer ()
  "A window that gets the buffer shows the prompt."
  (test-my-claude--with-followed-terminal window
    (run-hook-with-args 'window-buffer-change-functions window)
    (should (test-my-claude--shows-prompt-p window))))

(ert-deftest my-vterm-prompt-shows-when-the-window-is-selected ()
  "A window that becomes the selected window shows the prompt."
  (test-my-claude--with-followed-terminal window
    (test-my-claude--selection-change window (split-window))
    (should (test-my-claude--shows-prompt-p window))))

(ert-deftest my-vterm-prompt-ignores-a-window-that-loses-selection ()
  "A window that is no longer the selected window keeps its position."
  (test-my-claude--with-followed-terminal window
    (select-window (split-window))
    (test-my-claude--selection-change window window)
    (should (equal (test-my-claude--window-lines window) '(1 1)))))

(ert-deftest my-vterm-prompt-ignores-a-selection-with-the-mouse ()
  "A mouse event keeps the position, so that a drag selects the text."
  (test-my-claude--with-followed-terminal window
    (dolist (type '(down-mouse-1 mouse-1 drag-mouse-1 mouse-movement))
      (let ((last-input-event (list type (posn-at-point nil window))))
        (test-my-claude--selection-change window (split-window))
        (should (equal (test-my-claude--window-lines window) '(1 1)))
        (delete-other-windows window)))))

(ert-deftest my-vterm-prompt-ignores-a-return-from-the-minibuffer ()
  "A window keeps its position when its minibuffer returns the selection."
  (test-my-claude--with-followed-terminal window
    (test-my-claude--open-minibuffer window)
    (test-my-claude--selection-change window (minibuffer-window))
    (should (equal (test-my-claude--window-lines window) '(1 1)))))

(ert-deftest my-vterm-prompt-ignores-a-return-from-nested-minibuffers ()
  "A window keeps its position when nested minibuffers return the selection."
  (test-my-claude--with-followed-terminal window
    (test-my-claude--open-minibuffer window)
    (test-my-claude--open-minibuffer (minibuffer-window))
    (test-my-claude--selection-change window (minibuffer-window))
    (should (equal (test-my-claude--window-lines window) '(1 1)))))

(ert-deftest my-vterm-prompt-shows-after-a-minibuffer-of-another-window ()
  "The window shows the prompt when another window opened the minibuffer.
This is true even if the window opened an earlier minibuffer."
  (test-my-claude--with-followed-terminal window
    (test-my-claude--open-minibuffer window)
    (test-my-claude--open-minibuffer (split-window))
    (test-my-claude--selection-change window (minibuffer-window))
    (should (test-my-claude--shows-prompt-p window))))

(ert-deftest my-vterm-prompt-shows-when-the-minibuffer-went-elsewhere ()
  "The window shows the prompt when another window had the selection.
This is true even if the window opened a minibuffer earlier."
  (test-my-claude--with-followed-terminal window
    (test-my-claude--open-minibuffer window)
    (test-my-claude--selection-change window (split-window))
    (should (test-my-claude--shows-prompt-p window))))

(ert-deftest my-vterm-selection-change-laws ()
  "The window shows the prompt if it is selected, with two exceptions.
A mouse event selected it, or the selection returns from a
minibuffer whose most recent opener is the window.  A minibuffer
does not count as an opener.  Otherwise nothing moves."
  (test-my-claude--for-random-cases
      ((selected (zerop (random 2)))
       (event    (seq-random-elt '(nil ?x (mouse-1) (down-mouse-1))))
       (openers  (cl-loop repeat (random 5)
                          collect (seq-random-elt '(window other mini))))
       (old      (seq-random-elt '(other mini)))
       (returns  (and (eq old 'mini)
                      (eq (car (last (remq 'mini openers))) 'window))))
    (test-my-claude--with-followed-terminal window
      (let* ((other   (split-window))
             (windows `((window . ,window) (other . ,other)
                        (mini . ,(minibuffer-window))))
             (last-input-event event))
        (dolist (opener openers)
          (test-my-claude--open-minibuffer (alist-get opener windows)))
        (unless selected (select-window other))
        (test-my-claude--selection-change window (alist-get old windows))
        (if (and selected (not (consp event)) (not returns))
            (should (test-my-claude--shows-prompt-p window))
          (should (equal (test-my-claude--window-lines window) '(1 1))))))))

(ert-deftest my-vterm-show-prompt-counts-scaled-rows ()
  "The last rows fill the window when its text is scaled.
A batch Emacs cannot scale text, so the test scales the row count
of the selected window."
  (test-my-claude--with-terminal 300 294
    (let ((window (selected-window)))
      (cl-letf (((symbol-function 'window-screen-lines)
                 (lambda () (if (eq (selected-window) window) 10.8 5.0))))
        (with-selected-window (split-window)
          (my--vterm-show-prompt window)))
      (should (equal (test-my-claude--window-lines window) '(291 294))))))

(ert-deftest my-vterm-prompt-does-not-move-other-buffers ()
  "A buffer that is not a vterm buffer keeps its position."
  (test-my-claude--with-followed-terminal window
    (with-temp-buffer
      (dotimes (_ 300) (insert "x\n"))
      (test-my-claude--show-top window (current-buffer))
      (run-hook-with-args 'window-buffer-change-functions window)
      (test-my-claude--selection-change window (split-window))
      (should (equal (test-my-claude--window-lines window) '(1 1))))))

;;; --- Integration: server name is deterministic ---

(ert-deftest my-claude-server-name-deterministic ()
  "Calling my--claude-server-name twice yields the same result."
  (should (string= (my--claude-server-name)
                    (my--claude-server-name))))

;;; --- Session lookup: generators and fixtures ---

(defun test-my-claude--random-parent-map ()
  "Return a random parent map from PIDs 0 to 7 to a parent PID or nil.
The map can contain cycles, PIDs that are their own parent, and
parents that the map does not contain."
  (let ((map (make-hash-table)))
    (dotimes (pid 8)
      (unless (zerop (random 4))
        (puthash pid (and (not (zerop (random 4))) (random 10)) map)))
    map))

(defun test-my-claude--parent-fn (map)
  "Return a parent function that looks up a PID in MAP."
  (lambda (pid) (gethash pid map)))

(defun test-my-claude--random-pids ()
  "Return a list of 0 to 5 random PIDs from 0 to 9."
  (cl-loop repeat (random 6) collect (random 10)))

(defun test-my-claude--random-pid-alist ()
  "Return an alist of 0 to 5 random PIDs to distinct symbols."
  (cl-loop for i below (random 6)
           collect (cons (random 10) (intern (format "buffer-%d" i)))))

(defmacro test-my-claude--with-buffers (vars &rest body)
  "Run BODY with each of VARS bound to a new buffer.
Kill the buffers after BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (var)
                   `(,var (generate-new-buffer ,(format " *test-%s*" var))))
                 vars)
     (unwind-protect (progn ,@body)
       (dolist (buffer (list ,@vars))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

(defmacro test-my-claude--with-process-buffer (var &rest body)
  "Run BODY with VAR bound to a buffer that runs a sleep process."
  (declare (indent 1))
  `(test-my-claude--with-buffers (,var)
     (set-process-query-on-exit-flag
      (start-process "test-session" ,var "sleep" "60") nil)
     ,@body))

(defmacro test-my-claude--with-layout (&rest body)
  "Run BODY in one window, and restore the window layout after BODY."
  (declare (indent 0))
  `(save-window-excursion
     (delete-other-windows)
     ,@body))

;;; --- my--claude-ancestor-pids ---

(ert-deftest my-claude-ancestor-pids-stops-at-pid-1 ()
  "The chain of PID 1 is PID 1 alone, even when PID 1 has a parent."
  (should (equal (my--claude-ancestor-pids 1 (lambda (_) 0)) '(1))))

(ert-deftest my-claude-ancestor-pids-follows-parents ()
  "The chain follows each parent until a PID has no parent."
  (should (equal (my--claude-ancestor-pids
                  30 (lambda (pid) (alist-get pid '((30 . 20) (20 . 10)))))
                 '(30 20 10))))

(ert-deftest my-claude-ancestor-pids-laws ()
  "The chain starts at PID, follows parents, ends, and has no repeats."
  (test-my-claude--for-random-cases
      ((map    (test-my-claude--random-parent-map))
       (parent (test-my-claude--parent-fn map))
       (pid    (random 10))
       (chain  (my--claude-ancestor-pids pid parent))
       (last   (car (last chain))))
    (should (eql (car chain) pid))
    (cl-loop for (child next) on chain while next
             do (should (eql (funcall parent child) next)))
    (should (or (eql last 1)
                (null (funcall parent last))
                (memql (funcall parent last) chain)))
    (should-not (memql 1 (butlast chain)))
    (should (equal chain (seq-uniq chain)))
    (should (<= (length chain) (1+ (hash-table-count map))))))

;;; --- my--claude-process-parent-pid ---

(ert-deftest my-claude-process-parent-pid-of-emacs ()
  "The chain of this Emacs ends at PID 1 or at a PID with no parent."
  (let ((last (car (last (my--claude-ancestor-pids
                          (emacs-pid) #'my--claude-process-parent-pid)))))
    (should (or (eql last 1) (null (my--claude-process-parent-pid last))))))

(ert-deftest my-claude-process-parent-pid-of-missing-process ()
  "A PID with no process has no parent."
  (should-not (my--claude-process-parent-pid 999999999)))

;;; --- my--claude-match-session-buffer ---

(ert-deftest my-claude-match-session-buffer-nearest-wins ()
  "When two PIDs match, the buffer of the PID first in the chain wins."
  (should (eq (my--claude-match-session-buffer
               '(5 4 3) '((3 . far) (4 . near)))
              'near)))

(ert-deftest my-claude-match-session-buffer-laws ()
  "The result belongs to the first PID in the chain that has a buffer."
  (test-my-claude--for-random-cases
      ((pids   (test-my-claude--random-pids))
       (alist  (test-my-claude--random-pid-alist))
       (first  (seq-find (lambda (pid) (assq pid alist)) pids))
       (result (my--claude-match-session-buffer pids alist)))
    (if first
        (should (eq result (cdr (assq first alist))))
      (should-not result))))

;;; --- my--claude-session-buffer ---

(ert-deftest my-claude-session-buffer-finds-process-buffer ()
  "The PID of a buffer process finds that buffer."
  (test-my-claude--with-process-buffer buffer
    (should (eq (my--claude-session-buffer
                 (process-id (get-buffer-process buffer)))
                buffer))))

(ert-deftest my-claude-session-buffer-ignores-killed-buffer ()
  "A killed buffer never matches."
  (let (pid)
    (test-my-claude--with-process-buffer buffer
      (setq pid (process-id (get-buffer-process buffer))))
    (should-not (my--claude-session-buffer pid))))

(ert-deftest my-claude-session-buffer-no-match ()
  "The chain of this Emacs contains no buffer process."
  (should-not (my--claude-session-buffer (emacs-pid))))

(ert-deftest my-claude-session-buffer-laws ()
  "The lookup matches the parent chain of PID against the process buffers."
  (test-my-claude--for-random-cases
      ((map    (test-my-claude--random-parent-map))
       (parent (test-my-claude--parent-fn map))
       (alist  (test-my-claude--random-pid-alist))
       (pid    (random 10)))
    (cl-letf (((symbol-function 'my--claude-process-parent-pid) parent)
              ((symbol-function 'my--claude-process-buffer-alist)
               (lambda () alist)))
      (should (eq (my--claude-session-buffer pid)
                  (my--claude-match-session-buffer
                   (my--claude-ancestor-pids pid parent) alist))))))

;;; --- my--claude-most-recent-window ---

(ert-deftest my-claude-most-recent-window-empty ()
  "An empty list has no most recent window."
  (should-not (my--claude-most-recent-window nil)))

(ert-deftest my-claude-most-recent-window-laws ()
  "The result is the first window with the largest use time."
  (test-my-claude--for-random-cases
      ((times   (cl-loop repeat (1+ (random 5)) collect (random 4)))
       (windows (number-sequence 0 (1- (length times)))))
    (cl-letf (((symbol-function 'window-use-time)
               (lambda (window) (nth window times))))
      (should (eql (my--claude-most-recent-window windows)
                   (seq-position times (apply #'max times)))))))

;;; --- my--claude-session-window and my--claude-work-window ---

(defun test-my-claude--work-window (session)
  "Return the work window next to the session buffer SESSION, or nil."
  (let ((window (my--claude-session-window session)))
    (and window (my--claude-work-window window))))

(defun test-my-claude--eligible-windows (session-window)
  "Return the windows in the frame of SESSION-WINDOW that show another buffer.
The order is the order of `my--claude-work-window'."
  (seq-remove (lambda (window)
                (eq (window-buffer window) (window-buffer session-window)))
              (window-list (window-frame session-window) 'never
                           (frame-first-window session-window))))

(defun test-my-claude--random-layout (buffers)
  "Split the selected frame at random and show one of BUFFERS in each window.
Then select windows at random, so that the windows get use times."
  (dotimes (_ (random 5))
    (let ((windows (window-list nil 'never)))
      (ignore-errors
        (split-window (nth (random (length windows)) windows)
                      nil (zerop (random 2))))))
  (dolist (window (window-list nil 'never))
    (set-window-buffer window (nth (random (length buffers)) buffers)))
  (let ((windows (window-list nil 'never)))
    (dotimes (_ (random 6))
      (select-window (nth (random (length windows)) windows)))))

(ert-deftest my-claude-session-window-not-shown ()
  "When no window shows the session buffer, there is no session window."
  (test-my-claude--with-buffers (session)
    (test-my-claude--with-layout
      (split-window)
      (should-not (my--claude-session-window session)))))

(ert-deftest my-claude-work-window-one-other-window ()
  "With one other window, that window is the work window."
  (test-my-claude--with-buffers (session)
    (test-my-claude--with-layout
      (let ((work (selected-window))
            (other (split-window)))
        (set-window-buffer other session)
        (should (eq (test-my-claude--work-window session) work))))))

(ert-deftest my-claude-work-window-most-recent ()
  "Of two other windows, the most recently used one is the work window."
  (test-my-claude--with-buffers (session)
    (test-my-claude--with-layout
      (let* ((older (selected-window))
             (newer (split-window))
             (shown (split-window newer)))
        (set-window-buffer shown session)
        (select-window older)
        (select-window newer)
        (select-window shown)
        (should (eq (test-my-claude--work-window session) newer))))))

(ert-deftest my-claude-work-window-no-other-window ()
  "When the session buffer fills the frame, there is no work window."
  (test-my-claude--with-buffers (session)
    (test-my-claude--with-layout
      (set-window-buffer (selected-window) session)
      (should-not (test-my-claude--work-window session)))))

(ert-deftest my-claude-work-window-laws ()
  "The work window is the first most recent eligible window."
  (test-my-claude--with-buffers (session a b)
    (test-my-claude--for-random-cases
        ((result nil))
      (test-my-claude--with-layout
        (test-my-claude--random-layout (list session a b))
        (setq result (test-my-claude--work-window session))
        (let* ((shown    (get-buffer-window-list session 'never t))
               (eligible (and shown (test-my-claude--eligible-windows
                                     (car shown))))
               (latest   (and eligible
                              (apply #'max (mapcar #'window-use-time
                                                   eligible)))))
          (should (eq result
                      (seq-find (lambda (window)
                                  (= (window-use-time window) latest))
                                eligible))))))))

;;; --- my--claude-line-position ---

(ert-deftest my-claude-line-position-laws ()
  "The position of LINE is the start of LINE."
  (test-my-claude--for-random-cases
      ((count (1+ (random 6)))
       (line  (1+ (random count))))
    (with-temp-buffer
      (dotimes (i count)
        (insert (format "line %d\n" (1+ i))))
      (goto-char (my--claude-line-position (current-buffer) line))
      (should (bolp))
      (should (= (line-number-at-pos) line)))))

(ert-deftest my-claude-line-position-past-end ()
  "A line after the last line gives the end of the buffer."
  (with-temp-buffer
    (insert "line 1\nline 2\n")
    (should (= (my--claude-line-position (current-buffer) 9) (point-max)))))

;;; --- Session entry points ---

(defmacro test-my-claude--with-session-layout (names &rest body)
  "Run BODY with a session window next to a work window.
NAMES is (SESSION PID SESSION-WINDOW WORK-WINDOW).  SESSION is a
buffer that runs a sleep process, and PID is the PID of that
process.  SESSION-WINDOW shows SESSION.  WORK-WINDOW is the selected
window, and it shows a buffer with the text \"work text\"."
  (declare (indent 1))
  (pcase-let ((`(,session ,pid ,session-window ,work-window) names)
              (work (make-symbol "work")))
    `(test-my-claude--with-process-buffer ,session
       (test-my-claude--with-buffers (,work)
         (test-my-claude--with-layout
           (let* ((,work-window    (selected-window))
                  (,session-window (split-window))
                  (,pid            (process-id (get-buffer-process ,session))))
             (with-current-buffer ,work
               (insert "work text"))
             (set-window-buffer ,work-window ,work)
             (set-window-buffer ,session-window ,session)
             ,@body))))))

(defmacro test-my-claude--with-temp-file (var &rest body)
  "Run BODY with VAR bound to a file of five numbered lines."
  (declare (indent 1))
  `(let ((,var (make-temp-file "test-session" nil ".txt"
                               "line 1\nline 2\nline 3\nline 4\nline 5\n")))
     (unwind-protect (progn ,@body)
       (let ((buffer (find-buffer-visiting ,var)))
         (when buffer (kill-buffer buffer)))
       (delete-file ,var))))

(defun test-my-claude--user-error-message (form-fn)
  "Call FORM-FN and return the message of the `user-error' it signals."
  (condition-case err
      (progn (funcall form-fn) nil)
    (user-error (cadr err))))

(ert-deftest my-claude-session-read-buffer-reads-work-window ()
  "The text comes from the buffer in the work window of the session."
  (test-my-claude--with-session-layout (_session pid _session-window _work)
    (should (string= (my-claude-session-read-buffer pid) "work text"))))

(ert-deftest my-claude-session-read-buffer-reads-whole-buffer ()
  "A narrowed work buffer gives all of its text."
  (test-my-claude--with-session-layout (_session pid _session-window work)
    (with-current-buffer (window-buffer work)
      (narrow-to-region 1 3))
    (should (string= (my-claude-session-read-buffer pid) "work text"))))

(ert-deftest my-claude-session-read-buffer-no-session ()
  "A PID with no session buffer signals a user error."
  (should-error (my-claude-session-read-buffer (emacs-pid))
                :type 'user-error))

(ert-deftest my-claude-session-read-buffer-session-not-shown ()
  "A session buffer that no window shows gives the matching message."
  (test-my-claude--with-session-layout (_session pid session-window _work)
    (delete-window session-window)
    (should (equal (test-my-claude--user-error-message
                    (lambda () (my-claude-session-read-buffer pid)))
                   "No window shows the session buffer"))))

(ert-deftest my-claude-session-read-buffer-no-work-window ()
  "A session buffer with no window next to it gives the matching message."
  (test-my-claude--with-session-layout (_session pid _session-window work)
    (delete-window work)
    (should (equal (test-my-claude--user-error-message
                    (lambda () (my-claude-session-read-buffer pid)))
                   "No window shows another buffer next to the session"))))

(ert-deftest my-claude-session-open-file-uses-work-window ()
  "The file shows at LINE in the work window, and the selection stays."
  (test-my-claude--with-temp-file file
    (test-my-claude--with-session-layout (session pid session-window work)
      (let ((older (split-window work)))
        (select-window older)
        (select-window work)
        (should (eq (my-claude-session-open-file pid file 3) work))
        (should (eq (selected-window) work))
        (should (eq (window-buffer session-window) session))
        (should-not (eq (window-buffer older) (find-buffer-visiting file)))
        (should (eq (window-buffer work) (find-buffer-visiting file)))
        (should (= (with-current-buffer (window-buffer work)
                     (line-number-at-pos (window-point work)))
                   3))))))

(ert-deftest my-claude-session-open-file-without-work-window ()
  "With no work window, `display-buffer' shows the file in a new window."
  (test-my-claude--with-temp-file file
    (test-my-claude--with-session-layout (session pid session-window work)
      (delete-window work)
      (let* ((display-buffer-alist        nil)
             (display-buffer-base-action  '(nil))
             (split-width-threshold       nil)
             (split-height-threshold      4)
             (window (my-claude-session-open-file pid file 1)))
        (should (eq (selected-window) session-window))
        (should (eq (window-buffer session-window) session))
        (should (eq (window-buffer window) (find-buffer-visiting file)))))))

(ert-deftest my-claude-session-open-file-no-session ()
  "A PID with no session buffer signals a user error."
  (test-my-claude--with-temp-file file
    (should-error (my-claude-session-open-file (emacs-pid) file 1)
                  :type 'user-error)))

(ert-deftest my-claude-session-open-file-session-not-shown ()
  "A session buffer that no window shows signals a user error."
  (test-my-claude--with-temp-file file
    (test-my-claude--with-session-layout (_session pid session-window _work)
      (delete-window session-window)
      (should-error (my-claude-session-open-file pid file 1)
                    :type 'user-error))))

(ert-deftest my-claude-session-open-file-no-window-for-file ()
  "When no work window exists and `display-buffer' finds no window,
a user error results."
  (test-my-claude--with-temp-file file
    (test-my-claude--with-session-layout (_session pid _session-window work)
      (delete-window work)
      (let ((display-buffer-overriding-action
             '((display-buffer-no-window) (allow-no-window . t))))
        (should-error (my-claude-session-open-file pid file 1)
                      :type 'user-error)))))

(provide 'test-my-claude-vterm)
;;; test-my-claude-vterm.el ends here
