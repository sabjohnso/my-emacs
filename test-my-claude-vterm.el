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
      (should-not (string-match-p "[ \t\n\"'`$\\\\|&;<>()]" arg)))))

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

(ert-deftest my-llm-vterm-keymap-install-is-idempotent ()
  "A second install replaces our keymap instead of stacking another on it."
  (with-temp-buffer
    (let ((parent (make-sparse-keymap)))
      (define-key parent (kbd "C-c C-t") #'ignore)
      (use-local-map parent)
      (my--llm-vterm-setup-keymap)
      (my--llm-vterm-setup-keymap)
      (should (eq (keymap-parent (current-local-map)) parent))
      (should (eq (lookup-key (current-local-map) (kbd "C-c C-t")) #'ignore))
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

;;; --- Integration: server name is deterministic ---

(ert-deftest my-claude-server-name-deterministic ()
  "Calling my--claude-server-name twice yields the same result."
  (should (string= (my--claude-server-name)
                    (my--claude-server-name))))

(provide 'test-my-claude-vterm)
;;; test-my-claude-vterm.el ends here
