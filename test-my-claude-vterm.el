;;; test-my-claude-vterm.el --- ERT tests for claude-vterm server management -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the Emacs server management functions in my-claude-vterm.el.
;; Run with: emacs --batch -L . -l test-my-claude-vterm.el -f ert-run-tests-batch-and-exit

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

;;; --- Integration: server name is deterministic ---

(ert-deftest my-claude-server-name-deterministic ()
  "Calling my--claude-server-name twice yields the same result."
  (should (string= (my--claude-server-name)
                    (my--claude-server-name))))

(provide 'test-my-claude-vterm)
;;; test-my-claude-vterm.el ends here
