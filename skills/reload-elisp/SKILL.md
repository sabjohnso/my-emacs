---
description: After an Edit or Write to an .el file in this repo, load-file it into the live Emacs session via emacsclient so the change takes effect without the user reloading it manually. Use automatically as a post-condition on every elisp edit in this repo when EMACS_SOCKET_NAME is set — not only when the user explicitly asks to reload a file.
allowed-tools: Bash(emacsclient:*)
---

# Reload edited Elisp into the live Emacs session

Goal: make an on-disk edit to a `.el` file in this repo take effect in the user's running Emacs immediately, without the user having to switch to Emacs and reload it themselves.

## When this applies

Applies immediately after a successful Edit or Write to any `.el` file in this repo, whenever the `EMACS_SOCKET_NAME` environment variable is set (meaning a live Emacs server is attached to this session — see CLAUDE.md's "Emacs Session Integration"). If `EMACS_SOCKET_NAME` is unset, skip this skill entirely; there is no live session to reload into.

## Steps

1. Confirm `EMACS_SOCKET_NAME` is set: `echo "$EMACS_SOCKET_NAME"`. If empty, stop — do not attempt `emacsclient`.
2. Load the edited file into the running session:
   ```
   emacsclient -s "$EMACS_SOCKET_NAME" --eval '(load-file "/absolute/path/to/file.el")'
   ```
   Always use the absolute path, not one relative to the current shell directory.
3. If the edit changed a `defcustom`, `defvar`, or other state that only takes effect in newly created buffers, also apply it to the relevant live buffer directly, e.g.:
   ```
   emacsclient -s "$EMACS_SOCKET_NAME" --eval '(with-current-buffer "buffer-name" (setq-local var value))'
   ```
   Do this only when the change specifically requires it — most `defun`/`defvar` reloads take effect through `load-file` alone.
4. If `emacsclient` reports an error (an `(error ...)` return or a non-zero exit), report the raw error to the user rather than retrying blindly — a load error usually means the edit introduced a syntax problem or an unresolved dependency that reloading again will not fix.
5. Do not report success on this step alone as if the broader task were done. Reloading confirms the file is syntactically loadable, not that the intended behavior is correct — use `/run` or the relevant ERT suite (see the `run-ert` skill) to verify actual behavior when that matters.
