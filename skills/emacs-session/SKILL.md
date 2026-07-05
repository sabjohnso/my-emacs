---
description: Interact with the user's live Emacs session via emacsclient when EMACS_SOCKET_NAME is set — read the current buffer, open a file at a specific line, list open buffers, or evaluate arbitrary elisp. Use whenever a task needs to observe or drive the user's actual running Emacs rather than just the files on disk.
argument-hint: "[what to do: read current buffer | open <file>:<line> | list buffers | eval <form>]"
allowed-tools: Bash(emacsclient:*)
---

# Drive the live Emacs session

Goal: use the attached Emacs server to observe or change the user's actual editing session, rather than only the files on disk.

## Precondition

Only applies when the `EMACS_SOCKET_NAME` environment variable is set. Check with `echo "$EMACS_SOCKET_NAME"` before running anything below; if it's empty, there is no live session — say so and fall back to file-based tools instead of guessing.

## Common operations

**Open a file at a specific line:**
```
emacsclient -s "$EMACS_SOCKET_NAME" --eval '(progn (find-file "/absolute/path/to/file") (goto-line 42))'
```

**Read the buffer currently in the selected window:**
```
emacsclient -s "$EMACS_SOCKET_NAME" --eval '(with-current-buffer (window-buffer (selected-window)) (buffer-string))'
```

**List open buffers:**
```
emacsclient -s "$EMACS_SOCKET_NAME" --eval '(mapcar #'\''buffer-name (buffer-list))'
```

**Evaluate arbitrary elisp** (inspect a variable, call a function, check state):
```
emacsclient -s "$EMACS_SOCKET_NAME" --eval '(some-form)'
```

## Rules

1. Always use absolute paths — the shell's current directory has no relationship to Emacs's.
2. Prefer the narrowest read (a specific buffer, a specific variable) over dumping global state — buffer *names*, not full buffer contents, unless the content is actually needed.
3. Treat this as a live user session, not a sandbox: avoid evaluating a form that kills buffers, closes frames, or discards unsaved edits unless the task explicitly calls for it. When in doubt about a destructive form, ask first.
4. If `emacsclient` fails to connect (no server running, stale socket), report the raw error — do not silently fall back to guessing at file contents without telling the user the live session was unreachable.
