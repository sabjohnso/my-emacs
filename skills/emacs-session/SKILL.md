---
description: Interact with the user's live Emacs session via emacsclient when EMACS_SOCKET_NAME is set — read the buffer that the user works in next to this session, open a file at a specific line next to this session, list open buffers, or evaluate arbitrary elisp. Use whenever a task needs to observe or drive the user's actual running Emacs rather than just the files on disk.
argument-hint: "[what to do: read work buffer | open <file>:<line> | list buffers | eval <form>]"
allowed-tools: Bash(emacsclient:*)
---

# Drive the live Emacs session

Goal: use the attached Emacs server to observe or change the user's actual editing session, rather than only the files on disk.

## Precondition

Only applies when the `EMACS_SOCKET_NAME` environment variable is set. Check with `echo "$EMACS_SOCKET_NAME"` before running anything below; if it's empty, there is no live session — say so and fall back to file-based tools instead of guessing.

## Find this session

One Emacs server can serve several Claude Code sessions and several
remote clients. The socket identifies the server, not this session.
The selected window of Emacs can belong to a different session or to
a different display. Do not use `selected-window` or `current-buffer`
to find the buffer of the user.

The shell of each Bash tool call is a child process of this session.
Pass its process identifier (PID), `$$`, to the `my-claude-session-*`
functions of `my-claude-vterm.el`. They follow the chain of parent
processes to the vterm buffer of this session. If Emacs reports that
such a function is void, `my-claude-vterm.el` is not loaded. Report
that to the user.

## Common operations

**Open a file at a specific line next to this session:**
```
emacsclient -s "$EMACS_SOCKET_NAME" --eval "(my-claude-session-open-file $$ \"/absolute/path/to/file\" 42)"
```
The file shows in the work window: the window that the "read" recipe
below reads. If the frame has no work window, Emacs finds or makes a
window other than the window of this session. The selected window does
not change.

**Read the buffer that the user works in next to this session:**
```
emacsclient -s "$EMACS_SOCKET_NAME" --eval "(my-claude-session-read-buffer $$)"
```
The work window is the most recently used window, in the frame of this
session, that does not show this session. If no such window exists,
Emacs signals a user error. Report the error; do not read another
buffer.

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
