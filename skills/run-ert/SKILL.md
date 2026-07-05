---
description: Run this repo's ERT (Emacs Lisp Regression Testing) suites — test-my-themes.el and test-my-gptel.el — via emacs --batch, and interpret pass/fail output. Use whenever a change to my-theme-palette.el, a my-<name>-theme.el file, or my-gptel.el needs to be verified against its test suite, or when the user asks to run the elisp tests.
argument-hint: "[test file, or none for all]"
allowed-tools: Bash(emacs --batch:*)
---

# Run the ERT test suites

Goal: execute this repo's Emacs Lisp test suites and report which tests passed or failed, with enough detail to act on a failure.

## Steps

1. Determine scope: a specific test file if named, otherwise run all test files present. `test-my-themes.el` has real tests; `test-my-gptel.el` is currently a stub containing only `(require 'ert)` and has no tests to run — skip it unless it has since gained `ert-deftest` forms.
2. Run from the repo root so `-L .` resolves the load path:
   ```
   emacs --batch -L . -l test-my-themes.el -f ert-run-tests-batch-and-exit
   ```
3. Read the batch output: ERT reports a summary line (`Ran N tests, M unexpected`) and, for each failure, the test name, the failing form, and the expected vs. actual value.
4. If a test fails, report the specific test name and the expected/actual mismatch — do not just say "tests failed." If the failure looks like a load error rather than an assertion failure (a Lisp error printed before ERT's summary line), say so explicitly; that means the file didn't load, not that a test's logic failed.
5. Do not modify test files to make a failure disappear unless the user asks for that — report the failure and let the user decide whether the test or the implementation is wrong.
