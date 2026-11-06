#!/usr/bin/env bash
# Tests for the plugin-install and plugin-update targets of the Makefile.
# The tests use "make -n", so they do not change the Claude Code configuration.
set -u

failures=0

check() {
  local name=$1 expected=$2 actual=$3
  if [[ "$actual" == "$expected" ]]; then
    echo "PASS $name"
  else
    echo "FAIL $name"
    diff <(echo "$expected") <(echo "$actual")
    failures=$((failures + 1))
  fi
}

test_dry_run_prints_the_cli_commands() {
  local expected
  expected="claude plugin marketplace add $PWD
claude plugin install --scope user emacs-interaction@my-emacs
claude plugin marketplace update my-emacs
claude plugin update --scope user emacs-interaction@my-emacs"
  check "dry run prints the CLI commands" "$expected" \
        "$(make -s -n plugin-install plugin-update 2>&1)"
}

test_missing_name_stops_make() {
  if make -s -n plugin-install MARKETPLACE_JSON=/dev/null >/dev/null 2>&1; then
    check "missing name stops make" "make fails" "make succeeds"
  else
    check "missing name stops make" "make fails" "make fails"
  fi
}

test_dry_run_prints_the_cli_commands
test_missing_name_stops_make

echo "Ran 2 tests, $failures unexpected"
exit $((failures > 0))
