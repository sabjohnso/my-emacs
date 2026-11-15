#!/usr/bin/env bash
# Tests for the help, plugin-install, and plugin-update targets of the Makefile.
# The tests use "make -n" or "make help", so they do not change the Claude Code
# configuration.
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

# The phony targets that make reads from the Makefile, one on each line.
phony_targets() {
  make -pRrq -f Makefile : 2>/dev/null \
    | sed -n 's/^\.PHONY: //p' | tr ' ' '\n' | grep -v '^$' | sort -u
}

test_help_lists_each_other_target_with_a_description() {
  local help missing="" target targets
  help=$(make -s help 2>&1)
  targets=$(phony_targets | grep -vx help)
  [[ -n "$targets" ]] || missing="no phony targets found "
  for target in $targets; do
    grep -Eq "^  $target +[^ ]" <<<"$help" || missing+="$target "
  done
  check "help lists each other target with a description" "" "$missing"
}

test_help_does_not_list_itself() {
  if grep -Eq '^  help ' <<<"$(make -s help 2>&1)"; then
    check "help does not list itself" "absent" "present"
  else
    check "help does not list itself" "absent" "absent"
  fi
}

test_help_does_not_read_the_manifest() {
  if make -s help MARKETPLACE_JSON=/dev/null >/dev/null 2>&1; then
    check "help does not read the manifest" "make succeeds" "make succeeds"
  else
    check "help does not read the manifest" "make succeeds" "make fails"
  fi
}

test_dry_run_prints_the_cli_commands
test_missing_name_stops_make
test_help_lists_each_other_target_with_a_description
test_help_does_not_list_itself
test_help_does_not_read_the_manifest

echo "Ran 5 tests, $failures unexpected"
exit $((failures > 0))
