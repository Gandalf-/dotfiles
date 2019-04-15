#!/usr/bin/env bash

tested=0
failures=0

# linting with shellcheck

declare -A shell_ignore
shell_ignore[../wizard]=1
shell_ignore[../pydo]=1
shell_ignore[../d]=1
shell_ignore[../d.c]=1
shell_ignore[../Makefile]=1

for file in ../* ../../lib/{,**/}*.sh; do

  [[ ${shell_ignore[$file]} ]] && continue
  [[ -f "$file" ]] || continue

  (( tested++ ))
  if ! shellcheck "$file"; then
    echo "failure in $file"
    (( failures++ ))
  fi

done

# linting with pylint and flake8
for file in ../../lib/python/{,**/}*.py; do

  [[ -f "$file" ]] || continue

  (( tested++ ))
  if ! pylint -s n -f parseable -r no "$file" 2>/dev/null; then
    echo "failure in $file"
    (( failures++ ))
  fi

  (( tested++ ))
  if ! flake8 "$file"; then
    echo "failure in $file"
    (( failures++ ))
  fi

done

echo "$failures failures out of $tested tests"
exit "$failures"
