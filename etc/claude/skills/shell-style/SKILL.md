---
name: shell-style
description: Austin's shell scripting standards. TRIGGER whenever writing or editing shell scripts (bash, .sh files, or extensionless executables in bin/).
---

# Shell script style

Match these conventions when writing or editing any shell script.

## Basics

- Bash, not POSIX sh. Shebang is `#!/usr/bin/env bash` for portable/dotfiles scripts, `#!/bin/bash` acceptable for machine-local one-purpose scripts.
- 2-space indentation.
- Scripts in `bin/` directories have no extension and are executable; scripts inside project repos use `.sh`.
- Must be shellcheck-clean. When a warning is intentionally suppressed, use a targeted `# shellcheck disable=SCxxxx` near the top or at the site, ideally with a short reason.
- File header: shebang, blank line, then a brief lowercase comment describing what the script does (what, not how). No banners.

## Error handling

- Linear top-to-bottom scripts: `set -e` near the top. Pipeline-heavy scripts: `set -o pipefail`.
- Function-heavy scripts prefer explicit guards over `set -e`:

  ```bash
  die() { echo "$@" >&2; exit 1; }
  report() { echo "$@" >&2; }
  debug() { (( DEBUG )) && report "$@"; }
  ```

  and then `some-command ... || die "some-command failure $input"`.
- Diagnostics go to stderr; stdout is reserved for data.
- Validate inputs early with readable guards: `[[ -e $folder ]] || die "No such path: $folder"`.
- Optional behavior via environment flags checked arithmetically: `(( DEBUG ))`, `DEBUG=${DEBUG:-0}`.

## Functions

- Small, single-purpose functions; a `main()` (or subcommand functions) at the bottom, invoked as `main "$@"` — or, for multi-command scripts, dispatch with a bare `"$@"` at the end so the script is called like `./main.sh fetch` / `./main.sh auto`.
- Naming: kebab-case for standalone helpers (`docx-to-markdown`, `get-port`); snake_case when functions form a subcommand hierarchy. Scripts are completely standalone — define small helpers (`die`, `report`, `log`) in place rather than sourcing a shared library.
- All function variables declared `local`. Split declaration from command substitution when the exit code matters: `local key; key="$( unique "$fname" )"`.
- Docstring comments inside library functions: a type-signature-ish line like `# file path -> exit code`, blank comment line, then a one-line description. Skip for trivial functions.
- Wrap external tools in same-named functions when adding policy (pinned paths, nice, logging): e.g. `ffmpeg() { log nice -n 10 /opt/homebrew/opt/ffmpeg@6/bin/ffmpeg "$@"; }` — use `command nginx` inside a wrapper to reach the real binary.

## Syntax preferences

- `[[ ]]` always, never `[ ]` or `test`. Arithmetic with `(( ))`.
- Prefer parameter expansion over sed/awk/cut for string work: `${x%%.*}`, `${x##*.}`, `${x,,}`, `${folder#"$HOME"/}`, `${fname%.png}.webp`.
- `case` statements are the workhorse for dispatch, file-type filtering, and platform switching. Compact one-line cases are fine when uniform:

  ```bash
  case $OSTYPE in
    darwin*)  ... ;;
    freebsd*) ... ;;
  esac
  ```

- Read line-oriented data with `while read -r x; do ...; done < <( producer )` — process substitution, never pipes into `while` (avoids subshell loss), never `for x in $(...)`.
- Quote all expansions. Space-padded command substitution is house style: `"$( basename "$fin" )"`.
- Strongly prefer long-style flags (`--archive`, `--quiet`) over short ones — but portability comes first: if the long flag isn't supported everywhere the script runs (BSD tools, macOS coreutils), use the portable short form.
- Heredocs (`cat << EOF`) for embedded config files/templates.
- Multi-flag commands: one flag per line, backslash continuations, input first / output last:

  ```bash
  ffmpeg \
    -nostdin \
    -loglevel fatal \
    -i "$fin" \
    -c:v libx264 \
    -crf 28 \
    "$fout" \
    || die "ffmpeg failure $fin"
  ```

## Patterns

- **Idempotency / caching**: early-return when output already exists: `[[ -f "$fout" ]] && return`. Use content hashes (`sha1sum`/`md5sum` prefixes) for stable unique output names; use `find -newer $stamp` + `touch` epoch files for incremental runs.
- **Parallelism**: background subshells with `&` and a final `wait`; throttle with a worker count polled via `jobs -r | wc -l`.
- **Cleanup**: `trap cleanup EXIT` (or `trap 'rm -f "$tmp"' EXIT`); `mktemp` for temp files.
- **Locking**: `exec 9>"$LOCK"; flock 9` for single-instance scripts.
- **Logging**: a `log()` that both records and runs: `log() { echo "$@" >> "$LOGFILE"; "$@"; }` — commands stay visible and greppable.
- **Prerequisites**: check tools up front with a `require()` helper (`require tidy`) rather than failing midway.
- **Platform differences**: branch on `$OSTYPE` or `$(uname)` once near the top, binding variables (`sha=shasum`, `max_workers=8`) rather than branching repeatedly.
- **User-facing progress**: terse dots (`echo -n "."`) during parallel work; short `report` lines naming the item processed.

## Comments

Sparse, lowercase, present-tense, what-not-how. Comment only non-obvious intent or external quirks (e.g. why ffmpeg is pinned to version 6). Never narrate what the next line obviously does.
