#!/bin/env bash


wizard_show_next-break() {

  common::optional-help "$1" "[--script | break period]

  calculate the last time you took a break (no commands added to fish history)
  and compare that to the current time. if it was more than 30 minutes ago,
  tells you to take a break

    break_period  -> default 10 minutes
    --script      -> only give output when break is recommended
  "

  local now; now=$(date +%s)
  local history=~/.local/share/fish/fish_history
  local break_period=$(( 60 * 10 ))
  local work_period=$(( 60 * 30 ))

  local previous="$now"
  local last_break="$now"
  local called_by_script=0

  case $1 in
    --script) called_by_script=1 ;;
    '') ;;
    *) break_period="$1" ;;
  esac

  while read -r time; do

    if (( previous - time > break_period )); then
      last_break="$previous"
      break
    fi

    previous="$time"

  done < <(
    grep when: $history \
      | grep -v cmd: \
      | sort -r \
      | awk '{print $2}'
    )

  local time=$(( now - last_break ))

  (( called_by_script )) || \
    echo -n "you've been working for $(( time / 60 )) minutes. "

  if (( time > work_period )); then
    echo "Take a break!"

  elif ! (( called_by_script )); then
    echo "You're doing great!"
  fi
}


wizard_show_progress() {

  common::required-help "$1" "

  run a command repeatedly, clear the screen between runs
  "

  while :; do
    eval "$@"
    sleep 1
    clear
  done
}


wizard_show_history() {

  common::optional-help "$1" "(amount) (range)

  show the <amount> of the most frequently run commands
  "

  local amount=${1:-25}
  local range=${2:-1}

  fish -c history \
    | cut -f "$range" -d' ' \
    | sort \
    | uniq -c \
    | sort -nr \
    | head -n "$amount"
}


wizard_show_weather() {

  curl http://wttr.in/~"${1:-Seattle}";
}


wizard_show_haskell_documentation() {

  common::optional-help "$1" "

  start a Python http.server in the Haskell documentation directory
  "

  common::require -f stack python3
  common::cd ~/.stack/programs/x86_64-linux/

  local latest; latest="$(
    find . -maxdepth 1 -type d | tail -n 1
  )"
  latest="${latest//.\/}"

  [[ $latest ]] ||
    common::error "Couldn't find any documentation"

  common::cd "$latest"/share/doc/"$latest"/html
  common::echo "http://localhost:8000"
  common::do python3 -m http.server
}
