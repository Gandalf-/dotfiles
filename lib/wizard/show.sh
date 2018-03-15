#!/bin/env bash

wizard_show_projects() {

  while read -r project; do

    printf '%-15s %s\n' \
      "$project" "$(d "$project" desc)"

  done < <(d projects)
}

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

  return $#
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

  return $#
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

  [[ $1 && $2 ]] && return 2
  [[ $1 ]] && return 1
  return 0
}


wizard_show_disk() {

  common::optional-help "$1" "

  show disk and partition usage
  "

  df -h
  return 0
}


wizard_show_weather() {

  curl http://wttr.in/~"${1:-Seattle}";
  [[ $1 ]] && return 1
  return 0
}
