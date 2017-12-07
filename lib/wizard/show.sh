#!/bin/env bash

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
