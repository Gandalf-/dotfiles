#!/bin/bash

# common
#   commonly useful functions across all scripts

DEBUG=${DEBUG:-0}
green="\033[01;32m"
normal="\033[00m"

common::debug() {
  (( "$DEBUG" )) && eval "$*"
}

common::required_help() {
  # produce help message when $1 is required

  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in ""|-h|--help) _error "
${caller} ${*:2}";; esac
}

common::optional_help() {
  # produce help message when $1 may be nothing

  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in -h|--help) _error "
${caller} ${*:2}";; esac
}

common::return() {
  return "$1"
}

common::error() {
  echo "$*"
  exit 1
}

common::color_error() {
  printf "%b%s%b\n" "$green" "$*" "$normal"
  exit 1
}

common::do() {
  printf "%b%s%b\n" "$green" "$*" "$normal"
  if (( QUIET )); then
    eval "$@" >/dev/null \
      || common::color_error "error running \"$*\""
  else
    eval "$@" \
      || common::color_error "error running \"$*\""
  fi
}

common::sudo() {
  printf "%b%s%b\n" "$green" "sudo $*" "$normal"
  if (( QUIET )); then
    eval sudo "$@" >/dev/null \
      || common::color_error "error running \"$*\""
  else
    eval sudo "$@" \
      || common::color_error "error running \"$*\""
  fi
}

confirm() {
  if [[ "$1" != 0 ]]; then
    shift
    printf "%b%s%b " "$green" "$@" "$normal"
    read -r reply; [[ "$reply" =~ [Nn] ]] && exit 1
    return 0

  else
    shift
    printf "%b%s%b\n" "$green" "$@" "$normal"
    return 0
  fi
}

_confirm() {
  val=$1; shift
  confirm "$val" "${@/ \"\"/}" && eval "${@/ \"\"/}"
}
