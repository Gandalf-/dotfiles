#!/bin/bash

# common
#   commonly useful functions across all scripts

DEBUG=${DEBUG:-0}
green="\033[01;32m"
normal="\033[00m"
PLATFORM="$(uname)"

export PLATFORM

common::open-link() {

  google-chrome "$1" 2>/dev/null >/dev/null &
}


common::require() {

  # string, ... -> bool

  while [[ $1 ]]; do
    case $1 in

      *)
        if ! common::program-exists "$1"; then
          return 1
        fi
        ;;
    esac
    shift
  done

  return 0
}

common::program-exists() {

  which "$1" >/dev/null 2>/dev/null
}

common::contains() {
  [[ $1 =~ $2 ]]
}

common::debug() {
  (( "$DEBUG" )) && eval "$*"
}

common::clone() {

  common::required_help "$2" "[target] [location]"

  local target="$1"
  local location="$2"

  [[ -d "$location" ]] || \
    common::do git clone --depth 1 "$target" "$location"
}

common::required_help() {
  # produce help message when $1 is required

  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in ""|-h|--help) common::error "
${caller} ${*:2}";; esac
}

common::optional_help() {
  # produce help message when $1 may be nothing

  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in -h|--help) common::error "
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
  local green="\033[01;32m" normal="\033[00m"
  printf "%b%s%b\n" "$green" "$*" "$normal"
  exit 1
}

common::do() {
  # print what we're about to do, then do it
  local green="\033[01;32m" normal="\033[00m"

  if ! (( "$AUTO" )); then
    printf "%b%s%b\n" "$green" "$*" "$normal"
  fi

  if (( "$CONFIRM" )); then
    read -r reply
    [[ "$reply" =~ [Nn] ]] && exit 1
  fi

  if (( "$ECHO" )); then
    true

  elif (( "$QUIET" )); then
    eval "${@/ \"\"/}" >/dev/null \
      || common::color_error "error running \"$*\""

  elif (( "$SILENT" )); then
    eval "${@/ \"\"/}" >/dev/null 2>/dev/null \
      || common::color_error "error running \"$*\""

  else
    eval "${@/ \"\"/}" \
      || common::color_error "error running \"$*\""
  fi
}

common::sudo() {
  # print what we're about to do, then sudo do it

  common::do "sudo" "$@"
}

common::echo() {
  local green="\033[01;32m" normal="\033[00m"

  printf "%b%s%b\n" "$green" "$@" "$normal"
}

common::confirm() {
  local green="\033[01;32m" normal="\033[00m"

  printf "%b%s%b " "$green" "$@" "$normal"

  if ! (( "$CONFIRM" )); then
    read -r reply; [[ "$reply" =~ [Nn] ]] && exit 1
  fi
}
