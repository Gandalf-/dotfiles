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

common::file-exists() {
  # check if a file exists
  #
  # common::file-exists $filename && echo "yes"

  [[ -f "$1" ]]
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
  # only run the arguments if the DEBUG flag is set
  #
  # common::debug echo this is debug mode

  (( "$DEBUG" )) && eval "$*"
}

common::clone() {

  common::required-help "$2" "[target] [location]"

  local target="$1"
  local location="$2"

  [[ -d "$location" ]] || \
    common::do git clone --depth 1 "$target" "$location"
}

common::required-help() {
  # produce help message when $1 is required
  #
  # common::required-help "$1" "[some arguments]
  # this is some help text!
  # "

  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in ""|-h|--help) common::error "
${caller} ${*:2}";; esac
}

common::optional-help() {
  # produce help message when $1 may be nothing
  #
  # common::optional-help "$1" "
  # this function doesn't take any arguments!
  # "

  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in -h|--help) common::error "
${caller} ${*:2}";; esac
}

common::return() {
  # force a return code
  #
  # common::return 42

  return "$1"
}

common::error() {
  # print a message and exit with failure
  #
  # common:error "something went wrong"

  echo "$*"
  exit 1
}

common::color-error() {
  # print a colored message and exit with failure
  #
  # common:color-error "something went very wrong"

  common::echo "$@"
  exit 1
}

common::echo() {
  # print a colored message
  #
  # common:echo "something happened"

  local green="\033[01;32m" normal="\033[00m"
  printf "%b%s%b\n" "$green" "$*" "$normal"
}

common::wait-until() {
  # keep running a command sequence until it passes
  #
  # common::wait-until '[[ $RANDOM == 42 ]]'

  while eval "$@" >/dev/null 2>/dev/null; do
    sleep 1
    echo -n "."
  done
  echo -n " Done!"
  echo
}

common::do() {
  # print what we're about to do, then do it
  #
  # common::do rm -rf /tmp/*

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
      || common::color-error "error running \"$*\""

  elif (( "$SILENT" )); then
    eval "${@/ \"\"/}" >/dev/null 2>/dev/null \
      || common::color-error "error running \"$*\""

  else
    eval "${@/ \"\"/}" \
      || common::color-error "error running \"$*\""
  fi
}

common::sudo() {
  # print what we're about to do, then sudo do it

  common::do "sudo" "$@"
}

common::confirm() {
  # ask for confirmation with a message, if they reply n or N, exit
  #
  # common::confirm "are you sure?"

  common::echo "$@"

  if ! (( "$AUTO" )); then
    read -r reply; [[ "$reply" =~ [Nn] ]] && exit 1
  fi
}
