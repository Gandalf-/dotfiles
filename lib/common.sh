#!/bin/bash

# common
#
#   commonly useful functions across all scripts

DEBUG=${DEBUG:-0}
DNSSERVER=''


common::branch-exists() {

  git rev-parse --verify "$1" >/dev/null 2>&1
}

common::verify-global() {

  # string -> none || exit
  #
  # check if a variable is defined

  local global="$1"

  [[ ${!global} ]] ||
    common::error "$global not defined in ~/.wizardrc.

  run \"wizard help ${global//_/-}\" for more information
  "
}


common::process-exists() {

  # pid -> bool
  #
  # check if a process is running

  kill -0 "$1"
}


common::file-not-empty() {

  # file -> bool
  #
  # check if a file's size is non zero

  [[ -s "$1" ]]
}

common::dir-exists() {

  # path -> bool
  #
  # check if a directory exists

  [[ -d "$1" ]]
}


common::inplace-file-op() {

  # file, string -> none
  #
  # common::inplace-file-op words.txt "grep -o . | sort | uniq -c"

  local file="$1"; shift
  local copy="$file".copy
  local ops="$*"

  mv "$file" "$copy"
  eval "cat $copy | $ops > $file"
  rm "$copy"
}


common::check-network() {

  # none -> bool

  nc -w 1 -z "$DNSSERVER" 53
}


common::file-exists() {
  # check if a file exists
  #
  # common::file-exists $filename && echo "yes"

  [[ -e "$1" ]]
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


common::program-exists "google-chrome" &&
common::open-link() {
  # open a link in chrome

  google-chrome "$1" 2>/dev/null >/dev/null &
}


common::debug() {
  # only run the arguments if the DEBUG flag is set
  #
  # common::debug echo this is debug mode

  (( "$DEBUG" )) && eval "$*"
}


common::clone() {
  # clone a repository to a location

  local target="${1?target required}"
  local location="${2?location required}"

  [[ -d "$location" ]] || common::error "location doesn't exist!"

  common::do git clone --depth 1 "$target" "$location"
}


common::required-help() {
  # produce help message when $1 is required
  #
  # common::required-help "$1" "[some arguments]
  # this is some help text!
  # "

  local caller="${FUNCNAME[1]//_/ }"

  case $1 in ''|-h|--help) common::error "
${caller} ${*:2}";; esac
}


common::optional-help() {
  # produce help message when $1 may be nothing
  #
  # common::optional-help "$1" "
  # this function doesn't take any arguments!
  # "

  local caller="${FUNCNAME[1]//_/ }"

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

  # shellcheck disable=SC1117
  local green="\033[01;32m" normal="\033[00m"
  printf "%b%s%b"\\n "$green" "$*" "$normal"
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

  if ! (( "$AUTO" )); then
    common::echo "$*"
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
