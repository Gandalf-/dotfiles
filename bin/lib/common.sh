#!/bin/bash

readonly green="\033[01;32m"
readonly normal="\033[00m"
QUIET=0

_help() {
  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in ""|-h|--help) _error "
${caller} ${*:2}";; esac
}

__help() {
  caller="$(tr '_' ' ' <<< "${FUNCNAME[1]}")"
  case $1 in -h|--help) _error "
${caller} ${*:2}";; esac
}

_error() {
  echo "$*"
  exit 1
}

error() {
  printf "%b%s%b\n" "$green" "$*" "$normal"
  exit 1
}

chk() {
  printf "%b%s%b\n" "$green" "$*" "$normal"
  if (( QUIET )); then
    eval "$@" >/dev/null \
      || error "error running \"$*\""
  else
    eval "$@" \
      || error "error running \"$*\""
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
