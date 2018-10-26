#!/bin/bash

# shellcheck disable=SC1117

# common
#   commonly useful functions across all scripts


# globals used by this libary
BROWSER=''
DEBUG=${DEBUG:-0}
PLATFORM="$(uname)"
export PLATFORM

green="\033[01;32m"
normal="\033[00m"


# database functions
db::get() {
  # variable name -> [keys] -> ()
  #
  # db::get 'value' some keys that produce value

  local variable="$1"
  shift
  mapfile -t "${variable?}" < <(d "$@")
}


# git functions
git::is-clean() {

  # exit code
  #
  # check for non committed changes

  local clean='nothing to commit, working directory clean'
  grep -q "$clean" <<< "$(git status)"
}


git::commit-exists() {

  # commit -> exit code
  #
  # check if the commit exists in the current git repo

  local target="$1"
  grep -q "$target" <<< "$(git log --oneline | head -n 1)"
}


git::branch-exists() {

  # branch name -> exit code
  #
  # check if the branch exists in the current git repo

  local branch="$1"
  git rev-parse --verify "$branch" >/dev/null 2>&1
}


git::current-branch() {

  # branch name
  #
  # get the current branch's name

  git rev-parse --abbrev-ref HEAD
}


common::pip() {

  common::do python3 -m pip "$*"
}


common::verify-global() {

  # string -> () || exit
  #
  # check if a variable is defined

  local global="$1"

  [[ ${!global} ]] ||
    common::error "$global not defined in ~/.wizardrc.

  run \"wizard help ${global//_/-}\" for more information
  "
}


common::is-integer() {

  # string -> exit code

  [ "$1" -eq "$1" ] 2>/dev/null
}


common::process-exists() {

  # pid -> exit code
  #
  # check if a process is running

  kill -0 "$1"
}


common::file-not-empty() {

  # file -> exit code
  #
  # check if a file's size is non zero

  [[ -s "$1" ]]
}

common::dir-exists() {

  # path -> exit code
  #
  # check if a directory exists

  [[ -d "$1" ]]
}


common::inplace-file-op() {

  # file name -> command string -> exit code
  #
  # common::inplace-file-op words.txt "grep -o . | sort | uniq -c"

  local file="$1"; shift
  local copy="$file".copy
  local ops="$*"

  mv "$file" "$copy"
  eval "cat $copy | $ops > $file"
  local code=$?
  rm "$copy"
  return $code
}


common::check-network() {

  # exit code

  common::verify-global "DNSSERVER"
  nc -w 1 -z "$DNSSERVER" 53
}


common::open-link() {

  # url -> ()

  common::verify-global "BROWSER"
  "$BROWSER" "$1" 2>/dev/null >/dev/null &
}


common::open-file() {

  "${EDITOR:-vim}" "$@"
}


common::file-exists() {

  # file path -> exit code
  #
  # check if a file exists

  [[ -f "$1" ]]
}


common::require() {

  # ["-f" | progam name] -> exit code

  local caller="${FUNCNAME[1]//_/ }"
  local force=0

  while [[ $1 ]]; do
    case $1 in
      -f)
        force=1
        ;;

      *)
        common::program-exists "$1" || {
          if (( force )); then
            common::error "Need $1 to continue with $caller"

          else
            return 1
          fi
        }
        ;;
    esac
    shift
  done
}


common::map() {

  # stdin: items to map
  # args:  commands to apply to items
  #
  # apply a function to each line of input
  # have to read all, then apply incase function clobbers stdin
  #
  # find . -type f | common::map wc

  local __items=()
  local __item
  while read -r __item; do
    __items+=( "$__item" )
  done

  [[ ${__items[*]} ]] || exit 1

  for __item in "${__items[@]}"; do
    "$@" "$__item"
  done
}


common::for() {

  # list -> intersperse '\n' list

  while [[ $1 ]]; do
    echo "$1"
    shift
  done
}


common::mapl() {

  # create a function from the provided shell code and apply it to each line
  # from stdin
  #
  # find . -type f | common::mapl 'echo {} {} {}'

  [[ $1 ]] || return

  eval "
  __lambda() {
    ${*//\{\}/\"\$1\"}
  }
  "

  common::map __lambda
}


common::filterl() {

  [[ $1 ]] || return
  common::mapl "$* && echo {}"
}


common::mmap() {

  # concurrent map

  local __items=()
  local __item
  while read -r __item; do
    __items+=( "$__item" )
  done

  local __tmps=()
  for __item in "${__items[@]}"; do
    __tmp="$(mktemp /dev/shm/mmap.XXXXXXXXXXXXXXXXXXXXXX)"
    __tmps+=( "$__tmp" )

    "$@" "$__item" > "$__tmp" 2>&1 &
  done

  wait

  for __tmp in "${__tmps[@]}"; do
    cat "$__tmp"
    rm -f "$__tmp"
  done
}


common::program-exists() {

  # program name -> exit code

  command -v "$1" >/dev/null 2>/dev/null
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


common::cd() {

  local target="$1"
  if [[ "$target" == "$(pwd)" ]]; then
    return
  fi

  common::do cd "$target"
}


common::do() {
  # print what we're about to do, then do it
  #
  # common::do rm -rf /tmp/*

  if ! (( "$AUTO" )); then
    common::echo "$*"
  fi

  if (( "$CONFIRM" )); then
    common::echo "Continue? [Yn]"
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

  elif (( "$IGNORE" )); then
    eval "${@/ \"\"/}"

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

common::translate-time() {

  local time="$1"

  if (( time <= 60 )); then
    echo "$time seconds"

  elif (( time <= 3600 )); then
    echo "$(( time / 60 )) minutes"

  elif (( time <= 86400 )); then
    echo "$(( time / 3600 )) hours"

  else
    echo "$(( time / 86400 )) days"
  fi
}


common::choice() {
  # return a random element from the arguments

  items=( "$@" )
  echo "${items[$RANDOM % ${#items[@]} ]}"
}
