#!/usr/bin/env bash

# shellcheck disable=SC2155

# devbot library
#
#   daemon to handle background tasks automatically based on a schedule
#
#   when a tasks time is reached, the command is run and it's added back to the
#   schedule with an updated time (current time + interval)
#
#   this approach makes devbot's schedule persist between runs, allowing very
#   infrequent tasks to be scheduled with confidence
#
#   we'll use this version if we don't have a 'devbot' binary
#
#   controlled through 'w devbot ...'


devbot::main() {

  # none -> none
  #
  # run the main event loop

  mkdir -p ~/.devbot/locks/

  devbot::log "starting devbot"
  while :; do

    local debug="$(d devbot debug)"
    local now="$(date '+%s')"
    devbot::debug "reading events"

    d devbot events --keys \
      | common::map devbot::task

    sleep 5
  done
}


devbot::task() {

  # event -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  devbot::debug "$*"

  local -r event="$1"
  local -r when="$(d devbot data "$event" when)"

  if (( when < now )); then

    # check for prerequisites
    local -r req_name="$(d devbot events "$event" require)"

    if [[ $req_name ]]; then

      # grab the action to run out of the table
      local -r req_action="$(d devbot requirements "$req_name")"

      if ! eval "$req_action"; then
        devbot::log \
          "$event requirement \"$req_name\" ($req_action) not met"
        return
      fi
    fi

    # update when to run next
    local interval="$(d devbot events "$event" interval)"
    case $interval in
      [0-9]*)
        # seconds
        ;;
      daily)
        interval=86400
        ;;
      hourly)
        interval=3600
        ;;
      weekly)
        interval=604800
        ;;
      '')
        devbot::log "::task error: $event has no interval"
        return
        ;;
      *)
        devbot::log "::task error: $event has invalid interval"
        return
        ;;
    esac
    d devbot data "$event" when = $(( interval + now ))

    # run the action
    local -r action="$(d devbot events "$event" action)"
    [[ $action ]] || {
      devbot::log "::task error: $event has no action"
      return
    }

    devbot::eval "$event" "$action"
  fi
}


devbot::eval() {

  # string, string ... -> none
  #
  # evaluate the given shell code, but don't wait for it to finish

  local -r name="$1"; shift
  local -r lockfile=~/.devbot/locks/"$name".lock

  {
    local -r begin="$(date '+%s')"
    devbot::debug "$*"

    if flock --nonblock --exclusive "$lockfile" bash -c "set -e; $*"; then
      if [[ $(d devbot data "$name" errors) ]]; then
        d devbot data "$name" errors -d
      fi

    else
      # the command failed
      # increment number of errors
      local -i errors="$(d devbot data "$name" errors)"  # default == 0
      errors+=1

      d devbot data "$name" errors = "$errors"

      # set the backoff
      local -r backoff="$(bc <<< "$errors * 10 ^ 2 / 1")"
      local -r when="$(d devbot data "$name" when)"
      d devbot data "$name" when = "$(( when + backoff ))"

      # log it
      devbot::log \
        "error while running \"$name\" ($*), backing off $backoff seconds"
    fi

    local -r end="$(date '+%s')"

    d devbot data "$name" duration = "$(( end - begin ))"
  } &
  disown
}


devbot::log() {
  printf '%s - %s - %s\n' \
    "$(date +'%x %X')" "${FUNCNAME[1]}" "$*"
}


devbot::debug() {
  [[ $debug ]] || return
  printf '%s - %s - %s\n' \
    "$(date +'%x %X')" "${FUNCNAME[1]}" "$*"
}
