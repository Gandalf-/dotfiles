#!/bin/bash

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

devbot::log() {
  printf '%s - %s\n' "$(date +'%x %X')" "$*"
}

devbot::task() {

  # event -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged


  common::debug "task:handle $*"

  local event="$1"
  local now="$(date '+%s')"
  local when="$(d devbot data "$event" when)"

  if (( when < now )); then

    # check for prerequisites
    local req_name="$(d devbot events "$event" require)"

    if [[ $req_name ]]; then

      # grab the action to run out of the table
      local req_action="$(d devbot requirements "$req_name")"

      if ! eval "$req_action"; then
        devbot::log \
          "$event requirement \"$req_name\" ($req_action) not met"
        return
      fi
    fi

    # update when to run next
    local interval="$(d devbot events "$event" interval)"
    case $interval in
      daily)
        interval=86400
        ;;
      hourly)
        interval=3600
        ;;
      weekly)
        interval=604800
        ;;
    esac
    d devbot data "$event" when = $(( interval + now ))

    # run the action
    local action="$(d devbot events "$event" action)"
    [[ $action ]] || {
      devbot::log "task:handle error: no action"
      return
    }

    devbot::eval "$event" "$action"
  fi
}


devbot::eval() {

  # string, string ... -> none
  #
  # evaluate the given shell code, but don't wait for it to finish

  local name="$1"; shift
  common::debug "eval: $*"

  {
    local begin="$(date '+%s')"

    bash -c "set -e; $*" || {

      # the command failed
      # increment number of errors
      local errors="$(d devbot data "$name" errors)"
      [[ $errors ]] || errors="0"
      (( errors += 1))
      d devbot data "$name" errors = "$errors"

      # set the backoff
      local backoff="$(( 10 * errors ))"
      local when="$(d devbot data "$name" when)"
      d devbot data "$name" when = "$(( when + backoff ))"

      # log it
      devbot::log "error while running \"$name\" ($*), backing off $backoff seconds"
    }
    local end="$(date '+%s')"

    d devbot data "$name" duration = "$(( end - begin ))"
  } &
  disown
}


devbot::main() {

  # none -> none
  #
  # run the main event loop

  mkdir -p ~/.devbot

  devbot::log "starting devbot"
  while :; do

    while read -r event; do
      devbot::task "$event"
    done < <(d devbot events --keys)

    sleep 5
  done
}
