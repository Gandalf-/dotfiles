#!/bin/bash

# devbot library
#
#   daemon to handle background tasks automatically based on a schedule
#
#   tasks are added to a schedule file, that's read every 5 seconds.
#
#   when a tasks time is reached, the command is run and it's added back to the
#   schedule with an updated time (current time + interval)
#
#   this approach makes devbot's schedule persist between runs, allowing very
#   infrequent tasks to be scheduled with confidence

devbot::task:handle() {

  # event -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  common::debug "task:handle $*"

  local event="$1"
  local interval when action require

  when="$(d devbot data "$event" when)"

  if (( when < $(date '+%s') )); then

    # check for prerequisites
    require="$(d devbot events "$event" require)"

    if [[ $require ]]; then

      # grab the action to run out of the table
      require="$(d devbot requirements "$require")"

      if ! eval "$require"; then
        echo "$event requirement \"$require\" not met"
        return
      fi
    fi

    # run the event, update time
    interval="$(d devbot events "$event" interval)"
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
    action="$(d devbot events "$event" action)"
    [[ $action ]] || { echo "task:handle error: no action"; return; }

    d devbot data "$event" when = $(( interval + $(date '+%s') ))
    devbot::eval "$event" "$action"
  fi
}


devbot::eval() {

  # string, string ... -> none
  #
  # evaluate the given shell code by throwing in into the wind

  local name="$1"; shift
  common::debug "eval: $*"

  {
    local begin end
    begin="$(date '+%s')"
    bash -c "set -e; $*" || echo "error while running \"$*\""
    end="$(date '+%s')"

    d devbot data "$name" duration = "$(( end - begin ))"
  } &
  disown
}


devbot::main() {

  # none -> none
  #
  # run the main event loop

  mkdir -p ~/.devbot

  echo "starting devbot"
  while :; do

    while read -r event; do
      devbot::task:handle "$event"

    done < <(d devbot events --keys)

    sleep 5
  done
}
