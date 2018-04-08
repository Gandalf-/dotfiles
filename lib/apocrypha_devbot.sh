#!/bin/bash

# devbot library
#
#   daemon to handle simple background tasks automatically based on a schedule
#
#   tasks are added to a schedule file, that's read every 5 seconds.
#
#     task :: interval time command ...
#
#   when a tasks time is reached, the command is run and it's added back to the
#   schedule with an updated time (current time + interval)
#
#   this approach makes devbot's schedule persist between runs, allowing very
#   infrequent tasks to be scheduled with confidence

devbot::service:handle() {

  common::debug "service:handle $*"

  local service="$1"
  local pfile=~/.devbot/"$service".pid
  local lfile=~/.devbot/"$service".log

  if [[ -f "$pfile" ]]; then

    if pkill -0 -F "$pfile"; then
      # service is running fine
      return
    fi
  fi

  touch "$pfile"

  local action; action="$(d devbot services "$service" action)"
  eval "LOCK=$pfile $action" >> "$lfile" 2>&1 &

  local pid=$!
  disown

  echo "$pid" > "$pfile"
}

devbot::task:handle() {

  # interval -> when -> action -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  common::debug "task:handle $*"

  local event="$1"
  local interval when action require

  when="$(d devbot events "$event" when)"

  if (( when < $(date '+%s') )); then

    require="$(d devbot events "$event" require)"

    if [[ $require ]]; then
      if ! eval "$require"; then
        echo "task:handle $event requirement \"$require\" not met"
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
    d devbot events "$event" when = $(( interval + $(date '+%s') ))
    devbot::eval "$action"
  fi
}


devbot::eval() {

  # string ... -> none
  #
  # evaluate the given shell code by throwing in into the wind

  common::debug "eval: $*"
  bash -c "$*" || echo "error while running \"$*\"" &
  disown
}


devbot::main() {

  # none -> none
  #
  # set up the schedule if it's not already there, run the main event loop

  mkdir -p ~/.devbot

  echo "starting devbot"
  while :; do

    # read the schedule from a copy, since we'll be rewriting the original

    while read -r event; do

      devbot::task:handle "$event"

    done < <(d devbot events --keys)

    while read -r service; do

      devbot::service:handle "$service"

    done < <(d devbot services --keys)

    sleep 5
  done
}
