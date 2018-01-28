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


devbot::task:handle() {

  # interval -> when -> action -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  common::debug "task:handle $*"

  local event="$1"

  local interval when action
  when="$(d devbot events "$event" when)"

  if (( when < $(date '+%s') )); then

    # run the event, update time
    interval="$(d devbot events "$event" interval)"
    action="$(d devbot events "$event" action)"

    [[ $action ]] || { echo "task:handle error: no action"; return; }
    d devbot events "$event" when = $(( interval + $(date '+%s') ))
    devbot::eval "$action"

  fi
}


devbot::runner() {

  # string -> ... -> none
  #
  # figure out which type of event this is, and call the handler

  common::debug "runner: $*"

  local event="$1"
  devbot::task:handle "$event"
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

      devbot::runner "$event"

    done < <(d devbot events --keys)

    sleep 5
  done
}
