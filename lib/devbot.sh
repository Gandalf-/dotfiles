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


devbot::task:write() {

  common::debug "task:write $*"

  local schedule=~/.devbot/schedule
  echo "task $*" >> "$schedule"
}


devbot::task:add() {

  # interval -> command -> none
  #
  # writes an event to the schedule at (now + interval)

  common::debug "task:add $*"

  local interval="$1"; shift
  local when="$(( interval + $(date '+%s') ))"
  local action="$*"

  devbot::task:write "$interval $when $action"
}


devbot::task:handle() {

  # interval -> when -> action -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  common::debug "task:handle $*"

  local interval="$1"
  local when="$2"
  local action="$3"

  if (( when < $(date '+%s') )); then
    # run the event, add to schedule with updated time

    [[ $action ]] || { echo "task:handle error: no action"; return; }
    devbot::eval "$action"
    devbot::task:add "$interval" "$action"

  else
    # put the event back on the schedule unchanged
    devbot::task:write "$interval $when $action"
  fi
}


devbot::report:write() {

  common::debug "report:write $*"

  local schedule=~/.devbot/schedule
  echo "report $*" >> "$schedule"
}


devbot::report:add() {

  # interval -> command -> none
  #
  # writes an event to the schedule at (now + interval)

  common::debug "report:add $*"

  local interval="$1"; shift
  local when="$(( interval + $(date '+%s') ))"
  local action="$*"

  devbot::report:write "$interval $when $action"
}


devbot::report:handle() {

  # interval -> when -> action -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  common::debug "report:handle $*"

  local interval="$1"
  local when="$2"
  local action="$3"

  local rfile=~/.devbot/report

  if (( when < $(date '+%s') )); then
    # run the event, add to schedule with updated time

    [[ $action ]] || { echo "report:handle error: no action"; return; }

    devbot::eval "$action" >> $rfile
    common::inplace-file-op "$rfile" "sort | uniq"

    devbot::report:add "$interval" "$action"

  else
    # put the event back on the schedule unchanged
    devbot::report:write "$interval $when $action"
  fi

}


devbot::initialize() {

  # none -> none
  #
  # converts events in ~/.devbotrc to schedule events
  #
  # very similar to devbot::runner, except that the input doesn't have
  # timestamps

  echo "loading events from ~/.devbotrc"

  while read -r line; do

    # shellcheck disable=SC2206
    local data=( $line )
    local type="${data[0]}"

    case $type in
      task)
        # type | interval | action ...
        local interval="${data[1]}"
        local action="${data[*]:2}"

        devbot::task:add "$interval" "$action"
        ;;

      *)
        echo "initialize error: unrecongized event type: $type"
        ;;
    esac

  done < ~/.devbotrc
}


devbot::runner() {

  # string -> ... -> none
  #
  # figure out which type of event this is, and call the handler

  common::debug "runner: $*"

  # shellcheck disable=SC2206
  local data=( $@ )
  local type="${data[0]}"

  case $type in
    task)
      # type | interval | when | action ...
      local interval="${data[1]}"
      local when="${data[2]}"
      local action="${data[*]:3}"

      devbot::task:handle "$interval" "$when" "$action"
      ;;

    report)
      # type | interval | when | action ...
      local interval="${data[1]}"
      local when="${data[2]}"
      local action="${data[*]:3}"

      devbot::report:handle "$interval" "$when" "$action"
      ;;

    *)
      echo "runner error, unrecongized task type: $type"
      ;;
  esac
}


devbot::save() {

  # none -> none
  #
  # save the schedule to ~/.devbotrc minus the run timestamps

  local schedule=~/.devbot/schedule
  # TODO update to parse types
  # cut -f 1,2,4- -d ' ' "$schedule" > ~/.devbotrc
}


devbot::eval() {

  # string ... -> none
  #
  # evaluate the given shell code with safety checks and timeout

  common::debug "eval: $*"
  timeout 300 bash -c "$*" || echo "error while running \"$*\""
}


devbot::main() {

  # none -> none
  #
  # set up the schedule if it's not already there, run the main event loop

  mkdir -p ~/.devbot
  local schedule=~/.devbot/schedule
  local copy_schedule=~/.devbot/schedule-copy

  [[ -s $schedule ]] || devbot::initialize

  echo "starting devbot"
  while :; do

    # read the schedule from a copy, since we'll be rewriting the original
    touch "$schedule"
    mv $schedule $copy_schedule

    while read -r event; do

      devbot::runner "$event"

    done < $copy_schedule

    sleep 5
  done
}
