#!/bin/bash

# devbot
#
#   daemon to handle simple background tasks automatically based on a schedule
#
#   tasks are added to a schedule file, that's read every 5 seconds.
#     task :: interval time command ...
#
#   when a tasks time is reached, the command is run and it's added back to the
#   schedule with an updated time (current time + interval)
#
#   this approach makes devbot's schedule persist between runs, allowing very
#   infrequent tasks to be scheduled with confidence

# wizard commands

wizard_devbot_start() {

  common::optional-help "$1" "

  start devbot, will fail if already running
  "

  local pfile=~/.devbot-pid
  local lfile=~/.devbot-log

  [[ -e $pfile ]] && common::error "devbot already running"

  devbot::main >> $lfile 2>&1 &
  local pid=$!
  disown

  echo "$pid" > $pfile

  return $#
}

wizard_devbot_edit() {

  common::optional-help "$1" "

  open up the devbot schedule in Vim to make manual changes.
  devbot is paused while Vim is open.
  "

  local schedule=~/.devbot-schedule

  wizard devbot kill
  vim ~/.devbot-schedule

  devbot::save
  wizard devbot start

  return $#
}

wizard_devbot_bounce() {

  wizard devbot kill
  wizard devbot start

  return $#
}

wizard_devbot_kill() {

  common::optional-help "$1" "

  stop devbot, will fail if not running
  "

  local pfile=~/.devbot-pid

  [[ -e $pfile ]] || common::error "devbot is not running"

  pkill -F $pfile
  rm $pfile

  return $#
}

wizard_devbot_status() {

  common::optional-help "$1" "

  report whether devbot is running, used by tmux status
  "

  local pfile=~/.devbot-pid
  local lfile=~/.devbot-log

  if [[ -e $pfile ]]; then
    read -r pid < $pfile

    if kill -0 "$pid"; then
      echo "✓"

    else
      echo detected stale pid file, restarting >> $lfile
      rm $pfile
      wizard devbot start
      echo "✓"
    fi

  else
    echo "✗"
  fi
}

wizard_devbot_list() {

  local schedule=~/.devbot-schedule

  translate-time() {

    local time="$1"

    if (( time <= 60 )); then
      echo "$time seconds"

    elif (( time <= 3600 )); then
      echo "$(( time / 60 )) minutes"

    elif (( time <= 86400 )); then
      echo "$(( time / 3600 )) hours"

    else
      echo "more than a day"
    fi
  }

  echo
  while read -r event; do
    # shellcheck disable=SC2206
    local data=( $event )
    local interval="${data[1]}"
    local when="${data[2]}"
    local procedure="${data[*]:3}"

    local time; time="$(translate-time $(( when - $(date '+%s') )) )"

    common::echo "$procedure"
    echo "  every $(translate-time "$interval")"
    echo "  next $time from now"
    echo

  done < $schedule

  return $#
}


# devbot library

devbot::task:write() {

  local schedule=~/.devbot-schedule
  echo "task $*" >> "$schedule"
}

devbot::task:add() {

  # interval -> command -> none
  #
  # writes an event to the schedule at (now + interval)

  echo "task:add $*"

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

  local interval="$1"
  local when="$2"
  local action="$3"

  if (( when < $(date '+%s') )); then
    # run the event, add to schedule with updated time

    common::debug "task:handle $action, $interval, $when"

    [[ $action ]] || { echo "task:handle error: no action"; return; }
    devbot::eval "$action"
    devbot::task:add "$interval" "$action"

  else
    # put the event back on the schedule unchanged
    devbot::task:write "$interval $when $action"
  fi
}


devbot::remind:write() {

  local schedule=~/.devbot-schedule
  echo "remind $*" >> "$schedule"
}

devbot::remind:add() {

  # when -> command -> none

  local when="$1"; shift
  local action="$*"

  devbot::remind:write "$when $action"
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

    *)
      echo "runner error, unrecongized task type: $type"
      ;;
  esac
}

devbot::save() {

  # none -> none
  #
  # save the schedule to ~/.devbotrc minus the run timestamps

  local schedule=~/.devbot-schedule
  # TODO update to parse types
  # cut -f 1,2,4- -d ' ' "$schedule" > ~/.devbotrc
}


devbot::eval() {

  # string ... -> none
  #
  # evaluate the given shell code with safety checks and timeout

  timeout 30 bash -c "$*" || echo "error while running \"$*\""
}


devbot::main() {

  # none -> none
  #
  # set up the schedule if it's not already there, run the main event loop

  local schedule=~/.devbot-schedule
  local copy_schedule=~/.devbot-schedule-copy

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
