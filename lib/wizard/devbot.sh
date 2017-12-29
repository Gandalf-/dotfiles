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
}

wizard_devbot_add() {

  common::required-help "$2" "[interval] [command ...]

  add a new event to the devbot schedule

    w devbot add 60 'cd some/path/to/git/repo && git fetch'
  "

  local interval="$1"
  local procedure="${*:2}"

  devbot::add "$interval" "$procedure"

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
      wizard devbot start &
      disown
      echo "✓"
    fi

  else
    echo "✗"
  fi
}

wizard_devbot_show() {

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
    local interval="${data[0]}"
    local when="${data[1]}"
    local procedure="${data[*]:2}"

    local time; time="$(translate-time $(( when - $(date '+%s') )) )"

    common::echo "$procedure"
    echo "  every $(translate-time "$interval")"
    echo "  next $time from now"
    echo

  done < $schedule
}


# devbot library

devbot::add() {

  # interval -> command ... -> none
  #
  # writes an event to the schedule at (now + interval)

  echo "add got $*"

  local schedule=~/.devbot-schedule
  local interval="$1"
  local procedure="$2"
  local when="$(( interval + $(date '+%s') ))"

  echo "$interval $when $procedure" >> $schedule
}

devbot::initialize_events() {

  # none -> none
  #
  # add some basic tasks to the schedule

  local fivem=300
  local hour=3600
  local day=86400

  devbot::add $fivem \
    'insync-headless reject_all_new_shares austin.voecks@gmail.com'

  devbot::add $hour "cd $HOME && wizard git fetch"

  devbot::add $day 'echo "" > ~/.devbot-log'
  devbot::add $day 'wizard update pip'
  devbot::add $day 'wizard update apt'
  devbot::add $day 'rm -rf ~/google_drive/.insync-trash'
  devbot::add $day 'vim +PluginUpdate +qall'

}

devbot::runner() {

  # interval -> unix epoch time -> command ... -> none
  #
  # check the time field of the input, if it's passed then run the command.
  # otherwise we add it back to schedule unchanged

  # shellcheck disable=SC2206
  local data=( $@ )
  local schedule=~/.devbot-schedule

  local interval="${data[0]}"
  local when="${data[1]}"
  local procedure="${data[*]:2}"

  if (( when < $(date '+%s') )); then
    # run the event, add to schedule with updated time

    if [[ $procedure ]]; then
      (
        eval timeout 30 "$procedure"
        devbot::add "$interval" "$procedure"
      ) &

    else
      echo "runner error, no procedure"
    fi

  else
    # put the event back on the schedule unchanged
    echo "$interval $when $procedure" >> $schedule
  fi
  wait
}

devbot::main() {

  # none -> none
  #
  # set up the schedule if it's not already there, run the main event loop

  local schedule=~/.devbot-schedule
  local copy_schedule=~/.devbot-schedule-copy

  [[ -e $schedule ]] || devbot::initialize_events

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
