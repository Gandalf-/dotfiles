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

wizard_devbot_yaml_dump() {

  common::optional-help "$1" "

  write out the current devbot configuration file to disk
  "

  devbot_config=~/.devbot/config.yml

  python3 -c "
import yaml
from apocrypha.client import Client

client = Client()
data = client.get('devbot')
print('$devbot_config')

with open('$devbot_config', 'w+') as yaml_file:
    yaml.dump(data, yaml_file, default_flow_style=False)
"
}

wizard_devbot_yaml_load() {

  common::optional-help "$1" "

  read the current devbot configuration file from disk into apocrypha
  "
  devbot_config=~/.devbot/config.yml

  test -s "$devbot_config" ||
    common::error "$devbot_config not found"

  python3 -c "
import yaml
from apocrypha.client import Client

with open('$devbot_config', 'r') as yaml_file:
    data = yaml.load(yaml_file)

client = Client()
for key in data:
    client.set('devbot', key, value=data[key])
"

}

wizard_devbot_start() {

  common::optional-help "$1" "

  start devbot, will fail if already running
  "

  mkdir -p ~/.devbot
  local pfile=~/.devbot/pid
  local lfile=~/.devbot/log

  common::file-exists "$pfile" &&
    common::error "devbot already running"

  devbot::main >> $lfile 2>&1 &
  local pid=$!
  disown

  echo "$pid" > $pfile
}

wizard_devbot_bounce() {

  common::optional-help "$1" "

  restart devbot
  "

  wizard devbot kill
  sleep 0.1
  wizard devbot start
}

wizard_devbot_kill() {

  common::optional-help "$1" "

  stop devbot, will fail if not running
  "

  local pfile=~/.devbot/pid

  common::file-exists $pfile ||
    common::error "devbot is not running"

  pkill -F $pfile
  rm $pfile
}

wizard_devbot_list() {

  common::optional-help "$1" "

  print out the current devbot schedule
  "

  devbot_list
  return

  {
    echo
    while read -r event; do
      local interval; interval="$(d devbot events "$event" interval)"

      if ! common::is-integer "$interval"; then
        string_interval=1
      else
        string_interval=0
      fi

      local when action next
      when="$(d devbot data "$event" when)"
      action="$(d devbot events "$event" action)"
      next="$(common::translate-time $(( when - $(date '+%s') )) )"
      duration="$(d devbot data "$event" duration)"

      common::echo "$action"
      if (( string_interval )); then
        echo -n "  $interval"
      else
        echo -n "  every $(common::translate-time "$interval")"
      fi
      echo ", next $next from now."
      echo "  last run took $duration seconds"
      echo

    done < <(d devbot events --keys | sort)
  } > /dev/shm/devbot-list

  cat /dev/shm/devbot-list
}
