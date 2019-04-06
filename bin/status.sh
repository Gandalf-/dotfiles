#!/usr/bin/env bash

# does the same as wizard devbot status, but without the overhead. this is
# important because it's called every 5 seconds by each tmux session

pfile=~/.devbot/pid
lfile=~/.devbot/log

if ! d database alive check >/dev/null; then
  echo "!"

elif [[ -e $pfile ]]; then
  read -r pid < $pfile

  if kill -0 "$pid"; then
    echo "✓"

  else
    echo "detected stale pid file" >> "$lfile"
    rm "$pfile"
    w devbot start
  fi

else
  echo "✗"
fi
