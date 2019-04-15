#!/usr/bin/env bash

async=0
assign=0

while [[ $1 ]]; do
  case $1 in
    async)
      async=1
      ;;
    set)
      assign=1
      ;;
  esac
  shift
done

key="$(head -c 50 /dev/urandom | md5sum | head -c 32)"

echo setup: "$key"
d "$key" get = value
echo
echo ======================

echo synchronous get
time for _ in {1..100}; do
  d "$key" get
done | grep -v 'value'
echo
echo ======================

(( async )) && {
  echo asynchronous get
  time {
    for _ in {1..100}; do
      d "$key" get &
    done
    wait
  } | grep -v 'value'
  echo
  echo ======================
}

(( assign )) && {
  echo synchronous set
  time for _ in {1..100}; do
    d "$key" "$RANDOM" = value
  done
  echo
  echo ======================

  (( async )) && {
    echo asynchronous set
    time {
      for _ in {1..100}; do
      d "$key" "$RANDOM" = value &
      done
      wait
    }
    echo
    echo ======================
  }
}

echo cleanup
d "$key" --del
