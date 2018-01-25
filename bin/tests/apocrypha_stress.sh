#!/bin/bash

host="p.anardil.net"

key="$(head -c 50 /dev/urandom | md5sum | head -c 32)"

echo setup: "$key"
d -h "$host" "$key" get = value


echo synchronous get
time for _ in {1..100}; do
  d -h "$host" "$key" get
done | grep -v 'value'


echo asynchronous get
time {
  for _ in {1..100}; do
    d -h "$host" "$key" get &
  done
  wait
} | grep -v 'value'


echo synchronous set
time for _ in {1..100}; do
  d -h "$host" "$key" "$RANDOM" = value
done

echo asynchronous set
time {
  for _ in {1..100}; do
  d -h "$host" "$key" "$RANDOM" = value &
  done
  wait
}

echo cleanup
d "$key" --del
