#!/bin/bash

AHOST=''

apocrypha::query() {

  if [[ $AHOST ]]; then
    d -h "$AHOST" "$@"

  else
    d "$@"
  fi
}
