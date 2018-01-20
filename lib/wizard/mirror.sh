#!/bin/env bash

# mirror - wizard libary
#
# this commands only make sense in a ChromeOS -> Chromebrew context

common::require "rsync" &&
wizard_mirror_push() {

  common::optional-help "$1" "

  push /usr/local changes to archive
  "

  case $1 in
    --archway)  remote=Archway:/mnt/z/Austin/Documents/local ;;
    ''|--local) remote=~/Downloads/local ;;
  esac

  [[ $remote ]] || \
      common::error "Unrecongized remote \"$remote\""

  common::do \
    rsync --delete --human-readable --archive --update --progress \
    /usr/local/ "$remote"
}


wizard_mirror_diff() {

  common::optional-help "$1" "

  show what would be pushed and pulled by a mirror command
  "

  case $1 in
    --archway)  remote=Archway:/mnt/z/Austin/Documents/local ;;
    ''|--local) remote=~/Downloads/local ;;
  esac

  [[ $remote ]] || \
      common::error "Unrecongized remote \"$remote\""

  common::echo "push..."
  common::do \
    rsync --human-readable --dry-run --archive --update --verbose \
    /usr/local/ "$remote"

  common::echo ""
  common::echo "pull..."
  common::do \
    rsync --human-readable --dry-run --archive --update --verbose \
    "$remote"/ /usr/local

  return $#
}


common::require "rsync" &&
wizard_mirror_pull() {

  common::optional-help "$1" "

  pull archive into /usr/local
  "

  case $1 in
    --archway)  remote=Archway:/mnt/z/Austin/Documents/local ;;
    ''|--local) remote=~/Downloads/local ;;
  esac

  [[ $remote ]] || \
      common::error "Unrecongized remote \"$remote\""

  common::do \
    rsync --delete --human-readable --archive --update --progress \
    "$remote"/ /usr/local
}
