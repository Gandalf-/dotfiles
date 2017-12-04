#!/bin/env bash

common::require "rsync" &&
wizard_mirror_push() {

  common::optional_help "$1" "

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

  common::optional_help "$1" "

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

  common::optional_help "$1" "

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


common::require "apt" &&
wizard_configure_ubuntu_developement() {

  common::sudo apt update -y
  common::sudo apt upgrade -y
  common::sudo apt install htop python-pip tmux silversearcher-ag

  wizard_install_git
  wizard_install_fish
  wizard_build_vim
}


