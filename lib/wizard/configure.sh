#!/bin/env bash

# configure - wizard libary
#
# initial system configuration, mostly useful for droplets


common::require "apt" &&
wizard_configure_ubuntu_small() {

  common::optional-help "$1" "

  install basic development packages
  "

  common::sudo apt-add-repository ppa:fish-shell/release-2
  common::sudo apt update
  common::sudo apt upgrade
  common::sudo apt install \
    make gcc libreadline-dev build-essential cmake \
    tmux fish vim git ipython python-pip \
    silversearcher-ag
}


wizard_configure_add-user() {

  common::required-help "$1" "[user name]

  add a new sudo user to the system
    - creates user, add to sudoers
    - create home directory
    - copy root ssh keys to directory
  "
  user="$1"

  common::do adduser "$user"
  common::do usermod -aG sudo "$user"
  common::do chsh -s /usr/bin/fish "$user"
  common::do mkdir -p /home/"$user"/.ssh/
  common::do cp -r /root/.ssh/ /home/"$user"/

  common::do chown -R "$user:$user" /home/"$user"/
  return 1
}


common::require "apt" &&
wizard_configure_ubuntu_developement() {

  common::optional-help "$1" "

  install additional development packages from source
  "

  common::sudo apt update -y
  common::sudo apt upgrade -y
  common::sudo apt install htop python-pip tmux silversearcher-ag

  wizard_install_git
  wizard_install_fish
  wizard_build_vim
}
