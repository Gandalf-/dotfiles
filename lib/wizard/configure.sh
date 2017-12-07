#!/bin/env bash


common::require "apt" &&
wizard_configure_ubuntu_small() {
  # install basic programs

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

  common::sudo apt update -y
  common::sudo apt upgrade -y
  common::sudo apt install htop python-pip tmux silversearcher-ag

  wizard_install_git
  wizard_install_fish
  wizard_build_vim
}
