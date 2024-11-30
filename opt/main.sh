#!/usr/bin/env bash

set -e

replace() {
  local src="$1"
  local tgt="$2"
  rm -f "$tgt"
  mkdir -p "$( dirname "$tgt" )"
  ln -s "$src" "$tgt"
}

install() {
  replace "$PWD"/etc/clang-format ~/.clang-format
  replace "$PWD"/etc/tidyrc       ~/.tidyrc

  replace "$PWD"/etc/gitconfig    ~/.gitconfig
  replace "$PWD"/etc/tmux.conf    ~/.tmux.conf

  replace "$PWD"/etc/init.vim     ~/.config/nvim/init.vim
  replace "$PWD"/etc/init.vim     ~/.vimrc

  replace "$PWD"/etc/fish         ~/.config/fish
  replace "$PWD"/etc/bashrc       ~/.bashrc
}

"$@"
