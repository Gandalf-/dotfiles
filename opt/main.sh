#!/usr/bin/env bash

set -e

replace() {
  local src="$1"
  local tgt="$2"
  echo "$src -> $tgt"

  [[ -e "$src" ]]
  rm -rf "$tgt"   # handles files, symlinks, and (e.g. nvim/fish) directory targets
  mkdir -p "$( dirname "$tgt" )"
  ln -s "$src" "$tgt"
}

install() {
  replace "$PWD"/etc/clang-format ~/.clang-format
  replace "$PWD"/etc/tidyrc       ~/.tidyrc
  replace "$PWD"/etc/prettierrc   ~/.prettierrc
  replace "$PWD"/etc/shellcheckrc ~/.shellcheckrc

  replace "$PWD"/etc/gitconfig    ~/.gitconfig
  replace "$PWD"/etc/tmux.conf    ~/.tmux.conf
  replace "$PWD"/etc/ghostty      ~/.config/ghostty/config

  replace "$PWD"/etc/nvim         ~/.config/nvim
  replace "$PWD"/etc/vimrc        ~/.vimrc

  replace "$PWD"/etc/fish         ~/.config/fish
  replace "$PWD"/etc/bashrc       ~/.bashrc
  replace "$PWD"/etc/bashrc       ~/.zshrc
}

lint() {
  mapfile -t scripts < <(
    find ./* -type f -exec file {} \; \
      | awk -F: '/shell script/ { print $1 }' \
  )
  shellcheck "${scripts[@]}"
}

"$@"
