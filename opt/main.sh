#!/usr/bin/env bash

set -e

replace() {
  local src="$1"
  local tgt="$2"
  echo "$src -> $tgt"

  [[ -e "$src" ]]
  rm -f "$tgt"
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

  replace "$PWD"/etc/init.vim     ~/.config/nvim/init.vim
  replace "$PWD"/etc/init.vim     ~/.vimrc

  replace "$PWD"/etc/fish         ~/.config/fish
  replace "$PWD"/etc/bashrc       ~/.bashrc

  replace "$PWD"/etc/claude-settings.json ~/.claude/settings.json
}

lint() {
  mapfile -t scripts < <(
    find ./* -type f -exec file {} \; \
      | awk -F: '/shell script/ { print $1 }' \
  )
  shellcheck "${scripts[@]}"
}

"$@"
