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

prune() {
  local tgt="$1"
  [[ -L "$tgt" ]] || return 0
  echo "prune $tgt"
  rm -f "$tgt"
}

install() {
  replace "$PWD"/etc/clang-format ~/.clang-format
  replace "$PWD"/etc/tidyrc       ~/.tidyrc
  replace "$PWD"/etc/prettierrc   ~/.prettierrc
  replace "$PWD"/etc/shellcheckrc ~/.config/shellcheckrc

  replace "$PWD"/etc/gitconfig    ~/.config/git/config
  replace "$PWD"/etc/tmux.conf    ~/.config/tmux/tmux.conf
  replace "$PWD"/etc/ghostty      ~/.config/ghostty/config

  replace "$PWD"/etc/nvim         ~/.config/nvim
  replace "$PWD"/etc/vimrc        ~/.vimrc

  replace "$PWD"/etc/fish         ~/.config/fish
  replace "$PWD"/etc/bashrc       ~/.bashrc
  replace "$PWD"/etc/bashrc       ~/.zshrc

  # relocated under ~/.config
  prune ~/.shellcheckrc
  prune ~/.gitconfig
  prune ~/.tmux.conf

  for s in "$PWD"/etc/claude/skills/*; do
    local name; name="$( basename "$s" )"
    replace "$s" ~/.claude/skills/"$name"
  done
}

lint() {
  mapfile -t scripts < <(
    find ./* -type f -exec file {} \; \
      | awk -F: '/shell script/ { print $1 }' \
  )
  shellcheck "${scripts[@]}"
}

"$@"
