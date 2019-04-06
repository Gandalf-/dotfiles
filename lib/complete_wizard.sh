#!/usr/bin/env bash

# create fish completions for wizard
#   writes them to {root}/lib/fish/completions/w.fish

# shellcheck disable=SC1090
root="$(dirname "${BASH_SOURCE[0]}")"/..
source "${root}/bin/wizard"

mkdir -p ~/.config/fish/completions

{
  echo "complete -e -c w"

  while read -r line; do
    line="${line//_/ }"         # replace '_' with ' '
    line="${line//wizard/}"     # remove 'wizard'
    # shellcheck disable=SC2206
    line=( $line )              # break into array of words

    if (( ${#line[@]} == 1 )); then
      echo complete -f -c w -n __fish_prog_needs_command -a "'${line[0]} '"

    else
      for ((i=1; i < ${#line[@]}; i++)); do
        prefix="${line[$(( i - 1))]}"
        sufix="${line[$i]}"
        echo -n "complete -f -c w -n"
        echo    "'__fish_prog_using_command $prefix'" -a "'$sufix '"
      done
    fi

  done < <(
    declare -F -p \
      | awk '{print $3}' \
      | grep -v 'common::\|autocli::\|devbot::'
      # not interested in library functions
    )
} > ~/.config/fish/completions/w.fish
