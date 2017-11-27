#!/bin/env bash

# create fish completions for wizard
#   writes them to root/lib/

source $(which wizard)
mkdir -p ~/.config/fish/completions

{
  echo "complete -e -c w"

  while read -r line; do
    line="${line//_/ }"
    line="${line//wizard/}"
    line=( ${line} )

    last=$(( ${#line[@]} - 1 ))
    nlast=$(( last - 1 ))

    if (( ${#line[@]} == 1 )); then
      echo complete -f -c w -n __fish_prog_needs_command -a "'${line[0]} '"

    else
      for ((i=1; i < ${#line[@]}; i++)); do
        prefix="${line[$(( i - 1))]}"
        sufix="${line[$i]}"
        echo complete -f -c w -n "'__fish_prog_using_command $prefix'" -a "'$sufix '"
      done
    fi

  done < <(
    declare -F -p \
      | cut -f 3 -d ' ' \
      | grep -v 'common::\|autocli::'
    )
} > ~/.config/fish/completions/w.fish
