#!/bin/env bash

# create fish completions for wizard
#   writes them to root/lib/

source $(which wizard)  # grab all function definitions
mkdir -p ~/.config/fish/completions

{
  echo "complete -e -c w"

  while read -r line; do
    line="${line//_/ }"         # replace '_' with ' '
    line="${line//wizard/}"     # remove 'wizard'
    line=( $line )              # break into array of words

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
      | awk '{print $3}' \
      | grep -v 'common::\|autocli::\|devbot::'
      # not interested in library functions
    )
} > ~/.config/fish/completions/w.fish
