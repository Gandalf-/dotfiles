#!/bin/env bash

# clean - wizard libary
#
#   remove unnecessary files or packages intelligently

wizard_clean_every-other() {

  common::optional-help "$1" "

  delete every other file in the current directory
  "

  local delete=0

  for file in *; do
    (( delete )) && rm -v "$file" &
    delete=$(( ! delete ))
  done

  wait
}

common::require dpkg apt-get &&
wizard_clean_boot() {

  common::optional-help "$1" "

  safely cleans up old Linux kernel versions from /boot
  "

  dpkg --list \
    | grep linux-image \
    | awk '{ print $2 }' \
    | sort -V \
    | sed -n '/'"$(uname -r)"'/q;p' \
    | xargs sudo apt-get -y purge
}


common::require 'dpkg' &&
wizard_clean_apt() {

  common::optional-help "$1" "

  force purge removed apt packages. this will remove configuration files too.
  "

  dpkg --list \
    | grep "^rc" \
    | cut -d " " -f 3 \
    | xargs sudo dpkg --purge \
    || common::color-error "Looks like there's nothing to clean!"
}


wizard_clean_haskell() {

  common::optional-help "$1" "

  remove intermediary GHC compilation files, *.hi *.o
  "

  common::do rm ./*.hi ./*.o || common::error "No files to clean"
}


wizard_clean_files() {

  local dry=0 counter=0

  common::optional-help "$1" "[--dry]

  smart remove duplicate file names and intermediary file types

    clean up the filesystem under the current directory, mostly useful for
    removing duplicate files insync creates
  "

  case $1 in -d|--dry) dry=1; shift; esac

  while read -r file; do
    local fixed; fixed="$(sed -e 's/[ ]*([0-9]\+)//' <<< "$file")"

    # make sure the file still exists
    if [[ -e "$file" ]] ; then

      # target file exists too, make sure they're different
      if [[ -f "$fixed" ]]; then

        soriginal=$(sha1sum "$file")
        snew=$(sha1sum "$fixed")

        echo "remove dup: $file"
        if [[ $soriginal != "$snew" ]]; then
          (( dry )) \
            || rm "$file" \
            || exit

        else
          echo "$file $fixed both exist but are different"
        fi

      else
        echo "rename dup: $file"
        (( dry )) \
          || mv "$file" "$fixed" \
          || exit
      fi

      (( counter++ ))
    fi
  done < <(find . -regex '.*([0-9]+).*')

  while read -r file; do
    echo "remove: $file"

    (( dry )) \
      || rm "$file" \
      || exit

    (( counter++ ))

  done < <(find . -regex '.*\.\(pyc\|class\|o\|bak\)')

  if (( dry )); then
    echo "Would have cleaned up $counter files"
  else
    echo "Cleaned up $counter files"
  fi
}
