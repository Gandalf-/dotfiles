#!/usr/bin/env bash

# clean - wizard libary
#
#   remove unnecessary files or packages intelligently


wizard_clean_docker_images() {

  case $1 in
    -f|--force) force="--force" ;;
  esac

  docker images \
    | common::multi-menu \
    | awk '{ print $3 }' \
    | common::map docker image rm "$force"
}


wizard_clean_docker_containers() {

  case $1 in
    -f|--force) force="--force" ;;
  esac

  docker ps -a \
    | common::multi-menu \
    | awk '{ print $1 }' \
    | common::map docker rm "$force"
}


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

  common::optional-help "$1" "[--dry]

  smart remove duplicate file names and intermediary file types

    clean up the filesystem under the current directory, mostly useful for
    removing duplicate files insync creates
  "

  local dry=0
  local counter=0
  case $1 in -d|--dry) dry=1; esac

  run() {
    # maybe execute the action
    echo "$@"

    if (( ! dry )); then
      "$@" || {
        common::error "unexpected failure while running $*"
      }
    fi

    (( counter++ ))
  }

  while read -r duplicated_path; do

    # determine what the path 'should' be
    local fixed_path; fixed_path="$(
      # shellcheck disable=SC2001
      sed -e 's/[ ]*([0-9]\+)//' <<< "$duplicated_path"
    )"

    # make sure the file still exists, this may not be the case if it's
    # directory was fixed before we got here
    [[ -e "$duplicated_path" ]] || continue

    if [[ -f "$fixed_path" ]]; then
      # both 'file' and 'file (2)' exist, pick the newer one

      fixed_path_time="$( stat -c '%Y' "$fixed_path" )"
      duplicate_time="$( stat -c '%Y' "$duplicated_path" )"

      if (( duplicate_time < fixed_path_time )); then
        run rm "$duplicated_path"
      else
        run mv "$duplicated_path" "$fixed_path"
      fi

    elif [[ -d "$fixed_path" ]]; then
      # both 'dir' and 'dir (2)' exist, pick the newer one

      fixed_path_time="$( stat -c '%Y' "$fixed_path" )"
      duplicate_time="$( stat -c '%Y' "$duplicated_path" )"

      if (( duplicate_time < fixed_path_time )); then
        run rm "$duplicated_path"
      else
        run mv "$duplicated_path" "$fixed_path"
      fi

    else
      # only 'file (2)' exists, rename it
      run mv "$duplicated_path" "$fixed_path"
    fi

  done < <(find . -regex '.*([0-9]+).*')

  # delete junk files
  while read -r file; do
    run rm "$file"
  done < <(find . -regex '.*\.\(pyc\|class\|o\|bak\)')

  if (( dry )); then
    common::echo "Would have cleaned up $counter files"
  else
    common::echo "Cleaned up $counter files"
  fi
}
