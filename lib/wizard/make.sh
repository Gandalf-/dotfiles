#!/bin/env bash

# shellcheck disable=SC2155

# wizard
#   The main wizard library. Functions are defined here, processed by
#   auto_cli.sh and built into a library sourced by bin/w
#
#   All the intermediary functions are produced by auto_cli.sh

common::require ctags &&
wizard_make_ctags() {

  ctags \
    --c++-kinds=+p \
    --fields=+iaS \
    --extra=+q \
    --language-force=C++ \
    -R .
}


wizard_make_passwordless-sudo() {

  common::echo "Add this to /etc/sudoers"
  echo "$(whomai) ALL=(ALL) NOPASSWD:ALL"
}


wizard_make_hash() {

  head -c 50 /dev/urandom | md5sum | cut -f 1 -d ' '
}


common::require mount &&
wizard_make_tmpfs() {

  common::required-help "$1" "[target directory]

  create a tmpfs in the target directory. this doesn't overwrite the target
  directory if it exists; it will be available again after you unmount the
  tmpfs

    wizard make tmpfs /usr/local/tmp/tmpfs
    wizard make tmpfs ~/.stack
  "

  local -r target="$1"
  local -r uid="$(id -u)"
  local -r gid="$(id -g)"

  common::do mkdir -p "$target"
  CONFIRM=1 common::sudo \
    mount -t tmpfs \
    -o size=12G,nr_inodes=0,mode=700,uid="$uid",gid="$gid" \
    tmpfs_"$(basename "$target")" \
    "$target"
}


common::require 'git' 'mount' &&
wizard_make_tmpfs-git-clone() {

  common::required-help "$1" "[name] (git repo)

  clone a repo into a new tmpfs directory.

  useful for cloning, make, install, delete work flows

    w make tmpfs-git-clone haskell
    w make tmpfs-git-clone https://github.com/danilop/yas3fs.git
  "

  is_repo() {
    [[ $1 =~ github ]]
  }

  name-from-repo() {
    # https://github.com/danilop/yas3fs.git -> yas3fs

    # shellcheck disable=2206
    local array=( ${1//\// } )
    local name="${array[ $(( ${#array[@]} - 1)) ]}"
    result="${name%.*}"
  }

  # given just a repo url
  if is_repo "$1"; then
    name-from-repo "$1"
    local name="$result"
    local repo="$1"

  # just a name, try my github
  else
    local name="$1"
    local repo="https://github.com/Gandalf-/$name.git"
  fi

  wizard_make_tmpfs "$name"
  common::do git clone --depth 1 "$repo" "$name"
}


wizard_make_session() {
  common::required-help "$1" "[name] (command)

  create a new tmux session and move to it
  "

  local name="$1"
  local command="$2"

  tmux list-sessions | grep -q "$name" || {
    if [[ $command ]]; then
      tmux new-session -d -s "$name" "$command"
    else
      tmux new-session -d -s "$name"
    fi
  }

  _set_context "$name" 2>/dev/null

  if [[ $TMUX ]]; then
    tmux switch-client -t "$name"
  else
    tmux attach-session -t "$name"
  fi
}

wizard_layout_vertical() {
  tmux select-layout even-vertical
}

wizard_layout_horizontal() {
  tmux select-layout even-horizontal
}

wizard_layout_tiled() {
  tmux select-layout tiled
}


wizard_make_file_shell() {

  common::required-help "$1" "[name]

  create a shell script out of a template
  "

  cat > "$1".sh << EOF
#!/bin/bash

EOF
}


wizard_make_file_python() {
  common::required-help "$1" "[name]

  create a pythonscript out of a template
  "

  cat > "$1.py" << EOF
#!/usr/bin/env python3

'''
template program
'''

import sys


def main(_):
    ''' list of strings -> none
    '''
    pass


if __name__ == '__main__':
    main(sys.argv)
EOF
}


wizard_make_file_c() {
  common::required-help "$1" "[name]

  create a c program out of a template
  "

  cat > "$1".c << EOF
#include <stdio.h>

int main(int argc, char ** argv) {

  return 0;
}
EOF
}


wizard_make_file_cpp() {
  common::required-help "$1" "[name]

  create a cpp program file out of a template
  "

  cat > "$1".cpp << EOF
#include <iostream>

int main(int argc, char const *argv[]) {

  return 0;
}
EOF
}


wizard_make_file_java() {
  common::required-help "$1" "[name]

  create a java program file out of a template
  "

  cat > "$1".java << EOF
public class $1 {

  public static void main(String[] argv) {

    System.out.println("Hello world");
  }
}
EOF
}
