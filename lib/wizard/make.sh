#!/bin/env bash

# wizard
#   The main wizard library. Functions are defined here, processed by
#   auto_cli.sh and built into a library sourced by bin/w
#
#   All the intermediary functions are produced by auto_cli.sh

wizard_make_hash() {

  head -c 50 /dev/urandom | md5sum | cut -f 1 -d ' '
}


common::require 'mount' &&
wizard_make_tmpfs() {

  common::required-help "$1" "[target directory]

  create a tmpfs in the target directory

    wizard make tmpfs /usr/local/tmp/tmpfs
  "

  local target="$1"

  common::do mkdir -p "$target"
  common::sudo \
    mount -t tmpfs \
    -o size=4G,nr_inodes=0,mode=700,uid=1000,gid=1000 \
    tmpfs_"$(basename "$target")" "$target"

  return $#
}


common::require 'git' 'mount' &&
wizard_make_tmpfs-git-clone() {

  common::required-help "$1" "[name] (git repo)

  clone a repo into a new tmpfs directory

    w make tmpfs-git-clone haskell
    w make tmpfs-git-clone https://github.com/danilop/yas3fs.git
  "

  is_repo() {
    [[ $1 =~ github ]]
  }

  name_from_repo() {
    # https://github.com/danilop/yas3fs.git -> yas3fs
    local array; read -ar array <<< "${1//\// }"
    local name="${array[ $(( ${#array[@]} - 1)) ]}"
    result="${name%.*}"
  }

  # given just a repo url
  if is_repo "$1"; then
    name_from_repo "$1"
    local name="$result"
    local repo="$1"

  # just a name, try my github
  else
    local name="$1"
    local repo="https://github.com/Gandalf-/$name.git"
  fi

  wizard_make_tmpfs "$name"
  common::do git clone "$repo" "$name"

  return $#
}


common::require 'mount' &&
wizard_make_mirror() {

  common::required-help "$2" "[source] [target]

  mirror a root directory (source) into a tmpfs directory (target)

    wizard make mirror ~/google_drive /usr/local/tmp/
  "

  local source="$1"
  local target="$2/$1"

  [[ -d $source ]] || common::error "$source does not exist"
  [[ -d $target ]] && commor::error "$target already exists"

  common::do mkdir -p "$target"
  common::sudo \
    mount -t tmpfs \
    -o size=4G,nr_inodes=0,mode=700,uid=1000,gid=1000 \
    tmpfs_"$(basename "$source")" "$target"

  common::do cp -r "$source"/\* "$target"

  return $#
}


if common::program-exists 'tmux'; then
  wizard_make_session() {
    common::optional-help "$1" "[name]

  create a new tmux session and move to it
    "

    name="${*:-$RANDOM}"
    tmux new -d -s "$name"
    tmux switch-client -t "$name"
    return $#
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
fi


wizard_make_file_shell() {
  cat > "$1".sh << EOF
#!/bin/bash

main() {

  exit 0
}

main "\$@"
EOF
}


wizard_make_file_python() {
  cat > "$1.py" << EOF
#!/usr/bin/python

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
  cat > "$1".c << EOF
#include <stdio.h>

int main(int argc, char *argv[]) {

  return 0;
}
EOF
}


wizard_make_file_cpp() {
  cat > "$1".cpp << EOF
#include <iostream>

int main(int argc, char const *argv[]) {

  return 0;
}
EOF
}


wizard_make_file_java() {
  cat > "$1".java << EOF
public class $1 {

  public static void main(String[] argv) {

    System.out.println("Hello world");
  }
}
EOF
}


wizard_make_project_python() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file_python "$1"
  return 1
}


wizard_make_project_c() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file_c "$1"
  touch "$1".h
  mmake -l c -o "$1"
}


wizard_make_project_cpp() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file_cpp "$1"
  touch "$1".h
  mmake -l cpp -o "$1"
}


wizard_make_project_java() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file_java "$1"
  mmake -l java -o "$1"
}
