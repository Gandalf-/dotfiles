#!/bin/bash

# wizard
#   The main wizard library. Functions are defined here, processed by
#   auto_cli.sh and built into a library sourced by bin/w
#
#   All the intermediary functions are produced by auto_cli.sh

__name=""

common::program-exists 'ffmpeg' &&
wizard_do_transcode_movies() {
  #
  local preset=slow

  echo "Processing: $*"
  for file in "$@"; do
    ffmpeg -hide_banner -i "$file" \
      -c:v libx264 -crf 19 -preset "$preset" -strict -2 \
      -c:a aac -b:a 192k -ac 2 "${file%.*}.mp4" \
      || common::error "failed on \"$file\". Giving up"

    echo "Waiting..."; sleep 5
    rm "$file"
  done

  return $#
}

wizard_do_pin-to-home() {

  [[ ! -z "$*" ]] && ln -s "$@" ~/;
}

wizard_show_largest-packages() {

  common::optional_help "$1" "

  list all packages installed, sorted by size
  "

  # shellcheck disable=SC2016
  dpkg-query -Wf '${Installed-Size}\t${Package}\n' \
    | sort -n
}

wizard_show_progress() {

  common::required_help "$1" "

  run a command repeatedly, clear the screen between runs
  "

  while :; do
    eval "$@"
    sleep 1
    clear
  done

  return $#
}

wizard_show_history() {
  #

  common::optional_help "$1" "(amount) (range)

  show the <amount> of the most frequently run commands
  "

  local amount=${1:-25}
  local range=${2:-1}

  fish -c history \
    | cut -f "$range" -d' ' \
    | sort \
    | uniq -c \
    | sort -nr \
    | head -n "$amount"

  [[ $1 && $2 ]] && return 2
  [[ $1 ]] && return 1
  return 0
}

wizard_show_disk() {
  common::optional_help "$1" "

  show disk and partition usage
  "

  df -h
}

wizard_show_weather() {

  curl http://wttr.in/~"${1:-Seattle}";
  [[ $1 ]] && return 1
  return 0
}

wizard_start_http-server() {

  python -m SimpleHTTPServer
  return 0
}

wizard_do_first-time-install_small() {
  # install basic programs

  common::sudo apt-add-repository ppa:fish-shell/release-2
  common::sudo apt update
  common::sudo apt upgrade
  common::sudo apt install \
    make gcc libreadline-dev build-essential cmake \
    tmux fish vim git ipython python-pip \
    silversearcher-ag
}

if common::program-exists 'insync-headless'; then
  wizard_do_insync_start() {
    insync-headless start
  }
  wizard_do_insync_pause-syncing() {
    insync-headless pause_syncing
  }
  wizard_do_insync_resume-syncing() {
    insync-headless resume_syncing
  }
  wizard_do_insync_retry-errors() {
    insync-headless retry_errors
  }
  wizard_do_insync_status() {
    insync-headless get_status
  }
  wizard_do_insync_errors() {
    insync-headless get_errors
  }
  wizard_do_insync_sync-progress() {
    insync-headless get_sync_progress
  }
fi

wizard_do_frequencies() {

  common::required_help "$1" "$__name [amount]"

  sort | uniq -c | sort -nr | head -n "$1";
  return 1
}

wizard_do_ratio() {

  common::required_help "$1" "[amount]

  count the occurances of each input line, produce ratio data
  "

  sort \
    | uniq -c \
    | sort -nr \
    | head -n "$1" \
    | awk '{a[$2]=$1;s+=$1}END{for(i in a)printf"%-40s%-15d%6.2f%%\n",i,a[i],a[i]/s*100}' \
    | sort -r -k 2,2 -n

  return 1
}

wizard_do_parse_json() {

  python -m json.tool
}

wizard_do_parse_xml() {

  common::required_help "$1" "< file.xml

  pipe in a file an pretty print XML
  "

  xmllint --format -
}

wizard_add_user () {

  common::required_help "$1" "[user name]

  add a new sudo user to the system
  "
  user="$1"

  common::do adduser "$user"
  common::do usermod -aG sudo "$user"
  common::do chsh -s /usr/bin/fish "$user"
  common::do mkdir -p /home/"$user"/.ssh/
  common::do cp -r /root/.ssh/ /home/"$user"/

  common::do chown -R "$user:$user" /home/"$user"/
  return 1
}

wizard_clean_boot() {

  common::optional_help "$1" "

  safely cleans up old Linux kernel versions from /boot
  "

  dpkg --list \
    | grep linux-image \
    | awk '{ print $2 }' \
    | sort -V \
    | sed -n '/'"$(uname -r)"'/q;p' \
    | xargs sudo apt-get -y purge

  return $#
}

wizard_clean_apt() {

  common::optional_help "$1" "

  force purge removed apt packages
  "
  dpkg --list \
    | grep "^rc" \
    | cut -d " " -f 3 \
    | xargs sudo dpkg --purge \
    || common::color_error "Looks like there's nothing to clean!"
}

wizard_clean_files() {

  local fixed dry=0 counter=0 usage="
  $__name [-d|--dry]

  smart remove duplicate file names and intermediary file types
  "
  case "$1" in
    -d|--dry) dry=1 ;;
    *)        common::error "$usage" ;;
  esac

  while read -r file; do
    fixed="$(sed -e 's/[ ]*([0-9]\+)//' <<< "$file")"

    # make sure the file still exists
    if [[ -e "$file" ]] ; then

      if [[ -f "$fixed" ]]; then
        echo "remove dup: $file"
        (( dry )) \
          || rm "$file" \
          || exit

      else
        echo "rename dup: $file"
        (( dry )) \
          || mv "$file" "$fixed" \
          || exit
      fi

      let counter++
    fi
  done < <(find . -regex '.*([0-9]+).*')

  while read -r file; do
    echo "remove: $file"

    (( dry )) \
      || rm "$file" \
      || exit
    let counter++

  done < <(find . -regex '.*\.\(pyc\|class\|o\|bak\)')

  if (( dry )); then
    echo "Would have cleaned up $counter files"
  else
    echo "Cleaned up $counter files"
  fi
  return 1
}

if common::program-exists 'tmux'; then
  wizard_make_session() {
    common::optional_help "$1" "[name]

  create a new tmux session and move to it
    "

    name="${*:-$RANDOM}"
    tmux new -d -s "$name"
    tmux switch-client -t "$name"
    return $#
  }

  wizard_do_layout_vertical() {
    tmux select-layout even-vertical
  }
  wizard_do_layout_horizontal() {
    tmux select-layout even-horizontal
  }
  wizard_do_layout_tiled() {
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

wizard_update_platform() {
  # update everything, whatever that means

  case $PLATFORM in
    Linux)
      wizard update apt
      wizard update pip
      ;;
  esac
}

wizard_update_apt() {

  common::optional_help "$1" "

  update all apt packages
  "
  common::sudo apt update
  common::sudo apt upgrade -y
  common::sudo apt-get autoremove
}

wizard_update_pip() {

  common::optional_help "$1" "

  update all python packages installed by pip
  "
  sudo -H pip freeze --local \
    | grep -v '^\-e' \
    | cut -d = -f 1  \
    | xargs -n1 sudo -H pip install -U
}

wizard_build_vim () {
  # compile and install the lastest vim

  echo "installing vim"
  wizard install lua

	common::sudo apt-get build-dep vim-gnome
	common::sudo apt-get install \
    libncurses5-dev libgnome2-dev libgnomeui-dev \
		libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
		libcairo2-dev libx11-dev libxpm-dev libxt-dev

  common::do cd /tmp/
  common::do wget -N 'https://github.com/vim/vim/archive/master.zip'
  common::do unzip master.zip
  common::do cd vim-master

	common::do ./configure \
    --with-features=huge \
    --with-lua-prefix=/usr/local \
    --with-lua-jit=yes \
    --enable-multibyte \
    --enable-rubyinterp=yes \
    --enable-pythoninterp=yes \
    --with-python-config-dir=/usr/lib/python2.7/config \
    --enable-python3interp=yes \
    --with-python3-config-dir=/usr/lib/python3.5/config \
    --enable-perlinterp=yes \
    --enable-luainterp=yes \
    --enable-gui=auto \
    --enable-cscope \
    --prefix=/usr

	common::do make -j 4
	common::sudo make install
  echo "done"
}

wizard_open() {

  common::required_help "$1" "

  open a file based on it's type and available programs
  "
  local filetype

  for target in "$@"; do

    # shellcheck disable=SC2076
    filetype="$(file -b "$target")"

    if common::contains "$filetype" 'ASCII text'; then
      vim "$target"

    elif common::program-exists 'xdg-open'; then

      if common::program-exists 'xiwit'; then
        xiwit xdg-open "$target"

      else
        xdg-open "$target"
      fi
    fi

  done
  return $#
}

wizard_bookmark() {

  common::required_help "$1" "[+]

  bookmark current directory
  "
  local conf="$HOME/.bookmarks"

  case $1 in
    +) pwd >> "$conf" ;;
    -) grep -wv "^$(pwd)$" "$conf" > "$conf.swap" && mv "$conf.swap" "$conf";;

    -h|--help) echo "$usage" ;;

    *)
      if [[ $1 ]]; then
        files=( $(grep -i -- "$1" "$conf") )
      else
        files=( $(cat "$conf") )
      fi

      case ${#files[@]} in
        0) exit 0   ;;
        1) choice=0 ;;
        *)
          let i=0
          sort <<< "${files[@]}" | tr ' ' '\n' | while read -r file; do
            printf "(%d) %s\n" "$i" "$file"
            let i++
          done

          read -p '? ' -r choice
          (( choice < 0 || choice > ${#files[@]} )) && exit 0
          ;;
      esac

      line="$(grep -nw -- "^${files[$choice]}$" "$conf" | cut -d ':' -f 1)"
      #shellcheck disable=SC2086
      exit $line
      ;;
  esac
}

wizard_start_sshd() {

  common::optional_help "$1" "

  start sshd on Chrome OS
  "

  common::sudo mkdir -p -m0755 /var/run/sshd
  common::sudo /usr/sbin/sshd
}

wizard_quick_shell() {

  common::optional "$1" "

  open a throw away shell file
  "
  common::do cd /tmp/
  wizard_make_file_shell quick
  vim quick.sh
}

wizard_quick_python() {

  common::optional "$1" "

  open a throw away python file
  "
  common::do cd /tmp/
  wizard_make_file_python quick
  vim quick.py
  common::do cd -
}

wizard_quick_c()
{
  common::optional_help "$1" "

  open a throw away c file
  "
  common::do cd /tmp/
  wizard_make_file_c quick
  vim quick.c
  common::do cd -
}


# this is where we can inject code into the generated functions
#
# shellcheck disable=SC2034,SC2154,SC2016
{
meta_head[wizard_make_file]='
common::required_help "$1" "[language] [file name]
$__usage
"
'
meta_head[wizard_make_project]='
common::required_help "$1" "[language] [project name]
$__usage
"
'
meta_head[wizard]='
common::required_help "$1" "(-q | -s)
$__usage
"
'
meta_body[wizard]='
-q|--quiet)  QUIET=1  ;;
-s|--silent) SILENT=1 ;;
'
}
