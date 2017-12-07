#!/bin/env bash

# wizard
#   The main wizard library. Functions are defined here, processed by
#   auto_cli.sh and built into a library sourced by bin/w
#
#   All the intermediary functions are produced by auto_cli.sh

__name=""


wizard_regenerate() {

  auto_wizard | grep -v 'work_wizard' # don't care that work_wizard not found
  echo 'done'
}


wizard_macro() {

  common::optional_help "$1" "(amount)

  replay a portion of fish history in the current terminal

    the order of selection in fzf matters
  "

  while read -r command; do
    eval "$command"

  done < <(fish -c 'history' \
    | head -n "${1:-10}" \
    | fzf -m)
}


common::require 'ffmpeg' &&
wizard_transcode_movies() {
  #
  local preset=slow

  echo "Processing: $*"
  for file in "$@"; do

    common::do ffmpeg -hide_banner -i "$file" \
      -c:v libx264 -crf 19 -preset "$preset" -strict -2 \
      -c:a aac -b:a 192k -ac 2 "${file%.*}.mp4" \
      || common::error "failed on \"$file\". Giving up"

    common::echo "Waiting..."; sleep 5
    common::do rm "$file"
  done

  return $#
}


wizard_file_remove-trailing-whitespace() {

  common::required_help "$1" "[file ...]

  remove trailing whitespace in the target files
  "
  while [[ $1 ]]; do
    common::do sed -i 's/[ \t]*$//' "$1"
    shift
  done

  return $#
}


common::require 'service' 'ntpd' &&
wizard_sync_time() {

  common::optional_help "$1" "

  synchonrize the system clock with NTP
  "

  common::sudo service ntp stop
  common::sudo ntpd -gq
  common::sudo service ntp start
}


wizard_file_pin-to-home() {

  common::required_help "$1" "[target]

  create a symbolic link in the home directory to [target]
  "

  [[ $1 ]] && common::do ln -s "$1" ~/;
}


common::require 'xcape' &&
wizard_chromeos_swap-search-escape() {

  common::do xcape -e 'Super_L=Escape'
}


common::require 'dpkg' &&
wizard_show_largest-packages() {

  common::optional_help "$1" "

  list all packages installed, sorted by size
  "

  # shellcheck disable=SC2016
  dpkg-query -Wf '${Installed-Size}\t${Package}\n' \
    | sort -n
}


wizard_start_http-server() {

  python -m SimpleHTTPServer
  return 0
}


wizard_frequencies() {

  common::required_help "$1" "[amount]

  count the occurances of each input line
  "

  sort \
    | uniq -c \
    | sort -nr \
    | head -n "$1";
  return 1
}


wizard_ratio() {

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


common::require 'python' &&
wizard_parse_json() {

  common::optional_help "$1" "

  pipe in json and pretty print it
  "

  python -m json.tool
}


common::require "xmllint" &&
wizard_parse_xml() {

  common::required_help "$1" "< file.xml

  pipe in a file an pretty print XML
  "

  xmllint --format -
}


common::require 'dpkg' &&
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


common::require 'dpkg' &&
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


wizard_update_platform() {
  # update everything, whatever that means

  case $PLATFORM in
    Linux)
      wizard update apt
      wizard update pip
      ;;
  esac
}

common::require "apt" &&
wizard_update_apt() {

  common::optional_help "$1" "

  update all apt packages
  "
  common::sudo apt update
  common::sudo apt upgrade -y
  common::sudo apt-get autoremove
}


common::require "pip" &&
wizard_update_pip() {

  common::optional_help "$1" "

  update all python packages installed by pip
  "
  sudo -H pip freeze --local \
    | grep -v '^\-e' \
    | cut -d = -f 1  \
    | xargs -n1 sudo -H pip install -U
}


common::require "wget" "pip" "apt" &&
wizard_build_vim () {
  # compile and install the latest vim

  echo "installing vim"
  wizard_install_lua

  # common::sudo apt-get build-dep vim-gnome
  common::sudo apt-get install \
    libncurses5-dev libgnome2-dev libgnomeui-dev \
    libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
    libcairo2-dev libx11-dev libxpm-dev libxt-dev \
    silversearcher-ag python-pip unzip

  common::sudo -H pip install pylint flake8

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

  common::do \
    make -j "$(getconf _NPROCESSORS_ONLN)" CFLAGS='"-oFast -march=native"'
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


common::require "sshd" &&
wizard_start_sshd() {

  common::optional_help "$1" "

  start sshd on Chrome OS
  "

  common::sudo mkdir -p -m0755 /var/run/sshd
  common::sudo /usr/sbin/sshd
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
common::required_help "$1" "(-q | -s | -e)
$__usage
"

NUM_CPUS=$(getconf _NPROCESSORS_ONLN)
'

meta_body[wizard]='
-q|--quiet)  QUIET=1  ;;
-s|--silent) SILENT=1 ;;
-e|--echo)   ECHO=1 ;;
'
}
