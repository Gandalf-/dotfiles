#!/bin/bash

# wizard
#   The main wizard library. Functions are defined here, processed by
#   auto_cli.sh and built into a library sourced by bin/w
#
#   All the intermediary functions are produced by auto_cli.sh

__name=""

which ffmpeg >/dev/null &&
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
  return $#
}

wizard_start_http-server() {

  python -m SimpleHTTPServer
  return $#
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

if which insync-headless >/dev/null; then
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

wizard_add_configs () {

  common::do git clone https://github.com/Gandalf-/DotFiles.git /tmp/DotFiles
  common::do mkdir -p "$HOME"/.config/fish
  common::do ln -sf /tmp/DotFiles/config.fish  "$HOME"/.config/fish/config.fish
  common::do ln -sf /tmp/DotFiles/vimrc        "$HOME"/.vimrc
  common::do ln -sf /tmp/DotFiles/tmux.conf    "$HOME"/.tmux.conf
  common::do ln -sf /tmp/DotFiles/bashrc       "$HOME"/.bashrc
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
    -d|--dry)
      dry=1 ;;
    *)
      common::error "$usage" ;;
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

if which tmux >/dev/null; then
  wizard_make_session() {
    common::optional_help "$1" "[name]

  create a new tmux session and move to it
    "

    name=${1:-$RANDOM}
    tmux new -d -s "$name"
    tmux switch-client -t "$name"
    return 1
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

def main(args):
  ''' list of strings -> none
  '''
  pass

if __name__ == '__main__':
  main(sys.argv)
EOF
}

wizard_make_file_c() {
  cat > "$1".c << EOF
#include "$name.h"

int main(int argc, char *argv[]) {

  return 0;
}
EOF
}

wizard_make_file_cpp() {
  cat > "$1".cpp << EOF
#include "$name.h"

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
  wizard_make_file python "$1"
  return 1
}

wizard_make_project_c() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file c "$1"
  touch "$1".h
  mmake -l c -o "$1"
}

wizard_make_project_cpp() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file cpp "$1"
  touch "$1".h
  mmake -l cpp -o "$1"
}

wizard_make_project_java() {
  common::do mkdir "$1"
  common::do cd "$1"
  wizard_make_file java "$1"
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

  return 1
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
  common::do wget 'https://github.com/vim/vim/archive/master.zip'
  common::do unzip master.zip
  common::do cd vim-master

	common::do ./configure \
    --with-features=huge \
    --with-lua-prefix=/usr/local \
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

wizard_install_apt() {
  common::required_help "$1" "

  install distribution packages
  "
  case $PLATFORM in
    Linux)
      if which apt >/dev/null; then
        common::sudo apt install "$@"
      fi
      ;;
    *)
      common::error "Unsupported platform \"$PLATFORM\""
      ;;
  esac

  return $#
}

wizard_install_git() {

  common::optional_help "$1" "

  install the newest git version
  "
  common::sudo add-apt-repository ppa:git-core/ppa -y
  common::sudo apt-get update
  common::sudo apt-get install git -y
}

wizard_install_vnc() {

  common::optional_help "$1" "

  install xfce4 and start a VNC server. for droplets
  "
  common::sudo apt update
  common::sudo apt install xfce4 xfce4-goodies tightvncserver
  common::sudo vncserver
  common::sudo vncserver -kill :1
  cat > "$HOME"/.vnc/xstartup << EOF
#!/bin/bash
xrdb \$HOME/.Xresources
startxfce4 &
EOF
  common::do chmod +x "$HOME"/.vnc/xstartup
}

wizard_install_java() {

  common::optional_help "$1" "

  install the Oracle JDK
  "
  common::sudo add-apt-repository ppa:webupd8team/java
  common::sudo apt update
  common::sudo apt install oracle-java8-installer
}

wizard_install_shellcheck() {

  common::optional_help "$1" "

  download and install the latest shellcheck
  "
  common::do cd /tmp/
  common::do wget \
    'http://ftp.us.debian.org/debian/pool/main/s/shellcheck/shellcheck_0.4.6-1_i386.deb'
  common::sudo dpkg -i shellcheck_*.deb || true
  common::sudo apt-get install -f
  common::do cd -
}

wizard_install_lua() {

  common::optional_help "$1" "

  compile and install lua 5.3.3
  "

  # shellcheck disable=SC2076
  if [[ $(lua -v) =~ "Lua 5.3.3" ]]; then
    echo "Lua already installed"
    return
  fi

  echo "installing lua"
  common::do cd /tmp/

  common::do wget 'https://www.lua.org/ftp/lua-5.3.3.tar.gz'
  common::do tar zxvf lua-5.3.3.tar.gz
  common::do cd lua-5.3.3
  common::do make linux
  common::sudo make install

  common::do cd -
  echo "done"
}

wizard_install_docker() {

  common::optional_help "$1" "

  install the dependencies and kernel headers for docker-ce
  "
  common::do curl -fsSL 'https://download.docker.com/linux/ubuntu/gpg' \
    | sudo apt-key add -
  common::sudo add-apt-repository \
    "\"deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable\""
  common::sudo apt-get update
  common::do apt-cache policy docker-ce
  common::sudo apt-get install -y docker-ce
}

wizard_find() {
  # find by file name or contents

  local fuzzy=1 file=1 text=1 args=1 prog=1

  common::required_help "$1" "(option) [object]

    option
      --exact
      --text-only
      --file-only
  "

  while [[ $1 ]]; do
    case $1 in
      -e|--exact)     fuzzy=0 ;;
      -t|--text-only) file=0; prog=0 ;;
      -f|--file-only) text=0; prog=0 ;;
      -p|--prog-only) file=0; text=0 ;;
      *) break ;;
    esac
    shift; let args++
  done

  local ag_options="-lS $1"
  local find_options=". -iname $1"
  local which_options="$1"

  if (( fuzzy )); then
    ag_options="-lS $1"
    find_options=". -iname *$1*"
    which_options="$1"
  fi

  # shellcheck disable=SC2086
  {
    (( text )) && { ag --nocolor $ag_options; echo; }
    (( file )) && { find $find_options;       echo; }
    (( prog )) && { which $which_options;     echo; }
  }

  return $args
}

wizard_open() {

  common::required_help "$1" "

  open a file based on it's type and available programs
  "

  for target in "$@"; do

    if [[ $(file -b "$target") =~ 'ASCII text' ]]; then
      vim "$target"

    elif which xdg-open >/dev/null; then

      if which xiwit >/dev/null; then
        xiwit xdg-open "$target"

      else
        xdg-open "$target"
      fi
    fi

  done
  return $#
}

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
common::required_help "$1" "(-q | -s)
$__usage
"
'
meta_body[wizard]='
-q|--quiet)  QUIET=1  ;;
-s|--silent) SILENT=1 ;;
'
}
