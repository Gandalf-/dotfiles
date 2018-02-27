#!/bin/bash

# install - wizard libary
#
#   install packages, programs from source and more


wizard_install_dot-files() {

  common::optional-help "$1" "

  link all the configuration files in this repository to their correct
  locations in the system
  "

  local root; root="$(dirname "${BASH_SOURCE[0]}")"/..

  common::do mkdir -p "$HOME"/.vim
  common::do mkdir -p "$HOME"/.config/fish

  link() {
    local here; here="$(readlink -e "${root}/$1")"
    local there="${HOME}/$2"

    common::do ln -sf "$here" "$there"
  }

  link etc/config.fish         .config/fish/config.fish
  link etc/vimrc               .vimrc
  link etc/tmux.conf           .tmux.conf
  link etc/bashrc              .bashrc
  link etc/gitignore_global    .gitignore_global
  link etc/pylintrc            .pylintrc
  link etc/devbotrc            .devbotrc

  link etc/vim/snippets        .vim/
  link etc/irssi               .irssi
  link lib/fish/functions      .config/fish/

  # remove directory, not symbolic link
  local completions="$HOME/.config/fish/completions";
  [[ -L "$completions" ]] || common::do rm -rf "$completions"

  link lib/fish/completions    .config/fish/
}

common::require 'apt' &&
wizard_install_apt() {

  common::required-help "$1" "[package...]

  install a package with apt
  "

  common::sudo apt install -y "$@"
}


common::require 'apt' &&
wizard_install_irssi() {

  common::optional-help "$1" "

  install irssi's dependencies with apt, then clone and compile from source
  "

  wizard_install_apt libtool libglib2.0-dev libssl-dev

  common::do cd /tmp
  wizard make git-tmpfs-clone https://github.com/irssi/irssi.git
  common::do cd irssi
  common::do ./autogen.sh
  common::do make -j
}


wizard_install_autojump() {

  common::optional-help "$1" "

  install autojump from github
  "

  common::clone git://github.com/joelthelion/autojump.git /tmp/autojump

  common::do cd /tmp/autojump
  common::do ./install.py
  common::do cd -
}


common::require 'apt' &&
wizard_install_git() {

  common::optional-help "$1" "

  install the newest git version
  "
  common::sudo add-apt-repository ppa:git-core/ppa -y
  common::sudo apt-get update
  wizard_install_apt git
}


common::require 'apt' &&
wizard_install_vnc() {

  common::optional-help "$1" "

  install xfce4 and start a VNC server. for droplets
  "
  common::sudo apt update
  wizard_install_apt xfce4 xfce4-goodies tightvncserver
  common::sudo vncserver
  common::sudo vncserver -kill :1
  cat > "$HOME"/.vnc/xstartup << EOF
#!/bin/bash
xrdb \$HOME/.Xresources
startxfce4 &
EOF
  common::do chmod +x "$HOME"/.vnc/xstartup
}


common::require 'apt' &&
wizard_install_java() {

  common::optional-help "$1" "

  install the Oracle JDK
  "
  common::sudo add-apt-repository -y ppa:webupd8team/java
  common::sudo apt update
  wizard_install_apt oracle-java8-installer
}


common::require 'dpkg' &&
wizard_install_shellcheck() {

  common::optional-help "$1" "

  download and install the latest shellcheck
  "
  common::do cd /tmp/
  common::do wget -N \
    'http://ftp.us.debian.org/debian/pool/main/s/shellcheck/shellcheck_0.4.6-1_i386.deb'
  common::sudo dpkg -i shellcheck_*.deb || true
  common::sudo apt-get install -f
  common::do cd -
}


common::require 'apt' &&
wizard_install_lua() {

  common::optional-help "$1" "

  install dependencies with apt, compile and install lua 5.3.3
  "
  common::sudo apt install gcc build-essential libreadline-dev

  echo "installing lua"
  common::do cd /tmp/

  common::do wget -N 'https://www.lua.org/ftp/lua-5.3.3.tar.gz'
  common::do tar zxvf lua-5.3.3.tar.gz
  common::do cd lua-5.3.3
  common::do make -j linux
  common::sudo make install

  common::do cd -
  echo "done"
}


common::require 'apt' &&
wizard_install_fish() {

  common::optional-help "$1" "

  install fish from the official repository so we get the most recent version
  "

  wizard_install_apt software-properties-common python-software-properties
  common::sudo apt-add-repository -y ppa:fish-shell/release-2
  common::sudo apt-get update
  wizard_install_apt fish
}


common::require 'apt' &&
wizard_install_docker() {

  common::optional-help "$1" "

  install the dependencies and kernel headers for docker-ce
  "

  common::do curl -fsSL 'https://download.docker.com/linux/ubuntu/gpg' \
    | sudo apt-key add -

  common::sudo add-apt-repository -y \
    "\"deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable\""

  common::sudo apt-get update
  common::do apt-cache policy docker-ce
  wizard_install_apt -y docker-ce
}

