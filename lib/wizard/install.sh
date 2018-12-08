#!/bin/bash

# install - wizard libary
#
#   install packages, programs from source and more


wizard::apt() {

  common::required-help "$1" "[package...]

  install a package with apt
  "
  common::sudo apt install -y "$@"
}



wizard_install_dot-files() {

  common::required-help "$1" "[link | copy]

  link all the configuration files in this repository to their correct
  locations in the system

  if the system doesn't support links (SMB mount), you can use 'copy'
  "
  common::require -f unzip

  local root; root="$(dirname "${BASH_SOURCE[0]}")"/..

  common::do mkdir -p "$HOME"/.vim
  common::do mkdir -p "$HOME"/.config/fish
  common::cd "$HOME"

  link() {
    # create symbolic links between here and there

    local here; here="$(readlink -e "${root}/$1")"
    local there="${HOME}/$2"

    [[ -e "$here" ]] ||
      common::error "Couldn't find $here"

    common::do ln -sf "$here" "$there"
  }

  copy() {
    # copy files from here to there

    local here; here="$(readlink -e "${root}/$1")"
    local there="${HOME}/$2"

    common::file-exists "$here" ||
      common::error "Couldn't find $here"

    common::do cp -r "$here" "$there"
  }

  unpack-irssi() {
    # unpack irssi scripts, since they're zipped

    common::cd "$root/etc/irssi"

    [[ -d scripts ]] ||
      common::do unzip scripts.zip

    common::cd -
  }

  case $1 in
    link|copy)
      op="$1"
      ;;
    *)
      common::error 'options are link or copy'
      ;;
  esac

  $op etc/config.fish         .config/fish/config.fish
  $op etc/vimrc               .vimrc
  $op etc/tmux.conf           .tmux.conf
  $op etc/bashrc              .bashrc
  $op etc/pylintrc            .pylintrc

  $op etc/vim/snippets        .vim/
  $op etc/irssi               .irssi
  $op lib/fish/functions      .config/fish/

  # remove directory, not symbolic link
  local completions="$HOME/.config/fish/completions";
  [[ -L "$completions" ]] || common::do rm -rf "$completions"

  $op lib/fish/completions    .config/fish/
  unpack-irssi
}


wizard_install_irssi() {

  common::optional-help "$1" "

  install irssi's dependencies with apt, then clone and compile from source
  "
  wizard::apt libtool libglib2.0-dev libssl-dev

  common::do cd /tmp
  [[ -d irssi ]] ||
    common::do git clone --depth 1 https://github.com/irssi/irssi.git

  common::do cd irssi
  common::do ./autogen.sh
  common::do make -j
  common::sudo make install
}


wizard_install_git() {

  common::optional-help "$1" "

  install the newest git version
  "
  common::sudo add-apt-repository ppa:git-core/ppa -y
  common::sudo apt-get update
  wizard::apt git
}


wizard_install_vnc() {

  common::optional-help "$1" "

  install xfce4 and start a VNC server. for droplets
  "
  common::sudo apt update
  wizard::apt xfce4 xfce4-goodies tightvncserver
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

  common::optional-help "$1" "

  install the Oracle JDK
  "
  common::sudo add-apt-repository -y ppa:webupd8team/java
  common::sudo apt update
  wizard::apt oracle-java8-installer
}


wizard_install_shellcheck() {

  common::optional-help "$1" "

  download and install the latest shellcheck
  "
  common::require -f stack
  common::do stack install shellcheck
}


wizard_install_lua() {

  common::optional-help "$1" "

  install dependencies with apt, compile and install lua 5.3.3
  "

  common::sudo apt install gcc build-essential libreadline-dev
  local builddir='lua-5.3.3'

  common::do cd /tmp/
  [[ -d $builddir ]] || {
    common::do wget -N 'https://www.lua.org/ftp/lua-5.3.3.tar.gz'
    common::do tar zxvf lua-5.3.3.tar.gz
  }

  common::do cd lua-5.3.3
  common::do make -j linux
  common::sudo make install
}


wizard_install_tmux() {

  common::optional-help "$1" "

  download, build and install tmux 2.7
  "
  wizard::apt \
    libevent-dev \
    build-essential \
    libncurses5-dev

  common::cd /tmp/
  local builddir='tmux-2.7'

  [[ -d $builddir ]] || {
    common::do wget \
      'https://github.com/tmux/tmux/releases/download/2.7/tmux-2.7.tar.gz'
    common::do tar xf tmux-2.7.tar.gz
  }

  common::cd $builddir
  common::do ./configure
  common::do make -j 4

  common::sudo make install
}


wizard_install_fish() {

  common::optional-help "$1" "

  install fish from the official repository so we get the most recent version
  "
  wizard::apt software-properties-common python-software-properties
  common::sudo apt-add-repository -y ppa:fish-shell/release-2
  common::sudo apt-get update
  wizard::apt fish
}


wizard_install_vim() {

  common::optional-help "$1" "

  compile and install with all feaures enabled

  run these first
    - w install lua
  "
  wizard::apt \
    git \
    build-essential \
    silversearcher-ag \
    python3-pip

  common::do cd /tmp/

  [[ -d vim ]] ||
    common::do git clone --depth 1 https://github.com/vim/vim.git

  common::do cd vim

  common::do ./configure \
    --with-features=huge \
    --with-lua-prefix=/usr/local \
    --enable-multibyte \
    --enable-rubyinterp=yes \
    --enable-pythoninterp=yes \
    --enable-python3interp=yes \
    --enable-perlinterp=yes \
    --enable-luainterp=yes \
    --enable-gui=auto \
    --enable-cscope \
    --prefix=/usr/local

  common::do make -j CFLAGS='"-oFast -march=native"'
  common::sudo make install
}


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
  wizard::apt -y docker-ce
}
