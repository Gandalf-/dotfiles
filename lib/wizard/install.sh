#!/usr/bin/env bash

# install - wizard libary
#
#   install packages, programs from source and more


wizard::apt() {

  common::required-help "$1" "[package...]

  install a package with apt
  "
  common::sudo apt install -y "$@"
}


wizard::file-equal() {

  local first second
  first="$( sha1sum "$1" | awk '{print $1}' )"
  second="$( sha1sum "$2" | awk '{print $1}' )"
  [[ "$first" == "$second" ]]
}


wizard_install_dot-files() {

  common::required-help "$1" "[link | copy]

  link all the configuration files in this repository to their correct
  locations in the system

  if the system doesn't support links (SMB mount), you can use 'copy'
  "
  common::do mkdir -p "$HOME"/.vim
  common::do mkdir -p "$HOME"/.config/fish

  link() {
    # create symbolic links between here and there
    local here; here="$( readlink -f "$1" )"
    local there="${HOME}/$2"

    [[ "$here" ]] ||
      common::error "Couldn't find '$1'"

    common::do ln -sf "$here" "$there"
  }

  copy() {
    # copy files from here to there

    local here; here="$( readlink -f "$1" )"
    local there="${HOME}/$2"

    [[ "$here" ]] ||
      common::error "Couldn't find '$1'"

    common::do cp -r "$here" "$there"
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

  $op lib/fish/functions      .config/fish/
  $op lib/fish/completions    .config/fish/
  $op lib/fish/conf.d         .config/fish/
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
#!/usr/bin/env bash
xrdb \$HOME/.Xresources
startxfce4 &
EOF
  common::do chmod +x "$HOME"/.vnc/xstartup
}


wizard_install_rust() {

  common::program-exists cargo &&
    common::error "Rust is already installed"

  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
}


wizard_install_java() {

  common::optional-help "$1" "

  install the Oracle JDK
  "
  common::sudo add-apt-repository -y ppa:webupd8team/java
  common::sudo apt update
  wizard::apt oracle-java8-installer
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

  local builddir='tmux-2.7'

  [[ -d $builddir ]] || {
    common::do wget \
      'https://github.com/tmux/tmux/releases/download/2.7/tmux-2.7.tar.gz'
    common::do tar xf tmux-2.7.tar.gz
  }

  common::cd $builddir
  common::do ./configure
  common::do make -j
  common::echo "Run sudo make install"
}


wizard_install_fish() {

  common::optional-help "$1" "

  download and compile fish
  "
  local version=3.3.1
  case $( fish --version ) in
    *3.3.1)
      common::error "fish $version already installed"
      ;;
    *)
      ;;
  esac

  common::do wget \
    https://github.com/fish-shell/fish-shell/releases/download/$version/fish-$version.tar.xz
  tar xf fish-$version.tar.xz
  common::cd fish-$version

  common::do cmake .
  common::do make -j
  common::echo "Run sudo make install"
}


wizard_install_fish-dependencies() {

  common::program-exists fd ||
    common::do cargo install fd-find

  common::program-exists bat ||
    common::do cargo install --locked bat

  common::program-exists fisher ||
    fish -c 'curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher'

  common::do fisher install PatrickF1/fzf.fish
}


wizard_install_vim() {

  common::optional-help "$1" "

  compile and install with all feaures enabled
  "
  wizard::apt \
    git \
    build-essential \
    silversearcher-ag \
    python3-pip

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
}
