#!/bin/sh

# usage
#   sh prepare.sh (username)

do_check () {

  if [ "$(whoami)" != 'root' ] || [ "$(uname)" != 'Linux' ]; then
    echo "Must be root on Linux"
    exit
  fi

  if [ -z "$1" ]; then
    echo "Must provide user name"
    exit
  fi
}

do_update () {
  apt-add-repository ppa:fish-shell/release-2
  apt update
  apt upgrade
  apt install tmux fish vim git make gcc libreadline-dev
}

do_security () {

  ufw allow 22/tcp
  ufw enable
}

do_vnc () {
  apt install xfce4 xfce4-goodies tightvncserver
  vncserver
  vncserver -kill :1
  echo "
#!/bin/bash
xrdb \$HOME/.Xresources
startxfce4 &
" > "$HOME"/.vnc/xstartup
  chmod +x "$HOME"/.vnc/xstartup
}

add_user () {
  user="$1"

  adduser "$user"
  usermod -aG sudo "$user"
  chsh -s /usr/bin/fish "$user"
  mkdir -p /home/"$user"/.ssh/
  cp -r /root/.ssh/ /home/"$user"/

  git clone https://github.com/Gandalf-/DotFiles.git /tmp/DotFiles
  ln -sf /tmp/DotFiles/config.fish  /home/"$user"/.config/fish/config.fish
  ln -sf /tmp/DotFiles/vimrc        /home/"$user"/.vimrc
  ln -sf /tmp/DotFiles/tmux.conf    /home/"$user"/.tmux.conf
  ln -sf /tmp/DotFiles/bashrc       /home/"$user"/.bashrc

  chown -R "$user":"$user" /home/"$user"/
}

install_vim () {
  cd /tmp/ || exit
  wget https://www.lua.org/ftp/lua-5.3.3.tar.gz \
        && tar zxvf lua-5.3.3.tar.gz && cd lua-5.3.3 \
        && make linux && make install

  git clone https://github.com/vim/vim.git /tmp/vim
  cd /tmp/vim || exit
  export LUA_PREFIX=/usr/local
  ./configure \
      --with-features=huge \
      --enable-multibyte \
      --enable-rubyinterp=yes \
      --enable-pythoninterp=yes \
      --with-python-config-dir=/usr/lib/python2.7/config \
      --enable-python3interp=yes \
      --with-python3-config-dir=/usr/lib/python3.5/config \
      --enable-perlinterp=yes \
      --enable-luainterp=yes \
      --enable-gui=gtk2 --enable-cscope --prefix=/usr
  make install
}

do_check "$@"
do_update
add_user "$@"
install_vim
do_security
do_vnc
