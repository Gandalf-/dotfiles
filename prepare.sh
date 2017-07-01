#!/bin/sh

user=leaf

[ "$(whoami)" = 'root' ] || { echo "Must be root"; exit; }

apt update
apt upgrade
apt install tmux fish vim git

adduser $user
usermod -aG sudo $user
chsh -s /usr/bin/fish $user
mkdir -p /home/leaf/.ssh/
cp -r /root/.ssh/ /home/$user/

git clone https://github.com/Gandalf-/DotFiles.git /tmp/DotFiles
ln -sf /tmp/DotFiles/config.fish  /home/$user/.config/fish/config.fish
ln -sf /tmp/DotFiles/vimrc        /home/$user/.vimrc
ln -sf /tmp/DotFiles/tmux.conf    /home/$user/.tmux.conf
ln -sf /tmp/DotFiles/bashrc       /home/$user/.bashrc

chown -R $user:$user /home/$user/
