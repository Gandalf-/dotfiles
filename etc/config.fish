# vim: set syntax=bash

# fish global settings
#===========================

set -gx __HOST__ (hostname       | sed 's/localhost/home/')
set -gx __HOST__ (echo $__HOST__ | sed 's/remotedev-avoecks.*/work/')

set -gx EDITOR vim
set -gx XDG_CONFIG_HOME "$HOME"/.config/
set -gx BROWSER google-chrome


# where are we?
test $__HOST__ = 'work'; and set at_work yes
test (whoami) = 'chronos'; and set at_cros yes
set fish_version (fish --version | grep -o '[0-9]\+' | tr -d '\n')


# 'aliases'
function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      l ; end
function   ...; builtin cd ../../;   l ; end
function  ....; builtin cd ../../../;l ; end


# location specific settings
#===========================

if test -d ~/DotFiles
  set scripts ~/DotFiles

else if test -d ~/dotfiles
  set scripts ~/dotfiles
end

if test -d ~/dotfiles
  set -gx DIMENSION ubuntu
  set wiki_loc ~/wiki/index.md

# temporary
else
  set -gx DIMENSION unknown
end


# Languages
#===========================

# rust
test -d ~/.cargo/bin/
  and set PATH ~/.cargo/bin/ $PATH

test -d ~/.cabal/bin/
  and set PATH ~/.cabal/bin $PATH

# go
if test -d /usr/local/go/bin; and test -d $HOME/working/go/bin
  set PATH $HOME/working/go/bin /usr/local/go/bin $PATH
  set -x -U GOPATH $HOME/working/go
end

# haskell & misc
test -d ~/.local/bin/
  and set PATH ~/.local/bin $PATH

# ubuntu snaps
test -d /snap/bin
  and set PATH /snap/bin/ $PATH

# npm
test -d ~/.local/bin/node/
  and set PATH ~/.local/bin/node/ $PATH


# Other executables
#===========================

set -gx TMP /tmp


# fzf
if not test (command -v fzf)

  if test -e ~/.vim/bundle/fzf.vim/bin/fzf
    set PATH ~/.vim/bundle/fzf.vim/bin $PATH

  else if test -e ~/.vim/bundle/fzf/bin/fzf
    set PATH ~/.vim/bundle/fzf/bin $PATH
  end
end

set -gx FZF_DEFAULT_COMMAND 'ag -g ""'
set -gx FZF_DEFAULT_OPTS '--height 50% --border --cycle'
fzf_configure_bindings --directory=\ei --history=\cr --git_status=\eg

if test -e ~/.pythonrc
  set -gx PYTHONSTARTUP ~/.pythonrc
end


# scripts
test "$scripts"; and set PATH $scripts/bin $PATH


# vimwiki
if test "$wiki_loc"
  function vws; v "$wiki_loc" +"VimwikiSearch $argv"; end
  function vw ; cd (dirname "$wiki_loc"); vim "$wiki_loc"; end
end


# Fish git prompt and colors
#===========================

set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

if test "$at_work"
  set -gx DISPLAY ':0'

# else
#   set __fish_git_prompt_showdirtystate 'yes'
#   set __fish_git_prompt_showstashstate 'yes'
#   set __fish_git_prompt_showuntrackedfiles 'no'
#   set __fish_git_prompt_color_upstream_ahead green
#   set __fish_git_prompt_color_upstream_behind red
#   set __fish_git_prompt_color_upstream_ahead green
#   set __fish_git_prompt_color_upstream_behind red
end

# Status Chars
set __fish_git_prompt_char_dirtystate 'd'
set __fish_git_prompt_char_stagedstate 'p'
set __fish_git_prompt_char_untrackedfiles 'n'
set __fish_git_prompt_char_stashstate 's'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'

# colorful man pages
set -x LESS_TERMCAP_mb (printf "\033[01;31m")
set -x LESS_TERMCAP_md (printf "\033[01;31m")
set -x LESS_TERMCAP_me (printf "\033[0m")
set -x LESS_TERMCAP_se (printf "\033[0m")
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\033[0m")
set -x LESS_TERMCAP_us (printf "\033[01;32m")
