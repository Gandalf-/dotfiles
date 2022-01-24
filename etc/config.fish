# vim: set syntax=bash

# fish global settings
#===========================

set -gx __HOST__ (hostname       | sed 's/localhost/home/')
set -gx __HOST__ (echo $__HOST__ | sed 's/remotedev-avoecks.*/work/')

set -gx EDITOR vim
set -gx XDG_CONFIG_HOME "$HOME"/.config/
set -gx TMP /tmp

test $__HOST__ = 'work'; and set at_work yes
test (whoami) = 'chronos'; and set at_cros yes
set fish_version (fish --version | grep -o '[0-9]\+' | tr -d '\n')

function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      l ; end
function   ...; builtin cd ../../;   l ; end
function  ....; builtin cd ../../../;l ; end


# Location Specific
#===========================

if test -d ~/DotFiles
  set scripts ~/DotFiles

else if test -d ~/dotfiles
  set scripts ~/dotfiles
  set wiki_loc ~/wiki/index.md
end

if test "$wiki_loc"
  function vws; v "$wiki_loc" +"VimwikiSearch $argv"; end
  function vw ; cd (dirname "$wiki_loc"); vim "$wiki_loc"; end
end


# Languages
#===========================

# rust
test -d ~/.cargo/bin/
  and fish_add_path ~/.cargo/bin/

# haskell
test -d ~/.cabal/bin/
  and fish_add_path ~/.cabal/bin

# go
if test -d /usr/local/go/bin; and test -d $HOME/working/go/bin
  fish_add_path $HOME/working/go/bin /usr/local/go/bin
  set -x -U GOPATH $HOME/working/go
end

# ubuntu snaps
test -d /snap/bin
  and fish_add_path /snap/bin/

# npm
test -d ~/.local/bin/node/
  and fish_add_path ~/.local/bin/node/

# scripts
test -d "$scripts"
  and fish_add_path $scripts/bin

# misc
test -d ~/.local/bin/
  and fish_add_path ~/.local/bin

# python
test -e ~/.pythonrc
  and set -gx PYTHONSTARTUP ~/.pythonrc


# FZF
#===========================
if not test (command -v fzf)
  if test -e ~/.vim/bundle/fzf.vim/bin/fzf
    fish_add_path ~/.vim/bundle/fzf.vim/bin

  else if test -e ~/.vim/bundle/fzf/bin/fzf
    fish_add_path ~/.vim/bundle/fzf/bin
  end
end

set -gx FZF_DEFAULT_COMMAND 'ag -g ""'
set -gx FZF_DEFAULT_OPTS '--height 50% --border --cycle'

# https://github.com/jethrokuan/fzf
set -U FZF_COMPLETE 3
if test (command -v bat)
  set -U FZF_PREVIEW_FILE_CMD 'bat'
else
  set -U FZF_PREVIEW_FILE_CMD 'cat'
end
set -U FZF_PREVIEW_DIR_CMD 'ls -h --color=always'

# https://github.com/PatrickF1/fzf.fish
fzf_configure_bindings --directory=\ei --history=\cr --git_status=\eg


# git prompt and colors
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
