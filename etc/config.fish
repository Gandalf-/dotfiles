# vim: set syntax=bash

# fish global settings
#===========================
set -gx __HOST__ (hostname       | sed 's/localhost/home/')
set -gx __HOST__ (echo $__HOST__ | sed 's/wkstn-avoecks/work/')


# where are we?
test (hostname) = 'wkstn-avoecks'; and set at_work yes
set fish_version (fish --version | grep -o '[0-9]\+' | tr -d '\n')

# Abbreviations
if status --is-interactive

  if test $fish_version -ge 220; 
    set -g fish_user_abbreviations
    abbr --add bash b
    abbr --add ls   l
  end
end

# 'aliases'
function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      command ls --color=auto ; end
function   ...; builtin cd ../../;   command ls --color=auto ; end
function  ....; builtin cd ../../../;command ls --color=auto ; end

# Location
#===========================

# workstation
if test "$at_work"
  set     wiki_loc ~/cribshome/wiki/index.md
  set     scripts  ~/cribshome/DotFiles
  set -gx DIFFDIR  ~/cribshome/diffs/
  set -gx SCRIPITY /mnt/ssd/

  set PATH ~/scripity-scripts/bin $PATH

  function fl
    if echo (pwd) | grep -q "onefs";
      cd (echo (pwd) | sed -e "s/onefs/twofs/")
    else if echo (pwd) | grep -q "twofs"
      cd (echo (pwd) | sed -e "s/twofs/onefs/")
    end
  end

# personal
else if test -d ~/google_drive
  set wiki_loc ~/google_drive/index.md 
  set scripts  ~/google_drive/personal/share/Public/DotFiles

# temporary
else if test -d /tmp/DotFiles
  set scripts /tmp/DotFiles
end

# Languages
#===========================

# rust
if test -d ~/.cargo/bin/
  set PATH ~/.cargo/bin/ $PATH
end

# go
if test -d /usr/local/go/bin/
  set PATH /usr/local/go/bin/ $PATH
  set -gx GOPATH ~/google_drive/code/go
end

# Other executables
#===========================

set -gx TMP /tmp

# fzf
if test -d ~/.vim/bundle/fzf/bin
  set PATH ~/.vim/bundle/fzf/bin $PATH
  source ~/.vim/bundle/fzf/shell/key-bindings.fish

  set -gx FZF_DEFAULT_COMMAND 'ag -g ""'
  set -gx FZF_DEFAULT_OPTS '--height 40% --border'
end

# autojump
if test -f ~/.autojump/share/autojump/autojump.fish
  . ~/.autojump/share/autojump/autojump.fish
end

# DotFiles scripts
if test "$scripts"
  set PATH $scripts/bin $PATH
end

# vimwiki
if test "$wiki_loc"
  function vws; v "$wiki_loc" +"VimwikiSearch $argv"; end
  alias vw="v $wiki_loc"
end

# Fish git prompt and colors
#===========================
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

if test "$at_work"
  set -gx DISPLAY ':0'

else
  set __fish_git_prompt_showdirtystate 'yes'
  set __fish_git_prompt_showstashstate 'yes'
  set __fish_git_prompt_showuntrackedfiles 'no'
  set __fish_git_prompt_color_upstream_ahead green
  set __fish_git_prompt_color_upstream_behind red
  set __fish_git_prompt_color_upstream_ahead green
  set __fish_git_prompt_color_upstream_behind red
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
