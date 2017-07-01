# vim: set syntax=bash

# fish global settings
#===========================
set -gx __HOST__ (hostname       | sed 's/localhost/home/')
set -gx __HOST__ (echo $__HOST__ | sed 's/wkstn-avoecks/work/')

# where are we?
test (hostname) = 'wkstn-avoecks'; and set at_work yes

function fish_prompt
  # add the current directory to the path
  set PATH $OLD_PATH
  set PATH $PWD $PATH

  echo -n (whoami)"@$__HOST__"
  set_color $fish_color_cwd
  echo -n ' '(prompt_pwd)
  set_color normal
  printf '%s' (__fish_git_prompt)
  set_color normal
  echo '> '
end

function fish_greeting
  ls
end

# PATH
set -gx TMP /tmp

if test -d ~/.cabal/bin
  set PATH /home/leaf/.cabal/bin $PATH
end

set -x OLD_PATH $PATH

# Abbreviations
if status --is-interactive
    set -g fish_user_abbreviations
    abbr --add bash b
    abbr --add cd   c
    abbr --add echo e
    abbr --add head h
    abbr --add ls   l
    abbr --add vim  v
end

# Vim mode
#===========================
function fish_mode_prompt --description 'Displays the current mode'

	if test "$fish_key_bindings" = "fish_vi_key_bindings"
		switch $fish_bind_mode
      case default; set_color --bold red;   echo N
      case insert;  set_color --bold green; echo I
      case visual;  set_color --bold blue;  echo V
		end

		set_color normal; printf " "
	end
end

# Key bindings
function fish_user_key_bindings
  bind \e. 'history-token-search-backward'
  bind \co 'history-search-forward'
  bind \cb 'commandline -i "bash -c \'";commandline -a "\'"'

  fish_default_key_bindings -M insert
  fish_vi_key_bindings insert

  bind -M insert \cf forward-char
  bind -M insert \ce end-of-line
  bind -M insert \ca beginning-of-line

  bind -M default E end-of-line
  bind -M default B beginning-of-line
end

# Location
#===========================

# workstation
if test $at_work
  set wiki_loc ~/cribshome/wiki/index.md
  set scripts  ~/cribshome/DotFiles/scripts.sh

# personal
else if test -d ~/google_drive
  set wiki_loc ~/google_drive/index.md 
  set scripts  ~/google_drive/personal/share/Public/DotFiles/scripts.sh
end

# Fish Functions
#===========================

if test "$scripts"
  # external functions
  for ex_function in (grep '()' "$scripts" | grep -v '#' | cut -f 1 -d ' ')
    eval "function $ex_function ; bash $scripts $ex_function \$argv ; end"
  end

  # external aliases
  for ex_alias in (grep '^alias ' "$scripts")
    eval "$ex_alias"
  end
end

# vimwiki
if test "$wiki_loc"
  function vws; vim "$wiki_loc" +"VimwikiSearch $argv"; end
  alias vw="vim $wiki_loc"
end

function f 
  # smart cd, find, jump, open, create
  
  if test -z "$argv" 
    cd 

  else if test -f "$argv[1]"
    vim -p $argv

  else if test -d "$argv[1]"
    cd "$argv[1]"; ls

  else if j "$argv[1]" ^/dev/null
    return

  else 
    if test "$argv[1]" = 'z'
      set files (find . -name "*$argv[1]*")
    else
      set files (find . -name "$argv[1]")
    end

    set num_files (echo "$files" | wc -w)
    echo $num_files

    if not test -z "$files"
      #cd dirname ("$files")
      vim -p "$files"
    else
      vim -p "$argv"
    end

  end
end

function repeat
  # replay some number of commands from history (experiment)
  #
  for cmd in (seq "$argv[1]" -1 1)
    if test (echo "$history[$cmd]" | head -c 6) != "repeat"
      show "[repeat] $history[$cmd]"
      eval $history[$cmd]
    end
  end
end

# autojump
#===========================
if test -f ~/.autojump/share/autojump/autojump.fish
  . ~/.autojump/share/autojump/autojump.fish
end

function j
  set new_path (autojump $argv)

  if test -d "$new_path"
    echo $new_path
    cd "$new_path"
    ls
  else
    echo "autojump: directory '$argv' not found"
    false
  end
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
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
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
