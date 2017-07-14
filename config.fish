# vim: set syntax=bash

# fish global settings
#===========================
set -gx __HOST__ (hostname       | sed 's/localhost/home/')
set -gx __HOST__ (echo $__HOST__ | sed 's/wkstn-avoecks/work/')

# where are we?
test (hostname) = 'wkstn-avoecks'; and set at_work yes
set fish_version (fish --version | grep -o '[0-9]\+' | tr -d '\n')

function fish_prompt
  echo -n (whoami)"@$__HOST__"
  set_color $fish_color_cwd
  echo -n ' '(prompt_pwd)
  set_color normal
  printf '%s' (__fish_git_prompt)
  set_color normal
  echo '> '
end

function fish_greeting
  ls --color=auto
end

# PATH
set -gx TMP /tmp

# Abbreviations
if status --is-interactive

  if test $fish_version -ge 220; 
    set -g fish_user_abbreviations
    abbr --add bash b
    abbr --add ls   l
    abbr --add vim  v
  end
end

# 'aliases'
function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      command ls ;end
function   ...; builtin cd ../../;   command ls; end
function  ....; builtin cd ../../../;command ls; end

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
  set     wiki_loc ~/cribshome/wiki/index.md
  set     scripts  ~/cribshome/DotFiles
  set -gx DIFFDIR  ~/cribshome/diffs/

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

# Fish Functions
#===========================

if test "$scripts"
  set PATH $scripts/bin $PATH
end

# vimwiki
if test "$wiki_loc"
  function vws; vim "$wiki_loc" +"VimwikiSearch $argv"; end
  alias vw="vim $wiki_loc"
end

function f 
  # smart cd, find, jump, open
  # f       -> cd ~
  # f file  -> vim file
  # f dir   -> cd dir
  # f name  -> autojump name

  if test -z "$argv" 
    cd ; return
  end
  
  set files
  set fuzzy
  set locate
  set move

  for arg in $argv

    # enable fuzzy search
    if test $arg = 'z'
      set fuzzy "yes"

    # enable locate mode - just print accrued files
    else if test $arg = 'l'
      set locate "yes"

    # enable search file -> move
    else if test $arg = 'c'
      set move "yes"

    # file
    else if test -f "$arg"
      set files $arg $files

    # directory
    else if test -d "$arg"
      cd "$arg"; ls

    # attempt autojump
    else if not test "$locate"; and not test "$move"; and j "$arg" ^/dev/null
      true

    # attempt search
    else 
      set search

      if test "$fuzzy"
        set search (timeout 1 find . -name '*'"$arg"'*')
      else
        set search (timeout 1 find . -name "$arg")
      end

      if test "$search"
        for new_file in $search
          set files $new_file $files
        end
      end
    end

  end
  
  # open, print accrued files, if any
  if test "$files"

    if test "$locate"
      for file in $files
        echo $file
      end

    else if test "$move"
      cd (dirname $files[1])

    else
      vim -p $files
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

  if test -d "$new_path" -a "$new_path" != "."
    printf "%s\n\n" $new_path
    cd "$new_path"
    ls
  else
    echo "autojump: directory '$argv' not found" >&2
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
