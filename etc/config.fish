# vim: set syntax=bash

# fish global settings
#===========================
set -gx __HOST__ (hostname       | sed 's/localhost/home/')
set -gx __HOST__ (echo $__HOST__ | sed 's/wkstn-avoecks/work/')

# where are we?
test (hostname) = 'wkstn-avoecks'; and set at_work yes
set fish_version (fish --version | grep -o '[0-9]\+' | tr -d '\n')

function fish_prompt

  if test $status -ne 0
    set_color bryellow
  else
    set_color normal
  end

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
  end
end

# 'aliases'
function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      command ls --color=auto ; end
function   ...; builtin cd ../../;   command ls --color=auto ; end
function  ....; builtin cd ../../../;command ls --color=auto ; end

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

# rust
if test -d ~/.cargo/bin/
  set PATH ~/.cargo/bin/ $PATH
end

# go
if test -d /usr/local/go/bin/
  set PATH /usr/local/go/bin/ $PATH
  set -gx GOPATH ~/google_drive/code/go
end

# fzf
if test -d ~/.vim/bundle/fzf/bin
  set PATH ~/.vim/bundle/fzf/bin $PATH
  source ~/.vim/bundle/fzf/shell/key-bindings.fish

  set -gx FZF_DEFAULT_COMMAND 'ag -g ""'
  set -gx FZF_DEFAULT_OPTS '--height 40% --border --preview="file {}" --preview-window=up:30%:wrap'
end

function c
  fzf | read -l result
  # pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | eval (fzf) +m --select-1 --exit-0 $FZF_BCD_OPTS | read -l result
  [ "$result" ]; and cd (dirname $result)
  commandline -f repaint
  ls --color=auto
end

function k
  fzf | read -l result
  [ "$result" ]; and vim "$result"
  commandline -f repaint
end

# Fish Functions
#===========================

if test "$scripts"
  set PATH $scripts/bin $PATH
end

# vimwiki
if test "$wiki_loc"
  function vws; v "$wiki_loc" +"VimwikiSearch $argv"; end
  alias vw="v $wiki_loc"
end

function f
  # f [azlct] [target]
  #
  # f       -> cd ~
  # f file  -> vim file
  # f dir   -> cd dir
  # f name  -> autojump name

  if test -z "$argv"; cd; return; end
  
  set files
  set fuzzy
  set grepper
  set has_flag
  set locate
  set move
  set notimeout
  set vim_opt

  # check for flags if we have enough arguments
  if test (count $argv) -ge 2
    if not string match -q -r '[^azlct-]' $argv[1]
      if string match -q -r '.*z.*' $argv[1]; set fuzzy     "yes"; end
      if string match -q -r '.*l.*' $argv[1]; set locate    "yes"; end
      if string match -q -r '.*c.*' $argv[1]; set move      "yes"; end
      if string match -q -r '.*t.*' $argv[1]; set notimeout "yes"; end
      if string match -q -r '.*t.*' $argv[1]; set notimeout "yes"; end
      if string match -q -r '.*a.*' $argv[1]; set grepper   "yes"; end
      if string match -q    '-'     $argv[1]; cd -; return; end

      set --erase argv[1]
      set has_flag "yes"
    end
  end

  for arg in $argv
    if test (string sub -l 1 "$arg") = '+'                  # vim option
      set vim_opt "$arg"

    else if test -f "$arg"                                  # file
      set files $arg $files

    else if test -d "$arg"                                  # directory
      cd "$arg"; ls --color=auto

    else if not test "$has_flag"; and j "$arg" ^/dev/null   # autojump
      true

    else                                                    # search
      set search

      if test "$grepper"                                      # contents
        set cmd "ag"
        set flags "-l"

        test "$fuzzy";     and set flags "$flags" -i
        test "$notimeout"; and set cmd   "timeout 1 $cmd"

        set search (eval $cmd $flags $arg)
        test "$search"; and not test "$fuzzy"; and set vim_opt "+/$arg"

      else                                                    # name
        set cmd "find . "
        set flags "-name $arg -not -path './.*'"

        test "$fuzzy";     and set flags "-iname '*'$arg'*' -not -path './.*'"
        test "$notimeout"; and set cmd "timeout 1 $cmd"

        set search (eval $cmd $flags)
      end

      if test "$search"
        for new_file in $search
          set files "$new_file" $files
        end
      end

    end
  end
  
  # open, print accrued files, if any
  if test "$files"
    if test "$locate"
      for file in $files; echo $file; end

    else if test "$move"
      cd (dirname $files[1])
      ls --color=auto

    else
      v $files $vim_opt
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
    cd "$new_path"; ls --color=auto
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

# Tab completion
#===========================
complete -e -c wizard
complete -x -c wizard -a '(wizard | tail -n +4 | head -n -1 | sed -e "s/[\ \.]//g")'

