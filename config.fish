#vi:syntax=bash

# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'no'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

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

# Fish Global Settings
# ====================
set -gx __HOST__ (hostname | sed 's/localhost/home/')
#setxkbmap -option caps:swapescape ^/dev/null

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
    abbr --add sudo s
    abbr --add vim  v
end

# Vim mode
#===================
function fish_mode_prompt --description 'Displays the current mode'
	# Do nothing if not in vi mode
	if test "$fish_key_bindings" = "fish_vi_key_bindings"
		switch $fish_bind_mode
				case default
						set_color --bold red
						echo N
				case insert
						set_color --bold green
						echo I
				case replace-one
						set_color --bold green
						echo R
				case visual
						set_color --bold brmagenta
						echo V
		end
		set_color normal
		printf " "
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

# Fish Aliases
#===================
alias al alias

# conversions
#-------------------
al dos2unix 'recode dos/CR-LF..l1'
al unix2win 'recode l1..windows-1250'
al unix2dos 'recode l1..dos/CR-LF'

# novelty
#-------------------
al ta      'tmux attach; or tmux'
al dsh     'du -sh'
al dfh     'df -h'
al lsn     'ls -al --time-style=+%D | grep `date +%D` '
al how     'howdoi -c'
al xklip   'head -c -1 | xclip -selection c'
al silent  'cat - >/dev/null ^/dev/null'
al weather 'curl http://wttr.in/'

# shortcuts
#-------------------
al b 'bash'
al e 'echo'
al F 'find . -name'
al h 'head'
al l 'ls'
al p 'python'
al r 'ranger'
al s 'sudo'
al t 'task'
al v 'vim -w ~/.vimkeys.log'
al w 'which'

# misc
#-------------------
al p3 'python3'
al hn 'head -n'
al ss 'sudo service'
al pi 'ipython'
al vp 'vim -p'
al vd 'vimdiff'
al vs 'vim - ; fg'
al sv 'sudo vim'
al vv 'vim -p *.{h,c{,c,++,xx,pp},java,sh,py,md,html,css,js,php,pl,txt}'
al cl 'clear;ls'
al lo 'locate -A'
al !! 'sudo $history[1]'

al sai 'sudo apt install'
al kut 'cut -d " " -f'
al aup 'sudo apt update; sudo apt upgrade; sudo apt-get autoremove'
al loc 'locate --database=/home/leaf/.locatedb'

al updb      'updatedb --localpaths=/home/leaf/ --output=/home/leaf/.locatedb'
al shttp     'python -m SimpleHTTPServer'
al sandman   'kill -9 (jobs -p)'
al cleanup   'rm (find -regex ".*\.\(pyc\|class\|o\|bak\)")'
al cleanup!  'rm -f (find -regex ".*\.\(pyc\|class\|o\|bak\)")'
al startsshd 'sudo mkdir -p -m0755 /var/run/sshd; sudo /usr/sbin/sshd'

al vw      'v ~/google_drive/index.md'
al vfish   'v ~/.config/fish/config.fish'
al srcfish '. ~/.config/fish/config.fish'

# quick progs
#-------------------
al qc    'vim ~/*/code/c/quick/quick.c'
al qpy   'vim ~/*/code/python/quick.py'
al qjava 'vim ~/*/code/java/Quick/Quick.java'
al qsh   'vim ~/*/code/shell/quick.sh'

# coreutils
#-------------------
al ls  'ls --color=auto'
al rm  'rm -i'
al rmm 'rm -rf'
al cp  'cp -i'
al mv  'mv -i'

al ..   'cd ../;ls'
al ...  'cd ../../;ls'
al .... 'cd ../../../;ls'

# Fish Functions
#===================

function vws ; vim ~/google_drive/index.md +"VimwikiSearch $argv" ; end

function c     ; test -z "$argv"; and cd; or cd "$argv"; ls       ; end
function pin   ; test ! -z "$argv"; and ln -s "$argv" ~/          ; end
function mkc   ; mkdir "$argv[1]"; and c "$argv[1]"               ; end
function vlo   ; command vim -p (loc -A "$argv")                  ; end

set scripts /home/leaf/google_drive/personal/share/Public/DotFiles/scripts.sh

for function in (grep '()' $scripts | cut -f 1 -d ' ')
  eval "function $function ; bash $scripts $function \$argv ; end"
end

function repeat
  for cmd in (seq "$argv[1]" -1 1)
    if test (echo "$history[$cmd]" | head -c 6) != "repeat"
      show "[repeat] $history[$cmd]"
      eval $history[$cmd]
    end
  end
end

function a
  if test -z "$argv"

    if test -d "$argv[1]"
      cd "$argv"

    else if test -f "$argv[1]"

    end

  else
    cd
  end
end


# autojump
#===================
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
