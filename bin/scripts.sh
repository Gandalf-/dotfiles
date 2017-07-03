#!/bin/bash

# =============================================================================
# script.sh
# =============================================================================
#
# shell agnostic 'shell' functions and aliases
#
# using in fish shell
#   set scripts ~/.config/fish/script.sh
#
#   # external functions
#   for ex_function in (grep '()' $scripts | grep -v '#' | cut -f 1 -d ' ')
#     eval "function $ex_function ; bash $scripts $ex_function \$argv ; end"
#   end
#
#   # external aliases
#   for ex_alias in (grep '^alias ' $scripts)
#     eval "$ex_alias"
#   end
#
# using in bash
#   scripts='~/.config/fish/script.sh'
#   source $scripts

# =============================================================================
# aliases
# =============================================================================
alias b='bash'
alias e='echo'
alias h='head'
alias l='ls'
alias p='python'
alias t='task'
alias w='which'
alias v='vim'

alias p3='python3'
alias ls='ls --color=auto'
alias ta='tmux attach; or tmux'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias hn='head -n'
alias ss='sudo service'
alias pi='ipython'
alias vp='vim -p'
alias vd='vimdiff'
alias vs='vim - ; fg'
alias sv='sudo vim'
alias vv='vim -p *.{h,c{,c,++,xx,pp},java,sh,py,md,html,css,js,php,pl,txt}'
alias lo='locate -A'

alias sai='sudo apt install'
alias kut='cut -d " " -f'
alias aup='sudo apt update; sudo apt upgrade; sudo apt-get autoremove'
alias dsh='du -sh'
alias dfh='df -h'
alias lsn='ls -al --time-style=+%D | grep `date +%D` '
alias how='howdoi -c'

alias  vvim='v ~/.vimrc'
alias vtmux='v ~/.tmux.conf'
alias vfish='v ~/.config/fish/config.fish'
alias vbash='v ~/.bashrc'
alias sfish='. ~/.config/fish/config.fish'
alias sbash='. ~/.bashrc'

alias   ..='cd ../;ls'
alias  ...='cd ../../;ls'
alias ....='cd ../../../;ls'

alias    qc='vim ~/*/code/c/quick/quick.c'
alias   qpy='vim ~/*/code/python/quick.py'
alias qjava='vim ~/*/code/java/Quick/Quick.java'
alias   qsh='vim ~/*/code/shell/quick.sh'

alias dos2unix='recode dos/CR-LF..l1'
alias unix2win='recode l1..windows-1250'
alias unix2dos='recode l1..dos/CR-LF'

alias xklip='head -c -1 | xclip -selection c'

# =============================================================================
# functions
# =============================================================================

shttp ()     { python -m SimpleHTTPServer; }
silent ()    { cat - >/dev/null 2>/dev/null; }
sandman ()   { kill -9 "$(jobs -p)"; }
startsshd () { sudo mkdir -p -m0755 /var/run/sshd; sudo /usr/sbin/sshd; }

pin () { [[ ! -z "$@" ]] && ln -s "$@" ~/; }
mkc () { mkdir "$1" && cd "$1" || return; }
tmr () { tmux send-keys -t right "$@" C-m; }
tml () { tmux send-keys -t left  "$@" C-m; }

# shellcheck disable=SC2009
grap () { ps aux | grep "$1" | grep -v "grep $1"; }
grip () { ps -fC "$1"; }
calc () { bc -l <<< "$@"; }
freq () { sort | uniq -c | sort -nr | head -n "$1"; }

weather () { curl http://wttr.in/~"$1"; }

F () {
  if [[ "$1" == "z" ]]; then
    shift
    find . -name '*'"$1"'*'

  else
    find . -name "$1"
  fi
}

cleanup () {
  # smart remove duplicate file names and intermediary file types

  local dry counter fixed; dry=0
  [[ "$1" == '-i' ]] && dry=1

  counter=0
  while read -r file; do
    fixed="$(sed -e 's/[ ]*([0-9]\+)//' <<< "$file")"

    if [[ -f "$fixed" ]]; then
      echo "remove dup: $file"
      (( dry )) && rm "$file"

    else
      echo "rename dup: $file"
      (( dry )) && mv "$file" "$fixed"
    fi

    let counter++
  done < <(find -regex '.*([0-9]+).*')

  while read -r file; do
    echo "remove    : $file"
    (( dry )) && rm "$file"
    let counter++
  done < <(find -regex '.*\.\(pyc\|class\|o\|bak\)')

  echo "Cleaned up $counter files"
}

ratio (){
  freq "$1" |
  awk '{a[$2]=$1;s+=$1}END{for(i in a)printf"%-40s%-15d%6.2f%%\n",i,a[i],a[i]/s*100}' |
  sort -r -k2,2 -n
}

ttmux (){
  # easier tmux layouts

  if [[ -z "$2" ]]; then
    echo "t (t,h,v) (amount)"; exit
  fi

  for _ in $( seq $(( $2 - 1 )) | xargs ); do
    case $1 in
      t) tmux split-window -h; tmux select-layout tiled           ;;
      h) tmux split-window -h; tmux select-layout even-vertical   ;;
      v) tmux split-window -h; tmux select-layout even-horizontal ;;
      *) echo "t (t,h,v) (amount)" ;;
    esac
  done
}

gin (){
  # insync-headless wrapper

  case "$1" in
    "s")   insync-headless start             ;;
    "ps")  insync-headless pause_syncing     ;;
    "rs")  insync-headless resume_syncing    ;;
    "re")  insync-headless retry_errors      ;;
    "gs")  insync-headless get_status        ;;
    "ge")  insync-headless get_errors        ;;
    "gsp") insync-headless get_sync_progress ;;
    *)
      echo "
insync-headless wrapper
   s : start
  ps : pause syncing
  rs : resume syncing
  re : retry errors
  gs : get status
  ge : get errors
 gsp : get sync progress
       "
      ;;
  esac
}

# provide functions to callers
"$@"
