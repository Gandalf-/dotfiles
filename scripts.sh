#!/bin/bash

# =============================================================================
# script.sh
# =============================================================================

# =============================================================================
# aliases
# =============================================================================
alias dos2unix='recode dos/CR-LF..l1'
alias unix2win='recode l1..windows-1250'
alias unix2dos='recode l1..dos/CR-LF'

alias      ta='tmux attach; or tmux'
alias     dsh='du -sh'
alias     dfh='df -h'
alias     lsn='ls -al --time-style=+%D | grep `date +%D` '
alias     how='howdoi -c'
alias   xklip='head -c -1 | xclip -selection c'

alias b='bash'
alias e='echo'
alias F='find . -name'
alias h='head'
alias l='ls'
alias p='python'
alias t='task'
alias w='which'
alias v='vim'

alias p3='python3'

alias ls='ls --color=auto'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias   ..='cd ../;ls'
alias  ...='cd ../../;ls'
alias ....='cd ../../../;ls'

alias    qc='vim ~/*/code/c/quick/quick.c'
alias   qpy='vim ~/*/code/python/quick.py'
alias qjava='vim ~/*/code/java/Quick/Quick.java'
alias   qsh='vim ~/*/code/shell/quick.sh'

alias hn='head -n'
alias ss='sudo service'
alias pi='ipython'
alias vp='vim -p'
alias vd='vimdiff'
alias vs='vim - ; fg'
alias sv='sudo vim'
alias vv='vim -p *.{h,c{,c,++,xx,pp},java,sh,py,md,html,css,js,php,pl,txt}'
alias cl='clear;ls'
alias lo='locate -A'

alias sai='sudo apt install'
alias kut='cut -d " " -f'
alias aup='sudo apt update; sudo apt upgrade; sudo apt-get autoremove'

alias  vvim='v ~/.vimrc'
alias vtmux='v ~/.tmux.conf'
alias vfish='v ~/.config/fish/config.fish'
alias vbash='v ~/.bashrc'
alias sfish='. ~/.config/fish/config.fish'
alias sbash='. ~/.bashrc'

# =============================================================================
# functions
# =============================================================================

shttp ()     { python -m SimpleHTTPServer; }
silent ()    { cat - >/dev/null 2>/dev/null; }
sandman ()   { kill -9 "$(jobs -p)"; }
startsshd () { sudo mkdir -p -m0755 /var/run/sshd; sudo /usr/sbin/sshd; }

tmr () { tmux send-keys -t right "$@" C-m; }
tml () { tmux send-keys -t left  "$@" C-m; }

# shellcheck disable=SC2009
grap () { ps aux | grep "$1" | grep -v "grep $1"; }
grip () { ps -fC "$1"; }
calc () { bc -l <<< "$@"; }
freq () { sort | uniq -c | sort -nr | head -n "$1"; }

weather () { curl http://wttr.in/~"$1"; }

cleanup () {
  # smart remove duplicate file names and intermediary file types

  counter=0
  while read -r file; do
    fixed="$(sed -e 's/[ ]*([0-9]\+)//' <<< "$file")"

    if [[ -f "$fixed" ]]; then
      echo "remove dup: $file"
      rm "$file"

    else
      echo "rename dup: $file"
      mv "$file" "$fixed"
    fi

    let counter++
  done < <(find -regex '.*([0-9]+).*')

  while read -r file; do
    echo "remove    : $file"
    rm "$file"
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

confirm (){
  # print arguments with green color

  green="\033[01;32m"
  normal="\033[00m"

  if [[ "$1" != 0 ]]; then
    shift
    printf "%b%s%b " "$green" "$@" "$normal"
    read -r reply; [[ "$reply" =~ [Nn] ]] && exit

  else
    shift
    printf "%b%s%b " "$green" "$@" "$normal"
  fi
}

g (){
  # super git wrapper!

  if [[ -z "$1" ]]; then
    g --help
    return
  fi

  cnfrm=1
  fmt="\
%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset"

  while [[ ! -z "$1" ]]; do
    case "$1" in
      "!")  cnfrm=$(( ! cnfrm ))                          ;;

      "a")  confirm "$cnfrm" "git add -A :/"
            git add -A :/                                           ;;

      "bv") confirm "$cnfrm" "git branch -vv"
            git branch -vv                                          ;;

      "rv") confirm "$cnfrm" "git remote -vv"
            git remote -vv                                          ;;

      "c")  confirm "$cnfrm" "git commit"
            git commit                                              ;;

      "ca") confirm "$cnfrm" "git commit --amend"
            git commit --amend                                      ;;

      "co") confirm "$cnfrm" "git checkout $2"
            git checkout "$2"   ; shift                             ;;

      "cb") confirm "$cnfrm" "git checkout -b $2"
            git checkout -b "$2"; shift                             ;;

      "cg") bug_dir='/home/avoecks/cribshome/wiki/bugs/'
            bug_branch="BR_BUG_$(find "$bug_dir"   |
                                 grep "$2"         |
                                 grep -o '[0-9]\+' |
                                 head -n 1)"
            confirm "cnfrm" "git checkout $bug_branch"
            git checkout "$bug_branch"; shift                       ;;

      "f")  confirm "$cnfrm" "git fetch"
            git fetch                                               ;;

      "l")  confirm "$cnfrm" "git log"
            git log --color=always | head -n 20                     ;;

      "lo") confirm "$cnfrm" "git log --oneline"
            git log --oneline --color=always | head -n 10           ;;

      "ll") confirm "$cnfrm" "git log --graph"
            git log --graph --pretty=format:"$fmt" --abbrev-commit  ;;

      "s")  confirm "$cnfrm" "git status"
            git status                                              ;;

      "ri") confirm "$cnfrm" "git rebase -i $2"
            if [[ ! -z "$2" ]]; then
              git rebase -i "$2"; shift
            else
              git rebase -i
            fi
        ;;

      "d")  if [[ -f "$2" ]]; then
              confirm "$cnfrm" "git diff --full-index > $2"
              git diff --full-index > "$2"
              shift

            else
              confirm "$cnfrm" "git diff --full-index"
              git diff --full-index
            fi
        ;;

      "dh") if [ "$2" -eq "$2" ] 2>/dev/null ; then
              commits="$2"
            else
              commits="1"
            fi

            if [[ -f "$3" ]]; then
              confirm "$cnfrm" "git diff --full-index HEAD~$commits > $3"
              git diff --full-index HEAD~"$commits" > "$3"
              shift

            else
              confirm "$cnfrm" "git diff --full-index HEAD~$commits"
              git diff --full-index HEAD~"$commits"
            fi
            shift
        ;;

      "ds") branch="$(git rev-parse --abbrev-ref HEAD)"
            diff="/home/avoecks/cribshome/diffs/${branch}.diff"
            if [ "$2" -eq "$2" ] 2>/dev/null ; then
              confirm "$cnfrm" "git diff --full-index HEAD~$2 > $diff"
              git diff --full-index HEAD~"$2" > "$diff"
              shift

            else
              confirm "$cnfrm" "git diff --full-index HEAD~1 > $diff"
              git diff --full-index HEAD~1 > "$diff"
              shift
            fi
        ;;

      "bn") confirm "$cnfrm" "git checkout $remote_branch -b $new_branch_name"
            remote_branch="$2"
            new_branch_name="$3"
            git checkout "$remote_branch" -b "$new_branch_name"
            shift
            shift
        ;;
      "cm") confirm "$cnfrm" "git commit -m $2"
            if [[ ! -z "$2" ]]; then
              git commit -m "$2"; shift
            fi
        ;;
      "pl") confirm "$cnfrm" "git pull $2"
            if [[ ! -z "$2" ]]; then
              git pull "$2"; shift
            else
              git pull
            fi
        ;;
      "ph") confirm "$cnfrm" "git push $2"
            if [[ ! -z "$2" ]]; then
              git push "$2"; shift
            else
              git push
            fi
        ;;
      "pf") confirm "$cnfrm" "git push --force $2"
            if [[ ! -z "$2" ]]; then
              git push --force "$2"; shift
            else
              git push --force
            fi
        ;;
      *)
        echo "
  g
    !  : toggle confirmation
    a  : add everything
    bv : branch -vv
    bn : checkout (remote branch) -b (local branch)
    c  : commit
    ca : commit amend
    cb : checkout -b (branch)
    cg : attempt to checkout branch by bug name
    cm : commit -m (message)
    co : checkout (file)
    d  : diff changes [output_file]
    dh : diff commits [number of commits] [output_file]
    ds : diff commits [number of commits] - auto names diff
    f  : fetch
    l  : log
    ll : log graph
    s  : status
    rv : remote -vv
    ri : interactive rebase
    p  : pause
    pl : pull [branch]
    ph : push [branch]
    pf : push --force [branch]
        "
        return
        ;;
    esac

    if (( $? )); then
      confirm 0 "Detected failure, not continuing"
      return
    fi

    shift

    [[ -z "$1" ]] || echo
  done
}

sf (){

  files=""

  while [[ ! -z "$1" ]]; do

    if [[ ! -f "$1" ]]; then
      # shellcheck disable=SC2038
      files="$files $(find . -name "$1" | xargs)"

    else
      files="$files $1"
    fi
    shift
  done

  if [[ ! -z "$files" ]]; then
    # shellcheck disable=SC2086
    vim -p $files

  else
    echo "error: no files found"
  fi
}

# provide functions to callers
"$@"
