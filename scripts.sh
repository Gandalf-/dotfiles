#!/bin/bash

startsshd () { sudo mkdir -p /var/run/sshd && sudo /usr/sbin/sshd; }

tmr () { tmux send-keys -t right "$@" C-m; }
tml () { tmux send-keys -t left  "$@" C-m; }
aup () { sudo apt update && sudo apt upgrade; sudo apt-get autoremove; }

# shellcheck disable=SC2009
grap () { ps aux | grep "$1" | grep -v "grep $1"; }
grip () { ps -fC "$1"; }
calc () { bc -l <<< "$@"; }
freq () { sort | uniq -c | sort -nr | head -n "$1"; }

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
    "p")   insync-headless pause_syncing     ;;
    "r")   insync-headless resume_syncing    ;;
    "ras") insync-headless reject_all_new_shares austin.voecks@gmail.com ;;
    "re")  insync-headless retry_errors      ;;
    "gs")  insync-headless get_status        ;;
    "sp")  insync-headless get_sync_progress ;;
    *)
      echo "
insync-headless wrapper
  s   : start
  p   : pause_syncing
  r   : resume_syncing
  ras : reject_all_new_shares
  re  : retry_errors
  gs  : get_status
  sp  : get_sync_progress
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
  fi
}

g (){
  # super git wrapper!

  if [[ -z "$1" ]]; then
    g --help
    return
  fi

  do_confirm=1
  fmt="\
%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset"

  while [[ ! -z "$1" ]]; do
    case "$1" in
      "!")  do_confirm=$(( ! do_confirm ))                          ;;

      "a")  confirm "$do_confirm" "[g] git add -A :/"
            git add -A :/                                           ;;

      "bv") confirm "$do_confirm" "[g] git branch -vv"
            git branch -vv                                          ;;

      "rv") confirm "$do_confirm" "[g] git remote -vv"
            git remote -vv                                          ;;

      "ca") confirm "$do_confirm" "[g] git commit --amend"
            git commit --amend                                      ;;

      "co") confirm "$do_confirm" "[g] git checkout $2"
            git checkout "$2"   ; shift                             ;;

      "cb") confirm "$do_confirm" "[g] git checkout -b $2"
            git checkout -b "$2"; shift                             ;;

      "cg") bug_dir='/home/avoecks/cribshome/wiki/bugs/'
            bug_branch="BR_BUG_$(find "$bug_dir"   |
                                 grep "$2"         |
                                 grep -o '[0-9]\+' |
                                 head -n 1)"
            confirm "do_confirm" "[g] git checkout $bug_branch"
            git checkout "$bug_branch"; shift                       ;;

      "f")  confirm "$do_confirm" "[g] git fetch"
            git fetch                                               ;;

      "l")  confirm "$do_confirm" "[g] git log"
            git log --color=always | head -n 20                     ;;

      "lo") confirm "$do_confirm" "[g] git log --oneline"
            git log --oneline --color=always | head -n 10           ;;

      "ll") confirm "$do_confirm" "[g] git log --graph"
            git log --graph --pretty=format:"$fmt" --abbrev-commit  ;;

      "s")  confirm "$do_confirm" "[g] git status"
            git status                                              ;;

      "ri") confirm "$do_confirm" "[g] git rebase -i $2"
            if [[ ! -z "$2" ]]; then
              git rebase -i "$2"; shift
            else
              git rebase -i
            fi
        ;;

      "d")  if [[ -f "$2" ]]; then
              confirm "$do_confirm" "[g] git diff --full-index > $2"
              git diff --full-index > "$2"
              shift

            else
              confirm "$do_confirm" "[g] git diff --full-index"
              git diff --full-index
            fi
        ;;

      "dh") if [ "$2" -eq "$2" ] 2>/dev/null ; then
              commits="$2"
            else
              commits="1"
            fi

            if [[ -f "$3" ]]; then
              confirm "$do_confirm" "[g] git diff --full-index HEAD~$commits > $3"
              git diff --full-index HEAD~"$commits" > "$3"
              shift

            else
              confirm "$do_confirm" "[g] git diff --full-index HEAD~$commits"
              git diff --full-index HEAD~"$commits"
            fi
            shift
        ;;

      "ds") branch="$(git rev-parse --abbrev-ref HEAD)"
            diff="/home/avoecks/cribshome/diffs/${branch}.diff"
            if [ "$2" -eq "$2" ] 2>/dev/null ; then
              confirm "$do_confirm" "[g] git diff --full-index HEAD~$2 > $diff"
              git diff --full-index HEAD~"$2" > "$diff"
              shift

            else
              confirm "$do_confirm" "[g] git diff --full-index HEAD~1 > $diff"
              git diff --full-index HEAD~1 > "$diff"
              shift
            fi
        ;;

      "bn") confirm "$do_confirm" "[g]git checkout $remote_branch -b $new_branch_name"
            remote_branch="$2"
            new_branch_name="$3"
            git checkout "$remote_branch" -b "$new_branch_name"
            shift
            shift
        ;;
      "cm") confirm "$do_confirm" "[g] git commit -m $2"
            if [[ ! -z "$2" ]]; then
              git commit -m "$2"; shift
            fi
        ;;
      "pl") confirm "$do_confirm" "[g] git pull $2"
            if [[ ! -z "$2" ]]; then
              git pull "$2"; shift
            else
              git pull
            fi
        ;;
      "ph") confirm "$do_confirm" "[g] git push $2"
            if [[ ! -z "$2" ]]; then
              git push "$2"; shift
            else
              git push
            fi
        ;;
      "pf") confirm "$do_confirm" "[g] git push --force $2"
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
    b? : branch -vv
    bn : checkout (remote branch) -b (local branch)
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
    r? : remote -vv
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
      echo "[g] Detected failure, not continuing"
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
