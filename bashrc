#~/.bashrc: executed by bash(1) for non-login shells.

# where are we?
at_work=0
[[ $(hostname) == 'wkstn-avoecks' ]] && at_work=1

if (( at_work )); then
	export QA=/usr/local/qa
	export TOOLS=/usr/local/tools
	export QALIB=$QA/lib
	export PERL5LIB=$QA/lib
	export PATH=$QA/bin:$QA/tests:$QA/linux/bin:$TOOLS:$PATH
fi

# colored man pages!
  export LESS_TERMCAP_mb=$'\E[01;31m'
  export LESS_TERMCAP_md=$'\E[01;31m'
  export LESS_TERMCAP_me=$'\E[0m'
  export LESS_TERMCAP_se=$'\E[0m'
  export LESS_TERMCAP_so=$'\E[01;44;33m'
  export LESS_TERMCAP_ue=$'\E[0m'
  export LESS_TERMCAP_us=$'\E[01;32m'

# all the cool features
  bind "set completion-ignore-case on"
  bind "set completion-map-case on"
  bind "set show-all-if-ambiguous on"
  bind 'TAB:menu-complete'
  bind 'set show-mode-in-prompt on'
  bind -m vi-insert "\C-l":clear-screen

  shopt -s dirspell 2> /dev/null
  shopt -s checkwinsize

# better completion?
  complete -cf man

# If not running interactively, don't do anything
  case $- in
    *i*) ;;
    *) return;;
  esac

# basics
  export EDITOR=vim
  export PATH=$PATH:/sbin:/usr/local/sbin:/usr/sbin
  export PROMPT_DIRTRIM=5
  bind '"\e[A": history-search-backward'
  bind '"\e[B": history-search-forward'

# scripts.sh
  if (( at_work )); then
    scripts=~/cribshome/DotFiles

  elif [[ -d ~/google_drive ]]; then
    scripts=~/google_drive/personal/share/Public/DotFiles/

  else
    scripts=/tmp/DotFiles
  fi

  if [[ ! -z "$scripts" ]]; then
    source $scripts/bin/aliases.list
		export PATH=$scripts/bin:"${PATH}"
  fi

# functions
_prompt() {
  local tmp dir_abbr ps1

  tmp=$(sed -r 's/(\/.)[^/]*/\1/g' <<< "${PWD/$HOME/\~}")
  dir_abbr="${tmp:0:$(( ${#tmp} - 1 ))}"
  ps1="$(basename "${PWD}")"

  if [[ ${#ps1} -gt 25 ]]; then
    ps1="${ps1:0:7}...${ps1: -7}"
  fi

  echo -n "${dir_abbr}${ps1}"
}

c() { cd "$@" && ls --color;}

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# History
  HISTCONTROL=ignoreboth
  shopt -s histappend
  HISTSIZE=5000
  HISTFILESIZE=10000
  export HISTTIMEFORMAT="%h/%d - %H:%M:%S "

# make less more friendly for non-text input files, see lesspipe(1)
  [[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
  if [[ -z "${debian_chroot:-}" && -r /etc/debian_chroot ]]; then
    debian_chroot=$(cat /etc/debian_chroot)
  fi

# colors + prompt
  case "$TERM" in
    xterm-color) color_prompt=yes;;
  esac
  force_color_prompt=yes

  if [[ -n "$force_color_prompt" ]]; then
    if [[ -x /usr/bin/tput ]] && tput setaf 1 >&/dev/null; then
      color_prompt=yes
    else
      color_prompt=
    fi
  fi

  if [[ "$color_prompt" == yes ]]; then
    host=$(hostname | sed -e 's/localhost/home/')
    host=$(sed -e 's/wkstn-avoecks/work/' <<< "$host")
    PS1='\[\033[01;32m\]\u@${host}\[\033[00m\] \[\033[01;36m\]$(_prompt)\[\033[00m\]$(parse_git_branch)> '

  else
    echo
  fi
  unset color_prompt force_color_prompt

# enable color support of ls and also add handy aliases
  if [[ -x /usr/bin/dircolors ]]; then

    if test -r ~/.dircolors; then
      eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    fi
  fi

# autojump
	export AUTOJUMP_SOURCED=1

	# set user installation paths
	if [[ -d ~/.autojump/ ]]; then
		export PATH=~/.autojump/bin:"${PATH}"
	fi

	# set error file location
	if [[ "$(uname)" == "Darwin" ]]; then
		export AUTOJUMP_ERROR_PATH=~/Library/autojump/errors.log
	elif [[ -n "${XDG_DATA_HOME}" ]]; then
		export AUTOJUMP_ERROR_PATH="${XDG_DATA_HOME}/autojump/errors.log"
	else
		export AUTOJUMP_ERROR_PATH=~/.local/share/autojump/errors.log
	fi

	if [[ ! -d "$(dirname ${AUTOJUMP_ERROR_PATH})" ]]; then
		mkdir -p "$(dirname ${AUTOJUMP_ERROR_PATH})"
	fi

	_autojump() {
		# enable tab completion
		local cur
		cur=${COMP_WORDS[*]:1}
		comps=$(autojump --complete "$cur")
		while read -r i; do
			COMPREPLY=("${COMPREPLY[@]}" "${i}")
		done <<EOF
		$comps
EOF
	}
	complete -F _autojump j

	autojump_add_to_database() {
		# change pwd hook
		if [[ -f "${AUTOJUMP_ERROR_PATH}" ]]; then
			(autojump --add "$(pwd)" >/dev/null 2>>${AUTOJUMP_ERROR_PATH} &) &>/dev/null
		else
			(autojump --add "$(pwd)" >/dev/null &) &>/dev/null
		fi
	}

	case $PROMPT_COMMAND in
		*autojump*)
			;;
		*)
			PROMPT_COMMAND="${PROMPT_COMMAND:+$(echo "${PROMPT_COMMAND}" | awk '{gsub(/; *$/,"")}1') ; }autojump_add_to_database"
			;;
	esac


	j() {
		# default autojump command

		if [[ ${1} == -* ]] && [[ ${1} != "--" ]]; then
			autojump "${@}"
			return
		fi

		output="$(autojump "${@}")"
		if [[ -d "${output}" ]]; then
			if [ -t 1 ]; then  # if stdout is a terminal, use colors
				echo -e "\\033[31m${output}\\033[0m"
			else
				echo -e "${output}"
			fi
			cd "${output}" || return
      l
		else
			echo "autojump: directory '${*}' not found"
			echo -e "\n${output}\n"
			echo "Try \`autojump --help\` for more information."
			false
		fi
	}

	jc() {
		# jump to child directory (subdirectory of current path)

		if [[ ${1} == -* ]] && [[ ${1} != "--" ]]; then
			autojump "${@}"
			return
		else
			j "$(pwd) ${*}"
		fi
	}
