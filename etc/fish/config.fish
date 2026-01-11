# vim: set syntax=bash

set -gx __HOST__        (hostname | sed 's/\.local//')
set -gx EDITOR          vim
set -gx XDG_CONFIG_HOME "$HOME"/.config/
set -gx TMP             /tmp

function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      l ; end
function   ...; builtin cd ../../;   l ; end
function  ....; builtin cd ../../../;l ; end

function _add_path_if_exists
  if test -d $argv[1]
    fish_add_path --global $argv[2..-1] $argv[1]
  end
end

# LOCATIONS

set --erase fish_user_paths

_add_path_if_exists ~/dotfiles/bin
_add_path_if_exists ~/Documents/dotfiles/bin
_add_path_if_exists ~/.local/bin/
_add_path_if_exists ~/Library/Python/3.13/bin/

# work
_add_path_if_exists ~/scripts/bin
_add_path_if_exists /opt/qumulo/toolchain/bin --append --path

test -f ~/scripts/etc/work.fish
  and source ~/scripts/etc/work.fish


# PLATFORMS

# macos
if test -d /opt/homebrew/bin
  set -x -U C_INCLUDE_PATH ( xcrun --show-sdk-path )/usr/include/ffi
  fish_add_path --global /opt/homebrew/bin
end

# ubuntu
_add_path_if_exists /snap/bin/


# LANGUAGES

# rust
_add_path_if_exists ~/.cargo/bin/

# haskell
_add_path_if_exists ~/.cabal/bin
_add_path_if_exists ~/.ghcup/bin

# go
_add_path_if_exists /usr/local/go/bin

# npm
_add_path_if_exists ~/.local/bin/node/

# python
test -e ~/.pythonrc
  and set -gx PYTHONSTARTUP ~/.pythonrc

set -gx PYTHONPYCACHEPREFIX /tmp/pycache

# FZF

_add_path_if_exists ~/.vim/bundle/fzf/bin

set -gx FZF_DEFAULT_COMMAND 'rg --files'
set -gx FZF_DEFAULT_OPTS '--height 75% --border --cycle'

# https://github.com/jethrokuan/fzf
set -U FZF_COMPLETE 3
if test (command -v bat)
  set -gx FZF_PREVIEW_FILE_CMD 'bat'
else
  set -gx FZF_PREVIEW_FILE_CMD 'cat'
end

set -gx FZF_PREVIEW_DIR_CMD 'ls -lh --color=always'

function l
  command l $argv
end

function ll
  command ll $argv
end

fzf_configure_bindings --directory=\ei --history=\cr --git_status=\eg


# PROMPT

set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
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
