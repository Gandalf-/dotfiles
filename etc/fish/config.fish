# vim: set syntax=bash

set -gx __HOST__        (hostname | sed 's/\.local//')
set -gx EDITOR          vim
set -gx XDG_CONFIG_HOME "$HOME"/.config/
set -gx TMP             /tmp

function sfish; source ~/.config/fish/config.fish; end
function    ..; builtin cd ../;      l ; end
function   ...; builtin cd ../../;   l ; end
function  ....; builtin cd ../../../;l ; end


# LOCATIONS

test -d ~/dotfiles
  and fish_add_path ~/dotfiles/bin

test -d ~/Documents/dotfiles
  fish_add_path ~/Documents/dotfiles/bin

test -d ~/.local/bin/
  and fish_add_path ~/.local/bin

# work
test -d /opt/qumulo/toolchain/bin
  and fish_add_path /opt/qumulo/toolchain/bin

test -d ~/scripts/bin
  and fish_add_path ~/scripts/bin


# PLATFORMS

# macos
if test -d /opt/homebrew/bin
  set -x -U C_INCLUDE_PATH ( xcrun --show-sdk-path )/usr/include/ffi
  fish_add_path /opt/homebrew/bin
end

# ubuntu
test -d /snap/bin
  and fish_add_path /snap/bin/


# LANGUAGES

# rust
test -d ~/.cargo/bin/
  and fish_add_path ~/.cargo/bin/

# haskell
test -d ~/.cabal/bin/
  and fish_add_path ~/.cabal/bin

test -d ~/.ghcup/bin/
  and fish_add_path ~/.ghcup/bin

# go
if test -d /usr/local/go/bin; and test -d $HOME/working/go/bin
  fish_add_path $HOME/working/go/bin /usr/local/go/bin
  set -x -U GOPATH $HOME/working/go
end

# npm
test -d ~/.local/bin/node/
  and fish_add_path ~/.local/bin/node/

# python
test -e ~/.pythonrc
  and set -gx PYTHONSTARTUP ~/.pythonrc


# FZF

if not test (command -v fzf)
  if test -e ~/.vim/bundle/fzf.vim/bin/fzf
    fish_add_path ~/.vim/bundle/fzf.vim/bin

  else if test -e ~/.vim/bundle/fzf/bin/fzf
    fish_add_path ~/.vim/bundle/fzf/bin
  end
end

set -gx FZF_DEFAULT_COMMAND 'rg -g ""'
set -gx FZF_DEFAULT_OPTS '--height 75% --border --cycle'

# https://github.com/jethrokuan/fzf
set -U FZF_COMPLETE 3
if test (command -v bat)
  set -gx FZF_PREVIEW_FILE_CMD 'bat'
else
  set -gx FZF_PREVIEW_FILE_CMD 'cat'
end
set -gx FZF_PREVIEW_DIR_CMD 'ls -h --color=always'

# https://github.com/PatrickF1/fzf.fish
test (command -v exa)
  and set -gx fzf_preview_dir_cmd exa --all --long

fzf_configure_bindings --directory=\ei --history=\cr --git_status=\eg

alias ed "ed -p '🐠 '"
alias cs "gh copilot suggest"


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

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish ; or true
