# vim: set syntax=bash

function fish_user_key_bindings
  bind \e. 'history-token-search-backward'

  fish_vi_key_bindings # insert

  # give fish more time to reassemble split escape sequences (e.g. focus
  # reports \e[I / \e[O leaking on tmux pane switches) before vi-mode treats a
  # lone ESC as insert->normal and mis-reads the trailing bytes as commands
  set -g fish_escape_delay_ms 100

  bind -M insert \cf forward-char

  # emacs is convenient sometimes
  bind -M insert \ce end-of-line
  bind -M insert \ca beginning-of-line

  # vim
  bind -M default E end-of-line
  bind -M default B beginning-of-line

  # Kill "edit command line in $EDITOR": it fires unexpectedly when tmux/nvim
  # focus-report sequences (\e[I / \e[O) leak into the pane and vi-mode splits
  # the leading ESC. Erase every binding that reaches edit_command_buffer
  # (the \ee/\ev presets in all modes, plus the I override above is now gone).
  for mode in insert default visual
    bind --preset -e -M $mode \ee
    bind --preset -e -M $mode \ev
  end

  fzf_configure_bindings --directory=\ei --history=\cr --git_status=\eg
end
