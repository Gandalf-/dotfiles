# vim: set syntax=bash

function fish_user_key_bindings
  bind \e. 'history-token-search-backward'

  fish_vi_key_bindings # insert
  bind -M insert \cf forward-char

  # emacs is convenient sometimes
  bind -M insert \ce end-of-line
  bind -M insert \ca beginning-of-line

  # vim
  bind -M default E end-of-line
  bind -M default B beginning-of-line
  bind -M default I edit_command_buffer

  fzf_configure_bindings --directory=\ei --history=\cr --git_status=\eg
end
