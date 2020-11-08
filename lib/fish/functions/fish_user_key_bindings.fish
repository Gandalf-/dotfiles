# vim: set syntax=bash

function fish_user_key_bindings
  bind \e. 'history-token-search-backward'

  # fish_default_key_bindings -M insert
  fish_vi_key_bindings # insert
  bind -M insert \cf forward-char

  # emacs is convenient sometimes
  bind -M insert \ce end-of-line
  bind -M insert \ca beginning-of-line

  # vim
  bind -M default E end-of-line
  bind -M default B beginning-of-line
  bind -M default I edit_command_buffer

  # fuzzy completion
  fzf_key_bindings

  bind -M insert \ci fzf-complete           # context aware tab complete
  bind -M insert \ei fzf-file-widget        # files by name
  bind -M insert \cr fzf-history-widget     # backwards history search
  bind -M insert \ec fzf-cd-widget          # forward cd
  bind -M insert \eb cb                     # backwards cd
end
