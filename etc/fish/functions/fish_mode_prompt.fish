# vim: set syntax=bash

function fish_mode_prompt --description 'Displays the current mode'

  if test "$fish_key_bindings" = "fish_vi_key_bindings"

    switch $fish_bind_mode
      case default; set_color --bold red;   echo N
      case insert;  set_color --bold green; echo I
      case visual;  set_color --bold blue;  echo V
    end

    set_color normal; printf " "
  end
end
