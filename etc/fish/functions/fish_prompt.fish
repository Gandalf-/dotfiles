# vim: set syntax=bash


function fish_prompt

  if test $status -ne 0
    set_color bryellow
  else
    set_color normal
  end

  echo -n (whoami)"@$__HOST__"

  set_color $fish_color_cwd
  echo -n ' '(prompt_pwd)

  # disable: set -U __disable_git_prompt_on_this_system
  # enable:  set -e __disable_git_prompt_on_this_system
  if not set -q __disable_git_prompt_on_this_system
    set_color normal
    printf '%s' (__fish_git_prompt)
  end

  set_color normal
  echo '> '

end
