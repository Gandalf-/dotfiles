
function fish_prompt

  if test $status -ne 0
    set_color bryellow
  else
    set_color normal
  end

  echo -n (whoami)"@$__HOST__"

  set_color $fish_color_cwd
  echo -n ' '(prompt_pwd)

  set_color normal
  printf '%s' (__fish_git_prompt)

  set_color normal
  echo '> '

end

