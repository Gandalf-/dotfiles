# vim: set syntax=bash

function j
  set new_path (autojump $argv)

  if test -d "$new_path" -a "$new_path" != "."
    printf "%s\n\n" $new_path
    cd "$new_path"
    ls --color=auto
  else
    echo "autojump: directory '$argv' not found" >&2
    false
  end
end
