# vim: set syntax=bash

function l

  # bsd
  set -- color "-G"

  # gnu
  ls --version >/dev/null 2>/dev/null;
    and set -- color "--color=auto"

  ls $color $argv
end
