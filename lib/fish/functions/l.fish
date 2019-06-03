# vim: set syntax=bash

function l
  if ls --version >/dev/null 2>/dev/null;
    ls --color=auto "$argv"

  else
    ls -G
  end
end
