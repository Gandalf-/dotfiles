
function l

  if test (command -v exa)
    exa --group-directories-first $argv
  else
    # bsd
    set -- color "-G"

    # gnu
    ls --version >/dev/null 2>/dev/null;
      and set -- color "--color=auto"

    ls $color $argv
  end
end
