
function ll
  if test (command -v exa)
    exa --group-directories-first --long --git --octal-permissions --no-permissions $argv
  else
    ls -lh
  end
end
