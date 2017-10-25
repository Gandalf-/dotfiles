
function k
  fzf | read -l result
  [ "$result" ]; and vim "$result"
  commandline -f repaint
end
