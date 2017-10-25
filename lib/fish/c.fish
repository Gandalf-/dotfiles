
function c
  # cd
  fzf | read -l result
  [ "$result" ]; and cd (dirname $result)
  commandline -f repaint
  ls --color=auto
end

