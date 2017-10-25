
function cb
  # cd backwards
  pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | fzf | read -l result
  [ "$result" ]; and cd (dirname $result)
  commandline -f repaint
  ls --color=auto
end

