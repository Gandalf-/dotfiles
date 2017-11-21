# vim: set syntax=bash

function fzf-contents-widget -d "List contents of files"

  set -l commandline (__fzf_parse_commandline)
  set -l dir $commandline[1]
  set -l fzf_query $commandline[2]

  command ag --nobreak --nonumbers --noheading . \
    | sort \
    | uniq \
    | fzf --multi \
    | cut -f 1 -d : \
    | while read -l r
        set result $result $r
      end

  if [ -z "$result" ]
    commandline -f repaint
    return
  else
    commandline -t ""
  end

  for i in $result
    commandline -it -- (string escape $i)
    commandline -it -- ' '
  end

  commandline -f repaint
end

