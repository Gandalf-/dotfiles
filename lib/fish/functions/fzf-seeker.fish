# vim: set syntax=bash

# Store current token in $dir as root for the 'find' command
function fzf-seeker -d "List files and folders"
  set -l commandline (__fzf_parse_commandline)

  set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
  begin
    seeker | while read -l r; set result $result $r; end
  end
  if [ -z "$result" ]
    commandline -f repaint
    return
  else
    # Remove last token from commandline.
    commandline -t ""
  end
  for i in $result
    commandline -it -- (string escape $i)
    commandline -it -- ' '
  end
  commandline -f repaint
end

