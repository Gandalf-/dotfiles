# vim: set syntax=bash

function fzf-history-widget -d "Show command history"
  set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
  begin
    set -lx FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT $FZF_DEFAULT_OPTS --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m"

    set -l FISH_MAJOR (echo $FISH_VERSION | cut -f1 -d.)
    set -l FISH_MINOR (echo $FISH_VERSION | cut -f2 -d.)

    # history's -z flag is needed for multi-line support.
    # history's -z flag was added in fish 2.4.0, so don't use it for versions
    # before 2.4.0.
    if [ "$FISH_MAJOR" -gt 2 -o \( "$FISH_MAJOR" -eq 2 -a "$FISH_MINOR" -ge 4 \) ] 2>/dev/null;
      history -z | eval (__fzfcmd) --read0 -q '(commandline)' | perl -pe 'chomp if eof' | read -lz result
      and commandline -- $result
    else
      history | uniq | eval (__fzfcmd) -q '(commandline)' | read -l result
      and commandline -- $result
    end
  end
  commandline -f repaint
end
