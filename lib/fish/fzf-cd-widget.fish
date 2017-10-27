
function fzf-cd-widget -d "Change directory"
  set -l commandline (__fzf_parse_commandline)
  set -l dir $commandline[1]
  set -l fzf_query $commandline[2]

  set -q FZF_ALT_C_COMMAND; or set -l FZF_ALT_C_COMMAND "
  command find -L \$dir -mindepth 1 \\( -path \$dir'*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' \\) -prune \
  -o -type d -print 2> /dev/null | sed 's@^\./@@'"
  set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
  begin
    set -lx FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT --reverse $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS"
    eval "$FZF_ALT_C_COMMAND | "(__fzfcmd)' +m --query "'$fzf_query'"' | read -l result

    if [ -n "$result" ]
      cd $result
      ls --color=auto

      # Remove last token from commandline.
      commandline -t ""
    end
  end

  commandline -f repaint
end

