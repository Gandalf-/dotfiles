#!/usr/bin/env bash

# wizard
#   The main wizard library. Functions are defined here, processed by
#   auto_cli.sh and built into a library sourced by bin/w
#
#   All the intermediary functions are produced by auto_cli.sh


wizard_execute_timeless() {

  common::required-help "$2" "[command...] -- [paths...]

  Execute the command against the paths but preserve the timestamp after the
  command completes, regardless of success
  "
  local cmd=()
  local paths=()
  local found=0

  for var in "$@"; do
    if [[ $var == -- ]]; then
      found=1
    elif (( ! found )); then
      cmd+=( "$var" )
    else
      paths+=( "$var" )
    fi
  done

  (( found )) || common::error
    "didn't find -- delimiter to separate the command from the paths"

  execute() {
    local path="$1"
    local when=0
    when="$( find "$path" -printf '%Ty%Tm%Td%TH%TM\n' )" \
      || common::error "couldn't fetch timestamp for $path"

    "${cmd[@]}" "$path"

    touch -m -t "$when" "$path" \
      || die "restoring timestamp $when to $path returned $?"
  }

  common::for "${paths[@]}" |
    common::map execute
}


wizard_start_watcher() {

  common::required-help "$2" "[path] [command...]
  "
  local path="$1"
  local previous_value=""

  while :; do
    value="$( stat -c '%Y' "$path" )"

    if [[ "$value" != "$previous_value" ]]; then
      echo "detected change"
      "${@:2}" || common::error "command failure, breaking"
    fi

    previous_value="$value"
    sleep 1
  done
}


wizard_switch() {

  local selection; selection="$(wizard show projects | fzf --cycle)"

  [[ $selection ]] || common::error "no selection made"
  wizard make session "$selection"
}


wizard_hunt() {

  common::optional-help "$1" "(-#)

  find processes and send them a signal, default SIGTERM
  can signal multiple processes at once

    wizard hunt
    wizard hunt -9
    wizard hunt vim
    wizard hunt -HUP apache
  "

  local signal=-TERM
  case $1 in -*) signal="$1"; shift ;; esac

  ps ax \
    | { [[ $1 ]] && grep "$1"; cat; } \
    | cut -c 1-250 \
    | common::multi-menu \
    | awk '{print $1}' \
    | common::map kill "$signal"
}


wizard_regenerate() {

  # safely tell the script to rewrite itself. running auto_wizard in the middle
  # of a wizard call will break the script (since it's overwriting itself)

  common::optional-help "$1" "

  regenerate wizard using auto_wizard, without clobbering ourselves
  "

  ( sleep 0.5
    auto_wizard | grep -v 'scripity'
  ) &

  disown
  exit 0
}


wizard_git_fetch() {

  common::optional-help "$1" "

  recursively discover git directories under the current working directory,
  fetch all branches
  "

  common::check-network || common::error "no network connection"

  while read -r directory; do
    (
      # sub shell because we're changing directories

      local dir; dir="$(dirname "$directory")"
      cd "$dir" || exit
      git fetch --quiet --all --recurse-submodules --prune
      echo "$dir"
    ) &

  done < <(find . -name .git) | sort
  wait
}


wizard_git_report() {

  common::optional-help "$1" "

  recursively discover git directories under the current working directory,
  print out a small human readable report on their status
  "

  while read -r directory; do
    (
      local dir; dir="$(dirname "$directory")"
      cd "$dir" || exit
      local status; status="$(git status)"

      grep -q 'Your branch is ahead of' <<< "$status" &&
        echo "$dir has local commits not pushed to remote"

      grep -q 'can be fast-forwarded' <<< "$status" &&
        echo "$dir can be fast-forwarded"

      grep -q 'Changes not staged for commit' <<< "$status" &&
        echo "$dir has uncommited, modified files"

      grep -q 'Untracked files' <<< "$status" &&
        echo "$dir has untracked files"

    ) &

  done < <(find . -name .git) | sort
  wait
}


common::require 'ffmpeg' &&
wizard_transcode_movies() {

  common::required-help "$1" "[file.avi ...]

  convert all input files to mp4 using ffmpeg, asks for confirmation before
  deleting the source file
  "

  local preset=veryslow

  echo "Processing: $*"
  for file in "$@"; do

    common::do ffmpeg -hide_banner -i "$file" \
      -c:v libx264 -crf 19 -preset "$preset" -strict -2 \
      -c:a aac -b:a 192k -ac 2 "${file%.*}.mp4" \
      || common::error "failed on \"$file\". Giving up"

    common::echo "Waiting..."; sleep 5
    common::do rm -i "$file"
  done
}


wizard_transcode_mov-to-mp4() {

  common::required-help "$1" "(--mute) [input.mov ...]

  convert a .mov to .mp4 that's HTML5 compatible
  "
  local count=$#

  case "$1" in
    -m|--mute)
      mute=-an
      shift
      ;;
  esac

  while [[ $1 ]]; do
    common::echo "processing $1"

    local input="$1"
    local name="${input%.*}"
    local output="$name.mp4"

    common::file-exists "$output" && {
      "$output already exists"
      continue
    }

    # https://gist.github.com/jaydenseric/220c785d6289bcfd7366
    QUIET=1 common::do ffmpeg \
      -hide_banner \
      -loglevel error \
      -i "'$input'" \
      $mute \
      -c:v libx264 \
      -pix_fmt yuv420p \
      -profile:v baseline \
      -level 3.0 \
      -crf 20 \
      -preset veryslow \
      -vf scale=1280:-2 \
      -c:a aac \
      -strict experimental \
      -movflags +faststart \
      -threads 0 \
      "'$output'"

    QUIET=1 common::do chmod 644 "'$output'"
    shift
  done

  return $count
}


common::require 'service' 'ntpd' &&
wizard_sync_time() {

  common::optional-help "$1" "

  synchonrize the system clock with NTP
  "

  common::sudo service ntp stop
  common::sudo ntpd -gq
  common::sudo service ntp start
}


common::require 'xcape' &&
wizard_chromeos_swap-search-escape() {

  common::do xcape -e 'Super_L=Escape'
}


common::require 'dpkg' &&
wizard_show_largest-packages() {

  common::optional-help "$1" "

  list all packages installed, sorted by size

    $ w s lp | head -n 50
  "

  # shellcheck disable=SC2016
  dpkg-query -Wf '${Installed-Size}\t${Package}\n' \
    | sort -nr
}


http::docker() {

  local port="${1:-8080}"

  docker run -it --rm \
    -p "$port":80 \
    -v "$( pwd )":/usr/share/nginx/html \
    nginx
}


wizard_start_http-server() {

  # start an http server

  if common::program-exists docker; then
    http::docker "$@"

  else

python3 -c "
from http.server import SimpleHTTPRequestHandler, test
import argparse

class InlineHandler(SimpleHTTPRequestHandler):

    def end_headers(self):
        mimetype = self.guess_type(self.path)
        is_file = not self.path.endswith('/')
        print(mimetype, is_file)

        # This part adds extra headers for some file types.
        if is_file and mimetype in ['text/plain', 'application/octet-stream']:
            self.send_header('Content-Type', 'text/plain')
            self.send_header('Content-Disposition', 'inline')
        super().end_headers()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--bind', '-b', default='', metavar='ADDRESS',
                        help='Specify alternate bind address '
                             '[default: all interfaces]')
    parser.add_argument('port', action='store',
                        default=8000, type=int,
                        nargs='?',
                        help='Specify alternate port [default: 8000]')
    args = parser.parse_args()
    test(InlineHandler, port=args.port, bind=args.bind)
" "$@"
  fi
}


wizard_show_frequencies() {

  common::required-help "$1" "[amount]

  count the occurances of each input line
  "

  sort \
    | uniq -c \
    | sort -nr \
    | head -n "$1";
}


wizard_show_ratio() {

  common::required-help "$1" "[amount]

  count the occurances of each input line, produce ratio data
  "

  sort \
    | uniq -c \
    | sort -nr \
    | head -n "$1" \
    | awk '{a[$2]=$1;s+=$1}END{for(i in a)printf"%-55s%-15d%6.2f%%\n",i,a[i],a[i]/s*100}' \
    | sort -r -k 2,2 -n
}


wizard_parse_json() {

  common::optional-help "$1" "

  pipe in json and pretty print it

    $ curl remote.com/file.json | w parse json
  "

  python -m json.tool
}


wizard_parse_xml() {

  common::required-help "$1" "< file.xml

  pipe in a file an pretty print XML
  "

  xmllint --format -
}


wizard_update_platform() {

  common::optional-help "$1" "

  update system packages, whatever that means on this system
  "
  case "$( uname )" in
    Linux)
      (( QUIET )) && extra="-qq"
      common::sudo apt update "$extra"
      common::sudo apt upgrade -y "$extra"
      common::sudo apt-get autoremove -y "$extra"
      ;;

    FreeBSD)
      common::sudo pkg update
      common::sudo pkg upgrade
      ;;

    *)
      common::error "Unknown platform $( uname )"
      ;;
  esac
}

wizard_update_apt() {

  wizard_update_platform "$@"
}


wizard_update_python() {

  common::optional-help "$1" "

  update all python packages installed by pip
  "
  common::check-network ||
    common::error "no network connection"

  python3 -m pip install --upgrade pip
  python3 -m pip freeze --user --local \
    | grep -v '^\-e' \
    | cut -d = -f 1  \
    | xargs -n1 python3 -m pip install --user
}


# this is where we can inject code into the generated functions
#
# shellcheck disable=SC2034,SC2154,SC2016
{
meta_head[wizard_make_file]='
common::required-help "$1" "[language] [file name]
$__usage
"
'
meta_head[wizard_make_project]='
common::required-help "$1" "[language] [project name]
$__usage
"
'

meta_head[wizard]='
common::required-help "$1" "(-q | -s | -e)
$__usage
"

case $OSTYPE in
  linux*)
    NUM_CPUS="$( getconf _NPROCESSORS_ONLN )"
    ;;
  *bsd*)
    NUM_CPUS="$( sysctl -n hw.ncpu )"
    ;;
esac

[[ $NUM_CPUS ]] || NUM_CPUS=4
'

meta_body[wizard]='
-q|--quiet)  QUIET=1  ;;
-s|--silent) SILENT=1 ;;
-e|--echo)   ECHO=1 ;;
'
}
