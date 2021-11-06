#!/usr/bin/env bash

# auto_cli
#   generates intermediate functions for cli scripts
#   see bin/w and lib/wizard.sh for example usage

# shellcheck disable=SC1090,SC1091

set -o pipefail

declare -A meta_head meta_body meta_default meta_locked   # these are accessible to the caller
sources=''
inline=''

autocli::debug() { (( DEBUG )) && eval "$*"; }

autocli::create() {
  # location of generated file, source, source ... -> none
  #
  # regenerate output file if the sources have changed

  local root; root="$(dirname "${BASH_SOURCE[0]}")"/..
  source "${root}/lib/common.sh" 2>/dev/null

  local name="$1"
  local location="$2"
  local output="${location}/${name}"

  # source inline files
  for file in "${inline[@]}"; do
    [[ -e "$file" ]] && source "$file"
  done

  # pull in all the sources since we need them now
  if [[ ${sources[*]} ]]; then
    for source in "${sources[@]}"; do
      if [[ -f "$source" ]]; then
        source "$source"
      else
        echo "warn: $source not found"
      fi
    done
  fi

  # generate all the intermediary functions
  autocli::make-reflective-functions

  local tmp;
  case $OSTYPE in
    *bsd*)
      tmp="$( mktemp /tmp/wizard-write.XXXXXXXX )"
      ;;
    *)
      tmp="$( mktemp -p /tmp wizard-write.XXXXXXXX )"
      ;;
  esac

  # write the output script
  {
    echo '#!/usr/bin/env bash
# this is an auto generated file. do not edit manually
{
    '

    # write out inline files
    for file in "${inline[@]}"; do
      [[ -e "$file" ]] && cat "$file"
    done

    # write out all output functions
    while read -r fname; do
      case $fname in
        'autocli::'*)
          ;;
        *)
          declare -f -p "$fname"
          ;;
      esac
    done < <(declare -F | awk '{print $3}')

    echo "
[[ \"\${BASH_SOURCE[0]}\" == \"\${0}\" ]] && $name \"\$@\"
} || exit
"
  } > "$tmp"

  mv "$tmp" "$output"

  # make the output script executable
  chmod +x "$output"
}


autocli::make-reflective-functions() {
  # none -> none
  #
  # inspects all the bash function defined thusfar and attempts to build the
  # higher level functions that are missing. commands are seperated by
  # underscores. for instance, suppose that only the following functions are
  # defined and do something interesting:
  #
  #   chef_bake_cake()
  #   chef_bake_muffin()
  #   chef_cleanup()
  #
  # autocli will define chef() and chef_bake() functions for you. they'll look
  # something like the below. All substrings are valid.
  #
  # chef() {
  #   case $1 in
  #     b | ba | bak | bake)
  #       chef_bake "$@"
  #       ;;
  #     c | cl | cle | clea | clean)
  #       chef_clean "$@"
  #       ;;
  #     *)
  #       echo "$usage" ;;
  #   esac
  # }
  #
  # in your output script, you can then call chef "$@" and it'll handle
  # everything. you can then call the output script like:
  #
  #   chef_script bake cake
  #   chef_script bake --help
  #
  # it's an error to define chef() yourself when calling autocli.
  # You can inject code into the generated functions through the meta_head and
  # meta_body variables.
  #
  # meta_head allows you to do things before the case statement
  # meta_body allows you to add custom cases to the case statement
  #
  # example:
  #   meta_head[chef]='
  #   local done_cooking=0
  #   '
  #   meta_body[chef]='
  #   done-cooking) echo "$done_cooking" ;;
  #   '
  #
  # chef() {
  #   done_cooking=0
  #
  #   case $1 in
  #     done-cooking)
  #       echo "$done_cooking"
  #       ;;
  #     b | ba | bak | bake)
  #       chef_bake "$@"
  #       ;;
  #     c | cl | cle | clea | clean)
  #       chef_clean "$@"
  #       ;;
  #     *)
  #       echo "$usage" ;;
  #   esac
  # }

  declare -A __meta_functions
  local i j sub_commands=()

  # read in all functions declared thus far, split them on '_'
  while read -r func; do
    sub_commands+=( "${func//_/ }" )

  done < <(declare -F -p | awk '{print $3}')

  # for each defined function, determine the base and assign sub functions to
  # meta functions
  for ((i=0; i < ${#sub_commands[@]}; i++)); do

    # shellcheck disable=SC2206
    local commands=( ${sub_commands[$i]} )
    local len=${#commands[@]}
    local base=${commands[0]}

    for ((j=1; j < len; j++)); do
      autocli::debug echo "assign __meta_functions[$base] += ${commands[$j]}"

      if ! [[ ${__meta_functions[$base]} =~ .*${commands[$j]}.* ]]; then
        __meta_functions[$base]+="${commands[$j]} "
      fi

      base+="_${commands[$j]}"
    done
  done

  autocli::debug declare -p __meta_functions
  autocli::debug echo "keys ${!__meta_functions[*]}"

  local existing_functions
  # shellcheck disable=SC2207
  existing_functions=( $(declare -F | awk '{print $3}') )

  # define each meta function
  for meta_func in "${!__meta_functions[@]}"; do

    autocli::debug echo "meta_func: $meta_func, ${__meta_functions[$meta_func]}"

    if [[ ${existing_functions[*]} =~ .*$meta_func\ .* ]]; then
      echo "looks like $meta_func already exists! check your definitions"
      exit 1
    fi

    local auto_name="${meta_func//_/ }"
    local auto_usage=""
    local function_body=""

    for sub_func in ${__meta_functions[$meta_func]}; do

      # allow all sub strings of function name
      local cases="\"${sub_func:0:1}\""
      for ((i=2; i < $(( ${#sub_func} + 1)); i++)); do
        cases+="|\"${sub_func:0:$i}\""
      done

      # convert abc-def-ghi to adg if dashes are present
      if grep -q '-' <<< "${sub_func[@]}"; then
        cases+="|\"$(
          tr '-' '\n' <<< "${sub_func[@]}" \
            | cut -c 1 \
            | xargs \
            | tr -d ' '
          )\""
      fi

      local sub_name="${meta_func}_${sub_func}"

      if (( meta_locked["$sub_name"] )) && common::program-exists flock; then
        # user has says this function should run with a lock
        function_body+="
          $cases)
            (
              flock -n 111 || {
                echo \"${sub_name//_/ } is already running.\"
                exit 1
              }
              $sub_name \"\${@:2}\"
            ) 111> \"/tmp/$sub_name.lock\"
            true
            ;;
          "
      else
        function_body+="
          $cases)
            $sub_name \"\${@:2}\"
            exit \$?
            ;;
        "
      fi

      if [[ ${__meta_functions[$sub_name]} ]]; then
        auto_usage+="
  $sub_func ..."
      else
        auto_usage+="
  $sub_func"
      fi
    done
    function_body+="
    __list)
      echo ${__meta_functions[$meta_func]}
      ;;
    *)
      if [[ \$usage ]]; then
        echo \"\$usage\"
        exit 0
      else
        printf '\\n%s\\n%s\\n\\n' \
          \"\$__name\" \
          \"\$__usage\"
        exit 0
      fi
      ;;
    "

    if [[ ${meta_default[$meta_func]} ]]; then
      alt="${meta_default[$meta_func]}"
    else
      # shellcheck disable=SC2016
      alt='"${FUNCNAME[0]}" --help'
    fi

    eval "
    $meta_func() {
      [[ \$1 ]] || $alt
      local \
        __usage=\"$auto_usage\" \
        __name=\"${auto_name}\"

      ${meta_head[$meta_func]}

      while [[ -n \$1 ]]; do
        case \$1 in
          ${meta_body[$meta_func]}
          $function_body
        esac
        shift
      done
    }
    "
  done

  autocli::debug declare -f
}
