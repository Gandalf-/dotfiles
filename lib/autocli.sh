#!/bin/bash

# auto_cli
#   generates intermediate functions for cli scripts
#   see bin/w and lib/wizard.sh for example usage

# shellcheck disable=SC1090,SC1091

set -o pipefail


autocli::create() {
  # location of generated file, source, source ... -> none
  #
  # regenerate output file if the sources have changed

  local root name location sources output
  root="$(dirname "${BASH_SOURCE[0]}")"/..
  source "${root}/lib/common.sh"

  name="$1"
  location="$2"
  sources=( "${@:3}" )
  output="${location}/${name}"

  declare -A meta_head meta_body    # these are accessible to the caller

  # pull in all the sources since we need them now
  for source in "${sources[@]}"; do
    [[ -f "$source" ]] && source "$source"
  done

  autocli::make_reflective_functions

  {
    echo '#!/bin/bash
# this is an auto generated file. do not edit manually
    '
    declare -f -p

    echo "
$name \"\$@\"
true
"
  } > "$output"

  chmod +x "$output"
}


autocli::make_reflective_functions() {
  # none -> none
  #

  declare -A __meta_functions
  local i j sub_commands=()

  # read in all functions declared thus far, split them on '_'
  while read -r func; do
    sub_commands+=( "${func//_/ }" )
  done < <(declare -F -p | cut -d ' ' -f 3)

  # for each defined function, determine the base and assign sub functions to
  # meta functions
  for ((i=0; i < ${#sub_commands[@]}; i++)); do
    local commands=( ${sub_commands[$i]} )
    local len=${#commands[@]}
    local base=${commands[0]}

    for ((j=1; j < len; j++)); do
      common::debug echo "assign __meta_functions[$base] += ${commands[$j]}"

      if ! [[ ${__meta_functions[$base]} =~ .*${commands[$j]}.* ]]; then
        __meta_functions[$base]+="${commands[$j]} "
      fi

      base+="_${commands[$j]}"
    done
  done

  common::debug declare -p __meta_functions
  common::debug echo "keys ${!__meta_functions[*]}"

  local existing_functions
  existing_functions=( $(declare -F | cut -d ' ' -f 3) )

  # define each meta function
  for meta_func in "${!__meta_functions[@]}"; do
    common::debug echo "meta_func: $meta_func, ${__meta_functions[$meta_func]}"

    if [[ ${existing_functions[*]} =~ .*$meta_func\ .* ]]; then
      echo "looks like $meta_func already exists! check your definitions"
      exit 1
    fi

    local auto_name; auto_name="$(tr '_' ' ' <<< "$meta_func")"
    local auto_usage=""
    local function_body=""

    for sub_func in ${__meta_functions[$meta_func]}; do

      # allow all sub strings of function name
      local cases="\"${sub_func:0:1}\""
      for ((i=2; i < $(( ${#sub_func} + 1)); i++)); do
        cases+="|\"${sub_func:0:$i}\""
      done

      function_body+="
        $cases)
          ${meta_func}_$sub_func \"\${@:2}\";; "

      if [[ ${__meta_functions[${meta_func}_$sub_func]} ]]; then
        auto_usage+="
  $sub_func ..."
      else
        auto_usage+="
  $sub_func"
      fi
    done
    function_body+="*)
      if [[ \$usage ]]; then
        echo \"\$usage\"
        exit 0
      else
        echo
        echo \"\$__name\"
        echo \"\$__usage\"
        echo
        exit 0
      fi
      ;;
    "

    eval "
    $meta_func() {
      [[ \$1 ]] || \"\${FUNCNAME[0]}\" --help
      local __ret __shifts=0
      local __usage=\"$auto_usage\"
      local __name=\"${auto_name}\"
      ${meta_head[$meta_func]}

      while [[ \$1 ]]; do
        case \$1 in
          ${meta_body[$meta_func]}
          $function_body
        esac

        __ret=\$?; shift; shift \$__ret; let __shifts+=\$__ret+1
      done
      return \$__shifts
    }
    "
  done

  common::debug declare -f
}
