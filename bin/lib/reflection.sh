#!/bin/bash

set -o pipefail

DEBUG=${DEBUG:-0}

declare -A meta_head meta_body

_return() {
  return "$1"
}

_debug() {
  (( "$DEBUG" )) \
    && eval "$*"
}

make_reflective_functions() {
  #
  #

  local sub_commands commands len base i j
  local meta_functions auto_usage function_body

  declare -A meta_functions
  sub_commands=()

  # read in all functions declared thus far, split them on '_'
  while read -r func; do
    sub_commands+=( "${func//_/ }" )
  done \
    < <(declare -F -p | cut -d ' ' -f 3)

  # for each defined function, determine the base and assign sub functions to
  # meta functions
  for ((i=0; i < ${#sub_commands[@]}; i++)); do
    commands=( ${sub_commands[$i]} )
    len=${#commands[@]}
    base=${commands[0]}

    for ((j=1; j < len; j++)); do
      _debug echo "assign meta_functions[$base] += ${commands[$j]}"

      if ! [[ ${meta_functions[$base]} =~ .*${commands[$j]}.* ]]; then
        meta_functions[$base]+="${commands[$j]} "
      fi

      base+="_${commands[$j]}"
    done
  done

  _debug declare -p  meta_functions
  _debug echo "keys ${!meta_functions[*]}"

  existing_functions=( $(declare -F | cut -d ' ' -f 3) )

  # define each meta function
  for meta_func in "${!meta_functions[@]}"; do
    _debug echo "meta_func: $meta_func, ${meta_functions[$meta_func]}"

    if [[ ${existing_functions[*]} =~ .*$meta_func\ .* ]]; then
      echo "looks like $meta_func already exists! check your definitions"
      exit 1
    fi

    auto_usage=""
    function_body=""
    for sub_func in ${meta_functions[$meta_func]}; do
      function_body+="
        ${sub_func:0:1}|${sub_func:0:2}|$sub_func)
          ${meta_func}_$sub_func \"\${@:2}\";; "

      if [[ ${meta_functions[${meta_func}_$sub_func]} ]]; then
        auto_usage+="
  $sub_func ..."
      else
        auto_usage+="
  $sub_func"
      fi
    done
    function_body+="*)
      if [[ \$usage ]]; then
        echo \$usage
      else
        echo
        echo $(tr '_' ' ' <<< "$meta_func")
        echo \"$auto_usage\"
        echo
      fi
      ;;
    "

    eval "
    $meta_func() {
      [[ \$1 ]] || \$FUNCNAME --help
      local __ret __shifts=0
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

  _debug declare -f
}
