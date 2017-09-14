#!/bin/bash

DEBUG=${DEBUG:-0}
START=$(date +%N)

typeset -F SECONDS
declare -A meta_head meta_body

_profile() {
  (( "$DEBUG" )) && {
    echo "$@"
    now=$(date +%N)
    bc -l <<< "$now - $START"
    echo
    START=$now
  }
}

_return() {
  return "$1"
}

_debug() {
  if (( "$DEBUG" )); then
    eval "$*"
  fi
}

make_reflective_functions() {
  #
  #

  _profile "start"
  local commands len base function_body
  declare -A meta_functions
  functions=()

  # read in all functions declared thus far, split them on '_'
  while read -r func; do
    functions+=( "${func//_/ }" )
  done < <(declare -F -p | cut -d ' ' -f 3)
  _profile "functions read"

  # for each defined function, determine the base and assign sub functions to
  # meta functions
  for ((i=0; i < ${#functions[@]}; i++)); do
    commands=( ${functions[$i]} )
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
  _profile "meta functions discovered"

  _debug declare -p  meta_functions
  _debug echo "keys ${!meta_functions[@]}"

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
        ${sub_func:0:1}|$sub_func) ${meta_func}_$sub_func \"\${@:2}\";; "

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
        echo $(tr '_' ' ' <<< "$meta_func")
        echo \"$auto_usage\"
      fi
      ;;
    "
    _profile "$meta_func defined"

    eval "
    $meta_func() {
      [[ \$1 ]] || \$FUNCNAME --help
      local __shifts=0
      ${meta_head[$meta_func]}

      while [[ \$1 ]]; do
        case \$1 in
          ${meta_body[$meta_func]}
          $function_body
        esac

        ret=\$?; shift; shift \$ret; let __shifts+=\$ret+1
      done
      return \$__shifts
    }
    "
    _profile "$meta_func defined"
  done
  _profile "all meta functions defined"

  _debug declare -f
}
