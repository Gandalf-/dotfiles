# vim: set syntax=bash

function f
  # f [azlct] [target]
  #
  # f       -> cd ~
  # f file  -> vim file
  # f dir   -> cd dir
  # f name  -> autojump name

  if test -z "$argv"; cd; return; end
  
  set files
  set fuzzy
  set grepper
  set has_flag
  set locate
  set move
  set notimeout
  set vim_opt

  # check for flags if we have enough arguments
  if test (count $argv) -ge 2
    if not string match -q -r '[^azlct-]' $argv[1]
      if string match -q -r '.*z.*' $argv[1]; set fuzzy     "yes"; end
      if string match -q -r '.*l.*' $argv[1]; set locate    "yes"; end
      if string match -q -r '.*c.*' $argv[1]; set move      "yes"; end
      if string match -q -r '.*t.*' $argv[1]; set notimeout "yes"; end
      if string match -q -r '.*t.*' $argv[1]; set notimeout "yes"; end
      if string match -q -r '.*a.*' $argv[1]; set grepper   "yes"; end
      if string match -q    '-'     $argv[1]; cd -; return; end

      set --erase argv[1]
      set has_flag "yes"
    end
  end

  for arg in $argv
    if test (string sub -l 1 "$arg") = '+'                  # vim option
      set vim_opt "$arg"

    else if test -f "$arg"                                  # file
      set files $arg $files

    else if test -d "$arg"                                  # directory
      cd "$arg"; ls --color=auto

    else if not test "$has_flag"; and j "$arg" ^/dev/null   # autojump
      true

    else                                                    # search
      set search

      if test "$grepper"                                      # contents
        set cmd "ag"
        set flags "-l"

        test "$fuzzy";     and set flags "$flags" -i
        test "$notimeout"; and set cmd   "timeout 1 $cmd"

        set search (eval $cmd $flags $arg)
        test "$search"; and not test "$fuzzy"; and set vim_opt "+/$arg"

      else                                                    # name
        set cmd "find . "
        set flags "-name $arg -not -path './.*'"

        test "$fuzzy";     and set flags "-iname '*'$arg'*' -not -path './.*'"
        test "$notimeout"; and set cmd "timeout 1 $cmd"

        set search (eval $cmd $flags)
      end

      if test "$search"
        for new_file in $search
          set files "$new_file" $files
        end
      end

    end
  end
  
  # open, print accrued files, if any
  if test "$files"
    if test "$locate"
      for file in $files; echo $file; end

    else if test "$move"
      cd (dirname $files[1])
      ls --color=auto

    else
      v $files $vim_opt
    end
  end
end
