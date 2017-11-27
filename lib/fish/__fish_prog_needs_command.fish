
function __fish_prog_needs_command
  set cmd (commandline -opc)

  if [ (count $cmd) -eq 1 ]
    return 0
  end

  return 1
end

