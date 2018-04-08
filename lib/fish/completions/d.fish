function __apocrypha
  set cmd (string sub -s 2 (commandline))
  d (string split ' ' $cmd) --keys
end

complete -f -c d -a '(__apocrypha)' 
