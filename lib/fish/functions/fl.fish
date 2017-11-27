# vim: set syntax=bash

function fl
  if      echo (pwd) | grep -q "onefs"; cd (echo (pwd) | sed -e "s/onefs/twofs/")
  else if echo (pwd) | grep -q "twofs"; cd (echo (pwd) | sed -e "s/twofs/onefs/")
  end
end
