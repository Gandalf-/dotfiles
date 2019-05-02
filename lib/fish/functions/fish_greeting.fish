
function fish_greeting
  if ls --version >/dev/null 2>/dev/null
    # GNU ls
    ls --color=auto --ignore='*.pyc' --ignore='__pycache__'

  else
    # BSD ls
    ls -G
  end
end
