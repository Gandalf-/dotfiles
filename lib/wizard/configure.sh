#!/bin/env bash

# configure - wizard libary


wizard_configure_keyboard_swap-caps-escape() {

  common::optional-help "$1" "

  swap caps and escape, doesn't make sense on a Chromebook
  "
  common::require -f setxkbmap
  common::do setxkbmap -option caps:swapescape
}


wizard_configure_passwordless-sudo() {

  common::echo "Add this to /etc/sudoers"
  echo "$(whoami) ALL=(ALL) NOPASSWD:ALL"
}


wizard_configure_haskell_stack-links() {

  common::optional-help "$1" "

  create 'ghc' and 'ghci' scripts that link to the stack equivalents
  "

  mkdir -p ~/.local/bin/

cat << EOF > ~/.local/bin/ghc
#!/bin/sh

stack ghc -- "\$@"
EOF

cat << EOF > ~/.local/bin/ghci
#!/bin/sh

stack ghci -- "\$@"
EOF

  chmod +x ~/.local/bin/ghc
  chmod +x ~/.local/bin/ghci
}


wizard_configure_haskell_vim-depends() {

  common::optional-help "$1" "

  install Vim related Haskell dependencies
  "

  common::require -f stack
  common::do stack install \
    hlint \
    apply-refact \
    stylish-haskell
}


wizard_configure_wiki() {

  common::optional-help "$1" "

  create database entries for all wiki pages
  "
  [[ -d ~/wiki ]] || common::error '~/wiki not found'

  while read -r wikipage; do
    path="$( basename "$wikipage" )"
    path="${path//.md}"

    d "$path" wiki = "$wikipage"

  done < <( find -L ~/wiki/ -mindepth 1 -maxdepth 1 -type f )
}
