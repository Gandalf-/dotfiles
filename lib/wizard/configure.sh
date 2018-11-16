#!/bin/env bash

# configure - wizard libary


wizard_configure_passwordless-sudo() {

  common::echo "Add this to /etc/sudoers"
  echo "$(whomai) ALL=(ALL) NOPASSWD:ALL"
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
