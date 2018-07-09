#!/bin/bash

wizard_here_wiki() {

  common::optional-help "$1" "

  attempt to open the vimwiki entry for this context
  "

  local wiki; wiki="$(d "$WINDOW" wiki)"
  [[ $wiki ]] || wiki="$(d "$SESSION" wiki)"
  [[ $wiki ]] || common::error "no wiki for current context"

  vim "$wiki"
}
