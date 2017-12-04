#!/bin/env bash

wizard_quick_shell() {

  common::optional_help "$1" "

  open a throw away shell file
  "
  common::do cd /tmp/
  wizard_make_file_shell quick
  vim quick.sh
}


wizard_quick_python() {

  common::optional_help "$1" "

  open a throw away python file
  "
  common::do cd /tmp/
  wizard_make_file_python quick
  vim quick.py
  common::do cd -
}


wizard_quick_c()
{
  common::optional_help "$1" "

  open a throw away c file
  "
  common::do cd /tmp/
  wizard_make_file_c quick
  vim quick.c
  common::do cd -
}
