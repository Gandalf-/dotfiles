#!/bin/bash

for file in ../*; do

  [[ -f "$file" ]] && {
    echo "$file: "; shellcheck "$file" && echo "pass"
  }
done
