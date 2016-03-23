#!/bin/bash

# globals
FAIL=1
PASS=0

# setup
mkdir /tmp/qcrypt_test
cp ../qcrypt /tmp/qcrypt_test
cd /tmp/qcrypt_test
chmod +x qcrypt

# cleanup
function cleanup() {
  rm -rf /tmp/qcrypt_test
  exit
}

# get result of test
function tresult() {
  if test $? == $1 ; then
    echo "PASSED"
  else
    echo "FAILED"
  fi
  echo
}

echo encrypt while not in working directory
mkdir dir
touch dir/file
./qcrypt -e dir/file >/dev/null
tresult $FAIL

cleanup
