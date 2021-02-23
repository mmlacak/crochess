#! /usr/bin/env bash

# $ source options.sh

echo

export COMPILER=clang
# export COMPILER=gcc
echo "export COMPILER=${COMPILER}"

export OPTIONS="-Wall -pedantic -O3"
echo "export OPTIONS=${OPTIONS}"

echo
