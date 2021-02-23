#! /usr/bin/env bash


export COMPILER=clang
# export COMPILER=gcc

export OPTIONS="-Wall -pedantic -O3"

./clean.sh


echo
$COMPILER --version
echo




ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo
