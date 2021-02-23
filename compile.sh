#! /usr/bin/env bash


export COMPILER=clang # gcc

./clean.sh


echo
$COMPILER --version
echo




ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo

