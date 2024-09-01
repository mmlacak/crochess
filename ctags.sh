#! /usr/bin/env bash

# Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

# rm -rfv tags
echo

echo "0 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
echo

ctags -R ws/*
echo

echo "z --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
echo

ls -Fal tags
echo
