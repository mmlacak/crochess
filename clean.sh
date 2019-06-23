#! /usr/bin/env bash

# Copyright (c) 2010 - 2019 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


if [ ! -e obj ]; then
    mkdir obj
fi

if [ ! -e lib ]; then
    mkdir lib
fi

if [ ! -e bin ]; then
    mkdir bin
fi


rm -rfv bin/*
rm -rfv lib/*
rm -rfv obj/*

