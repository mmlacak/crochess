#! /usr/bin/env bash

# Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


cd src

# rm -rf *.hi
# rm -rf *.o
# rm -rf crochess*

ghc -O3 -threaded Main.hs -o crochess

ls -Fal --color=auto

cd ..

