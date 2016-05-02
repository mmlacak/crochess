#! /usr/bin/env bash

# Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


rm -rf src/*.hi
rm -rf src/*.o
rm -rf src/crochess*

rm -rf book/py/*.pyc
rm -rf book/py/*.pyo

# ghc -O3 -threaded Main.hs -o crochess

ls -Fal --color=auto src/
