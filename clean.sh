#! /usr/bin/env bash

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


rm -rfv src/*.hi
rm -rfv src/*.o
rm -rfv src/crochess*

rm -rfv book/py/*.pyc
rm -rfv book/py/*.pyo

rm -rfv book/tmp/*.png
rm -rfv book/tmp/*/*.png

# ghc -O3 -threaded Main.hs -o crochess

ls -Fal --color=auto src/

