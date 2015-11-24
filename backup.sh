#! /usr/bin/env bash

# Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


cd ..
rm -rf crochess.zip

echo
zip -rTv9 crochess.zip crochess/src crochess/*.txt crochess/*.sh -xcrochess/src/\*.o -xcrochess/src/\*.hi -xcrochess/src/crochess\*
echo

ls -Fal --color=auto 
echo
cd crochess

