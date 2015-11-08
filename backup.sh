#! /usr/bin/env bash

# Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
# See accompanying LICENSE.txt for details.


cd ..
rm -rf crochess.zip

echo
zip -rTv9 crochess.zip crochess/src -xcrochess/src/\*.o -xcrochess/src/\*.hi -xcrochess/src/crochess\*
echo

ls -Fal --color=auto 
echo
cd crochess

