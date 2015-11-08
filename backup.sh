#! /usr/bin/env bash

# Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
# All rights reserved. See accompanying LICENSE.txt for details.


cd ..
rm -rf crochess2.zip

echo
zip -rTv9 crochess2.zip crochess2/src -xcrochess2/src/\*.o -xcrochess2/src/\*.hi -xcrochess2/src/crochess\*
echo

ls -Fal --color=auto 
echo
cd crochess2

