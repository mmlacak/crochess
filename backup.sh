#! /usr/bin/env bash

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


rm -rfv ../crochess.zip

echo
zip -RTv9 ../crochess.zip *.hs *.txt *.sh *.py *.tex
echo

ls -Fal --color=auto ../*.zip
echo
