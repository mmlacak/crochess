#! /usr/bin/env bash

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# Fiddling with current dir, because zip stores relative path into archive,
# which is fine, but includes "../", which I don't want.

cd .. # out of crochess folder

rm -rfv crochess.zip

echo
zip -rTv9 crochess.zip crochess -x *.* *. -i \*.h \*.c \*.txt \*LICENSE\* \*.md \*.sh \*.py \*.tex -x \*.DIFF.\*
echo

ls -Fal --color=auto *.zip
echo

cd crochess # back into crochess folder
