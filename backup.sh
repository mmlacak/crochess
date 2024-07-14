#! /usr/bin/env bash

# Copyright (c) 2010 - 2019 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# Fiddling with current dir, because zip stores relative path into archive,
# which is fine, but includes "../", which I don't want.

cd .. # out of crochess folder

rm -rfv crochess.zip

echo
zip -rTv9 crochess.zip crochess -x *.* *. -i \*.txt \*LICENSING\* \*COPYING\* \*CODE_OF_CONDUCT\* \*CONTRIBUTING\* \*.rst \*.md \*.sh \*.py \*.tex \*.c \*.h \*.ico \*Doxyfile\* \*.geany -x \*tmp\* \*temp\* \*.github\* \*.DIFF.\* \*bin\* crochess/ws/docs/\* crochess/docs/build/\*
echo

ls -Fal --color=auto *.zip
echo

cd crochess # back into crochess folder
