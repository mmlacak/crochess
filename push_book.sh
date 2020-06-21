#! /usr/bin/env bash

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

# Do not escape double quotes (") and apostrophes (') around commit message, file paths:
#
# $ ./push_book.sh -m "Commented that escaped double quotes do not work properly." -- "push_book.sh" "book/crochess.tex"

# If not using -a option:
# 1. check if files are added to the stage (git add), not just modified
# 2. add "book/crochess.tex" to the list of files to commit

python2 book/py/book_log.py

git commit "${*}"

echo

git push
