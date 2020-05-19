#! /usr/bin/env bash

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

python2 book/py/book_log.py

git commit "${*}"

echo

git push
