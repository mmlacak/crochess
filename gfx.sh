#! /usr/bin/env bash

# Copyright (c) 2017, 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.


# Has to change dir, rendering is done in relative folder ("../gfx" in this instance).
cd book/py

echo

# To see all options, run "python3 main.py -h".

# Renders all images at final size (large!), slow rendering.
# python3 main.py -f -a

# Renders all images at normal size.
python3 main.py -n -a

# Renders all images at draft size.
# python3 main.py -d -a

echo

cd ../..
