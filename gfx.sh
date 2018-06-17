#! /usr/bin/env bash

# Copyright (c) 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# Has to change dir, rendering is done in relative folder ("../gfx" in this instance).
cd book/py

echo

# To see all options, run "python main.py -h".

# Renders all images at final size (large!), slow rendering.
# python main.py -f -a

# Renders all images at normal size.
python main.py -n -a

# Renders all images at draft size.
# python main.py -d -a

echo

cd ../..
