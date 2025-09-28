#! /usr/bin/env bash

# Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

cd ws

cp -uv linenoise/linenoise.h crochess/inc/linenoise.h

cp -uv linenoise/linenoise.h tests/inc/linenoise.h

cp -uv linenoise/linenoise.c crochess/src/linenoise.c

cp -uv linenoise/linenoise.c tests/src/linenoise.c

cd ..
