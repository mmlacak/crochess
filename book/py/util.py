#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


def xor(a, b, fail=None):
    if a and not b:
        return a
    elif b and not a:
        return b
    else:
        return fail
