#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

def p_(obj, msg=""):
    print
    print "-" * 42
    print msg
    print hex(id(obj)), type(obj)
    print dir(obj)
    print obj
    print "-" * 42
    print
