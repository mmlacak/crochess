#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016, 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

def p_(obj, msg=""):
    print
    print( "-" * 42 )
    print( msg )
    print( hex(id(obj)), type(obj) )
    print( dir(obj) )
    print( obj )
    print( "-" * 42 )
    print
