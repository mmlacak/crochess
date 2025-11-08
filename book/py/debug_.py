#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016, 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

def p_( obj, msg="" ):
    print
    print( "-" * 42 )
    print( msg )
    print( hex(id(obj)), type(obj) )
    print( dir(obj) )
    print( obj )
    print( "-" * 42 )
    print
