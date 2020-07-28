#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

def p_(obj, msg=""):
    print()
    print( "-" * 42 )
    print( msg )
    print( hex(id(obj)), type(obj) )
    print( dir(obj) )
    print( obj )
    print( "-" * 42 )
    print()


if __name__ == '__main__':
    p_(1)
    p_("foo")
    p_(p_)

    class Foo:
        pass

    foo = Foo()
    p_(foo)
