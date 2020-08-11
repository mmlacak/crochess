#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class UNDEFINED(object):
    def __nonzero__(self):
        return False

UNDEFINED = UNDEFINED()


def just_count(itr, default=UNDEFINED, default_few=UNDEFINED, default_many=UNDEFINED, count=1, unpack_one=True):
    # Using UNDEFINEDs, so that defaults can be None.

    assert isinstance(count, int)
    assert isinstance(unpack_one, bool)

    lst = list(itr)
    _l = len(lst)

    if _l < count:
        few = default_few if default_few is not UNDEFINED else default

        if few is not UNDEFINED:
            return few
        else:
            raise ValueError("Can't extract value(s), list is empty.")

    if _l > count:
        many = default_many if default_many is not UNDEFINED else default

        if many is not UNDEFINED:
            return many
        else:
            raise ValueError("Can't extract value(s), list contains too many values, expected %d, got %d." % (count, _l))

    if count == 1 and unpack_one:
        return lst[ 0 ]
    else:
        return tuple(lst)


def xor(a, b, default=None):
    if a and not b:
        return a
    elif b and not a:
        return b
    else:
        return default


def in_range(v, min_v, max_v, include_min=True, include_max=True):
    assert isinstance(include_min, bool)
    assert isinstance(include_max, bool)

    assert type(min_v) == type(v) == type(max_v)
    assert min_v <= max_v

    if v < min_v:
        return False

    if v > max_v:
        return False

    if v == min_v:
        return include_min

    if v == max_v:
        return include_max

    return True


def convert_to_tuple(color_str):
    assert isinstance(color_str, str)
    assert len(color_str) == 7
    assert color_str[0] == '#'

    r_str, g_str, b_str = color_str[1:3], color_str[3:5], color_str[5:7]
    r, g, b = int(r_str, base=16), int(g_str, base=16), int(b_str, base=16)
    return ( r / 255.0, g / 255.0, b / 255.0 )


def test_1():

    def print_color_and_tuple(color_str):
        print
        print( color_str )
        print( convert_to_tuple( color_str ) )

    print_color_and_tuple( "#123456" )
    print_color_and_tuple( '#4080C0' )
    print_color_and_tuple( '#3F7FBF' )

    print_color_and_tuple( "#AABBCC" )
    print_color_and_tuple( '#DDEEFF' )

    print


if __name__ == '__main__':
    test_1()
