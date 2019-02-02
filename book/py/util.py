#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2019 Mario Mlačak, mmlacak@gmail.com
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
