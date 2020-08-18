#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2019 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class Corner(int):
    Position = 0
    UpperLeft = 1
    UpperRight = 2
    LowerLeft = 3
    LowerRight = 4

    def __new__(cls, value):
        if Corner._is_valid(value):
            return super(Corner, cls).__new__(cls, value)
        else:
            raise ValueError("No such a corner, received '%s'." % (str(value), ))

    @staticmethod
    def iter(do_construct=True):
        lst = [ Corner.Position, \
                Corner.UpperLeft, \
                Corner.UpperRight, \
                Corner.LowerLeft, \
                Corner.LowerRight ]

        return [ Corner(c) if do_construct else c for c in lst ]

    @staticmethod
    def _is_valid(corner):
        return corner in Corner.iter(do_construct=False)

    def is_position(self):
        return self == Corner.Position

    def is_left(self):
        return self in [Corner.LowerLeft, Corner.UpperLeft]

    def is_right(self):
        return self in [Corner.LowerRight, Corner.UpperRight]

    def is_upper(self):
        return self in [Corner.UpperLeft, Corner.UpperRight]

    def is_lower(self):
        return self in [Corner.LowerLeft, Corner.LowerRight]
