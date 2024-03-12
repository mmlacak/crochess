#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2019 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


class Corner(int):
    Position = 0

    UpperLeft = 1
    UpperRight = 2
    LowerLeft = 3
    LowerRight = 4

    UpperLeftFieldMarker = 5
    UpperRightFieldMarker = 6
    LowerLeftFieldMarker = 7
    LowerRightFieldMarker = 8

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
                Corner.LowerRight, \
                Corner.UpperLeftFieldMarker, \
                Corner.UpperRightFieldMarker, \
                Corner.LowerLeftFieldMarker, \
                Corner.LowerRightFieldMarker ]

        return [ Corner(c) if do_construct else c for c in lst ]

    @staticmethod
    def _is_valid(corner):
        return corner in Corner.iter(do_construct=False)

    def is_position(self):
        return self == Corner.Position

    def is_with_field_marker(self):
        return self in [Corner.UpperLeftFieldMarker, Corner.UpperRightFieldMarker, Corner.LowerLeftFieldMarker, Corner.LowerRightFieldMarker]

    def is_left(self):
        return self in [Corner.LowerLeft, Corner.UpperLeft, Corner.LowerLeftFieldMarker, Corner.UpperLeftFieldMarker]

    def is_right(self):
        return self in [Corner.LowerRight, Corner.UpperRight, Corner.LowerRightFieldMarker, Corner.UpperRightFieldMarker]

    def is_upper(self):
        return self in [Corner.UpperLeft, Corner.UpperRight, Corner.UpperLeftFieldMarker, Corner.UpperRightFieldMarker]

    def is_lower(self):
        return self in [Corner.LowerLeft, Corner.LowerRight, Corner.LowerLeftFieldMarker, Corner.LowerRightFieldMarker]

    def is_upper_left(self):
        return self in [Corner.UpperLeft, Corner.UpperLeftFieldMarker]

    def is_upper_right(self):
        return self in [Corner.UpperRight, Corner.UpperRightFieldMarker]

    def is_lower_left(self):
        return self in [Corner.LowerLeft, Corner.LowerLeftFieldMarker]

    def is_lower_right(self):
        return self in [Corner.LowerRight, Corner.LowerRightFieldMarker]
