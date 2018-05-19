#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from mark import MarkType, Arrow, Text, FieldMarker

import pixel_math as pm


#
# arrow

def get_coord_offset(coord, offset=0.5):
    return float(coord + offset) if isinstance(coord, int) else float(coord)

def get_arrow_coordinates(start_i, start_j, end_i, end_j):
    starts_are_ints = bool(isinstance(start_i, int) and isinstance(start_j, int))
    starts_are_floats = bool(isinstance(start_i, float) and isinstance(start_j, float))
    ends_are_ints = bool(isinstance(end_i, int) and isinstance(end_j, int))
    ends_are_floats = bool((isinstance(end_i, float) and isinstance(end_j, float)))

    assert starts_are_ints or starts_are_floats, \
           "Unexpected types for starting i and j (or, x and y), found ('%s', '%s'), expected both to be either ints or floats." % (type(start_i), type(start_j))

    assert ends_are_ints or ends_are_floats, \
           "Unexpected types for ending i and j (or, x and y), found ('%s', '%s'), expected both to be either ints or floats." % (type(end_i), type(end_j))

    diff_i = end_i - start_i
    diff_j = end_j - start_j

    start_x_off = start_x = get_coord_offset(start_i)
    start_y_off = start_y = get_coord_offset(start_j)
    end_x_off = end_x = get_coord_offset(end_i)
    end_y_off = end_y = get_coord_offset(end_j)

    offset_x = 0.9 if diff_i > 0.0 else 0.1
    offset_y = 0.9 if diff_j > 0.0 else 0.1

    if pm.q_same_rounded_floats(end_x, start_x):
        start_y_off = get_coord_offset(start_j, offset=offset_y)
        end_y_off = get_coord_offset(end_j, offset=(1.0 - offset_y))
    elif pm.q_same_rounded_floats(end_y, start_y):
        start_x_off = get_coord_offset(start_i, offset=offset_x)
        end_x_off = get_coord_offset(end_i, offset=(1.0 - offset_x))
    else:
        a, b = pm.calc_straight_line((start_x, start_y), (end_x, end_y))

        is_x_crossed = bool( abs(diff_i) > abs(diff_j) )

        if is_x_crossed:
            start_x_off = get_coord_offset(start_i, offset=offset_x)
            end_x_off = get_coord_offset(end_i, offset=(1.0 - offset_x))

            start_y_off = a * start_x_off + b
            end_y_off = a * end_x_off + b
        else:
            start_y_off = get_coord_offset(start_j, offset=offset_y)
            end_y_off = get_coord_offset(end_j, offset=(1.0 - offset_y))

            start_x_off = (start_y_off - b) / a
            end_x_off = (end_y_off - b) / a

    if starts_are_floats:
        start_x_off = start_i
        start_y_off = start_j

    if ends_are_floats:
        end_x_off = end_i
        end_y_off = end_j

    return [start_x_off, start_y_off, end_x_off, end_y_off]

def get_new_arrow(start_i, start_j, end_i, end_j, mark_type=MarkType(MarkType.Legal), \
                  start_pointer=False, \
                  end_pointer=True):
    coords = get_arrow_coordinates(start_i, start_j, end_i, end_j)
    return Arrow(*coords, mark_type=mark_type, start_pointer=start_pointer, end_pointer=end_pointer)

#
# text

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

    def __iter__(self):
        for cr in [ Corner.Position, \
                    Corner.UpperLeft, \
                    Corner.UpperRight, \
                    Corner.LowerLeft, \
                    Corner.LowerRight ]:
            yield Corner(cr)

    @staticmethod
    def _is_valid(corner):
        return (Corner.Position) <= corner <= Corner.LowerRight

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

def get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45):
    def get_text_position(pos_i, pos_j, corner):
        crnr = Corner(corner)

        if crnr.is_position():
            return (float(pos_i), float(pos_j))

        x = left if crnr.is_left() else right
        y = top if crnr.is_upper() else bottom

        return (float(pos_i + x), float(pos_j + y))

    return get_text_position


