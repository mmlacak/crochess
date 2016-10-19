#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

from piece import PieceType
from board import BoardType
from board import Board
from mark import Arrow, Text, FieldMarker

import pixel_math as pm


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

def get_new_arrow(start_i, start_j, end_i, end_j, fg_color=None, bg_color=None):
    coords = get_arrow_coordinates(start_i, start_j, end_i, end_j)
    return Arrow(*coords, fg_color=fg_color, bg_color=bg_color)


def get_func_get_colors(fg_ok, bg_ok, fg_not_ok, bg_not_ok, font=None):
    def get_colors(is_ok):
        dct = { 'fg_color': fg_ok if is_ok else fg_not_ok, \
                'bg_color': bg_ok if is_ok else bg_not_ok }
        if font is not None:
            dct.update( { 'font': font } )
        return dct
    return get_colors


def get_nominal_font_size(board_size):
    return int(1920 // board_size)

def get_log_font_size(board_size):
    factor = math.log(10.0, 2.0)
    div = math.log(board_size, 2.0)
    return int(factor * 192 / div)

def get_sqrt_font_size(board_size):
    factor = math.sqrt(10.0)
    div = math.sqrt(board_size)
    return int(factor * 192 / div)


class Corner(int):
    # none = 0
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
    def foreach(start=None, end=None, step=1):
        # start = start or Corner.none
        start = start or Corner.UpperLeft
        end = end or Corner.LowerRight

        for cp in xrange(start, end+1, step):
            # Added +1 because upper limit is not included in loop.
            yield Corner(cp)

    @staticmethod
    def _is_valid(corner):
        # return (Corner.none) <= corner <= Corner.LowerRight
        return (Corner.UpperLeft) <= corner <= Corner.LowerRight

    def is_valid(self):
        return Corner._is_valid(self)

    # def is_none(self):
    #     return self == Corner.none

    def is_left(self):
        return self in [Corner.LowerLeft, Corner.UpperLeft]

    def is_right(self):
        return self in [Corner.LowerRight, Corner.UpperRight]

    def is_upper(self):
        return self in [Corner.UpperLeft, Corner.UpperRight]

    def is_lower(self):
        return self in [Corner.LowerLeft, Corner.LowerRight]

def get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05):
    def get_text_position(pos_i, pos_j, corner):
        crnr = Corner(corner)
        x = left if crnr.is_left() else right
        y = top if crnr.is_upper() else bottom
        return (pos_i + x, pos_j + y)
    return get_text_position

def get_new_text(text, pos_x, pos_y, font=None, fg_color=None, bg_color=None):
    return Text(text, pos_x, pos_y, font=font, fg_color=fg_color, bg_color=bg_color)


def get_new_field_marker(pos_i, pos_j, fg_color=None, bg_color=None):
    return FieldMarker(pos_i, pos_j, fg_color=fg_color, bg_color=bg_color)
