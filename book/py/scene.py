#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

import pixel_math as pm
from coords import Pos, RectPos
from board import BoardType, Board
from board_desc import BoardDesc
from mark import MarkType, Arrow, Text, FieldMarker


def get_coord_offset(coord, offset=0.5):
    return float(coord + offset) if isinstance(coord, int) else float(coord)

def recalc_arrow_ends(start_i, start_j, end_i, end_j):
    starts_are_ints = bool(isinstance(start_i, int) and isinstance(start_j, int))
    starts_are_floats = bool(isinstance(start_i, float) and isinstance(start_j, float))
    assert starts_are_ints or starts_are_floats, \
           "Unexpected types for starting i and j (or, x and y), found ('%s', '%s'), expected both to be either ints or floats." % (type(start_i), type(start_j))

    ends_are_ints = bool(isinstance(end_i, int) and isinstance(end_j, int))
    ends_are_floats = bool((isinstance(end_i, float) and isinstance(end_j, float)))
    assert ends_are_ints or ends_are_floats, \
           "Unexpected types for ending i and j (or, x and y), found ('%s', '%s'), expected both to be either ints or floats." % (type(end_i), type(end_j))

    if starts_are_ints or ends_are_ints:
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


def get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45):
    def get_text_position(pos_i, pos_j, corner):
        crnr = Corner(corner)

        if crnr.is_position():
            return (float(pos_i), float(pos_j))

        x = left if crnr.is_left() else right
        y = top if crnr.is_upper() else bottom

        return (float(pos_i + x), float(pos_j + y))

    return get_text_position


class Scene(object):

    def __init__(self, board=None, board_desc=None, *args, **kwargs):
        super(Scene, self).__init__(*args, **kwargs)
        self.reset(board=board, board_desc=board_desc)

    def reset(self, board=None, board_desc=None):
        assert isinstance(board, (Board, NoneType))
        assert isinstance(board_desc, (BoardDesc, NoneType))

        self.board = board
        self.board_desc = board_desc

        self.arrows = [] # :: [ mark.Arrow, ... ]
        self.texts = [] # :: [ mark.Text, ... ]
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

    def init_scene(self, board_type, width=None, height=None, board_desc=None):
        self.reset()

        bt = BoardType(board_type)
        board = Board(bt, width=width, height=height)

        self.board = board
        self.board_desc = board_desc or BoardDesc()

    def append_text(self, txt, pos_i, pos_j, \
                    corner=Corner(Corner.UpperLeft), \
                    mark_type=MarkType(MarkType.Legal), \
                    rect=(0.05, 1.0, 0.7, 0.35)):
        # assert isinstance(txt, str)
        assert isinstance(pos_i, (int, float))
        assert isinstance(pos_j, (int, float))
        # assert isinstance(corner, (Corner, int))
        # assert isinstance(mark_type, MarkType)
        assert isinstance(rect, (tuple, RectPos))

        assert type(pos_i) is type(pos_j)

        crnr = Corner(corner)
        left, top, right, bottom = rect.as_tuple() if isinstance(rect, RectPos) else rect

        get_text_position = get_func_get_text_position(left=left, top=top, right=right, bottom=bottom)
        pos_x, pos_y = get_text_position(pos_i, pos_j, crnr)

        txt_mark = Text(txt, pos_x, pos_y, mark_type=mark_type)
        self.texts.append(txt_mark)

        return txt_mark

    def append_arrow(self, start_i, start_j, end_i, end_j, \
                     mark_type=MarkType(MarkType.Legal), \
                     start_pointer=False, \
                     end_pointer=True, \
                     do_recalc_arrow_ends=True):
        # assert isinstance(start_i, (int, float))
        # assert isinstance(start_j, (int, float))
        # assert isinstance(end_i, (int, float))
        # assert isinstance(end_j, (int, float))
        # assert isinstance(mark_type, MarkType)

        start_x, start_y, end_x, end_y = recalc_arrow_ends(start_i, start_j, end_i, end_j) \
                                         if do_recalc_arrow_ends \
                                         else (start_i, start_j, end_i, end_j)

        arw_mark = Arrow(start_x, start_y, end_x, end_y, mark_type=mark_type, \
                         start_pointer=start_pointer, \
                         end_pointer=end_pointer)
        self.arrows.append(arw_mark)

        return arw_mark

    def append_field_marker(self, field_i, field_j, mark_type=MarkType(MarkType.Legal)):
        # assert isinstance(mark_type, MarkType)

        fld_mark = FieldMarker(field_i, field_j, mark_type=mark_type)
        self.field_markers.append(fld_mark)

        return fld_mark

