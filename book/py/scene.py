#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

from coords import Pos, RectPos
from board import BoardType, Board
from mark import MarkType, Arrow, Text, FieldMarker


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


class Scene(object):

    def __init__(self, *args, **kwargs):
        super(Scene, self).__init__(*args, **kwargs)
        self.reset()

    def reset(self):
        self.board = None
        self.arrows = [] # :: [ mark.Arrow, ... ]
        self.texts = [] # :: [ mark.Text, ... ]
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

    def init_scene(self, board_type, width=None, height=None):
        self.reset()

        bt = BoardType(board_type)
        self.board = Board(bt, width=width, height=height)
        # self.board.clear()

    def append_text(self, txt, pos_i, pos_j, \
                    corner=Corner(Corner.UpperLeft), \
                    mark_type=MarkType(MarkType.Legal), \
                    rect=(0.05, 1.0, 0.7, 0.45)):
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

    def append_arrow(self, start_x, start_y, end_x, end_y, \
                     mark_type=MarkType(MarkType.Legal), \
                     start_pointer=False, \
                     end_pointer=True):
        # assert isinstance(mark_type, MarkType)

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

    def format_return_values(self, filename, size_x=None, size_y=None):
        # size_x, size_y :: int | float | None

        width = size_x
        height = size_y

        b = self.board

        if width is None:
            if b.get_width() != b.type.get_size():
                width = float(b.get_width()) / float(b.type.get_size())

        if height is None:
            if b.get_height() != b.type.get_size():
                height = float(b.get_height()) / float(b.type.get_size())

        if isinstance(width, float):
            width = int( width * GD.DEFAULT_BOARD_RENDERING_SIZE )

        if isinstance(height, float):
            height = int( height * GD.DEFAULT_BOARD_RENDERING_SIZE )

        return filename, width, height, b.type
