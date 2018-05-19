#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

from coords import Pos, RectPos
from board import BoardType, Board
from mark import MarkType, Arrow, Text, FieldMarker
# import scene_helper as SH
from scene_helper import Corner, get_func_get_text_position


class Scene(object):

    def __init__(self, *args, **kwargs):
        super(Scene, self).__init__(*args, **kwargs)
        self.reset()

    def reset(self):
        self.board = None
        self.arrows = [] # :: [ mark.Arrow, ... ]
        self.texts = [] # :: [ mark.Text, ... ]
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

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
        assert isinstance(corner, Corner)
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


    #
    # common scenes

    def intro_piece(self, bt, piece_type=None):
        self.init_scene(bt, width=2, height=2)

        pt = piece_type or bt.get_newly_introduced_piece()

        self.board.set_pieces( [ ( 0, 0, PieceType(pt) ),
                                 ( 1, 0, PieceType(pt) ),
                                 ( 0, 1, PieceType(-pt) ),
                                 ( 1, 1, PieceType(-pt) ) ] )

        return piece_type

    def intro_castling(self, bt):
        self.init_scene(bt, width=bt.get_size(), height=1)

        pos_king_h = bt.get_size() // 2
        offset = 1 if bt.does_contain(PieceType.Star) else 0
        pos_rook_r = bt.get_size() - 1 - offset

        self.board.set_piece(pos_king_h, 0, PieceType.King)
        self.board.set_piece(offset, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        diff = pos_rook_r - pos_king_h
        for i in xrange(2, diff):
            pos_l = pos_king_h - i
            pos_r = pos_king_h + i

           self.append_text(str(i-1), pos_l, 0, corner=Corner.UpperLeft)
           self.append_text(str(i-1), pos_r, 0, corner=Corner.UpperLeft)

        return bt

    def castling_long_left(self, bt):
        self.init_scene(bt, width=bt.get_size(), height=1)

        pos_king_init = bt.get_size() // 2
        offset = 1 if bt.does_contain(PieceType.Star) else 0
        pos_rook_r = bt.get_size() - 1 - offset
        # diff = pos_rook_r - pos_king_init
        pos_king_h = offset + 2

        self.board.set_piece(pos_king_h, 0, PieceType.King)
        self.board.set_piece(pos_king_h + 1, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        self.append_text("K", pos_king_init, 0, corner=Corner.UpperLeft, mark_type=MarkType.Forbidden)

        return bt

    def castling_short_right(self, bt):
        self.init_scene(bt, width=bt.get_size(), height=1)

        pos_king_init = bt.get_size() // 2
        offset = 1 if bt.does_contain(PieceType.Star) else 0
        # pos_init_rook_r = bt.get_size() - 1 - offset
        # diff = pos_rook_r - pos_king_init
        pos_king_h = pos_king_init + 2
        pos_rook_r = pos_king_h - 1

        self.board.set_piece(pos_king_h, 0, PieceType.King)
        self.board.set_piece(offset, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        self.append_text("K", pos_king_init, 0, corner=Corner.UpperLeft, mark_type=MarkType.Forbidden)

        return bt

    def intro_en_passant(self, bt):
        size = (bt.get_size() + 1) // 2
        self.init_scene(bt, width=3, height=size)

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in xrange(3, size):
            loc = 0 if i % 2 == 0 else 2
            self.board.set_piece(loc, i, PieceType(-PieceType.Pawn))

            self.append_arrow(loc, i, 1, i-1)
            self.append_text(str(i-2), 1, i, corner=Corner.UpperLeft, rect=(0.09, 1.0, 0.5, 0.45))

        return bt

    def intro_rush(self, bt):
        size = (bt.get_size() + 1) // 2
        self.init_scene(bt, width=3, height=size)

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in xrange(2, size):
            self.append_arrow(1, i-1, 1, i)

            if i > 2:
                self.append_text(str(i-2), 1, i, corner=Corner.UpperLeft, rect=(0.09, 1.0, 0.5, 0.45))

        return bt
