#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneCommon(Scene):

    def intro_piece(self, bt, piece_type=None):
        bt = BoardType(bt)
        self.init_scene(bt, width=2, height=2)

        pt = piece_type or bt.get_newly_introduced_piece()

        if pt is not None:
            self.board.set_pieces( [ ( 0, 0, PieceType(pt) ),
                                     ( 1, 0, PieceType(pt) ),
                                     ( 0, 1, PieceType(-pt) ),
                                     ( 1, 1, PieceType(-pt) ) ] )

        return piece_type or pt

    def intro_castling(self, bt, move_king=0):
        assert isinstance(move_king, int)

        bt = BoardType(bt)

        offset = 1 if bt.does_contain(PieceType.Star) else 0

        pos_king_init = bt.get_size() // 2
        pos_rook_l_init = offset
        pos_rook_r_init = bt.get_size() - 1 - offset

        diff_min, diff_max = Board.get_castling_limits(bt)
        assert (move_king == 0) or in_range(abs(move_king), diff_min, diff_max)

        self.init_scene(bt, width=bt.get_size(), height=1)

        king_moved = (move_king != 0)
        king_moved_left = (move_king < 0)
        king_moved_right = (move_king > 0)

        pos_king = pos_king_init + move_king
        pos_rook_l = pos_king + 1 if king_moved_left else pos_rook_l_init
        pos_rook_r = pos_king - 1 if king_moved_right else pos_rook_r_init

        self.board.set_piece(pos_king, 0, PieceType.King)
        self.board.set_piece(pos_rook_l, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        if king_moved:
            self.append_text("K", pos_king_init, 0, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        mt = MarkType.Blocked if king_moved else MarkType.Legal

        for i in range(diff_min, diff_max+1):
            # diff_max + 1, because upper boundary is not included

            pos_l = pos_king_init - i
            pos_r = pos_king_init + i

            self.append_text(str(i-1), pos_l, 0, corner=Corner.UpperLeft, mark_type=mt)
            self.append_text(str(i-1), pos_r, 0, corner=Corner.UpperLeft, mark_type=mt)

        return bt

    def intro_en_passant(self, bt):
        bt = BoardType(bt)

        size = (bt.get_size() + 1) // 2
        self.init_scene(bt, width=3, height=size)

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in range(3, size):
            loc = 0 if i % 2 == 0 else 2
            self.board.set_piece(loc, i, PieceType(-PieceType.Pawn))

            self.append_arrow(loc, i, 1, i-1)
            self.append_text(str(i-2), 1, i, corner=Corner.UpperLeft, rect=(0.15, 1.0, 0.5, 0.45))

        return bt

    def intro_rush(self, bt):
        bt = BoardType(bt)

        size = (bt.get_size() + 1) // 2
        self.init_scene(bt, width=3, height=size)

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in range(2, size):
            self.append_arrow(1, i-1, 1, i)

            if i > 2:
                self.append_text(str(i-2), 1, i, corner=Corner.UpperLeft, rect=(0.15, 1.0, 0.5, 0.45))

        return bt
