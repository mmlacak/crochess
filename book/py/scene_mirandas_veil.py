#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017, 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneMirandasVeilMixin(Scene):

    def scn_mv_01_move_wave_init(self, bt=BoardType.MirandasVeil):
        # move_wave_init

        self.init_scene(bt)

        self.board.set_piece(1, 1, piece=PieceType.Wave)
        self.board.set_piece(0, 3, piece=PieceType.Knight)

        self.append_arrow(0, 3, 1, 1, mark_type=MarkType.Action)

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        self.board.set_piece(5, 9, piece=-PieceType.Queen)
        self.board.set_piece(7, 13, piece=PieceType.Bishop)

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType.King)
        self.board.set_piece(5, 3, piece=PieceType.Rook)
        self.board.set_piece(7, 4, piece=PieceType.Pyramid)
        self.board.set_piece(9, 5, piece=-PieceType.King)
        self.board.set_piece(13, 7, piece=-PieceType.Wave)

        return 'scn_mv_01_move_wave_init'
