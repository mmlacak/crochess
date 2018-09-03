#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, DEFAULT_NEIGHBOURING_REL_MOVES, DEFAULT_KING_REL_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneNineteenMixin(Scene):

    def scn_n_01_portal_fields(self, bt=BoardType.Nineteen):
        # move_wave_init

        self.init_scene(bt, width=7, height=7)

        startT1 = (2, 4)
        startT2 = (6, 2)
        startT3 = (0, 0)

        self.board.set_piece(*startT1, piece=PieceType.Star)
        self.board.set_piece(*startT2, piece=-PieceType.Star)
        self.board.set_piece(*startT3, piece=-PieceType.Star)

        self.append_text("1", *startT1, mark_type=MarkType.Blocked)
        self.append_text("2", *startT2, mark_type=MarkType.Blocked)
        self.append_text("3", *startT3, mark_type=MarkType.Blocked)

        # Star 1
        gen_abs_pos_1 = get_gen_multi_steps(start=startT1, rel_lst=DEFAULT_KING_REL_MOVES, pos_bounds=[(1, 3), (3, 5)])

        i = 1
        for pos in gen_abs_pos_1():
            self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Star 2
        gen_abs_pos_2 = get_gen_multi_steps(start=startT2, rel_lst=DEFAULT_KING_REL_MOVES, pos_bounds=[(5, 1), (6, 3)])

        i = 1
        for pos in gen_abs_pos_2():
            self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Star 3
        gen_abs_pos_3 = get_gen_multi_steps(start=startT3, rel_lst=DEFAULT_KING_REL_MOVES, pos_bounds=[(0, 0), (1, 1)])

        i = 1
        for pos in gen_abs_pos_3():
            self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_n_01_portal_fields'

