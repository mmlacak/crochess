#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
# from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, DEFAULT_NEIGHBOURING_REL_MOVES, DEFAULT_KING_REL_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneHemerasDawnMixin(Scene):

    def scn_hd_01_centaur_same_color(self, bt=BoardType.HemerasDawn):

        self.init_scene(bt, width=5, height=5)

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType.Centaur)

        gen_abs_pos = GS.get_gen_multi_steps(start=start, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_hd_01_centaur_same_color'

    def scn_hd_02_centaur_opposite_color(self, bt=BoardType.HemerasDawn):

        self.init_scene(bt)

        start = (6, 6)
        self.board.set_piece(*start, piece=PieceType.Centaur)

        # Centaur, long jump

        gen_abs_pos = GS.get_gen_multi_steps(start=start, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Knight, short jump

        gen_abs_pos_2 = GS.get_gen_multi_steps(start=start, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((4, 4), (8, 8)))

        i = 1
        for pos in gen_abs_pos_2():
            # self.append_field_marker(*pos)
            self.append_text(str(i), *pos, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_hd_02_centaur_opposite_color'
