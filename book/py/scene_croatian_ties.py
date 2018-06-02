#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, call_gen, get_gen_steps_prev, get_gen_multi_steps

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneCroatianTiesMixin(Scene):

    def scn_ct_pegasus_initial(self, bt=BoardType.CroatianTies):
        self.init_scene(bt, width=5, height=5)

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))

        gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return "scn_ct_pegasus_initial"
