#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps

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

    def scn_ct_pegasus_direction(self, bt=BoardType.CroatianTies):
        self.init_scene(bt)

        start = (2, 1)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))

        # main direction, i.e. <1, 2>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 2)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = call_gen( get_gen_steps(start=start, rel=(1, 2)) )
        self.append_text("1", *coords(), corner=Corner.UpperLeft)
        self.append_text("2", *coords(), corner=Corner.UpperLeft)
        self.append_text("3", *coords(), corner=Corner.UpperLeft)
        self.append_text("4", *coords(), corner=Corner.UpperLeft)

        # direction 2a, i.e. <2, 1>
        self.append_arrow(4, 5, 6, 6, mark_type=MarkType.Blocked)
        self.append_text("2a", 6, 6, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # direction 2b, i.e. <2, -1>
        self.append_arrow(4, 5, 6, 4, mark_type=MarkType.Blocked)
        self.append_text("2b", 6, 4, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # direction 2c, i.e. <1, -2>
        self.append_arrow(4, 5, 5, 3, mark_type=MarkType.Blocked)
        self.append_text("2c", 5, 3, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # direction 2d, i.e. <-2, -1>
        self.append_arrow(4, 5, 2, 4, mark_type=MarkType.Blocked)
        self.append_text("2d", 2, 4, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # direction 2e, i.e. <-2, 1>
        self.append_arrow(4, 5, 2, 6, mark_type=MarkType.Blocked)
        self.append_text("2e", 2, 6, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # direction 2f, i.e. <-1, 2>
        self.append_arrow(4, 5, 3, 7, mark_type=MarkType.Blocked)
        self.append_text("2f", 3, 7, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        return 'scn_ct_pegasus_direction'

    def scn_ct_pegasus_step_ply(self, bt=BoardType.CroatianTies):
        self.init_scene(bt)

        start = (2, 1)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))

        # direction 1, i.e. <2, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(2, 1)) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # main direction, i.e. <1, 2>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 2)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = call_gen( get_gen_steps(start=start, rel=(1, 2)) )
        self.append_text("1", *coords(), corner=Corner.UpperLeft)
        self.append_text("2", *coords(), corner=Corner.UpperLeft)
        self.append_text("3", *coords(), corner=Corner.UpperLeft)

        # direction 3, i.e. <-1, 2>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 2)) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction 4, i.e. <-2, 1>
        self.append_arrow(2, 1, 0, 2, mark_type=MarkType.Blocked )

        # direction 5, i.e. <-2, -1>
        self.append_arrow(2, 1, 0, 0, mark_type=MarkType.Blocked )

        # direction 6, i.e. <2, -1>
        self.append_arrow(2, 1, 4, 0, mark_type=MarkType.Blocked )

        return 'scn_ct_pegasus_step_ply'
