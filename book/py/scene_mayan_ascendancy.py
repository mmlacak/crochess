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


class SceneMayanAscendancyMixin(Scene):

    def scn_ma_pyramid_activation_init(self, bt=BoardType.MayanAscendancy):
        self.init_scene(bt)

        start = (11, 3)
        self.board.set_piece(3, 7, piece=PieceType.Pyramid)
        self.board.set_piece(6, 7, piece=PieceType.Bishop)
        self.board.set_piece(*start, piece=PieceType.Pegasus)
        self.board.set_piece(3, 9, piece=-PieceType.Knight)
        self.board.set_piece(3, 3, piece=-PieceType.Bishop)

        # direction <-2, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-2, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = call_gen( get_gen_steps(start=start, rel=(-2, 1)) )
        self.append_text("1", *coords(), corner=Corner.UpperRight)
        self.append_text("2", *coords(), corner=Corner.UpperRight)
        self.append_text("3", *coords(), corner=Corner.UpperRight)
        self.append_text("4", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        return 'scn_ma_pyramid_activation_init'

    def scn_ma_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        self.init_scene(bt)

        start = (3, 7)
        self.board.set_piece(*start, piece=PieceType.Pegasus)
        self.board.set_piece(6, 7, piece=PieceType.Bishop)
        self.board.set_piece(3, 9, piece=-PieceType.Knight)
        self.board.set_piece(3, 3, piece=-PieceType.Bishop)

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = call_gen( get_gen_steps(start=start, rel=(1, 0)) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)
        self.append_text("4", *coords(), mark_type=MarkType.Blocked)

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = call_gen( get_gen_steps(start=start, rel=(0, 1)) )
        self.append_text("1", *coords())
        self.append_text("2", *coords(), mark_type=MarkType.Action)
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)
        self.append_text("4", *coords(), mark_type=MarkType.Blocked)

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = call_gen( get_gen_steps(start=start, rel=(-1, 0)) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = call_gen( get_gen_steps(start=start, rel=(0, -1)) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords(), mark_type=MarkType.Action)

        return 'scn_ma_pyramid_activated'

    def scn_ma_pyramid_activation_end(self, bt=BoardType.MayanAscendancy):
        self.init_scene(bt)

        self.board.set_piece(3, 7, PieceType.Pegasus)
        self.board.set_piece(6, 7, PieceType.Bishop)
        self.board.set_piece(3, 9, PieceType.Pyramid)
        self.board.set_piece(3, 3, -PieceType.Bishop)

        return 'scn_ma_pyramid_activation_end'
