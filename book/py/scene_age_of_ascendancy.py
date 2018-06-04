#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneAgeOfAquariusMixin(Scene):

    def scn_aoa_01_unicorn_same_color(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_same_color

        self.init_scene(bt, width=5, height=5)

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType.Unicorn)

        gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos)
            i += 1

        return 'scn_aoa_01_unicorn_same_color'

    def scn_aoa_02_unicorn_opposite_color(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_opposite_color

        self.init_scene(bt)

        start = (6, 6)
        self.board.set_piece(*start, piece=PieceType.Unicorn)

        gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos )
            self.append_text(str(i), *pos)
            i += 1

        return 'scn_aoa_02_unicorn_opposite_color'

    #
    # Delayed promotion

    def scn_aoa_03_delayed_promo_init(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_promo_init

        self.init_scene(bt)

        startB = (12, 5)
        startA = (7, 10)
        startP1 = (8, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startA, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(*startB, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(4, 1, piece=PieceType(-PieceType.Unicorn))

        self.append_text("1", *startP1, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)
        self.append_text("2", *startP2, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)
        self.append_text("3", *startP3, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # direction <-1, 1>
        coords = call_gen( get_gen_steps_prev(start=startB, rel=(-1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=startA, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_aoa_03_delayed_promo_init'

