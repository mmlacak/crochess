#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_desc import BoardDesc
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneHemerasDawnMixin(Scene):

    def scn_hd_01_centaur_same_color(self, bt=BoardType.HemerasDawn):

        self.init_scene(bt, width=7, height=7)

        start = (3, 3)
        self.board.set_piece(*start, piece=PieceType.Centaur)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, bounds=self.board.get_position_limits())

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

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start, include_prev=False, bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, bounds=((4, 4), (8, 8)))

        i = 1
        for pos in gen_abs_pos_2():
            # self.append_field_marker(*pos)
            self.append_text(str(i), *pos, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_hd_02_centaur_opposite_color'

    def scn_hd_03_centaur_multi_step(self, bt=BoardType.HemerasDawn):

        self.init_scene(bt)
        rect = (0.05, 1.0, 0.6, 0.45)

        start = (3, 2)
        self.board.set_piece(*start, piece=PieceType.Centaur)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(7, 8, piece=PieceType.Pawn)
        self.board.set_piece(8, 9, piece=-PieceType.Pawn)
        self.board.set_piece(9, 9, piece=-PieceType.Pawn)
        self.board.set_piece(14, 16, piece=-PieceType.Bishop)

        #
        # short --> (-1, 2) direction
        # long --> (4, 1) direction

        rels = [(-1, 2), (4, 1), ]
        arr = GS.gen_next( GS.gen_steps(start=start, rels=rels, include_prev=True) )
        txt = GS.gen_next( GS.gen_steps(start=start, rels=rels, include_prev=False) )

        #
        # choose directions

        # short
        self.append_arrow( *arr(), mark_type=MarkType.Action )
        self.append_text("1", *txt(), mark_type=MarkType.Action, rect=rect)

        # long
        self.append_arrow( *arr(), mark_type=MarkType.Action )
        self.append_text("2", *txt(), corner=Corner.UpperRight, mark_type=MarkType.Action, rect=rect)

        #
        # follow directions

        # short
        self.append_arrow( *arr() )
        self.append_text("3", *txt(), rect=rect)

        # long
        self.append_arrow( *arr() )
        self.append_text("4", *txt(), corner=Corner.UpperRight, rect=rect)

        # short
        self.append_arrow( *arr() )
        self.append_text("5", *txt(), rect=rect)

        # long
        self.append_arrow( *arr() )
        self.append_text("6", *txt(), corner=Corner.UpperRight, rect=rect)

        # short
        self.append_arrow( *arr() )
        self.append_text("7", *txt(), rect=rect)

        # long
        self.append_arrow( *arr() )
        self.append_text("8", *txt(), corner=Corner.UpperRight, rect=rect)

        # short
        self.append_arrow( *arr(), mark_type=MarkType.Action )
        self.append_text("9", *txt(), mark_type=MarkType.Action, rect=rect)

        # long
        self.append_arrow( *arr(), mark_type=MarkType.Blocked )
        self.append_text("10", *txt(), corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=rect)

        # short
        self.append_arrow( *arr(), mark_type=MarkType.Blocked )
        self.append_text("11", *txt(), mark_type=MarkType.Blocked, rect=rect)

        #
        # forbidden directions change

        # (-1, 2) is ok, i.e. direction "7", here: 12, 11 --> 11, 13
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-1, 2), ) ) )
        startK = (12, 11)

        arr = GS.gen_next( GS.gen_multi_steps(multi_rels, start=startK, include_prev=True, count=1) )
        txt = GS.gen_next( GS.gen_multi_steps(multi_rels, start=startK, include_prev=False, count=1) )

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7a", *txt(), corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7b", *txt(), corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7c", *txt(), corner=Corner.UpperLeft, mark_type=MarkType.Illegal, rect=rect)

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7d", *txt(), corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=rect)

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7e", *txt(), corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=rect)

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7f", *txt(), corner=Corner.LowerRight, mark_type=MarkType.Illegal, rect=rect)

        self.append_arrow( *arr(), mark_type=MarkType.Illegal )
        self.append_text("7g", *txt(), corner=Corner.LowerRight, mark_type=MarkType.Illegal, rect=rect)

        return 'scn_hd_03_centaur_multi_step'


    def scn_hd_04_centaur_off_board(self, bt=BoardType.HemerasDawn):

        bd = BoardDesc(reverse_field_colors=True, off_board_top=1, off_board_right=4, reverse_off_board_field_colors=True)
        self.init_scene(bt, width=16, height=19, board_desc=bd)

        rect = (0.05, 1.0, 0.6, 0.45)

        start = (13, 2)
        self.board.set_piece(*start, piece=-PieceType.Centaur)


        #
        # short --> (-2, 1) direction
        # long --> (3, 2) direction

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True) )

        self.append_arrow( *coords() ) # short
        self.append_arrow( *coords() ) # long

        self.append_arrow( *coords() ) # short
        self.append_arrow( *coords() ) # long

        self.append_arrow( *coords() ) # short
        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long


        self.append_text("1", 14, 12, corner=Corner.UpperLeft, mark_type=MarkType.Illegal, rect=rect)
        self.append_text("2", 15, 15, corner=Corner.UpperLeft, mark_type=MarkType.Illegal, rect=rect)

        return 'scn_hd_04_centaur_off_board'
