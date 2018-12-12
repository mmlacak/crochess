#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
# from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, DEFAULT_NEIGHBOURING_REL_MOVES, DEFAULT_KING_REL_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_desc import BoardDesc
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

    def scn_hd_03_centaur_multi_step(self, bt=BoardType.HemerasDawn):

        self.init_scene(bt)
        rect = (0.05, 1.0, 0.6, 0.45)

        start = (3, 2)
        self.board.set_piece(*start, piece=PieceType.Centaur)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(7, 8, piece=PieceType.Pawn)
        self.board.set_piece(18, 17, piece=PieceType.Bishop)

        #
        # choose directions

        # short
        self.append_arrow( 3, 2, 2, 4, mark_type=MarkType.Action )
        self.append_text("1", 2, 4, mark_type=MarkType.Action, rect=rect)

        # long
        self.append_arrow( 2, 4, 6, 5, mark_type=MarkType.Action )
        self.append_text("2", 6, 5, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=rect)

        #
        # follow directions

        # short
        self.append_arrow( 6, 5, 5, 7 )
        self.append_text("3", 5, 7, rect=rect)

        # long
        self.append_arrow( 5, 7, 9, 8 )
        self.append_text("4", 9, 8, corner=Corner.UpperRight, rect=rect)

        # short
        self.append_arrow( 9, 8, 8, 10 )
        self.append_text("5", 8, 10, rect=rect)

        # long
        self.append_arrow( 8, 10, 12, 11 )
        self.append_text("6", 12, 11, corner=Corner.UpperRight, rect=rect)

        # short
        self.append_arrow( 12, 11, 11, 13 )
        self.append_text("7", 11, 13, rect=rect)

        # long
        self.append_arrow( 11, 13, 15, 14 )
        self.append_text("8", 15, 14, corner=Corner.UpperRight, rect=rect)

        # short
        self.append_arrow( 15, 14, 14, 16 )
        self.append_text("9", 14, 16, rect=rect)

        # long
        self.append_arrow( 14, 16, 18, 17, mark_type=MarkType.Illegal )
        self.append_text("10", 18, 17, corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        # short
        self.append_arrow( 18, 17, 17, 19, mark_type=MarkType.Blocked )
        self.append_text("11", 17, 19, mark_type=MarkType.Blocked, rect=rect)


        #
        # forbidden directions change

        self.append_arrow( 12, 11, 14, 12, mark_type=MarkType.Blocked )
        self.append_text("7a", 14, 12, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=rect)

        self.append_arrow( 12, 11, 13, 13, mark_type=MarkType.Blocked )
        self.append_text("7b", 13, 13, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=rect)

        # (-1, 2) is ok, i.e. 12, 11 --> 11, 13

        self.append_arrow( 12, 11, 10, 12, mark_type=MarkType.Blocked )
        self.append_text("7c", 10, 12, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=rect)

        self.append_arrow( 12, 11, 10, 10, mark_type=MarkType.Blocked )
        self.append_text("7d", 10, 10, corner=Corner.LowerLeft, mark_type=MarkType.Blocked, rect=rect)

        self.append_arrow( 12, 11, 11, 9, mark_type=MarkType.Blocked )
        self.append_text("7e", 11, 9, corner=Corner.LowerLeft, mark_type=MarkType.Blocked, rect=rect)

        self.append_arrow( 12, 11, 13, 9, mark_type=MarkType.Blocked )
        self.append_text("7f", 13, 9, corner=Corner.LowerRight, mark_type=MarkType.Blocked, rect=rect)

        self.append_arrow( 12, 11, 14, 10, mark_type=MarkType.Blocked )
        self.append_text("7g", 14, 10, corner=Corner.LowerRight, mark_type=MarkType.Blocked, rect=rect)

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

        self.append_arrow( 13, 2, 11, 3 ) # short
        self.append_arrow( 11, 3, 14, 5 ) # long

        self.append_arrow( 14, 5, 12, 6 ) # short
        self.append_arrow( 12, 6, 15, 8 ) # long

        self.append_arrow( 15, 8, 13, 9 ) # short
        self.append_arrow( 13, 9, 16, 11, mark_type=MarkType.Illegal ) # long

        self.append_arrow( 16, 11, 14, 12, mark_type=MarkType.Illegal ) # short
        self.append_arrow( 14, 12, 17, 14, mark_type=MarkType.Illegal ) # long

        self.append_arrow( 17, 14, 15, 15, mark_type=MarkType.Illegal ) # short
        self.append_arrow( 15, 15, 18, 17, mark_type=MarkType.Illegal ) # long

        self.append_arrow( 18, 17, 16, 18, mark_type=MarkType.Illegal ) # short
        self.append_arrow( 16, 18, 19, 20, mark_type=MarkType.Illegal ) # long

        self.append_text("1", 14, 12, corner=Corner.UpperLeft, mark_type=MarkType.Illegal, rect=rect)
        self.append_text("2", 15, 15, corner=Corner.UpperLeft, mark_type=MarkType.Illegal, rect=rect)

        return 'scn_hd_04_centaur_off_board'
