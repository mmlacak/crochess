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


class SceneNineteenMixin(Scene):

    def scn_n_01_portal_fields(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startT1 = (0, 0)
        startT2 = (17, 17)
        startT3 = (17, 0)
        startT4 = (0, 17)

        self.board.set_piece(*startT1, piece=PieceType.Star)
        self.board.set_piece(*startT2, piece=PieceType.Star)
        self.board.set_piece(*startT3, piece=-PieceType.Star)
        self.board.set_piece(*startT4, piece=-PieceType.Star)

        #
        # King
        start_K = (8, 8)
        self.board.set_piece(*start_K, piece=PieceType.King)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_K, include_prev=False, count=1) # , bounds=((1, 3), (3, 5)))

        i = 1
        for pos in gen_abs_pos_K():
            self.append_text(str(i), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT1, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos_1():
            if self.board.is_on_board(*pos):
                self.append_text(str(i), *pos, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
                i += 1

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT2, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos_2():
            if self.board.is_on_board(*pos):
                self.append_text(str(i), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
                i += 1

        #
        # Star 3
        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT3, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos_3():
            if self.board.is_on_board(*pos):
                self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
                i += 1

        #
        # Star 4
        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT4, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos_4():
            if self.board.is_on_board(*pos):
                self.append_text(str(i), *pos, corner=Corner.LowerRight, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
                i += 1

        return 'scn_n_01_portal_fields'

    #
    # teleportation

    def scn_n_02_teleport_init(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startB = (3, 14)
        startW = (6, 14)
        startG = (8, 10)
        start_R = (0, 12)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Wave)
        self.board.set_piece(0, 2, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.Rook)

        self.board.set_piece(15, 1, piece=PieceType.Pawn)
        self.board.set_piece(16, 1, piece=PieceType.Pawn)
        self.board.set_piece(17, 1, piece=PieceType.Pawn)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        self.board.set_piece(*startB, piece=PieceType.Bishop)
        self.board.set_piece(*startW, piece=PieceType.Wave)
        self.board.set_piece(*startG, piece=PieceType.Pegasus)
        self.board.set_piece(*start_R, piece=-PieceType.Rook)
        self.board.set_piece(11, 3, piece=PieceType.Pyramid)

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startB, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        self.append_text("1", 16, 17, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_n_02_teleport_init'

    def scn_n_03_teleport_move_2(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startW = (6, 14)
        startG = (8, 10)
        start_R = (0, 12)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Wave)
        self.board.set_piece(0, 2, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.Rook)

        self.board.set_piece(15, 1, piece=PieceType.Pawn)
        self.board.set_piece(16, 1, piece=PieceType.Pawn)
        self.board.set_piece(17, 1, piece=PieceType.Pawn)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        self.board.set_piece(16, 17, piece=PieceType.Bishop)
        self.board.set_piece(*startW, piece=PieceType.Wave)
        self.board.set_piece(*startG, piece=PieceType.Pegasus)
        self.board.set_piece(*start_R, piece=-PieceType.Rook)
        self.board.set_piece(11, 3, piece=PieceType.Pyramid)

        # Rook, direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start_R, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_n_03_teleport_move_2'

    def scn_n_04_teleport_move_3(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startW = (6, 14)
        startG = (8, 10)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Wave)
        self.board.set_piece(0, 2, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.Rook)

        self.board.set_piece(15, 1, piece=PieceType.Pawn)
        self.board.set_piece(16, 1, piece=PieceType.Pawn)
        self.board.set_piece(17, 1, piece=PieceType.Pawn)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        self.board.set_piece(16, 17, piece=PieceType.Bishop)
        self.board.set_piece(*startW, piece=PieceType.Wave)
        self.board.set_piece(*startG, piece=PieceType.Pegasus)
        self.board.set_piece(11, 3, piece=PieceType.Pyramid)

        # Pegasus, direction <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=startG, rels=[(-1, 2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # Wave, direction <-2, 1>
        coords = GS.gen_next( GS.gen_steps(start=startW, rels=[(-2, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_n_04_teleport_move_3'

    def scn_n_05_teleport_end(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startW = (17, 0)
        startG = (6, 14)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Wave)
        self.board.set_piece(0, 2, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.Rook)

        self.board.set_piece(15, 1, piece=PieceType.Pawn)
        self.board.set_piece(16, 1, piece=PieceType.Pawn)
        self.board.set_piece(17, 1, piece=PieceType.Pawn)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        self.board.set_piece(16, 17, piece=PieceType.Bishop)
        self.board.set_piece(*startG, piece=PieceType.Pegasus)
        self.board.set_piece(11, 3, piece=PieceType.Pyramid)

        gen_coords = GS.gen_steps(start=startW, rels=[(-2, 1), ], include_prev=True, bounds=self.board.get_position_limits())
        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Action if index in [0, 2] else MarkType.Legal
            self.append_arrow( *coords, mark_type=mark_type )

        return 'scn_n_05_teleport_end'

    def scn_n_06_teleport_wave_init(self, bt=BoardType.Nineteen):

        bd = BoardDesc(reverse_field_colors=True, off_board_top=1, off_board_right=4, reverse_off_board_field_colors=False)
        self.init_scene(bt, width=14, height=17, board_desc=bd)

        rect = (0.05, 1.0, 0.6, 0.45)

        start_T = (13, 16)
        self.board.set_piece(*start_T, piece=PieceType.Star)

        start = (12, 6)
        self.board.set_piece(*start, piece=PieceType.Wave)

        start_U = (8, 5)
        self.board.set_piece(*start_U, piece=PieceType.Unicorn)

        #
        # Wave activation
        self.append_arrow( *(start_U + start), mark_type=MarkType.Action ) # short

        #
        # short --> (-2, 1) direction
        # long --> (3, 2) direction

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True) )

        self.append_arrow( *coords() ) # short
        self.append_arrow( *coords() ) # long

        self.append_arrow( *coords() ) # short
        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        self.append_arrow( *coords() ) # short
        self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        self.append_arrow( *coords() ) # short
#         self.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        self.append_text("1", 12, 13, corner=Corner.UpperLeft, rect=rect)
        self.append_text("2", *start_T, corner=Corner.UpperLeft, rect=rect)

        return 'scn_n_06_teleport_wave_init'

    def scn_n_07_teleport_wave_end(self, bt=BoardType.Nineteen):

        bd = BoardDesc(reverse_field_colors=False, off_board_bottom=1, off_board_left=4, reverse_off_board_field_colors=True)
        self.init_scene(bt, width=14, height=17, board_desc=bd)

        rect = (0.05, 1.0, 0.6, 0.45)

        start_T = (0, 0)
        self.board.set_piece(*start_T, piece=PieceType.Star)

        self.board.set_piece(4, 5, piece=PieceType.Pyramid)
        self.board.set_piece(4, 12, piece=-PieceType.Pawn)

        start = start_T

        #
        # long --> (3, 2) direction
        # short --> (-2, 1) direction

        gen_coords = GS.gen_steps(start=start, rels=[(3, 2), (-2, 1), ], include_prev=True, bounds=((0, 0), (19, 19))) # self.board.get_position_limits())

        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Legal
            if index == 2:
                mark_type = MarkType.Action
            elif index == 7:
                mark_type = MarkType.Blocked
            self.append_arrow( *coords, mark_type=mark_type )

        return 'scn_n_07_teleport_wave_end'

    def scn_n_08_teleport_bishop(self, bt=BoardType.Nineteen):

        self.init_scene(bt)
        rect = (0.05, 1.0, 0.6, 0.45)

        startB = (3, 14)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(*startB, piece=PieceType.Bishop)

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startB, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        self.append_text("1", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal, rect=rect)
        self.append_text("2", 16, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal, rect=rect)

        return 'scn_n_08_teleport_bishop'
