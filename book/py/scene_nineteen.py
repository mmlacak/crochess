#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
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

        startR = (1, 0)
        startB = (3, 14)
        startW = (6, 14)
        startG = (8, 10)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(*startR, piece=PieceType.Rook)

        self.board.set_piece(15, 1, piece=PieceType.Pawn)
        self.board.set_piece(16, 1, piece=PieceType.Pawn)
        self.board.set_piece(17, 1, piece=PieceType.Pawn)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        self.board.set_piece(*startB, piece=PieceType.Bishop)
        self.board.set_piece(*startW, piece=PieceType.Wave)
        self.board.set_piece(*startG, piece=PieceType.Pegasus)
        self.board.set_piece(0, 12, piece=-PieceType.Rook)

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startB, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        self.append_text("1", 16, 17, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_n_02_teleport_init'

    def scn_n_03_teleport_dark(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startT1 = (17, 0)
        startT2 = (0, 17)

        startR = (0, 12)
        startU = (15, 14)
        startW = (16, 17)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(*startT1, piece=-PieceType.Star)
        self.board.set_piece(*startT2, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.King)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        # movers
        self.board.set_piece(*startU, piece=-PieceType.Unicorn)
        self.board.set_piece(3, 14, piece=PieceType.Bishop)
        self.board.set_piece(*startW, piece=PieceType.Wave)
        self.board.set_piece(*startR, piece=-PieceType.Rook)
        self.board.set_piece(12, 13, piece=PieceType.Pyramid)

        # Rook, direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startR, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # Unicorn, direction <2, 3>
        coords = GS.gen_next( GS.gen_steps(start=startU, rels=[(2, 3), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # Wave, direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=startW, rels=[(-1, -1), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Illegal )
        self.append_arrow( *coords(), mark_type=MarkType.Illegal )
        self.append_arrow( *coords(), mark_type=MarkType.Illegal )
        self.append_arrow( *coords(), mark_type=MarkType.Illegal )

        # Star 1
        coords = GS.gen_next( GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT1, include_prev=False, bounds=((16, 0), (17, 1))) )
        self.append_text("1", *coords(), corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", *coords(), corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("3", *coords(), corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))

        # Star 2
        coords = GS.gen_next( GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT2, include_prev=False, bounds=((0, 16), (1, 17))) )
        self.append_text("4", *coords(), corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("5", *coords(), corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("6", *coords(), corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_n_03_teleport_dark'

    def scn_n_04_teleport_end(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.King)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        # movers
        self.board.set_piece(17, 1, piece=-PieceType.Unicorn)
        self.board.set_piece(3, 14, piece=PieceType.Bishop)
        self.board.set_piece(16, 17, piece=PieceType.Wave)
        self.board.set_piece(0, 12, piece=-PieceType.Rook)
        self.board.set_piece(12, 13, piece=PieceType.Pyramid)

        return 'scn_n_04_teleport_end'
