#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2019 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneNineteenMixin(Scene):

    def scn_n_01_portal_fields(self, bt=BoardType.Nineteen):

        self.init_scene(bt, width=7, height=7)

        startT1 = (2, 4)
        startT2 = (6, 2)
        startT3 = (0, 0)

        self.board.set_piece(*startT1, piece=PieceType.Star)
        self.board.set_piece(*startT2, piece=-PieceType.Star)
        self.board.set_piece(*startT3, piece=-PieceType.Star)

        self.append_text("1", *startT1, mark_type=MarkType.Illegal)
        self.append_text("2", *startT2, mark_type=MarkType.Illegal)
        self.append_text("3", *startT3, mark_type=MarkType.Action)

        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT1, include_prev=False, bounds=((1, 3), (3, 5)))

        i = 1
        for pos in gen_abs_pos_1():
            # self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT2, include_prev=False, bounds=((5, 1), (6, 3)))

        i = 1
        for pos in gen_abs_pos_2():
            # self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Star 3
        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT3, include_prev=False, bounds=((0, 0), (1, 1)))

        i = 1
        for pos in gen_abs_pos_3():
            # self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_n_01_portal_fields'

    #
    # teleportation

    def scn_n_02_teleport_init(self, bt=BoardType.Nineteen):

        self.init_scene(bt)

        startK = (1, 0)
        startB = (14, 3)
        startW = (3, 14)

        # fixed set
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(17, 17, piece=PieceType.Star)
        self.board.set_piece(17, 0, piece=-PieceType.Star)
        self.board.set_piece(0, 17, piece=-PieceType.Star)

        self.board.set_piece(0, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(2, 1, piece=PieceType.Pawn)
        self.board.set_piece(*startK, piece=PieceType.King)

        self.board.set_piece(17, 16, piece=-PieceType.Pawn)
        self.board.set_piece(16, 16, piece=-PieceType.Pawn)
        self.board.set_piece(15, 15, piece=-PieceType.Pawn)

        # movers
        self.board.set_piece(15, 14, piece=-PieceType.Unicorn)
        self.board.set_piece(*startB, piece=PieceType.Bishop)
        self.board.set_piece(*startW, piece=PieceType.Wave)
        self.board.set_piece(0, 12, piece=-PieceType.Rook)
        self.board.set_piece(12, 13, piece=PieceType.Pyramid)

        # King, direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startK, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Illegal )

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startB, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # Wave, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startW, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # self.append_field_marker(16, 17, mark_type=MarkType.Action)
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
