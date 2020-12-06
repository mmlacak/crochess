#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneOneMixin:

    def scn_o_01_starchild_movement(self, bt=BoardType.One):

        scene = Scene('scn_o_01_starchild_movement', bt)

        scene.board.setup()
        scene.board.set_piece(5, 0, piece=PieceType.none) # Starchild was here

        start_I = (12, 12)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        return scene

    def scn_o_02_starchild_activating_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_02_starchild_activating_fields', bt, width=5, height=5)

        start_I = (2, 2)
        start_b = (2, 3)
        start_G = (3, 1)
        start_K = (1, 1)

        scene.board.set_piece(*start_I, piece=PieceType.Starchild)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        scene.append_arrow( *(start_I + start_G), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + start_K), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Illegal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 7 else MarkType.Illegal if index in [2, 5] else MarkType.Blocked
            scene.append_text( str(index + 1), *coords, mark_type=mark_type )

        return scene

    def scn_o_03_starchild_moving_star_init(self, bt=BoardType.One):

        scene = Scene('scn_o_03_starchild_moving_star_init', bt, width=5, height=5)

        start_I = (0, 1)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        startT1 = (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        # scene.append_arrow( *((2, 2) + start_I), mark_type=MarkType.Blocked )
        scene.append_arrow( *(start_I + startT1), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Action if index in [0, 6] else MarkType.Blocked
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_04_starchild_moving_star_end(self, bt=BoardType.One):

        scene = Scene('scn_o_04_starchild_moving_star_end', bt, width=5, height=5)

        start_I = (0, 0)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        startT1 = (1, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + startT1), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Action if index == 0 else MarkType.Illegal if index == 1 else MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_05_starchild_not_moving_monolith_init(self, bt=BoardType.One):

        scene = Scene('scn_o_05_starchild_not_moving_monolith_init', bt, width=5, height=5)

        start_I = (3, 3)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_M = (2, 2)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.board.set_piece(2, 1, piece=PieceType.Knight)
        scene.board.set_piece(1, 3, piece=PieceType.Bishop)

        scene.append_arrow( *(start_I + start_M), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Action if index == 5 else MarkType.Blocked
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_06_starchild_not_moving_monolith_end(self, bt=BoardType.One):

        scene = Scene('scn_o_06_starchild_not_moving_monolith_end', bt, width=5, height=5)

        start_I = (1, 2)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_M = (2, 2)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.board.set_piece(2, 1, piece=PieceType.Knight)
        scene.board.set_piece(1, 3, piece=PieceType.Bishop)

        scene.append_arrow( *(start_M + start_I), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Action if index == 4 else MarkType.Illegal if index in [3, 6] else MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_07_trance_journey_init(self, bt=BoardType.One):

        scene = Scene('scn_o_07_trance_journey_init', bt, width=9, height=9)

        start_b = (7, 7)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

        start_I = (6, 6)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_i = (7, 5)
        scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

        start_W = (4, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_w = (1, 5)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_h = (2, 1)
        scene.board.set_piece(*start_h, piece=-PieceType.Shaman)

        # A: dark Shaman --> dark Wave --> light Wave --> light Shaman
        scene.append_arrow( *(start_h + start_w), mark_type=MarkType.Action )
        scene.append_arrow( *(start_w + start_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + start_I), mark_type=MarkType.Action )

        scene.append_text( "A", *start_h, mark_type=MarkType.Action )
        scene.append_text( "A", *start_w, mark_type=MarkType.Action )
        scene.append_text( "A", *start_W, mark_type=MarkType.Action )

        # B: dark Starchild --> light Starchild
        scene.append_arrow( *(start_i + start_I), mark_type=MarkType.Action )

        scene.append_text( "B", *start_i, mark_type=MarkType.Action )

        # light Starchild --> dark Bishop
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Action )

        return scene
