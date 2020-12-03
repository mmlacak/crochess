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

        scene = Scene('scn_o_02_starchild_activating_fields', bt, width=7, height=7) # x=0, y=1,

        start_I = (3, 3)
        start_b = (3, 4)
        start_G = (4, 2)
        start_K = (2, 2)

        scene.board.set_piece(*start_I, piece=PieceType.Starchild)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        scene.append_arrow( *(start_I + start_G), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + start_K), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Illegal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        for index, coords in enumerate( gen() ):
            scene.append_text( str(index + 1), *coords )

        return scene

    def scn_o_03_starchild_activating_piece(self, bt=BoardType.One):

        scene = Scene('scn_o_03_starchild_activating_piece', bt, width=7, height=7)

        start_I = (3, 3)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_b = (3, 4)
        start_N = (4, 3)
        start_K = (2, 3)

        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        scene.append_arrow( *(start_I + start_N), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + start_K), mark_type=MarkType.Illegal )

        return scene
