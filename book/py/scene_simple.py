#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneSimpleMixin:

    def scn_ct14_01_sideways_pawns( self, bt=BoardType.Croatian_14 ):

        scene = Scene( 'scn_ct14_01_sideways_pawns', bt, width=5.3, height=8.3 ) # x=1.0, y=0.0,

        start_P = (2, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_B = (2, 0)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_p = (2, 7)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        adder_P = GS.adder( start_P, include_prev=True )
        scene.append_arrow( *adder_P( -1, 0, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_P( 0, 1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_P( 1, 0, do_advance=False ), mark_type=MarkType.Legal )

        adder_p = GS.adder( start_p, include_prev=True )
        scene.append_arrow( *adder_p( -1,  0, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_p( 0, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_p( 1,  0, do_advance=False ), mark_type=MarkType.Legal )

        return scene
