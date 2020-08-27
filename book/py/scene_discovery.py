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


class SceneDiscoveryMixin:

    def scn_d_01_knight_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_01_knight_steps', bt, width=7, height=7)

        start_N = (3, 3)
        scene.board.set_piece(*start_N, piece=-PieceType.Knight)

        scene.append_arrow( 3, 3, 3+0.5, 4+0.5, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False ) # up
        scene.append_arrow( *GS.add_to_all( (3, 4, 2, 5), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # up left
        scene.append_arrow( *GS.add_to_all( (3, 4, 4, 5), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up right

        scene.append_text("L", 2, 5, corner=Corner.UpperLeft, mark_type=MarkType.Legal)
        scene.append_text("R", 4, 5, corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_d_02_monolith_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_02_monolith_steps', bt, width=7, height=7)

        start_M = (3, 3)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        #
        # left diagonals

        coords = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_LEFT_MOVES, start=start_M, include_prev=False, count=1)

        for index, pos in enumerate( coords() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text("L", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal)

        #
        # right diagonals

        coords = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_RIGHT_MOVES, start=start_M, include_prev=False, count=1)

        for index, pos in enumerate( coords() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text("R", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action)

        return scene

    def scn_d_03_monolith_step_2(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_03_monolith_step_2', bt)

        start = (3, 3)
        start_M = (4, 5) # rel == (1, 2) --> right step
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.append_text("S", *start, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)
        scene.append_arrow( *(start + start_M), mark_type=MarkType.Blocked )

        #
        # left diagonals

        coords = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_LEFT_MOVES, start=start_M, include_prev=True, count=1)

        for index, pos in enumerate( coords() ):
            scene.append_field_marker(*pos[ 2 : ], mark_type=MarkType.Legal)
            # scene.append_text("L", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal)
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        return scene
