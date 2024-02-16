#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import enum
from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneClassicalChessMixin:

    def scn_cc_01_rook_not_blocked( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_01_rook_not_blocked', bt, width=3.2, height=4.2 )

        start_R = (1, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        end_R = (1, 3)
        scene.append_arrow( *( start_R + end_R ), mark_type=MarkType.Legal )

        return scene

    def scn_cc_02_rook_blocked( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_02_rook_blocked', bt, width=3.2, height=4.2 )

        start_R = (1, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_N = (1, 2)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # R --> n
        coords_R_N = GS.gen_steps( start=start_R, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_R_N() ):
            mark_type = MarkType.Legal if i < 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_cc_03_rook_capturing( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_03_rook_capturing', bt, width=3.2, height=4.2 )

        start_R = (1, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_n = (1, 2)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        # R -->|
        coords_R_n = GS.gen_steps( start=start_R, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_R_n() ):
            mark_type = MarkType.Legal if i < 1 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_cc_04_rook_illegal( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_04_rook_illegal', bt, width=3.2, height=4.2 )

        start_R = (1, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_n = (1, 2)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        # R -->|
        coords_R_n = GS.gen_steps( start=start_R, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_R_n() ):
            mark_type = MarkType.Legal if i < 1 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        start_R_1 = (1, 1)
        end_R_2 = (2, 1)
        scene.append_arrow( *( start_R_1 + end_R_2 ), mark_type=MarkType.Illegal )

        return scene
