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

    def scn_cc_01_bishop_not_blocked( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_01_bishop_not_blocked', bt )

        start_B = (1, 2)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        end_B = (6, 7)
        scene.append_arrow( *( start_B + end_B ), mark_type=MarkType.Legal )

        return scene

    def scn_cc_02_bishop_blocked( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_02_bishop_blocked', bt )

        start_B = (1, 2)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_N = (4, 5)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # B -->|
        coords_B_ = GS.gen_steps( start=start_B, rels=[ (1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_B_() ):
            mark_type = MarkType.Legal if i < 2 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene
