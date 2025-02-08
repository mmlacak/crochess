#!/usr/bin/env -S python3 -B
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

    def scn_cc_04_knight_stepping( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_04_knight_stepping', bt, width=3.2, height=4.2 )

        start_N = (0, 1)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_P = (1, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_b = (1, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_p = (2, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        end_N = start_p # (2, 2)
        scene.append_arrow( *( start_N + end_N ), mark_type=MarkType.Action )

        return scene

    def scn_cc_05_rook_illegal( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_05_rook_illegal', bt, width=3.2, height=4.2 )

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

    def scn_cc_06_pawns_labeled( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_06_pawns_labeled', bt, width=3.2, height=3.2 )

        start_N = (0, 0)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_p_A = (1, 1)
        scene.board.set_piece( *start_p_A, piece=-PieceType.Pawn )

        start_p_B = (1, 2)
        scene.board.set_piece( *start_p_B, piece=-PieceType.Pawn )

        scene.append_text( "A", *start_p_A, corner=Corner.UpperRight, mark_type=MarkType.Action )
        scene.append_text( "B", *start_p_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        legal_p_A = (1, 0)
        blocked_p_A = (2, 0)
        scene.append_text( "1", *start_N, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "2", *legal_p_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "3", *blocked_p_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )

        scene.append_arrow( *( start_p_A + start_N ), mark_type=MarkType.Action )
        scene.append_arrow( *( start_p_A + legal_p_A ), mark_type=MarkType.Legal )
        scene.append_arrow( *( start_p_A + blocked_p_A ), mark_type=MarkType.Blocked )

        return scene

    def scn_cc_07_knight_marked( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_07_knight_marked', bt, width=3.2, height=3.2 )

        start_N = (0, 0)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_p_A = (1, 1)
        scene.board.set_piece( *start_p_A, piece=-PieceType.Pawn )

        start_p_B = (1, 2)
        scene.board.set_piece( *start_p_B, piece=-PieceType.Pawn )

        legal_N = (2, 1)
        scene.append_field_marker( *legal_N, mark_type=MarkType.Legal )
        scene.append_field_marker( *start_p_B, mark_type=MarkType.Action )

        scene.append_text( "A", *start_p_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_p_B, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        return scene

    def scn_cc_08_ownership( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_08_ownership', bt, width=3.2, height=4.2 )

        start_N = (0, 1)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_P_A = (0, 2)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_P_B = (1, 3)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_b = (2, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        scene.append_arrow( *( start_N + start_P_B ), mark_type=MarkType.Blocked )
        scene.append_arrow( *( start_N + start_b ), mark_type=MarkType.Action )

        return scene

    def scn_cc_09_tags_rushing( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_09_tags_rushing', bt, width=3.2, height=4.2 )

        start_P = (1, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (2, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # P -->
        coords_P_ = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=2 )

        for i, arrow in enumerate( coords_P_() ):
            mark_type = MarkType.Action if i == 0 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_field_marker( *start_P, mark_type=MarkType.Action )

        return scene


    # TODO :: TEMP :: DEBUG :: DELETE

    #
    # Checking opponent's King

    def scn_cot_024_checking_king_init( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_024_checking_king_init', bt, height=6.3 )

        start_H = (1, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_p = (5, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_n = (9, 2)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (13, 3)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_r = (17, 4)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_k = (21, 5)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        # H -->
        coords_H_ = GS.gen_steps( start=start_H, rels=[ (4, 1), ], include_prev=True, count=5 )

        for i, arrow in enumerate( coords_H_() ):
            mark_type = MarkType.Action if i < 4 else \
                        MarkType.Illegal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_cot_025_checking_king_gap( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_025_checking_king_gap', bt, height=6.3 )

        start_H = (1, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_p = (5, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_n = (9, 2)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (13, 3)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        # start_r = (17, 4)
        # scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_k = (21, 5)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        # H -->
        coords_H_ = GS.gen_steps( start=start_H, rels=[ (4, 1), ], include_prev=True, count=4 )

        for i, arrow in enumerate( coords_H_() ):
            mark_type = MarkType.Action if i < 3 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_cot_026_checking_king_blocked( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_026_checking_king_blocked', bt, height=6.3 )

        start_H = (1, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_p = (5, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_n = (9, 2)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (13, 3)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_A = (17, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_k = (21, 5)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        # H -->
        coords_H_ = GS.gen_steps( start=start_H, rels=[ (4, 1), ], include_prev=True, count=5 )

        for i, arrow in enumerate( coords_H_() ):
            mark_type = MarkType.Action if i < 4 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene





    # TODO :: TEMP :: DEBUG :: DELETE
