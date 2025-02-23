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


    # TODO :: DEBUG :: MOVE

    # def scn_hd_60_multiple_rushes_init( self, bt=BoardType.HemerasDawn ):
    #
    #     scene = Scene( 'scn_hd_60_multiple_rushes_init', bt, width=7.3, height=10.3 )
    #
    #     start_P = (1, 1)
    #     scene.board.set_piece( *start_P, piece=PieceType.Pawn )
    #
    #     start_W = (1, 4)
    #     scene.board.set_piece( *start_W, piece=PieceType.Wave )
    #
    #     start_O = (3, 4)
    #     scene.board.set_piece( *start_O, piece=PieceType.Scout )
    #
    #     start_p = (0, 3)
    #     scene.board.set_piece( *start_p, piece=-PieceType.Pawn )
    #
    #     start_g = (4, 6)
    #     scene.board.set_piece( *start_g, piece=-PieceType.Grenadier )
    #
    #     # P --> W
    #     coords_P_W = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=3 )
    #     for i, arrow in enumerate( coords_P_W() ):
    #         mark_type = MarkType.Action if i == 2 else \
    #                     MarkType.Legal
    #         scene.append_arrow( *arrow, mark_type=mark_type )
    #
    #     # W --> O
    #     coords_W_O = GS.gen_steps( start=start_W, rels=[ (1, 0), ], include_prev=True, count=2 )
    #     for i, arrow in enumerate( coords_W_O() ):
    #         mark_type = MarkType.Action if i == 1 else \
    #                     MarkType.Legal
    #         scene.append_arrow( *arrow, mark_type=mark_type )
    #
    #     # O -->
    #     coords_O_ = GS.gen_steps( start=start_O, rels=[ (0, 1), ], include_prev=True, count=3 )
    #     for i, arrow in enumerate( coords_O_() ):
    #         scene.append_arrow( *arrow, mark_type=MarkType.Legal )
    #
    #     scene.append_field_marker( *start_P, mark_type=MarkType.Action )
    #     scene.append_field_marker( *start_O, mark_type=MarkType.Action )
    #
    #     return scene
    #
    #
    # def scn_hd_61_multiple_rushes_end( self, bt=BoardType.HemerasDawn ):
    #
    #     scene = Scene( 'scn_hd_61_multiple_rushes_end', bt, width=7.3, height=10.3 )
    #
    #     prev_P = (1, 1)
    #     prev_W = (1, 4)
    #     prev_O = (3, 4)
    #     prev_p = (0, 3)
    #     prev_g = (4, 6)
    #
    #     start_P = prev_W
    #     scene.board.set_piece( *start_P, piece=PieceType.Pawn )
    #
    #     start_W = prev_O
    #     scene.board.set_piece( *start_W, piece=PieceType.Wave )
    #
    #     start_O = (3, 7)
    #     scene.board.set_piece( *start_O, piece=PieceType.Scout )
    #
    #     start_p = prev_p
    #     scene.board.set_piece( *start_p, piece=-PieceType.Pawn )
    #
    #     start_g = prev_g
    #     scene.board.set_piece( *start_g, piece=-PieceType.Grenadier )
    #
    #     scene.append_arrow( *GS.append_pos_rel( prev_p, 1, -1 ), mark_type=MarkType.Action )
    #     scene.append_arrow( *GS.append_pos_rel( prev_g, -1, -1 ), mark_type=MarkType.Action )
    #
    #     scene.append_field_marker( *start_P, mark_type=MarkType.Legal )
    #     scene.append_field_marker( *start_O, mark_type=MarkType.Legal )
    #
    #     return scene


    def scn_n_31_multiple_rushes_init( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_31_multiple_rushes_init', bt )

        start_P_A = (2, 1)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_W_A = (2, 8)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (6, 8)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_P_B = (6, 1)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_W_C = (6, 4)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (10, 4)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_P_C = (10, 1)
        scene.board.set_piece( *start_P_C, piece=PieceType.Pawn )

        start_p_A = (1, 5)
        scene.board.set_piece( *start_p_A, piece=-PieceType.Pawn )

        start_p_B = (7, 3)
        scene.board.set_piece( *start_p_B, piece=-PieceType.Pawn )

        start_p_C = (11, 4)
        scene.board.set_piece( *start_p_C, piece=-PieceType.Pawn )

        # P(A) --> W(A)
        coords_PA_WA = GS.gen_steps( start=start_P_A, rels=[ (0, 1), ], include_prev=True, count=7 )
        for i, arrow in enumerate( coords_PA_WA() ):
            mark_type = MarkType.Action if i == 6 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> W(B)
        coords_WA_WB = GS.gen_steps( start=start_W_A, rels=[ (1, 0), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_WA_WB() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(B) --> P(B)
        coords_WB_PB = GS.gen_steps( start=start_W_B, rels=[ (0, -1), ], include_prev=True, count=7 )
        for i, arrow in enumerate( coords_WB_PB() ):
            mark_type = MarkType.Blocked if i == 3 else \
                        MarkType.Action if i == 6 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_P_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "B", *start_P_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "C", *start_P_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P_A, mark_type=MarkType.Action )
        scene.append_field_marker( *start_P_B, mark_type=MarkType.Action )
        scene.append_field_marker( *start_P_C, mark_type=MarkType.Action )

        return scene


    def scn_n_32_multiple_rushes_2nd_pawn( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_32_multiple_rushes_2nd_pawn', bt )

        # prev_P_A = (2, 1)
        prev_W_A = (2, 8)
        prev_W_B = (6, 8)
        prev_P_B = (6, 1)

        start_P_A = prev_W_A
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_W_A = prev_W_B
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = prev_P_B
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_P_B = prev_P_B
        # scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_W_C = (6, 4)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (10, 4)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_P_C = (10, 1)
        scene.board.set_piece( *start_P_C, piece=PieceType.Pawn )

        start_p_A = (1, 5)
        scene.board.set_piece( *start_p_A, piece=-PieceType.Pawn )

        start_p_B = (7, 3)
        scene.board.set_piece( *start_p_B, piece=-PieceType.Pawn )

        start_p_C = (11, 4)
        scene.board.set_piece( *start_p_C, piece=-PieceType.Pawn )

        # P(B) --> W(C)
        coords_PB_WC = GS.gen_steps( start=start_P_B, rels=[ (0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_PB_WC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(C) --> W(D)
        coords_WC_WD = GS.gen_steps( start=start_W_C, rels=[ (1, 0), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_WC_WD() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(D) --> P(C)
        coords_WD_PC = GS.gen_steps( start=start_W_D, rels=[ (0, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WD_PC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_P_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        # scene.append_text( "B", *start_P_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "C", *start_P_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P_A, mark_type=MarkType.Legal )
        # scene.append_field_marker( *start_P_B, mark_type=MarkType.Action )
        scene.append_field_marker( *start_P_C, mark_type=MarkType.Action )

        return scene


    def scn_n_33_multiple_rushes_3rd_pawn( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_33_multiple_rushes_3rd_pawn', bt )

        prev_W_C = (6, 4)
        prev_W_D = (10, 4)
        prev_P_C = (10, 1)

        start_P_A = (2, 8)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_W_A = (6, 8)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (6, 1)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_P_B = prev_W_C
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_W_C = prev_W_D
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = prev_P_C
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_P_C = prev_P_C
        # scene.board.set_piece( *start_P_C, piece=PieceType.Pawn )

        start_p_A = (1, 5)
        scene.board.set_piece( *start_p_A, piece=-PieceType.Pawn )

        start_p_B = (7, 3)
        scene.board.set_piece( *start_p_B, piece=-PieceType.Pawn )

        start_p_C = (11, 4)
        scene.board.set_piece( *start_p_C, piece=-PieceType.Pawn )

        # P(C) --> W(C)
        coords_PC_WC = GS.gen_steps( start=start_P_C, rels=[ (0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_PC_WC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(C) -->
        coords_WC_ = GS.gen_steps( start=start_W_C, rels=[ (0, 1), ], include_prev=True, count=7 )
        for i, arrow in enumerate( coords_WC_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "A", *start_P_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_P_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        # scene.append_text( "C", *start_P_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P_A, mark_type=MarkType.Legal )
        scene.append_field_marker( *start_P_B, mark_type=MarkType.Legal )
        # scene.append_field_marker( *start_P_C, mark_type=MarkType.Action )

        return scene


    def scn_n_34_multiple_rushes_end( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_34_multiple_rushes_end', bt )

        start_P_A = (2, 8)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_W_A = (6, 8)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (6, 1)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_P_B = (6, 4)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_W_C = (10, 11)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (10, 1)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_P_C = (10, 4)
        scene.board.set_piece( *start_P_C, piece=PieceType.Pawn )

        start_p_A = (1, 5)
        scene.board.set_piece( *start_p_A, piece=-PieceType.Pawn )

        start_p_B = (7, 3)
        scene.board.set_piece( *start_p_B, piece=-PieceType.Pawn )

        start_p_C = (11, 4)
        scene.board.set_piece( *start_p_C, piece=-PieceType.Pawn )

        # --> P(A)
        coords_PA_ = GS.gen_steps( end=start_P_A, rels=[ (0, 1), ], include_prev=True, count=7 )
        for i, arrow in enumerate( coords_PA_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # --> P(B)
        coords_PB_ = GS.gen_steps( end=start_P_B, rels=[ (0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_PB_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # --> P(C)
        coords_PC_ = GS.gen_steps( end=start_P_C, rels=[ (0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_PC_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # p(A) --> *
        scene.append_arrow( *GS.append_pos_rel( start_p_A, 1, -1 ), mark_type=MarkType.Action )

        # p(B) --> *
        scene.append_arrow( *GS.append_pos_rel( start_p_B, -1, -1 ), mark_type=MarkType.Action )

        # p(C) --> *
        scene.append_arrow( *GS.append_pos_rel( start_p_C, -1, -1 ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_P_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_P_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_P_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P_A, mark_type=MarkType.Legal )
        scene.append_field_marker( *start_P_B, mark_type=MarkType.Legal )
        scene.append_field_marker( *start_P_C, mark_type=MarkType.Legal )

        return scene



    # TODO :: DEBUG :: MOVE
