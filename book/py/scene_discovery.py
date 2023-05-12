#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneDiscoveryMixin:

    #
    # Movement

    def scn_d_01_monolith_patterns(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_01_monolith_patterns', bt) # , width=10, height=10)

        start_M = (11, 11)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )
        steps = 4

        # M --> x x
        for step in range( steps + 1 ):
            rels = GS.gen_monolith_default_steps( step )
            for divergence, rel in enumerate( rels ):
                coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
                for i, pos in enumerate( coords_M_() ):
                    offset = step % steps
                    mark_type = MarkType.Action if offset == 1 else \
                                MarkType.Blocked if offset == 2 else \
                                MarkType.Illegal if offset == 3 else \
                                MarkType.Legal
                    scene.append_field_marker( *pos, mark_type=mark_type )

                    if divergence == 0:
                        scene.append_text( str( step ), *pos, corner=Corner.UpperRightFieldMarker, mark_type=mark_type )

        return scene

    def scn_d_02_monolith_first_step(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_02_monolith_first_step', bt) # , width=10, height=10)

        start_M = (4, 4)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        start_W = (6, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_P_1 = (7, 2)
        scene.board.set_piece( *start_P_1, piece=PieceType.Pawn )

        start_P_2 = (11, 8)
        scene.board.set_piece( *start_P_2, piece=PieceType.Pawn )

        start_P_3 = (10, 7)
        scene.board.set_piece( *start_P_3, piece=PieceType.Pawn )

        start_P_4 = (12, 7)
        scene.board.set_piece( *start_P_4, piece=PieceType.Pawn )

        start_w = (3, 6)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_p_1 = (2, 7)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (4, 6)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        # M --> x x
        rels = GS.gen_monolith_default_steps( 1 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                mark_type = MarkType.Illegal if divergence in [2, 7] else \
                            MarkType.Legal
                scene.append_field_marker( *pos, mark_type=mark_type )
                # scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )

        return scene

    def scn_d_03_monolith_second_step(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_03_monolith_second_step', bt) # , width=10, height=10)

        prev_M = (4, 4)
        start_M = (6, 5)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        start_W = (6, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_P_1 = (7, 2)
        scene.board.set_piece( *start_P_1, piece=PieceType.Pawn )

        start_P_2 = (11, 8)
        scene.board.set_piece( *start_P_2, piece=PieceType.Pawn )

        start_P_3 = (10, 7)
        scene.board.set_piece( *start_P_3, piece=PieceType.Pawn )

        start_P_4 = (12, 7)
        scene.board.set_piece( *start_P_4, piece=PieceType.Pawn )

        start_w = (3, 6)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_p_1 = (2, 7)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (4, 6)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        # M --> x x x x
        rels = GS.gen_monolith_default_steps( 2 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                scene.append_field_marker( *pos, mark_type=MarkType.Legal )
                # scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )

        scene.append_arrow( *(prev_M + start_M), mark_type=MarkType.Blocked )

        # M --> x x
        rels = GS.gen_monolith_default_steps( 1 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        scene.append_text( "M", *prev_M, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        return scene

    def scn_d_04_monolith_third_step(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_04_monolith_third_step', bt) # , width=10, height=10)

        prev_M = (4, 4)
        prev_2_M = (6, 5)
        start_M = (7, 9)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        start_W = (6, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_P_1 = (7, 2)
        scene.board.set_piece( *start_P_1, piece=PieceType.Pawn )

        start_P_2 = (11, 8)
        scene.board.set_piece( *start_P_2, piece=PieceType.Pawn )

        start_P_3 = (10, 7)
        scene.board.set_piece( *start_P_3, piece=PieceType.Pawn )

        start_P_4 = (12, 7)
        scene.board.set_piece( *start_P_4, piece=PieceType.Pawn )

        start_w = (3, 6)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_p_1 = (2, 7)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (4, 6)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        # M --> x x x x x x
        rels = GS.gen_monolith_default_steps( 3 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                mark_type = MarkType.Illegal if divergence in [13, 14, 17, 22] else \
                            MarkType.Legal
                scene.append_field_marker( *pos, mark_type=mark_type )
                # scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )

        scene.append_arrow( *(prev_M + prev_2_M), mark_type=MarkType.Blocked )
        scene.append_arrow( *(prev_2_M + start_M), mark_type=MarkType.Blocked )

        # M --> x x x x
        rels = GS.gen_monolith_default_steps( 2 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        scene.append_text( "M", *prev_M, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        return scene

    def scn_d_05_monolith_fourth_step(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_05_monolith_fourth_step', bt) # , width=10, height=10)

        prev_M = (4, 4)
        prev_2_M = (6, 5)
        prev_3_M = (7, 9)
        start_M = (13, 10)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        start_W = (6, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_P_1 = (7, 2)
        scene.board.set_piece( *start_P_1, piece=PieceType.Pawn )

        start_P_2 = (11, 8)
        scene.board.set_piece( *start_P_2, piece=PieceType.Pawn )

        start_P_3 = (10, 7)
        scene.board.set_piece( *start_P_3, piece=PieceType.Pawn )

        start_P_4 = (12, 7)
        scene.board.set_piece( *start_P_4, piece=PieceType.Pawn )

        start_w = (3, 6)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_p_1 = (2, 7)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (4, 6)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        # M --> x x x x x x x x
        rels = GS.gen_monolith_default_steps( 4 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                scene.append_field_marker( *pos, mark_type=MarkType.Legal )
                # scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )

        scene.append_arrow( *(prev_M + prev_2_M), mark_type=MarkType.Blocked )
        scene.append_arrow( *(prev_2_M + prev_3_M), mark_type=MarkType.Blocked )
        scene.append_arrow( *(prev_3_M + start_M), mark_type=MarkType.Blocked )

        # M --> x x x x x x
        rels = GS.gen_monolith_default_steps( 3 )
        for divergence, rel in enumerate( rels ):
            coords_M_ = GS.gen_steps( start=start_M, rels=[ rel, ], include_prev=False, count=1 )
            for i, pos in enumerate( coords_M_() ):
                scene.append_text( str( divergence + 1 ), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        scene.append_text( "M", *prev_M, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        return scene

    #
    # Off-board Monolith

    def scn_d_06_monolith_off_board(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_06_monolith_off_board', bt, x=4.0, y=-1.0)

        start_M = (22, 6) # rel == (2, 1) --> left step
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        adder = GS.adder(start_M, include_prev=False)
        scene.board.set_piece(*adder( -2, 1 ), piece=PieceType.Pawn)
        scene.board.set_piece(*adder( 1, 1 ), piece=PieceType.Pawn)
        scene.board.set_piece(*adder( 0, 1 ), piece=-PieceType.Pawn)
        scene.board.set_piece(*adder( 1, 1 ), piece=-PieceType.Pawn)
        scene.board.set_piece(*adder( 1, 2 ), piece=PieceType.Pawn)

        adder_2 = GS.adder(start_M, include_prev=True)
        scene.append_arrow( *adder_2( 1, 2 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_2( 3, 2 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_2( -5, 2 ), mark_type=MarkType.Illegal )

        return scene

    #
    # Monolith is noble

    def scn_d_07_monolith_is_noble(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_07_monolith_is_noble', bt)

        #
        # upper-left corner

        start_M_A = (2, 21)
        scene.board.set_piece(*start_M_A, piece=PieceType.Monolith)

        start_k = (3, 20)
        scene.board.set_piece(*start_k, piece=-PieceType.King)

        scene.append_arrow( *(start_k + start_M_A), mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_M_A, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )

        #
        # upper-right corner

        start_M_B = (19, 21)
        scene.board.set_piece(*start_M_B, piece=PieceType.Monolith)

        start_a = (17, 21)
        scene.board.set_piece(*start_a, piece=-PieceType.Pyramid)

        start_b = (22, 16)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

        coords_B_A = GS.gen_steps([(-1, 1), ], start=start_b, include_prev=True, count=5)
        for i, arrow in enumerate( coords_B_A() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow(*arrow, mark_type=mark_type)

        coords_A_MB = GS.gen_steps([(1, 0), ], start=start_a, include_prev=True, count=2)
        for i, arrow in enumerate( coords_A_MB() ):
            mark_type = MarkType.Illegal if i == 1 else \
                        MarkType.Legal
            scene.append_arrow(*arrow, mark_type=mark_type)

        scene.append_text( "B", *start_M_B, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )

        #
        # lower-left corner

        start_M_C = (3, 1)
        scene.board.set_piece(*start_M_C, piece=PieceType.Monolith)

        start_W = (5, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_Q = (2, 6)
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        coords_Q_W = GS.gen_steps([(1, -1), ], start=start_Q, include_prev=True, count=3)
        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow(*arrow, mark_type=mark_type)

        coords_W_MC = GS.gen_steps([(-1, -1), ], start=start_W, include_prev=True, count=2)
        for i, arrow in enumerate( coords_W_MC() ):
            mark_type = MarkType.Illegal if i == 1 else \
                        MarkType.Legal
            scene.append_arrow(*arrow, mark_type=mark_type)

        scene.append_text( "C", *start_M_C, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )

        #
        # lower-right corner

        start_M_D = (19, 5)
        scene.board.set_piece(*start_M_D, piece=PieceType.Monolith)

        start_S = (16, 4)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        gen_S_ = GS.gen_steps( start=start_S, rels=[ (1, 1), (1, -1), ], include_prev=True, count=6 )
        for index, coords in enumerate( gen_S_() ):
            mark_type = MarkType.Legal if index < 2 else \
                        MarkType.Illegal if index == 2 else \
                        MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_text( "D", *start_M_D, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )

        return scene

    #
    # Trance-journey interaction

    def scn_d_08_monolith_shaman_interaction(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_08_monolith_shaman_interaction', bt, x=-4, y=0)

        start = (4, 12)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(4, 17, piece=PieceType.Monolith)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        # start_W = (3, 10)
        # scene.board.set_piece(*start_W, piece=PieceType.Wave)

        startH = (3, 11) # (5, 9)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        # scene.append_arrow( *(startH + start_W), mark_type=MarkType.Action )
        # scene.append_arrow( *(start_W + start), mark_type=MarkType.Action )
        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Illegal if i in [1, 3, 7] else MarkType.Legal
            # aba(str(i + 1), mark_type=mark_type)
            aba(None, mark_type=mark_type)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    #
    # Monolith is opaque

    def scn_d_09_monolith_is_opaque(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_09_monolith_is_opaque', bt)

        start_M1 = (9, 6)
        start_M2 = (16, 20)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(5, 8, piece=PieceType.Queen)
        scene.board.set_piece(14, 22, piece=-PieceType.Pawn)

        #
        # Pegasus
        start_E = (17, 7)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_abs_pos_E = GS.gen_steps([(-1, -2), ], start=start_E, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_E() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (15, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=7)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Blocked if i > 2 else \
                        MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Bishop
        start_B = (19, 17)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        gen_abs_pos_B = GS.gen_steps([(-1, 1), ], start=start_B, include_prev=True, count=6)

        for i, pos in enumerate( gen_abs_pos_B() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Blocked if i > 2 else \
                        MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    #
    # Teleporting

    def scn_d_10_teleport_via_monolith(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_10_teleport_via_monolith', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.board.set_piece(0, 7, piece=PieceType.Wave)

        #
        # Monolith 1
        start_M1 = (1, 6)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        #
        # Bishop
        start_B = (4, 3)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        coords = GS.gen_steps([(-1, 1), ], start=start_B, include_prev=True, count=3)
        for index, pos in enumerate( coords() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # King
        start_K = (2, 7)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        scene.append_arrow( *(start_K + start_M1), mark_type=MarkType.Illegal )

        #
        # Monolith 2
        start_M2 = (22, 17)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M2, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_1, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_2, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        #
        # Star 3
        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_3, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_3() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        #
        # Star 4
        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_4, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_4() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_11_teleport_via_star(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_11_teleport_via_star', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.board.set_piece(0, 7, piece=PieceType.Wave)
        scene.board.set_piece(2, 7, piece=PieceType.King)

        #
        # Bishop
        start_B = (4, 19)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        coords = GS.gen_steps([(-1, 1), ], start=start_B, include_prev=True, count=4)
        for index, pos in enumerate( coords() ):
            mark_type = MarkType.Action if index == 3 else MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # Monolith 1
        start_M1 = (1, 6)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M1, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            if i not in [1, 3]:
                scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Monolith 2
        start_M2 = (22, 17)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M2, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_1, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_2, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        return scene

    #
    # Teleporting Wave

    def scn_d_12_teleport_wave_via_star(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_12_teleport_wave_via_star', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.board.set_piece(1, 6, piece=PieceType.Monolith)
        scene.board.set_piece(22, 17, piece=PieceType.Monolith)

        scene.board.set_piece(17, 3, piece=PieceType.Bishop)

        #
        # Wave
        start_W = (6, 20)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Pegasus
        start_E = (4, 16)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_abs_pos_E = GS.gen_steps([(1, 2), ], start=start_E, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_E() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_T_3, include_prev=True, bounds=scene.board.get_position_limits()) # , count=3)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_T_4, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_T_3, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_13_teleport_wave_via_monolith(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_13_teleport_wave_via_monolith', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_M1 = (9, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(5, 8, piece=PieceType.Queen)
        scene.board.set_piece(16, 20, piece=PieceType.Bishop)

        #
        # Pegasus
        start_E = (17, 7)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_abs_pos_E = GS.gen_steps([(-1, -2), ], start=start_E, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_E() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (15, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=7)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Blocked if i > 2 else MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_14_teleported_wave_blocked(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_14_teleported_wave_blocked', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_M1 = (9, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(5, 8, piece=PieceType.Queen)

        #
        # Pawns
        gen_abs_pos_P = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_P() ):
            piece = -PieceType.King if i == 5 else -PieceType.Pawn
            scene.board.set_piece(*pos, piece=piece)

        #
        # Pegasus
        start_E = (17, 7)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_abs_pos_E = GS.gen_steps([(-1, -2), ], start=start_E, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_E() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (15, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=7)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Blocked if i > 2 else MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            scene.append_arrow(*pos, mark_type=MarkType.Blocked)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_15_wave_teleported_off_board(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_15_wave_teleported_off_board', bt, x=-4.0, y=1.0)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_M1 = (1, 6)
        start_M2 = (1, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        #
        # Wave
        start_W = (7, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Pegasus
        start_E = (9, 7)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_abs_pos_E = GS.gen_steps([(-1, -2), ], start=start_E, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_E() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            scene.append_arrow(*pos, mark_type=MarkType.Blocked)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_16_wave_teleport_on_and_off_board(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_16_wave_teleport_on_and_off_board', bt, x=-4.0, y=1.0)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_M1 = (0, 6)
        start_M2 = (0, 15)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(0, 19, piece=PieceType.Bishop)

        #
        # Serpent
        start_S = (3, 3)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        gen_abs_pos_W = GS.gen_steps([(-1, -1), (-1, 1), ], start=start_S, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (0, 2)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-1, 1), (1, 1), ], start=start_W, include_prev=True, count=4)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Legal
            if i == 3:
                mark_type = MarkType.Action
            elif i % 2 == 0:
                mark_type = MarkType.Illegal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-1, 1), (1, 1), ], start=start_M2, include_prev=True, count=10)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Illegal if i > 8 or i % 2 == 0 else MarkType.Legal
            if i in [3, 7, ]:
                mark_type = MarkType.Action
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    #
    # Teleportation cascade

    def scn_d_17_teleporting_wave_cascade(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_17_teleporting_wave_cascade', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_M1 = (1, 4)
        start_M2 = (12, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(17, 3, piece=PieceType.Bishop)

        #
        # Wave
        start_W = (7, 1)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Pegasus
        start_E = (9, 5)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_abs_pos_E = GS.gen_steps([(-1, -2), ], start=start_E, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_E() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, count=6) # bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 5 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_T_3, include_prev=True, bounds=scene.board.get_position_limits()) # , count=3)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("C", *start_T_4, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("D", *start_T_3, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    #
    # Steps after Teleportation

    def scn_d_18_steps_after_teleport_init(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_18_steps_after_teleport_init', bt)

        start_U = (13, 14)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_W = (12, 16)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_M_1 = (20, 20)
        scene.board.set_piece( *start_M_1, piece=PieceType.Monolith )

        start_M_2 = (2, 3)
        scene.board.set_piece( *start_M_2, piece=PieceType.Monolith )

        # U --> W
        scene.append_arrow( *( start_U + start_W ), mark_type=MarkType.Legal )

        # W --> M(1) # (2, 3), (2, -1)
        coords_W_M1 = GS.gen_steps( start=start_W, rels=[ (2, 3), (2, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_W_M1() ):
            mark_type = MarkType.Action if i % 2 != 0 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_d_19_steps_after_teleport_end(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_19_steps_after_teleport_end', bt)

        start_U = (12, 16)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_M_1 = (20, 20)
        scene.board.set_piece( *start_M_1, piece=PieceType.Monolith )

        start_M_2 = (2, 3)
        scene.board.set_piece( *start_M_2, piece=PieceType.Monolith )

        # W ---> # (2, 3), (2, -1)
        coords_W_ = GS.gen_steps( start=start_M_2, rels=[ (2, -1), (2, 3), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_() ):
            mark_type = MarkType.Action if i % 2 == 0 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Syzygy

    def scn_d_20_syzygy_explain(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_20_syzygy_explain', bt)

        start_T_1 = (0, 0)
        start_T_2 = (23, 23)
        start_T_3 = (23, 0)
        start_T_4 = (0, 23)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_M1 = (17, 2)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        start_M2 = (2, 5)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.append_arrow( *GS.append_tpl_rel(start_M1, 1, -2), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_tpl_rel(start_M2, -2, 1), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_tpl_rel(start_M2, 1, -2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(1, 0), ], start_T_1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Legal, end_pointer=False )

        gen = GS.gen_steps( [(1, 1), ], start_T_1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Illegal, end_pointer=False )

        gen = GS.gen_steps( [(0, 1), ], start_T_1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked, end_pointer=False )

        return scene

    def scn_d_21_syzygy_2_stars_init(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_21_syzygy_2_stars_init', bt)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(22, 1, piece=PieceType.Pawn)
        scene.board.set_piece(20, 3, piece=PieceType.King)
        scene.board.set_piece(19, 14, piece=PieceType.Knight)
        scene.board.set_piece(16, 7, piece=PieceType.Bishop)
        scene.board.set_piece(15, 8, piece=PieceType.Wave)

        scene.board.set_piece(1, 22, piece=-PieceType.Pawn)
        scene.board.set_piece(3, 20, piece=-PieceType.King)
        scene.board.set_piece(4, 11, piece=-PieceType.Queen)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        start_M = (13, 13)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(1, 6, piece=PieceType.Monolith)

        end_M = (12, 11)
        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Action )

        return scene

    def scn_d_22_syzygy_2_stars_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_22_syzygy_2_stars_steps', bt)

        start_T = (23, 0)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(22, 1, piece=PieceType.Pawn)
        scene.board.set_piece(20, 3, piece=PieceType.King)
        scene.board.set_piece(19, 14, piece=PieceType.Knight)
        scene.board.set_piece(16, 7, piece=PieceType.Bishop)
        scene.board.set_piece(15, 8, piece=PieceType.Wave)

        scene.board.set_piece(1, 22, piece=-PieceType.Pawn)
        scene.board.set_piece(3, 20, piece=-PieceType.King)
        scene.board.set_piece(4, 11, piece=-PieceType.Queen)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        start_M = (12, 11)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(1, 6, piece=PieceType.Monolith)

        #
        # diagonal arrows
        gen_abs_pos = GS.gen_steps([(-1, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False )

        #
        # field markers
        gen_abs_pos_1 = GS.gen_steps([(-1, 1)], start=start_T, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_1() ):
            mark_type = None
            if i in [0, 21]:
                mark_type = MarkType.Blocked
            elif i in [2, 10, 19, 22]:
                mark_type = MarkType.Illegal
            elif i in [15, ]:
                mark_type = MarkType.Legal
            elif i in [6, 7]:
                mark_type = MarkType.Action

            if mark_type is not None:
                scene.append_field_marker( *pos, mark_type=mark_type )

        scene.append_field_marker( *start_T, mark_type=MarkType.Illegal )

        return scene

    def scn_d_23_syzygy_2_monoliths_init(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_23_syzygy_2_monoliths_init', bt)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(16, 9, piece=PieceType.Bishop)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        scene.board.set_piece(21, 7, piece=PieceType.Knight)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(6, 2, piece=PieceType.Monolith)

        end_M = (12, 4)
        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Action )

        return scene

    def scn_d_24_syzygy_2_monoliths_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_24_syzygy_2_monoliths_steps', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(16, 9, piece=PieceType.Bishop)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        scene.board.set_piece(21, 7, piece=PieceType.Knight)

        start_M = (12, 4)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(6, 2, piece=PieceType.Monolith)

        #
        # diagonal arrows
        gen_abs_pos = GS.gen_steps([(3, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        #
        # texts, field markers
        gen_abs_pos_1 = GS.gen_steps([(3, 1)], start=start_T, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_1() ):
            scene.append_text(str(i+2), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

            mark_type = None
            if i in [1, 3, ]:
                mark_type = MarkType.Illegal
            elif i in [6, ]:
                mark_type = MarkType.Action

            if mark_type is not None:
                scene.append_field_marker( *pos, mark_type=mark_type )

        scene.append_text("1", *start_T, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_field_marker( *start_T, mark_type=MarkType.Illegal )

        return scene

    #
    # Reentering syzygy

    def scn_d_25_syzygy_reentering_same_move(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_25_syzygy_reentering_same_move', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        start_M = (4, 2)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(22, 11, piece=PieceType.Monolith)

        #
        # diagonal arrows
        gen_abs_pos = GS.gen_steps([(2, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        adder = GS.adder( start_M, include_prev=True )
        scene.append_arrow( *adder(-1, 2), mark_type=MarkType.Action )
        scene.append_arrow( *adder(2, 3), mark_type=MarkType.Action )
        scene.append_arrow( *adder(5, -2), mark_type=MarkType.Action )

        return scene

    def scn_d_26_syzygy_reentering_independent(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_26_syzygy_reentering_independent', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        start_T_1 = (0, 23)
        scene.board.set_piece(*start_T_1, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        start_M = (14, 14)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        #
        # diagonal SW-NE arrows
        gen_abs_pos = GS.gen_steps([(1, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        #
        # diagonal NW-SE arrows
        gen_abs_pos = GS.gen_steps([(1, -1)], start=start_T_1, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False )

        adder = GS.adder( start_M, include_prev=True )
        scene.append_arrow( *adder(-2, 1), mark_type=MarkType.Action )
        scene.append_arrow( *adder(-1, 4), mark_type=MarkType.Action )
        scene.append_arrow( *adder(-5, -2), mark_type=MarkType.Action )

        return scene

    #
    # In opponent's figure row

    def scn_d_27_syzygy_in_opponents_figure_row(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_27_syzygy_in_opponents_figure_row', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        # start_R = (7, 23)
        # scene.board.set_piece( *start_R, piece=-PieceType.Rook )

        end_M = (12, 23)
        scene.board.set_piece(*end_M, piece=PieceType.Monolith)

        start_M = (11, 21)
        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Blocked )

        start_P = (16, 23)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        scene.append_field_marker( *start_P, mark_type=MarkType.Action )

        return scene
