#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020, 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


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

        start_I = (5, 0)
        scene.board.set_piece(*start_I, piece=PieceType.none) # Starchild was here

        end_I = (12, 12)
        scene.board.set_piece(*end_I, piece=PieceType.Starchild)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Blocked )

        return scene

    def scn_o_02_starchild_activating_own_piece_init(self, bt=BoardType.One):

        scene = Scene('scn_o_02_starchild_activating_own_piece_init', bt) # , width=9, height=9)

        start_I = (8, 3)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W_A = (23, 21)
        scene.board.set_piece(*start_W_A, piece=PieceType.Wave)

        start_W_B = (17, 7)
        scene.board.set_piece(*start_W_B, piece=PieceType.Wave)

        start_w = (2, 17)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_n = (6, 13)
        scene.board.set_piece(*start_n, piece=-PieceType.Knight)

        start_g = (9, 9)
        scene.board.set_piece(*start_g, piece=-PieceType.Pegasus)

        start_I_2 = (22, 2)
        scene.board.set_piece(*start_I_2, piece=PieceType.Starchild)

        start_A = (19, 4)
        scene.board.set_piece(*start_A, piece=PieceType.Pyramid)

        scene.append_text( "A", *start_W_A, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Blocked )

        scene.append_arrow( *(start_I + start_W_A), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + start_W_B), mark_type=MarkType.Legal )

        scene.append_arrow( *(start_I + start_I_2), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_I + start_A), mark_type=MarkType.Illegal )

        scene.append_arrow( *(start_I + start_w), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_I + start_n), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_I + start_g), mark_type=MarkType.Illegal )

        return scene

    def scn_o_03_starchild_activating_own_piece_end(self, bt=BoardType.One):

        scene = Scene('scn_o_03_starchild_activating_own_piece_end', bt) # , width=9, height=9)

        start_I = (8, 3)
        end_I = (23, 21)
        scene.board.set_piece(*end_I, piece=PieceType.Starchild)

        start_W_A = (23, 21)
        # scene.board.set_piece(*start_W_A, piece=PieceType.Wave)

        start_W_B = (17, 7)
        scene.board.set_piece(*start_W_B, piece=PieceType.Wave)

        start_w = (2, 17)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_n = (6, 13)
        scene.board.set_piece(*start_n, piece=-PieceType.Knight)

        start_g = (9, 9)
        scene.board.set_piece(*start_g, piece=-PieceType.Pegasus)

        start_I_2 = (22, 2)
        scene.board.set_piece(*start_I_2, piece=PieceType.Starchild)

        start_A = (19, 4)
        scene.board.set_piece(*start_A, piece=PieceType.Pyramid)

        # scene.append_text( "A", *start_W_A, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Blocked )

        scene.append_arrow( *(start_I + start_W_A), mark_type=MarkType.Blocked )

        scene.append_arrow( *(start_W_A + start_W_B), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_W_A + start_w), mark_type=MarkType.Action )

        scene.append_arrow( *(start_W_A + start_I_2), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_W_A + start_A), mark_type=MarkType.Illegal )

        scene.append_arrow( *(start_W_A + start_n), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_W_A + start_g), mark_type=MarkType.Illegal )

        return scene

    def scn_o_04_activating_starchild(self, bt=BoardType.One):

        scene = Scene('scn_o_04_activating_starchild', bt)

        start_Q = (22, 8)
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        start_W_A = (17, 3)
        scene.board.set_piece(*start_W_A, piece=PieceType.Wave)

        start_I = (8, 3)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W_B = (23, 21)
        scene.board.set_piece(*start_W_B, piece=PieceType.Wave)

        start_w = (2, 17)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_n = (7, 9)
        scene.board.set_piece(*start_n, piece=-PieceType.Knight)

        start_P = (21, 2)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        scene.append_text( "A", *start_W_A, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Legal )

        # Q --> W(A)
        coords_Q_WA = GS.gen_steps( start=start_Q, rels=[(-1, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_Q_WA() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> I
        coords_Q_WA = GS.gen_steps( start=start_W_A, rels=[(-1, 0), ], include_prev=True, count=9 )
        for i, arrow in enumerate( coords_Q_WA() ):
            mark_type = MarkType.Action if i == 8 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # I --> W(B)
        scene.append_arrow( *(start_I + start_W_B), mark_type=MarkType.Legal )

        # W(B) --> w
        scene.append_arrow( *(start_W_B + start_w), mark_type=MarkType.Legal )

        # w --># n
        scene.append_arrow( *(start_w + start_n), mark_type=MarkType.Illegal )

        return scene

    def scn_o_05_neighboring_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_05_neighboring_fields', bt, width=5, height=5)

        start_I = (2, 2)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        for index, coords in enumerate( gen() ):
            scene.append_text( str(index + 1), *coords, mark_type=MarkType.Legal )

        return scene

    def scn_o_06_starchild_activating_on_neighboring_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_06_starchild_activating_on_neighboring_fields', bt, width=5, height=5)

        start_I = (2, 2)
        start_b = (2, 3)
        start_G = (3, 1)
        start_K = (1, 1)

        scene.board.set_piece(*start_I, piece=PieceType.Starchild)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)
        scene.board.set_piece(*start_G, piece=PieceType.Pyramid)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        scene.append_arrow( *(start_I + start_G), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + start_K), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Illegal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 7 else MarkType.Illegal if index in [2, 5] else MarkType.Blocked
            scene.append_text( str(index + 1), *coords, mark_type=mark_type )

        return scene

    def scn_o_07_starchild_activating_wave_on_neighboring_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_07_starchild_activating_wave_on_neighboring_fields', bt, width=5, height=5)

        start_I = (2, 2)
        start_W = (1, 1)
        start_G = (0, 2)
        start_B = (17, 17)

        scene.board.set_piece(*start_I, piece=PieceType.Starchild)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_T_1 = (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + start_T_1), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_W + start_G), mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_W, count=1 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Legal # if index in [3, 5] else MarkType.Blocked
            scene.append_text( str(index + 1), *coords, mark_type=mark_type )

        return scene

    def scn_o_08_starchild_moving_star_init(self, bt=BoardType.One):

        scene = Scene('scn_o_08_starchild_moving_star_init', bt, width=5, height=5)

        start_I = (0, 1)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        start_T_1 = (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        # scene.append_arrow( *((2, 2) + start_I), mark_type=MarkType.Blocked )
        scene.append_arrow( *(start_I + start_T_1), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Legal if index == 0 else MarkType.Action if index == 6 else MarkType.Blocked
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_09_starchild_moving_star_end(self, bt=BoardType.One):

        scene = Scene('scn_o_09_starchild_moving_star_end', bt, width=5, height=5)

        start_I = (0, 0)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        start_T_1 = (1, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + start_T_1), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Action if index == 0 else MarkType.Illegal if index == 1 else MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_10_starchild_moving_star_activating(self, bt=BoardType.One):

        scene = Scene('scn_o_10_starchild_moving_star_activating', bt, width=5, height=5)

        start_I = (0, 1)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        start_W = (0, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_R = (3, 3)
        scene.board.set_piece(*start_R, piece=PieceType.Rook)

        start_T_1 = (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        endT1 = (1, 0)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        gen = GS.gen_steps( [(-1, 0), ], start_R, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(0, -1), ], start_W, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Legal )

        scene.append_arrow( *(start_I + start_T_1), mark_type=MarkType.Action )
        scene.append_arrow( *(start_T_1 + endT1), mark_type=MarkType.Action )

        scene.append_text( "T", *endT1, corner=Corner.UpperRight, mark_type=MarkType.Action )

        return scene

    def scn_o_11_star_movement_blocked_init(self, bt=BoardType.One):

        scene = Scene('scn_o_11_star_movement_blocked_init', bt, width=5, height=5)

        start_I = (0, 1)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        start_W = (0, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_R_A = (3, 3)
        scene.board.set_piece(*start_R_A, piece=PieceType.Rook)

        start_R_B = (1, 0)
        scene.board.set_piece(*start_R_B, piece=PieceType.Rook)

        start_T_1 = (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        endT1 = (1, 0)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        gen = GS.gen_steps( [(-1, 0), ], start_R_A, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(0, -1), ], start_W, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Legal )

        scene.append_arrow( *(start_I + start_T_1), mark_type=MarkType.Action )
        scene.append_arrow( *(start_T_1 + endT1), mark_type=MarkType.Illegal )

        scene.append_text( "T", *endT1, corner=Corner.UpperRight, mark_type=MarkType.Illegal )

        return scene

    def scn_o_12_star_movement_blocked_end(self, bt=BoardType.One):

        scene = Scene('scn_o_12_star_movement_blocked_end', bt, width=5, height=5)

        start_R_A = (0, 3)
        scene.board.set_piece(*start_R_A, piece=PieceType.Rook)

        start_W = (0, 1)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        # start_I = (0, 1)
        # scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_P = (1, 1)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        start_R_B = (1, 0)
        scene.board.set_piece(*start_R_B, piece=PieceType.Rook)

        start_T_1 = (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        endT1 = (1, 0)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        gen = GS.gen_steps( [(-1, 0), ], end=start_R_A, include_prev=True, count=3) # bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        gen = GS.gen_steps( [(0, -1), ], end=start_W, include_prev=True, count=2) # bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        scene.append_arrow( *(start_W + start_T_1), mark_type=MarkType.Illegal )
        scene.append_arrow( *(start_T_1 + endT1), mark_type=MarkType.Illegal )

        scene.append_text( "T", *endT1, corner=Corner.UpperRight, mark_type=MarkType.Illegal )

        return scene

    def scn_o_13_starchild_not_moving_monolith_init(self, bt=BoardType.One):

        scene = Scene('scn_o_13_starchild_not_moving_monolith_init', bt, width=9, height=9)

        start_I = (5, 4)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_M = (2, 2)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.board.set_piece(2, 1, piece=PieceType.Knight)
        scene.board.set_piece(1, 3, piece=PieceType.Bishop)

        scene.append_arrow( *(start_I + start_M), mark_type=MarkType.Action )

        return scene

    def scn_o_14_starchild_not_moving_monolith_end(self, bt=BoardType.One):

        scene = Scene('scn_o_14_starchild_not_moving_monolith_end', bt, width=9, height=9)

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

    def scn_o_15_starchild_activated_wave_not_teleporting_init(self, bt=BoardType.One):

        scene = Scene('scn_o_15_starchild_activated_wave_not_teleporting_init', bt, width=9, height=9)

        start_I = (5, 1)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = (3, 6)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_T = (2, 2)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        scene.board.set_piece(2, 1, piece=PieceType.Knight)
        scene.board.set_piece(1, 3, piece=PieceType.Bishop)

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_W + start_T), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Illegal if index in [3, 6] else MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_16_starchild_activated_wave_not_teleporting_end(self, bt=BoardType.One):

        scene = Scene('scn_o_16_starchild_activated_wave_not_teleporting_end', bt, width=9, height=9)

        start_I = (3, 6)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = (1, 2)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_T = (2, 2)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        scene.board.set_piece(2, 1, piece=PieceType.Knight)
        scene.board.set_piece(1, 3, piece=PieceType.Bishop)

        scene.append_arrow( *(start_T + start_W), mark_type=MarkType.Action )

        return scene

    def scn_o_17_star_moved_wave_teleportation(self, bt=BoardType.One):

        scene = Scene('scn_o_17_star_moved_wave_teleportation', bt)

        start_W = (17, 9) # (11, 9)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_N = (16, 11) # (10, 11)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_Q = (7, 4)
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        start_T_1 = (11, 6) # (5, 6)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_N + start_W), mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(-2, -1), ], start_W, include_prev=True, count=8 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index in [2, 4] else MarkType.Legal # if index < 2 else MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_o_18_star_moved_wave_off_board(self, bt=BoardType.One):

        scene = Scene('scn_o_18_star_moved_wave_off_board', bt, x=-4)

        start_W = (11, 9)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_N = (10, 11)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (5, 6) # (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_N + start_W), mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(-2, -1), ], start_W, include_prev=True, count=3 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen = GS.gen_steps( [(-2, -1), ], start_T_4, include_prev=True, count=2 )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        return scene

    def scn_o_19_steps_after_teleport(self, bt=BoardType.One):

        scene = Scene('scn_o_19_steps_after_teleport', bt)

        start_U = (13, 14)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_W = (12, 16)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_T_A = (20, 20)
        scene.board.set_piece( *start_T_A, piece=PieceType.Star )

        start_T_B = (2, 3)
        scene.board.set_piece( *start_T_B, piece=PieceType.Star )

        # U --> W
        scene.append_arrow( *( start_U + start_W ), mark_type=MarkType.Legal )

        # W --> T(A) # (2, 3), (2, -1)
        coords_W_M1 = GS.gen_steps( start=start_W, rels=[ (2, 3), (2, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_W_M1() ):
            mark_type = MarkType.Action if i % 2 != 0 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # T(B) --W--> # (2, 3), (2, -1)
        coords_W_ = GS.gen_steps( start=start_T_B, rels=[ (2, -1), (2, 3), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_() ):
            mark_type = MarkType.Action if i % 2 == 0 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text("A", *start_T_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight)
        scene.append_text("B", *start_T_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight)

        return scene

    def scn_o_20_starchild_conversion_immunity_init(self, bt=BoardType.One):

        scene = Scene('scn_o_20_starchild_conversion_immunity_init', bt)

        start_I = (7, 22)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_a = (11, 22)
        scene.board.set_piece(*start_a, piece=-PieceType.Pyramid)

        start_b = (18, 15)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

        scene.append_arrow( *(start_b + start_a), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_a + start_I), mark_type=MarkType.Action )

        return scene

    def scn_o_21_trance_journey_init_starchild(self, bt=BoardType.One):

        scene = Scene('scn_o_21_trance_journey_init_starchild', bt, width=9, height=9)

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

        # dark Starchild --> light Starchild
        scene.append_arrow( *(start_i + start_I), mark_type=MarkType.Action )

        # light Starchild --> dark Bishop
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Legal )

        return scene

    def scn_o_22_trance_journey_init_shaman(self, bt=BoardType.One):

        scene = Scene('scn_o_22_trance_journey_init_shaman', bt, width=9, height=9)

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

        # dark Shaman --> dark Wave --> light Wave --> light Shaman
        scene.append_arrow( *(start_h + start_w), mark_type=MarkType.Action )
        scene.append_arrow( *(start_w + start_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + start_I), mark_type=MarkType.Action )

        # light Starchild --> dark Bishop
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Legal )

        return scene

    def scn_o_23_trance_journey_started_by_shaman(self, bt=BoardType.One):

        scene = Scene('scn_o_23_trance_journey_started_by_shaman', bt)

        start_b = (7, 7)
        end_b = (9, 18)
        scene.board.set_piece(*end_b, piece=-PieceType.Bishop)

        start_I = (7, 7)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_i = (7, 5)
        scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

        start_W = (6, 6)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_w = (4, 3)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_h = (1, 5)
        scene.board.set_piece(*start_h, piece=-PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start_b, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start_b, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_o_24_push_pull_trance_journey_init(self, bt=BoardType.One):

        scene = Scene('scn_o_24_push_pull_trance_journey_init', bt, width=9, height=9)

        start_i = (6, 5)
        scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

        start_w_A = (6, 6)
        scene.board.set_piece(*start_w_A, piece=-PieceType.Wave)

        start_w_B = (5, 7)
        scene.board.set_piece(*start_w_B, piece=-PieceType.Wave)

        start_n = (7, 7)
        scene.board.set_piece(*start_n, piece=-PieceType.Knight)

        scene.append_text( "A", *start_w_A, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_w_B, mark_type=MarkType.Blocked )

        scene.append_arrow( *(start_i + start_w_A), mark_type=MarkType.Action )
        scene.append_arrow( *(start_w_A + start_w_B), mark_type=MarkType.Action )

        return scene

    def scn_o_25_push_pull_trance_journey_entrancing(self, bt=BoardType.One):

        scene = Scene('scn_o_25_push_pull_trance_journey_entrancing', bt, width=9, height=9)

        start_i = (6, 6)
        scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

        start_w_A = (5, 7)
        scene.board.set_piece(*start_w_A, piece=-PieceType.Wave)

        # start_w_B = (3, 3)
        # scene.board.set_piece(*start_w_B, piece=-PieceType.Wave)

        start_n = (7, 7)
        scene.board.set_piece(*start_n, piece=-PieceType.Knight)

        scene.append_text( "A", *start_w_A, mark_type=MarkType.Blocked )
        # scene.append_text( "B", *start_w_B, mark_type=MarkType.Blocked )

        scene.append_arrow( *(start_w_A + start_i), mark_type=MarkType.Blocked )
        scene.append_arrow( *(start_i + start_n), mark_type=MarkType.Action )

        return scene

    def scn_o_26_push_pull_trance_journey_entranced(self, bt=BoardType.One):

        scene = Scene('scn_o_26_push_pull_trance_journey_entranced', bt)

        start_i = (7, 7)
        scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

        start_w_A = (5, 7)
        scene.board.set_piece(*start_w_A, piece=-PieceType.Wave)

        start_w_B = (6, 6)
        scene.board.set_piece(*start_w_B, piece=-PieceType.Wave)

        start_n = (7, 7)
        end_n = (6, 15)
        scene.board.set_piece(*end_n, piece=-PieceType.Knight)

        scene.append_text( "A", *start_w_A, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_w_B, mark_type=MarkType.Blocked )

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(scene, start_n, rel, outward_arrows=False, count=24) # , is_with_field_marker=True)

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start_n, rel, outward_arrows=False, count=24) # , is_with_field_marker=True)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Action)

        return scene

    def scn_o_27_trance_journey_failed(self, bt=BoardType.One):

        scene = Scene('scn_o_27_trance_journey_failed', bt)

        start_H = (20, 1)
        adder = GS.adder(start_H)
        scene.board.set_piece(*start_H, piece=PieceType.Shaman)

        start_W = adder(3, 2)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        # start_H_2 = (25, 0)
        start_H_2 = adder(2, -3)
        scene.board.set_piece(*start_H_2, piece=PieceType.Shaman)

        scene.append_arrow( *(start_H + start_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + start_H_2), mark_type=MarkType.Action )

        #
        # blocking step-fields

        scene.board.set_piece(23, 1, piece=PieceType.Pawn)
        scene.board.set_piece(24, 2, piece=PieceType.Pawn)

        #
        # blocking step-fields in a trance-journey

        scene.board.set_piece(25, 5, piece=PieceType.Star)
        scene.board.set_piece(1, 8, piece=PieceType.Star)
        scene.board.set_piece(5, 5, piece=-PieceType.Star)
        scene.board.set_piece(7, 6, piece=-PieceType.Star)

        scene.board.set_piece(11, 3, piece=PieceType.King)
        scene.board.set_piece(13, 4, piece=-PieceType.King)

        scene.board.set_piece(17, 1, piece=PieceType.Monolith)
        scene.board.set_piece(19, 2, piece=-PieceType.Monolith)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start_H_2, rel, count=32, is_with_field_marker=True)

        for i in range(32):
            aba(str(i + 1), mark_type=MarkType.Blocked) # Legal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start_H_2, rel, count=32, is_with_field_marker=True)

        for i in range(32):
            aba(str(i + 1), mark_type=MarkType.Blocked) # Action)

        return scene

    def scn_o_28_trance_journey_failed_2(self, bt=BoardType.One):

        scene = Scene('scn_o_28_trance_journey_failed_2', bt)

        start_h = (20, 1)
        adder = GS.adder(start_h)
        scene.board.set_piece(*start_h, piece=-PieceType.Shaman)

        # start_w = (22, 4)
        start_w = adder(2, 3)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_i = adder(3, -2)
        scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

        start_I = adder(-1, -1)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_b = adder(1, -1)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

        scene.append_arrow( *(start_h + start_w), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_w + start_i), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_i + start_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Action )

        #
        # blocking neighboring-fields

        scene.board.set_piece(23, 1, piece=PieceType.Pawn)
        scene.board.set_piece(23, 2, piece=PieceType.Pawn)
        scene.board.set_piece(24, 2, piece=PieceType.Pawn)
        scene.board.set_piece(24, 3, piece=PieceType.Pawn)
        scene.board.set_piece(25, 3, piece=PieceType.Pawn)

        scene.board.set_piece(23, 0, piece=PieceType.Rook)
        scene.board.set_piece(24, 0, piece=PieceType.Knight)
        scene.board.set_piece(25, 1, piece=PieceType.Pyramid)

        #
        # blocking step-fields

        # scene.board.set_piece(25, 5, piece=PieceType.Star)
        # scene.board.set_piece(1, 8, piece=PieceType.Star)
        # scene.board.set_piece(5, 5, piece=-PieceType.Star)
        # scene.board.set_piece(7, 6, piece=-PieceType.Star)

        # scene.board.set_piece(11, 3, piece=PieceType.King)
        # scene.board.set_piece(13, 4, piece=-PieceType.King)

        # scene.board.set_piece(17, 1, piece=PieceType.Monolith)
        # scene.board.set_piece(19, 2, piece=-PieceType.Monolith)

        scene.board.set_piece(25, 5, piece=PieceType.Pawn)
        scene.board.set_piece(1, 8, piece=PieceType.Pawn)
        scene.board.set_piece(5, 5, piece=-PieceType.Pawn)
        scene.board.set_piece(7, 6, piece=-PieceType.Pawn)

        scene.board.set_piece(11, 3, piece=PieceType.Pawn)
        scene.board.set_piece(13, 4, piece=-PieceType.Pawn)

        scene.board.set_piece(17, 1, piece=PieceType.Pawn)
        scene.board.set_piece(19, 2, piece=-PieceType.Pawn)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start_b, rel, count=32, is_with_field_marker=True)

        for i in range(32):
            aba(str(i + 1), mark_type=MarkType.Blocked) # Legal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start_b, rel, count=32, is_with_field_marker=True)

        for i in range(32):
            aba(str(i + 1), mark_type=MarkType.Blocked) # Action)

        return scene

    def scn_o_29_syzygy_monolith(self, bt=BoardType.One):

        scene = Scene('scn_o_29_syzygy_monolith', bt)

        start_M = (12, 8)
        end_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.board.set_piece(19, 8, piece=PieceType.Starchild)
        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_30_syzygy_star(self, bt=BoardType.One):

        scene = Scene('scn_o_30_syzygy_star', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.board.set_piece(19, 8, piece=PieceType.Starchild)
        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_I = (5, 6)
        end_I = (6, 5)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_T_1 = (6, 5) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        endT1 = (7, 4)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_T_1 + endT1), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_31_syzygy_starchild_init(self, bt=BoardType.One):

        scene = Scene('scn_o_31_syzygy_starchild_init', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (8, 22)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_32_syzygy_starchild_end(self, bt=BoardType.One):

        scene = Scene('scn_o_32_syzygy_starchild_end', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_Q = (19, 8)
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        return scene

    def scn_o_33_syzygy_starchild_resurrection(self, bt=BoardType.One):

        scene = Scene('scn_o_33_syzygy_starchild_resurrection', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_Ir = (20, 7)
        scene.board.set_piece(*start_Ir, piece=PieceType.Starchild)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start_I, count=1 )
        for index, coords in enumerate( gen() ):
            scene.append_text( "1", *coords, mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start_T_1, count=1 )
        for index, coords in enumerate( gen() ):
            scene.append_text( "2", *coords, mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start_M, count=1 )
        for index, coords in enumerate( gen() ):
            scene.append_text( "2", *coords, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=False, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            if scene.board.get_piece( *coords ) == PieceType.none:
                scene.append_text( "3", *coords, mark_type=MarkType.Legal )

        return scene

    def scn_o_34_syzygy_opponents_starchild(self, bt=BoardType.One):

        scene = Scene('scn_o_34_syzygy_opponents_starchild', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_G = (6, 17)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        start_W = (5, 19)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_w = (6, 21)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        start_I = (8, 22)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=-PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_G + start_W), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_W + start_w), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_w + start_I), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_35_syzygy_starchild_cascading(self, bt=BoardType.One):

        scene = Scene('scn_o_35_syzygy_starchild_cascading', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (8, 22)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = end_I
        end_W = (4, 12)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_B = end_W
        end_B = (3, 11)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        scene.board.set_piece(22, 9, piece=-PieceType.Rook)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + end_W), mark_type=MarkType.Action )

        adder = GS.adder(start_B, include_prev=True)
        scene.append_arrow( *adder(1, 1, do_advance=False), mark_type=MarkType.Legal )
        scene.append_arrow( *adder(-1, 1, do_advance=False), mark_type=MarkType.Legal )
        scene.append_arrow( *adder(-1, -1, do_advance=False), mark_type=MarkType.Legal )
        scene.append_arrow( *adder(1, -1, do_advance=False), mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_36_syzygy_double_starchilds(self, bt=BoardType.One):

        scene = Scene('scn_o_36_syzygy_double_starchilds', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (8, 22)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = end_I
        end_W = (5, 11)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_I2 = end_W
        end_I2 = (4, 3)
        scene.board.set_piece(*start_I2, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Rook)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + end_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I2 + end_I2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_37_two_syzygies_shared_celestial_piece(self, bt=BoardType.One):

        scene = Scene('scn_o_37_two_syzygies_shared_celestial_piece', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (20, 16)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = end_I
        end_W = (17, 21)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_I2 = end_W
        end_I2 = (11, 16)
        scene.board.set_piece(*start_I2, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Rook)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (13, 22) # (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (10, 13) # (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_B = (9, 10)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_syzygy_2 = (6, 1)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + end_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I2 + end_I2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(1, 3), ], start_syzygy_2, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Illegal )

        return scene

    def scn_o_38_two_syzygies_shared_field(self, bt=BoardType.One):

        scene = Scene('scn_o_38_two_syzygies_shared_field', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (20, 16)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = end_I
        end_W = (17, 21)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_I2 = end_W
        end_I2 = (11, 16)
        scene.board.set_piece(*start_I2, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Rook)
        scene.board.set_piece(7, 4, piece=-PieceType.Pawn)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (4, 3) # (0, 0)
        start_T_2 = (13, 22) # (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (10, 13) # (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_B = (9, 10)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_syzygy_2 = (6, 1)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + end_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I2 + end_I2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(1, 3), ], start_syzygy_2, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Illegal )

        return scene

    def scn_o_39_two_syzygies_shared_nothing(self, bt=BoardType.One):

        scene = Scene('scn_o_39_two_syzygies_shared_nothing', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (20, 16)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_W = end_I
        end_W = (11, 15) # (17, 21)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_I2 = end_W
        end_I2 = (15, 20) # (11, 16)
        scene.board.set_piece(*start_I2, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Rook)
        scene.board.set_piece(6, 23, piece=-PieceType.Pawn)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (4, 3) # (0, 0)
        start_T_2 = (12, 21) # (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25) # (10, 13) # (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_B = (21, 18)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + end_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I2 + end_I2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(3, -1), ], start_T_4, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Illegal )

        return scene

    def scn_o_40_reentering_syzygies(self, bt=BoardType.One):

        scene = Scene('scn_o_40_reentering_syzygies', bt)

        start_M = (9, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (18, 12)
        end_I = (15, 10)
        end_I_2 = (15, 20)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        scene.board.set_piece(21, 14, piece=-PieceType.Rook)
        scene.board.set_piece(6, 23, piece=-PieceType.Pawn)

        start_N = (3, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_T_1 = (0, 0)
        start_T_2 = (12, 21) # (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25) # (10, 13) # (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        start_B = (21, 18)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )
        scene.append_arrow( *(start_I + end_I_2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 2), ], start_T_1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            if index != 5:
                scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(3, -1), ], start_T_4, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Illegal )

        return scene
