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

    def scn_o_02_neighboring_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_02_neighboring_fields', bt, width=5, height=5)

        start_I = (2, 2)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, count=1 )
        for index, coords in enumerate( gen() ):
            scene.append_text( str(index + 1), *coords, mark_type=MarkType.Legal )

        return scene

    def scn_o_03_starchild_activating_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_03_starchild_activating_fields', bt, width=5, height=5)

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

    def scn_o_04_starchild_activating_wave_init(self, bt=BoardType.One):

        scene = Scene('scn_o_04_starchild_activating_wave_init', bt, width=5, height=5)

        start_I = (2, 2)
        start_W = (1, 1)
        start_G = (0, 2)
        start_B = (17, 17)

        scene.board.set_piece(*start_I, piece=PieceType.Starchild)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        startT1 = (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + startT1), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_W + start_G), mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_W, count=1 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Legal if index in [3, 5] else MarkType.Blocked
            scene.append_text( str(index + 1), *coords, mark_type=mark_type )

        return scene

    def scn_o_05_starchild_activating_wave_end(self, bt=BoardType.One):

        scene = Scene('scn_o_05_starchild_activating_wave_end', bt)

        start_I = (1, 1)
        start_W = (21, 21)
        start_G = (0, 2)
        start_B = (17, 17)

        scene.board.set_piece(*start_I, piece=PieceType.Starchild)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        startT1 = (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        gen = GS.gen_steps( [(-1, -1), ], start=startT2, count=25, include_prev=True )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index in [7, 23] else MarkType.Blocked if index < 4 else MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_o_06_starchild_moving_star_init(self, bt=BoardType.One):

        scene = Scene('scn_o_06_starchild_moving_star_init', bt, width=5, height=5)

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

    def scn_o_07_starchild_moving_star_end(self, bt=BoardType.One):

        scene = Scene('scn_o_07_starchild_moving_star_end', bt, width=5, height=5)

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

    def scn_o_08_starchild_not_moving_monolith_init(self, bt=BoardType.One):

        scene = Scene('scn_o_08_starchild_not_moving_monolith_init', bt, width=5, height=5)

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

    def scn_o_09_starchild_not_moving_monolith_end(self, bt=BoardType.One):

        scene = Scene('scn_o_09_starchild_not_moving_monolith_end', bt, width=5, height=5)

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

    def scn_o_10_starchild_conversion_immunity_init(self, bt=BoardType.One):

        scene = Scene('scn_o_10_starchild_conversion_immunity_init', bt)

        start_I = (7, 22)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_a = (11, 22)
        scene.board.set_piece(*start_a, piece=-PieceType.Pyramid)

        start_b = (18, 15)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

        scene.append_arrow( *(start_b + start_a), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_a + start_I), mark_type=MarkType.Action )

        return scene

    def scn_o_11_star_moved_wave_teleportation(self, bt=BoardType.One):

        scene = Scene('scn_o_11_star_moved_wave_teleportation', bt)

        start_W = (11, 9)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_N = (10, 11)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        startT1 = (5, 6)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.append_arrow( *(start_N + start_W), mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(-2, -1), ], start_W, include_prev=True, count=5 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal if index < 2 else MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_o_12_star_moved_wave_off_board(self, bt=BoardType.One):

        scene = Scene('scn_o_12_star_moved_wave_off_board', bt, x=-4)

        start_W = (11, 9)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_N = (10, 11)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        startT1 = (0, 0)
        startT2 = (25, 25)
        startT3 = (5, 6) # (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.append_arrow( *(start_N + start_W), mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(-2, -1), ], start_W, include_prev=True, count=3 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen = GS.gen_steps( [(-2, -1), ], startT4, include_prev=True, count=2 )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        return scene

    def scn_o_13_trance_journey_init_starchild(self, bt=BoardType.One):

        scene = Scene('scn_o_13_trance_journey_init_starchild', bt, width=9, height=9)

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

    def scn_o_14_trance_journey_init_shaman(self, bt=BoardType.One):

        scene = Scene('scn_o_14_trance_journey_init_shaman', bt, width=9, height=9)

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

    def scn_o_15_trance_journey_started_by_shaman(self, bt=BoardType.One):

        scene = Scene('scn_o_15_trance_journey_started_by_shaman', bt)

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

    def scn_o_16_push_pull_trance_journey_init(self, bt=BoardType.One):

        scene = Scene('scn_o_16_push_pull_trance_journey_init', bt, width=9, height=9)

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

    def scn_o_17_push_pull_trance_journey_entrancing(self, bt=BoardType.One):

        scene = Scene('scn_o_17_push_pull_trance_journey_entrancing', bt, width=9, height=9)

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

    def scn_o_18_push_pull_trance_journey_entranced(self, bt=BoardType.One):

        scene = Scene('scn_o_18_push_pull_trance_journey_entranced', bt)

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

    def scn_o_19_syzygy_monolith(self, bt=BoardType.One):

        scene = Scene('scn_o_19_syzygy_monolith', bt)

        start_M = (12, 8)
        end_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        scene.board.set_piece(19, 8, piece=PieceType.Starchild)
        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        startT1 = (7, 4) # (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_20_syzygy_starchild_init(self, bt=BoardType.One):

        scene = Scene('scn_o_20_syzygy_starchild_init', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (8, 22)
        end_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        startT1 = (7, 4) # (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    def scn_o_21_syzygy_starchild_end(self, bt=BoardType.One):

        scene = Scene('scn_o_21_syzygy_starchild_end', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_Q = (19, 8)
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        startT1 = (7, 4) # (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        return scene

    def scn_o_22_syzygy_starchild_ressurection(self, bt=BoardType.One):

        scene = Scene('scn_o_22_syzygy_starchild_ressurection', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_Ir = (20, 7)
        scene.board.set_piece(*start_Ir, piece=PieceType.Starchild)

        startT1 = (7, 4) # (0, 0)
        startT2 = (25, 25)
        startT3 = (25, 0)
        startT4 = (0, 25)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        i = 0
        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start_I, count=1 )
        for index, coords in enumerate( gen() ):
            i += 1
            scene.append_text( str( i ), *coords, mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, startT1, count=1 )
        for index, coords in enumerate( gen() ):
            i += 1
            scene.append_text( str( i ), *coords, mark_type=MarkType.Legal )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start_M, count=1 )
        for index, coords in enumerate( gen() ):
            i += 1
            scene.append_text( str( i ), *coords, mark_type=MarkType.Legal )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=False, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            if scene.board.get_piece( *coords ) == PieceType.none:
                i += 1
                scene.append_text( str( i ), *coords, mark_type=MarkType.Legal )

        return scene
