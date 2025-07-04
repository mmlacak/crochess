#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2020, 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneOneMixin:

    #
    # Movement

    def scn_o_01_starchild_movement(self, bt=BoardType.One):

        scene = Scene( 'scn_o_01_starchild_movement', bt, width=9, height=6 )

        start_I = (1, 1)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_M = (3, 2)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        start_q = (5, 3)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        end_I = (7, 4)
        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Legal )

        return scene

    #
    # Activating on step-fields

    def scn_o_02_starchild_activating_own_starchild(self, bt=BoardType.One):

        scene = Scene( 'scn_o_02_starchild_activating_own_starchild', bt, width=9, height=4 )

        start_I = (1, 1)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_W = (7, 2)
        scene.board.set_piece( *start_W, piece=PieceType.Starchild )

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Legal )

        return scene

    def scn_o_03_starchild_activating_own_wave(self, bt=BoardType.One):

        scene = Scene( 'scn_o_03_starchild_activating_own_wave', bt, width=9, height=6 )

        start_I = (1, 1)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_W = (7, 4)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Legal )

        return scene

    #
    # Miracle-fields

    def scn_o_05_miracle_fields(self, bt=BoardType.One):

        scene = Scene('scn_o_05_miracle_fields', bt, width=5, height=5)

        start_I = (2, 2)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        rect_I = (0.35, 0.5, 0.65, 0.1)
        coords_I_ = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, include_prev=False, count=1 )
        for i, pos in enumerate( coords_I_() ):
            scene.append_text( str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal, rect=rect_I )

        return scene

    #
    # Starchild cannot teleport

    def scn_o_07_starchild_not_moving_monolith_init( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_07_starchild_not_moving_monolith_init', bt, width=7.3, height=6.3 )

        start_I = (5, 4)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_M = (2, 2)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        scene.board.set_piece( 2, 1, piece=PieceType.Knight )
        scene.board.set_piece( 1, 3, piece=PieceType.Bishop )

        scene.append_arrow( *(start_I + start_M), mark_type=MarkType.Action )

        return scene

    def scn_o_08_starchild_not_moving_monolith_end( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_08_starchild_not_moving_monolith_end', bt, width=7.3, height=6.3 )

        start_I = (1, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_M = (2, 2)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        scene.board.set_piece( 2, 1, piece=PieceType.Knight )
        scene.board.set_piece( 1, 3, piece=PieceType.Bishop )

        scene.append_arrow( *(start_M + start_I), mark_type=MarkType.Action )

        gen = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M, count=1 )
        i = 0
        for index, coords in enumerate( gen() ):
            if scene.board.is_on_board( *coords ):
                i += 1
                mark_type = MarkType.Action if index == 4 else \
                            MarkType.Illegal if index in [3, 6] else \
                            MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    #
    # Moving a Star

    def scn_o_09_starchild_moving_star_init(self, bt=BoardType.One):

        scene = Scene('scn_o_09_starchild_moving_star_init', bt, width=5, height=5)

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
                mark_type = MarkType.Legal if index == 0 else \
                            MarkType.Action if index == 6 else \
                            MarkType.Blocked
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_10_starchild_moving_star_end(self, bt=BoardType.One):

        scene = Scene('scn_o_10_starchild_moving_star_end', bt, width=5, height=5)

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
                mark_type = MarkType.Action if index == 0 else \
                            MarkType.Illegal if index == 1 else \
                            MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_11_starchild_moving_star_activating(self, bt=BoardType.One):

        scene = Scene('scn_o_11_starchild_moving_star_activating', bt, width=5, height=5)

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

    #
    # Star movement blocked

    def scn_o_12_star_movement_blocked_init(self, bt=BoardType.One):

        scene = Scene('scn_o_12_star_movement_blocked_init', bt, width=5, height=5)

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

    def scn_o_13_star_movement_blocked_end(self, bt=BoardType.One):

        scene = Scene('scn_o_13_star_movement_blocked_end', bt, width=5, height=5)

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

    #
    # Rerouting Scout

    def scn_o_14_star_rerouting_scout(self, bt=BoardType.One):

        scene = Scene('scn_o_14_star_rerouting_scout', bt, width=10, height=5)

        start_O = (8, 2)
        scene.board.set_piece(*start_O, piece=PieceType.Scout)

        start_T = (5, 2)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        adder_r = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_r( -1,  0, do_advance=True ), mark_type=MarkType.Blocked )

        x_roads = GS.add_rel( start_T, 1, 0 )

        # down fork
        adder_d = GS.adder( x_roads, include_prev=True )
        scene.append_arrow( *adder_d( -1, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( -1,  0, do_advance=True ), mark_type=MarkType.Legal )

        # up fork
        adder_u = GS.adder( x_roads, include_prev=True )
        scene.append_arrow( *adder_u( -1,  1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_u( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_u( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_u( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_u( -1,  0, do_advance=True ), mark_type=MarkType.Legal )

        return scene

    #
    # Starchild is completely transparent

    def scn_o_15_starchild_is_transparent(self, bt=BoardType.One):

        scene = Scene('scn_o_15_starchild_is_transparent', bt, height=9.3)

        start_Q = (14, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_i = (12, 3)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        start_H = (10, 5)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_e = (12, 7)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_p = (7, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # Q --> w --> W
        coords_Q_w_W = GS.gen_steps( start=start_Q, rels=[(-1, 1), ], include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_w_W() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Illegal if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    def scn_o_16_starchild_is_completely_transparent(self, bt=BoardType.One):

        scene = Scene('scn_o_16_starchild_is_completely_transparent', bt, height=9.3)

        start_I = (10, 5)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_M = (1, 1)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        adr = GS.adder( start_M, include_prev=True )
        scene.append_arrow( *adr(1, 2), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(4, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(4, 3), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr(5, -4), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(5, 6), mark_type=MarkType.Legal )

        return scene

    #
    # Conversion immunity

    def scn_o_17_starchild_conversion_immunity_init(self, bt=BoardType.One):

        scene = Scene('scn_o_17_starchild_conversion_immunity_init', bt)

        start_I = (7, 22)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        start_a = (11, 22)
        scene.board.set_piece(*start_a, piece=-PieceType.Pyramid)

        start_b = (18, 15)
        scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

        scene.append_arrow( *(start_b + start_a), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_a + start_I), mark_type=MarkType.Action )

        return scene

    #
    # Activating Wave

    def scn_o_18_starchild_cannot_activate_wave_on_miracle_fields( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_18_starchild_cannot_activate_wave_on_miracle_fields', bt, width=7.3, height=6.3 )

        start_I = (3, 4)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_W = (2, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_T = (3, 2)
        scene.board.set_piece( *start_T, piece=PieceType.Star )

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Illegal )
        # scene.append_arrow( *(start_W + start_T), mark_type=MarkType.Illegal )

        return scene

    #
    # Wave cannot move a Star

    def scn_o_19_starchild_activating_wave_on_step_field(self, bt=BoardType.One):

        scene = Scene( 'scn_o_19_starchild_activating_wave_on_step_field', bt, width=7.3, height=6.3 )

        start_I = (2, 4)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_W = (2, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_T = (3, 2)
        scene.board.set_piece( *start_T, piece=PieceType.Star )

        scene.append_arrow( *(start_I + start_W), mark_type=MarkType.Action )
        scene.append_arrow( *(start_W + start_T), mark_type=MarkType.Illegal )

        return scene

    #
    # Wave cannot teleport

    def scn_o_20_starchild_activated_wave_not_teleporting_init(self, bt=BoardType.One):

        scene = Scene('scn_o_20_starchild_activated_wave_not_teleporting_init', bt, width=8, height=8)

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
                mark_type = MarkType.Illegal if index in [3, 6] else \
                            MarkType.Legal
                scene.append_text( str(i), *coords, mark_type=mark_type )

        return scene

    def scn_o_21_starchild_activated_wave_not_teleporting_end(self, bt=BoardType.One):

        scene = Scene('scn_o_21_starchild_activated_wave_not_teleporting_end', bt, width=8, height=8)

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

    #
    # Teleporting Wave

    def scn_o_22_star_moved_wave_teleportation(self, bt=BoardType.One):

        scene = Scene('scn_o_22_star_moved_wave_teleportation', bt)

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

    def scn_o_23_star_moved_wave_off_board(self, bt=BoardType.One):

        scene = Scene('scn_o_23_star_moved_wave_off_board', bt, x=-4)

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

    #
    # Steps after teleportation

    def scn_o_24_steps_after_teleport(self, bt=BoardType.One):

        scene = Scene('scn_o_24_steps_after_teleport', bt)

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

    #
    # Divergence

    def scn_o_30_starchild_divergence_init(self, bt=BoardType.One):

        scene = Scene('scn_o_30_starchild_divergence_init', bt, height=8.3)

        start_Q = (14, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_i_1 = (12, 3)
        scene.board.set_piece( *start_i_1, piece=-PieceType.Starchild )

        start_i_2 = (10, 5)
        scene.board.set_piece( *start_i_2, piece=-PieceType.Starchild )

        start_e = (12, 7)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_p = (7, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # Q --> i(1) --> i(2)
        coords_Q_i1_i2 = GS.gen_steps( start=start_Q, rels=[(-1, 1), ], include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_i1_i2() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Diverging limits

    def scn_o_32_activating_piece_surplus_momentum(self, bt=BoardType.One):

        scene = Scene( 'scn_o_32_activating_piece_surplus_momentum', bt, width=9, height=8 )

        start_R = (7, 6)
        scene.board.set_piece(*start_R, piece=PieceType.Rook)

        start_W = (7, 1)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_N = (3, 1)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_I = (2, 3)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        # R --> W
        coords_R_W = GS.gen_steps( start=start_R, rels=[(0, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_R_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> N
        coords_W_N = GS.gen_steps( start=start_W, rels=[(-1, 0), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_W_N() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_arrow( *( start_N + start_I ), mark_type=MarkType.Action )

        return scene

    def scn_o_33_diverging_piece_surplus_momentum(self, bt=BoardType.One):

        scene = Scene( 'scn_o_33_diverging_piece_surplus_momentum', bt, width=9, height=8 )

        prev_R = (7, 6)
        prev_W = (7, 1)
        prev_N = (3, 1)
        prev_I = (2, 3)

        start_R = prev_W
        scene.board.set_piece(*start_R, piece=PieceType.Rook)

        start_W = prev_N
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_N = prev_I
        # scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_I = prev_I
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        # |<-- K -->|
        coords_N_ = GS.gen_multi_steps( GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_N, include_prev=True, count=1 )
        for index, arrow in enumerate( coords_N_() ):
            mark_type = MarkType.Action if index == 6 else \
                        MarkType.Legal
            if scene.board.is_on_board( *GS.get_end( arrow ) ):
                scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Starchild cannot diverge

    def scn_o_34_starchild_cannot_diverge(self, bt=BoardType.One):

        scene = Scene( 'scn_o_34_starchild_cannot_diverge', bt, width=9, height=8 ) # , height=5.3 )

        # step-field

        start_I_A = (6, 1)
        scene.board.set_piece( *start_I_A, piece=PieceType.Starchild )

        start_H_1 = (1, 3)
        scene.board.set_piece( *start_H_1, piece=PieceType.Shaman )

        scene.append_arrow( *( start_I_A + start_H_1 ), mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_I_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        # miracle-field

        start_I_B = (7, 6)
        scene.board.set_piece( *start_I_B, piece=PieceType.Starchild )

        start_H_2 = (6, 5)
        scene.board.set_piece( *start_H_2, piece=PieceType.Shaman )

        scene.append_arrow( *( start_I_B + start_H_2 ), mark_type=MarkType.Illegal )

        scene.append_text( "B", *start_I_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    #
    # Wave cannot diverge

    def scn_o_35_wave_cannot_diverge(self, bt=BoardType.One):

        scene = Scene( 'scn_o_35_wave_cannot_diverge', bt, width=9, height=8 ) # , height=5.3 )

        # step-field

        start_I_A = (4, 1)
        scene.board.set_piece( *start_I_A, piece=PieceType.Starchild )

        start_W_B = (3, 6)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_H_1 = (1, 3)
        scene.board.set_piece( *start_H_1, piece=PieceType.Shaman )

        # I(A) --> W(B)
        scene.append_arrow( *( start_I_A + start_W_B ), mark_type=MarkType.Action )

        # W(B) --> H(1)
        scene.append_arrow( *( start_W_B + start_H_1 ), mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_I_A, mark_type=MarkType.Action, corner=Corner.UpperRight )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene


    #
    # Failed trance-journey

    def scn_o_36_trance_journey_failed(self, bt=BoardType.One):

        scene = Scene('scn_o_36_trance_journey_failed', bt)

        start_H = (24, 1)
        scene.board.set_piece(*start_H, piece=PieceType.Shaman)

        start_H_2 = (25, 0)
        scene.board.set_piece(*start_H_2, piece=PieceType.Shaman)

        scene.append_arrow( *(start_H + start_H_2), mark_type=MarkType.Action )

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
            aba(str(i + 1), mark_type=MarkType.Blocked)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start_H_2, rel, count=32, is_with_field_marker=True)

        for i in range(32):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        return scene

    #
    # Sense-journey

    # Activating Starchild


    def scn_o_40_uplifting_fields( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_40_uplifting_fields', bt, width=5, height=5 )

        start_I = (2, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        rect_I = (0.35, 0.5, 0.65, 0.1)
        coords_I_ = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_I, include_prev=False, count=1 )
        for i, pos in enumerate( coords_I_() ):
            scene.append_text( str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal, rect=rect_I )

        return scene

    def scn_o_41_uplifting_init( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_41_uplifting_init', bt, width=9, height=6 )

        start_b = (2, 4)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_I = (6, 1)
        end_I = (2, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_i = (3, 3)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        # I --> i
        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Legal )
        scene.append_arrow( *(end_I + start_i), mark_type=MarkType.Illegal )

        # i --> b
        # scene.append_arrow( *(start_i + start_b), mark_type=MarkType.Illegal )

        scene.append_text( "I", *end_I, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )

        return scene

    def scn_o_42_uplifting_step( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_42_uplifting_step', bt, width=9, height=6 )

        start_b = (2, 4)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_I = (2, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_i = (3, 3)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        # I --> i
        scene.append_arrow( *(start_I + start_i), mark_type=MarkType.Action )

        # i --> b
        scene.append_arrow( *(start_i + start_b), mark_type=MarkType.Legal )

        return scene

    def scn_o_43_uplifting_activated(self, bt=BoardType.One):

        scene = Scene( 'scn_o_43_uplifting_activated', bt, width=9, height=6 )

        start_b = (2, 4)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_N = (7, 3)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W_A = (6, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_E = (5, 3)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W_B = (4, 1)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_I = (2, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_i = (3, 3)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        # N --> W(A)
        scene.append_arrow( *(start_N + start_W_A), mark_type=MarkType.Legal )

        # W(A) --> E
        scene.append_arrow( *(start_W_A + start_E), mark_type=MarkType.Legal )

        # E --> W(B)
        scene.append_arrow( *(start_E + start_W_B), mark_type=MarkType.Legal )

        # W --> I
        scene.append_arrow( *(start_W_B + start_I), mark_type=MarkType.Action )

        # I --> i
        scene.append_arrow( *(start_I + start_i), mark_type=MarkType.Legal )

        # i --> b
        scene.append_arrow( *(start_i + start_b), mark_type=MarkType.Legal )

        return scene

    def scn_o_44_shaman_initiated_uplifting(self, bt=BoardType.One):

        scene = Scene( 'scn_o_44_shaman_initiated_uplifting', bt, width=9, height=6 )

        start_b = (2, 4)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_H = (2, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_i = (3, 3)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        # I --> i
        scene.append_arrow( *(start_H + start_i), mark_type=MarkType.Action )

        # i --> b
        scene.append_arrow( *(start_i + start_b), mark_type=MarkType.Legal )

        return scene

    def scn_o_45_dark_piece_sense_journey(self, bt=BoardType.One):

        scene = Scene( 'scn_o_45_dark_piece_sense_journey', bt )

        start_b = (2, 4)
        end_b = (13, 6)
        scene.board.set_piece( *end_b, piece=-PieceType.Bishop )

        prev_N = (5, 3)
        start_N = (4, 1)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        prev_W = (4, 1)
        start_W = (2, 2)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        prev_H = (2, 2)
        start_H = (3, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        prev_i = (3, 3)
        start_i = (2, 4)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        start_R = (19, 8)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        # N --> W
        scene.append_arrow( *(prev_N + prev_W), mark_type=MarkType.Blocked )

        # W --> H
        scene.append_arrow( *(prev_W + prev_H), mark_type=MarkType.Blocked )

        # H --> i
        scene.append_arrow( *(prev_H + prev_i), mark_type=MarkType.Blocked )

        # i --> b
        scene.append_arrow( *(prev_i + start_b), mark_type=MarkType.Blocked )

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(scene, start_b, rel, outward_arrows=False, count=24, is_with_field_marker=True)

        for i in range(14):
            aba(str(14 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start_b, rel, outward_arrows=False, count=24, is_with_field_marker=True)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Action)

        return scene

    #
    # Failed sense-journey

    def scn_o_46_sense_journey_failed(self, bt=BoardType.One):

        scene = Scene('scn_o_46_sense_journey_failed', bt)

        start_b = (2, 4)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_I = (2, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_i = (3, 3)
        scene.board.set_piece( *start_i, piece=-PieceType.Starchild )

        # I --> i
        scene.append_arrow( *(start_I + start_i), mark_type=MarkType.Action )

        # i --> b
        scene.append_arrow( *(start_i + start_b), mark_type=MarkType.Action )

        bounds = ((-37, -37), (63, 63)) # Leave as-is, because diagonals. # ((0, 0), (25, 25))
        rel_up = (1, 2)
        rel_down = (-1, -2)

        #
        # up step-fields
        gen_rels_H = GS.gen_shaman_rels( rel_up )
        gen_steps_H = GS.gen_steps( gen_rels_H, start=start_b, include_prev=False, bounds=bounds )
        for i, pos in enumerate( gen_steps_H() ):
            if scene.board.is_on_board( *pos ):
                scene.board.set_piece( *pos, piece=PieceType.Pawn )

        #
        # down step-fields
        gen_rels_H_2 = GS.gen_shaman_rels( rel_down )
        gen_steps_H_2 = GS.gen_steps( gen_rels_H_2, start=start_b, include_prev=False, bounds=bounds )
        for i, pos_2 in enumerate( gen_steps_H_2() ):
            if scene.board.is_on_board( *pos_2 ):
                scene.board.set_piece( *pos_2, piece=-PieceType.Pawn )

        #
        # up arm
        aba = self.append_broken_arrow(scene, start_b, rel_up, outward_arrows=False, count=24, is_with_field_marker=True)
        for i in range(14):
            aba(str(14 - i), mark_type=MarkType.Blocked)

        #
        # down arm
        aba = self.append_broken_arrow(scene, start_b, rel_down, outward_arrows=False, count=24, is_with_field_marker=True)
        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        return scene

    #
    # Syzygy

    def scn_o_50_syzygy_monolith(self, bt=BoardType.One):

        scene = Scene('scn_o_50_syzygy_monolith', bt)

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

    #
    # Resurrection syzygy

    def scn_o_51_syzygy_starchild_init(self, bt=BoardType.One):

        scene = Scene('scn_o_51_syzygy_starchild_init', bt)

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

    def scn_o_52_syzygy_starchild_end(self, bt=BoardType.One):

        scene = Scene('scn_o_52_syzygy_starchild_end', bt)

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

    def scn_o_53_syzygy_starchild_resurrection(self, bt=BoardType.One):

        scene = Scene('scn_o_53_syzygy_starchild_resurrection', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_I = (19, 8)
        scene.board.set_piece(*start_I, piece=PieceType.Starchild)

        scene.board.set_piece(22, 9, piece=-PieceType.Bishop)

        start_N = (1, 2)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_W_r = (20, 7)
        scene.board.set_piece(*start_W_r, piece=PieceType.Wave)

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        arrows = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, arr in enumerate( arrows() ):
            scene.append_arrow( *arr, end_pointer=False, mark_type=MarkType.Legal )

        pos = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start_I, count=1 )
        for index, coords in enumerate( pos() ):
            scene.append_text( str( index+1 ), *coords, mark_type=MarkType.Action )

        return scene

    #
    # Existing syzygy

    def scn_o_54_syzygy_existing( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_54_syzygy_existing', bt )

        start_M = (13, 6)
        scene.board.set_piece( *start_M, piece=PieceType.Monolith )

        start_I_A = (19, 8)
        scene.board.set_piece( *start_I_A, piece=PieceType.Starchild )

        start_W_r = (20, 7)
        scene.board.set_piece( *start_W_r, piece=PieceType.Wave )

        start_T_1 = (7, 4) # (0, 0)
        start_T_2 = (25, 25)
        start_T_3 = (25, 0)
        start_T_4 = (0, 25)

        scene.board.set_piece( *start_T_1, piece=PieceType.Star )
        scene.board.set_piece( *start_T_2, piece=PieceType.Star )
        scene.board.set_piece( *start_T_3, piece=-PieceType.Star )
        scene.board.set_piece( *start_T_4, piece=-PieceType.Star )

        start_I_B = (7, 15)
        end_I_B = (4, 3)
        scene.board.set_piece( *start_I_B, piece=PieceType.Starchild )

        scene.append_arrow( *( start_I_B + end_I_B ), mark_type=MarkType.Action )

        start = (1, 2)
        arrows = GS.gen_steps( [(3, 1), ], start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, arr in enumerate( arrows() ):
            scene.append_arrow( *arr, end_pointer=False, mark_type=MarkType.Legal )

        scene.append_text( "A", *start_I_A, mark_type=MarkType.Illegal, corner=Corner.UpperLeft ) #, rect=rect_mark )
        scene.append_text( "B", *start_I_B, mark_type=MarkType.Action, corner=Corner.UpperLeft ) #, rect=rect_mark )

        return scene

    #
    # Reentering syzygy

    def scn_o_55_reentering_syzygies(self, bt=BoardType.One):

        scene = Scene('scn_o_55_reentering_syzygies', bt)

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

    #
    # Cascading syzygy

    def scn_o_56_syzygy_starchild_cascading(self, bt=BoardType.One):

        scene = Scene('scn_o_56_syzygy_starchild_cascading', bt)

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

    #
    # Double syzygy

    def scn_o_57_two_syzygies_shared_celestial_piece(self, bt=BoardType.One):

        scene = Scene('scn_o_57_two_syzygies_shared_celestial_piece', bt)

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

    #
    # Opponent's Starchild

    def scn_o_58_syzygy_opponents_starchild(self, bt=BoardType.One):

        scene = Scene('scn_o_58_syzygy_opponents_starchild', bt)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_E = (6, 17)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

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

        scene.append_arrow( *(start_E + start_W), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_W + start_w), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_w + start_I), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_I + end_I), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(3, 1), ], start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, end_pointer=False, mark_type=MarkType.Legal )

        return scene

    #
    # Star-initiated syzygy

    def scn_o_59_syzygy_star(self, bt=BoardType.One):

        scene = Scene('scn_o_59_syzygy_star', bt)

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

    #
    # Starchild and Kings
    # Castling is not blocked

    def scn_o_60_king_castling_not_blocked(self, bt=BoardType.One):

        scene = Scene( 'scn_o_60_king_castling_not_blocked', bt, height=1.7 )

        start_K = (13, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_A = (1, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_R_B = (24, 0)
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_I = (8, 0)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        # |<-- K
        coords = GS.gen_steps( start=start_K, rels=[(-1, 0), ], include_prev=True, count=10 )
        for i, step in enumerate( coords() ):
            if i == 0:
                continue

            mark_type = MarkType.Illegal if i == 4 else \
                        MarkType.Legal

            scene.append_arrow( *step, mark_type=mark_type )

        # <<- <-- K
        coords = GS.gen_steps( start=start_K, rels=[(-1, 0), ], include_prev=False, count=10 )
        for i, pos in enumerate( coords() ):
            if i == 0:
                continue

            mark_type = MarkType.Illegal if i == 4 else \
                        MarkType.Legal

            scene.append_text( str( i ), *pos, corner=Corner.UpperLeft, mark_type=mark_type )

        scene.append_text( "K", *start_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "A", *start_R_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_R_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_o_61_rook_castling_not_blocked(self, bt=BoardType.One):

        scene = Scene( 'scn_o_61_rook_castling_not_blocked', bt, height=1.7 )

        prev_K = (13, 0)
        prev_R_A = (1, 0)
        prev_R_B = (24, 0)
        prev_I = (8, 0)

        start_K = (6, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_A = prev_R_A
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_R_B = prev_R_B
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_I = prev_I
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        # R -->|
        coords = GS.gen_steps( start=start_R_A, rels=[(1, 0), ], include_prev=True, count=6 )
        for i, step in enumerate( coords() ):
            mark_type = MarkType.Action if i == 5 else \
                        MarkType.Legal

            scene.append_arrow( *step, mark_type=mark_type )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "A", *start_R_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_R_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    # Castling is blocked

    def scn_o_62_castling_blocked(self, bt=BoardType.One):

        scene = Scene( 'scn_o_62_castling_blocked', bt, height=1.7 )

        prev_K = (13, 0)
        prev_R_A = (1, 0)
        prev_R_B = (24, 0)
        prev_I = (8, 0)

        start_K = (7, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_A = prev_R_A
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_R_B = prev_R_B
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_I = prev_I
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        # R -->|
        coords = GS.gen_steps( start=start_R_A, rels=[(1, 0), ], include_prev=True, count=7 )
        for i, step in enumerate( coords() ):
            mark_type = MarkType.Blocked if i == 5 else \
                        MarkType.Illegal if i == 6 else \
                        MarkType.Legal
            scene.append_arrow( *step, mark_type=mark_type )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "A", *prev_R_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "B", *prev_R_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    #
    # Rush, en passant / En passant turned teleportation

    def scn_o_63_en_passant_blocked_by_star_init(self, bt=BoardType.One):

        scene = Scene( 'scn_o_63_en_passant_blocked_by_star_init', bt, width=10.3, height=6.3 )

        field_E = (6, 3)

        start_P = (6, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (6, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_I_A = (3, 5)
        scene.board.set_piece( *start_I_A, piece=PieceType.Starchild )

        start_I_B = (4, 3)
        scene.board.set_piece( *start_I_B, piece=PieceType.Starchild )

        start_T = (5, 3)
        end_T = (6, 3)
        scene.board.set_piece( *start_T, piece=PieceType.Star )

        start_p = (7, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # P --> W
        gen_P_W = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( gen_P_W() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> I(A)
        gen_W_IA = GS.gen_steps( start=start_W, rels=[ (-1, 0), ], include_prev=True, count=3 )
        for i, arrow in enumerate( gen_W_IA() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # I(A) --> I(B)
        scene.append_arrow( *( start_I_A + start_I_B ), mark_type=MarkType.Action )

        # I(B) --> T
        scene.append_arrow( *( start_I_B + start_T ), mark_type=MarkType.Action )

        # T --> []
        scene.append_arrow( *( start_T + end_T ), mark_type=MarkType.Legal )

        scene.append_text( "A", *start_I_A, corner=Corner.UpperRight, mark_type=MarkType.Action )
        scene.append_text( "B", *start_I_B, corner=Corner.UpperRight, mark_type=MarkType.Action )
        scene.append_text( "E", *field_E, corner=Corner.LowerRight, mark_type=MarkType.Blocked )

        scene.append_field_marker( *start_P, mark_type=MarkType.Action )

        return scene

    def scn_o_64_en_passant_blocked_by_star_end(self, bt=BoardType.One):

        scene = Scene( 'scn_o_64_en_passant_blocked_by_star_end', bt, width=10.3, height=6.3 )

        field_E = (6, 3)

        start_P = (6, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (3, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_I_A = (4, 3)
        scene.board.set_piece( *start_I_A, piece=PieceType.Starchild )

        start_I_B = (5, 3)
        scene.board.set_piece( *start_I_B, piece=PieceType.Starchild )

        start_T = (6, 3)
        scene.board.set_piece( *start_T, piece=PieceType.Star )

        start_p = (7, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # # P --> W
        # gen_P_W = GS.gen_steps( end=start_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        # for i, arrow in enumerate( gen_P_W() ):
        #     scene.append_arrow( *arrow, mark_type=MarkType.Blocked )
        #
        # # W --> I(A)
        # gen_W_IA = GS.gen_steps( end=start_W, rels=[ (-1, 0), ], include_prev=True, count=3 )
        # for i, arrow in enumerate( gen_W_IA() ):
        #     scene.append_arrow( *arrow, mark_type=MarkType.Blocked )
        #
        # # I(A) --> I(B)
        # scene.append_arrow( *( start_W + start_I_A ), mark_type=MarkType.Blocked )
        #
        # # I(B) --> T
        # scene.append_arrow( *( start_I_A + start_I_B ), mark_type=MarkType.Blocked )

        # T --> [E]
        scene.append_arrow( *( start_I_B + start_T ), mark_type=MarkType.Blocked )

        # p --> *
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_I_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_I_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "E", *field_E, corner=Corner.LowerRight, mark_type=MarkType.Illegal )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene

    #
    # Rush, en passant / En passant turned teleportation / En passant not blocked

    def scn_o_65_en_passant_not_blocked_by_star_end(self, bt=BoardType.One):

        scene = Scene( 'scn_o_65_en_passant_not_blocked_by_star_end', bt, width=10.3, height=6.3 )

        field_E = (6, 3)

        start_P = (6, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (3, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_I_A = (4, 3)
        scene.board.set_piece( *start_I_A, piece=PieceType.Starchild )

        start_I_B = (5, 3)
        scene.board.set_piece( *start_I_B, piece=PieceType.Starchild )

        start_T = (6, 4)
        scene.board.set_piece( *start_T, piece=PieceType.Star )

        start_p = (7, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # # P --> W
        # gen_P_W = GS.gen_steps( end=start_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        # for i, arrow in enumerate( gen_P_W() ):
        #     scene.append_arrow( *arrow, mark_type=MarkType.Blocked )
        #
        # # W --> I(A)
        # gen_W_IA = GS.gen_steps( end=start_W, rels=[ (-1, 0), ], include_prev=True, count=3 )
        # for i, arrow in enumerate( gen_W_IA() ):
        #     scene.append_arrow( *arrow, mark_type=MarkType.Blocked )
        #
        # # I(A) --> I(B)
        # scene.append_arrow( *( start_W + start_I_A ), mark_type=MarkType.Blocked )
        #
        # # I(B) --> T
        # scene.append_arrow( *( start_I_A + start_I_B ), mark_type=MarkType.Blocked )

        # T --> [E]
        scene.append_arrow( *( start_I_B + start_T ), mark_type=MarkType.Blocked )

        # p --> *
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_I_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_I_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "E", *field_E, corner=Corner.LowerRight, mark_type=MarkType.Action )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene

    #
    # Rush, en passant / En passant turned divergence

    def scn_o_66_en_passant_turned_divergence_init( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_66_en_passant_turned_divergence_init', bt, width=10.3, height=7.3 )

        field_E = (7, 2)

        start_P = (7, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (7, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_I = (2, 6)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_p = (8, 3)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # P --> W
        gen_P_W = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( gen_P_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> I
        gen_W_I = GS.gen_steps( start=start_W, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arrow in enumerate( gen_W_I() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # I --> [E]
        scene.append_arrow( *( start_I + field_E ), mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.LowerRight, mark_type=MarkType.Blocked )

        scene.append_field_marker( *start_P, mark_type=MarkType.Action )

        return scene

    def scn_o_67_en_passant_turned_divergence_end( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_67_en_passant_turned_divergence_end', bt, width=10.3, height=7.3 )

        field_E = (7, 2)

        start_P = (7, 6)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (2, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_I = (7, 2)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_p = (8, 3)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # p --> @I -- * -->
        adder = GS.adder( start_p, include_prev=True )
        scene.append_arrow( *adder( -1, -1, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder( -1,  0, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder(  0, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder(  1,  0, do_advance=False ), mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.LowerRight, mark_type=MarkType.Illegal )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene

    #
    # Rush, en passant / En passant turned divergence / En passant not blocked

    def scn_o_68_en_passant_not_blocked__by_starchild( self, bt=BoardType.One ):

        scene = Scene( 'scn_o_68_en_passant_not_blocked__by_starchild', bt, width=10.3, height=7.3 )

        field_E = (7, 2)

        start_P = (7, 6)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (2, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        prev_I = (2, 6)
        start_I = (7, 4)
        scene.board.set_piece( *start_I, piece=PieceType.Starchild )

        start_p = (8, 3)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # I --> []
        scene.append_arrow( *( prev_I + start_I ), mark_type=MarkType.Blocked )

        # p --> @I -- * -->
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Blocked )

        scene.append_text( "E", *field_E, corner=Corner.LowerRight, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene


    #
    # test methods

    def test_o_97_quad_field_markers_full(self, bt=BoardType.One):

        scene = Scene('test_o_97_quad_field_markers_full', bt, x=-50, y=-50, width=128, height=128)

        start = (12, 12) # (11, 11)

        scene.append_text( "X", *start, corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=( 0.35, 0.35, 0.65, 0.65 ) )

        is_all_corners = True
        _ur = None if is_all_corners else Corner.UpperRight
        _ul = None if is_all_corners else Corner.UpperLeft
        _ll = None if is_all_corners else Corner.LowerLeft
        _lr = None if is_all_corners else Corner.LowerRight

        #
        # light Shaman

        rel = (2, 1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rels(rel)
        coords = GS.gen_next( GS.gen_steps( rels, start=start, include_prev=False ) ) # , bounds=bounds

        for i in range(44):
            scene.append_field_marker( *coords(), corner=_ur, mark_type=MarkType.Legal, force_unique=True )


        rel = (-2, -1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rels(rel)
        coords = GS.gen_next( GS.gen_steps( rels, start=start, include_prev=False ) ) # , bounds=bounds

        for i in range(44):
            scene.append_field_marker( *coords(), corner=_ul, mark_type=MarkType.Blocked, force_unique=True )

        #
        # dark Shaman

        rel = (1, 2)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rels(rel)
        coords = GS.gen_next( GS.gen_steps( rels, start=start, include_prev=False ) ) # , bounds=bounds

        for i in range(44):
            scene.append_field_marker( *coords(), corner=_ll, mark_type=MarkType.Action, force_unique=True )


        rel = (-1, -2)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rels(rel)
        coords = GS.gen_next( GS.gen_steps( rels, start=start, include_prev=False ) ) # , bounds=bounds

        for i in range(44):
            scene.append_field_marker( *coords(), corner=_lr, mark_type=MarkType.Illegal, force_unique=True )

        return scene

    def test_o_98_quad_stop_sign_pattern_full(self, bt=BoardType.One):

        scene = Scene('test_o_98_quad_stop_sign_pattern_full', bt, x=-50, y=-50, width=128, height=128)

        start = (12, 12) # (11, 11)

        #
        # light Shaman

        rel = (2, 1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal, end_pointer=False ) # right
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal ) # right-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal, end_pointer=False ) # up
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal ) # left-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal, end_pointer=False ) # left
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal ) # left-down

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal, end_pointer=False ) # down
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.7, 0.6 ), mark_type=MarkType.Legal ) # right-down


        rel = (-2, -1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked, end_pointer=False ) # right
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked ) # right-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked, end_pointer=False ) # up
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked ) # left-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked ) # left-down

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked, end_pointer=False ) # down
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.3, 0.4 ), mark_type=MarkType.Blocked ) # right-down

        #
        # dark Shaman

        rel = (1, 2)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action, end_pointer=False ) # right
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action ) # right-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action, end_pointer=False ) # up
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action ) # left-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action, end_pointer=False ) # left
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action ) # left-down

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action, end_pointer=False ) # down
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.5, 0.7 ), mark_type=MarkType.Action ) # right-down


        rel = (-1, -2)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # right
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal ) # right-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # up
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal ) # left-up

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # left
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal ) # left-down

            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
            scene.append_arrow( *GS.add_arrow_rel( coords(), 0.4, 0.5 ), mark_type=MarkType.Illegal ) # right-down

        return scene

    def test_o_99_stop_sign_pattern_full(self, bt=BoardType.One):

        scene = Scene('test_o_99_stop_sign_pattern_full', bt, x=-50, y=-50, width=128, height=128)

        start = (12, 12) # (11, 11)


        rel = (2, 1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal ) # right-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action ) # left-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # left
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal ) # left-down

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # down
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action ) # right-down


        rel = (-2, -1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # right
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked ) # right-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # up
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal ) # left-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked ) # left-down

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal ) # right-down

        return scene



def test_big_pattern():
    from scene_mix import SceneMix
    from save_scene import SaveScene
    from def_render import RenderingSizeEnum

    scene = SceneMix()
    ss = SaveScene( RenderingSizeEnum.Draft )
    # ss.render_example( scene,
    #                    scene.test_o_99_stop_sign_pattern_full,
    #                    board_types=[ BoardType.One, ],
    #                    path_prefix='temp/')
    #                    # , enforce_cot_in_bw=True)
    ss.render_example( scene,
                       scene.test_o_97_quad_field_markers_full, # scene.test_o_98_quad_stop_sign_pattern_full, # scene.test_o_99_stop_sign_pattern_full,
                       board_types=[ BoardType.One, ],
                       path_prefix='temp/',
                       enforce_in_bw=[ BoardType.One, ] )


if __name__ == '__main__':
    test_big_pattern()
