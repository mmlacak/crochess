#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneHemerasDawnMixin:

    #
    # Movement

    def scn_hd_01_centaur_same_color(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_01_centaur_same_color', bt, y=1, width=7, height=7)

        start = (3, 4)
        scene.board.set_piece(*start, piece=PieceType.Centaur)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Legal if i < 4 else MarkType.Action
            scene.append_field_marker(*arr, mark_type=mark_type)
            scene.append_text(str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        return scene

    def scn_hd_02_centaur_opposite_color(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_02_centaur_opposite_color', bt, width=11, height=11)

        start = (5, 5)
        scene.board.set_piece(*start, piece=PieceType.Centaur)

        # Centaur, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_LONG_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Legal if i < 8 else MarkType.Action
            scene.append_field_marker(*arr, mark_type=mark_type)
            scene.append_text(str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*arr)
            scene.append_text(str(i+1), *arr, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

        return scene


    def scn_hd_03_centaur_multi_step_init(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_03_centaur_multi_step_init', bt)

        start = (6, 5)
        scene.board.set_piece(*start, piece=PieceType.Centaur)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(14, 16, piece=-PieceType.Bishop)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Blocked if i == 1 else MarkType.Legal if i < 4 else MarkType.Action
            scene.append_field_marker(*arr, mark_type=mark_type)
            scene.append_text(str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        return scene

    def scn_hd_04_centaur_multi_step_second(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_04_centaur_multi_step_second', bt)

        start_0 = (6, 5)
        start = (5, 7)
        scene.board.set_piece(*start, piece=PieceType.Centaur)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(14, 16, piece=-PieceType.Bishop)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_LONG_I_III_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Blocked if i == 1 else MarkType.Legal if i < 8 else MarkType.Action
            scene.append_field_marker(*arr, mark_type=mark_type)
            scene.append_text(str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        scene.append_arrow( *(start_0 + start), mark_type=MarkType.Action )

        return scene

    def scn_hd_05_centaur_multi_step(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_05_centaur_multi_step', bt)

        start = (6, 5)
        # start = (5, 7)
        scene.board.set_piece(*start, piece=PieceType.Centaur)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(14, 16, piece=-PieceType.Bishop)

        #
        # short --> (-1, 2) direction
        # long --> (4, 1) direction

        rels = [(-1, 2), (4, 1), ]

        arr = GS.gen_steps(start=start, rels=rels, include_prev=True, bounds=scene.board_view.get_position_limits())
        for i, arr in enumerate( arr() ):
            mark_type = MarkType.Blocked if i > 6 else MarkType.Action if i % 2 == 0 else MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        txt = GS.gen_steps(start=start, rels=rels, include_prev=False, bounds=scene.board_view.get_position_limits())
        for i, arr in enumerate( txt() ):
            mark_type = MarkType.Blocked if i > 6 else MarkType.Action if i % 2 == 0 else MarkType.Legal
            scene.append_text( str(i+1), *arr, mark_type=mark_type )

        #
        # forbidden directions change

        # (-1, 2) is ok, i.e. direction "7", here: 12, 11 --> 11, 13
        # multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_CENTAUR_SHORT_REL_MOVES, to_remove=((-1, 2), ) ) )
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-1, 2), ) ) )
        start_X = (12, 11)

        arr = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=True, count=1)
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Illegal )

        txt = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=False, count=1)
        for i, arr in enumerate( txt() ):
            corner = Corner.LowerRight if i > 4 else \
                     Corner.LowerLeft if i > 2 else \
                     Corner.UpperLeft if i > 1 else \
                     Corner.UpperRight
            scene.append_text( str(i+1), *arr, corner=corner, mark_type=MarkType.Illegal )

        return scene

    #
    # Out of board steps

    def scn_hd_06_centaur_off_board(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_06_centaur_off_board', bt, x=4, y=1)

        start = (17, 3)
        scene.board.set_piece(*start, piece=-PieceType.Centaur)

        #
        # short --> (-2, 1) direction
        # long --> (3, 2) direction

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True) )

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords() ) # long

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords() ) # long

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long


        scene.append_text("1", 18, 13, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)
        scene.append_text("2", 19, 16, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)

        return scene

    #
    # Activating Wave

    def scn_hd_07_wave_activation_by_centaur_first_step(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_07_wave_activation_by_centaur_first_step', bt)

        start = (5, 5)
        start_C = (3, 6)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(*start_C, piece=PieceType.Centaur)

        scene.board.set_piece(7, 6, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)

        scene.board.set_piece(7, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)

        scene.board.set_piece(11, 16, piece=-PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_LONG_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Legal if i < 8 else MarkType.Action
            scene.append_field_marker(*arr, mark_type=mark_type)
            scene.append_text(str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        scene.append_arrow( *(start_C + start), mark_type=MarkType.Blocked )

        return scene

    def scn_hd_08_wave_activation_by_centaur_second_step(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_08_wave_activation_by_centaur_second_step', bt)

        start = (5, 5)
        start_W = (8, 7)
        start_C = (3, 6)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_C, piece=PieceType.Centaur)

        scene.board.set_piece(7, 6, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)

        scene.board.set_piece(7, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)

        scene.board.set_piece(11, 16, piece=-PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_II_IV_MULTI_REL_MOVES, start=start_W, include_prev=False, count=1)

        for i, arr in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Action
            scene.append_field_marker(*arr, mark_type=mark_type)
            scene.append_text(str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        scene.append_arrow( *(start_C + start), mark_type=MarkType.Blocked )
        scene.append_arrow( *(start + start_W), mark_type=MarkType.Legal )

        return scene

    def scn_hd_09_wave_activation_by_centaur_complete(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_09_wave_activation_by_centaur_complete', bt)

        start = (5, 5)
        start_C = (3, 6)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(*start_C, piece=PieceType.Centaur)

        scene.board.set_piece(7, 6, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)

        scene.board.set_piece(7, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)

        scene.board.set_piece(11, 16, piece=-PieceType.Wave)

        #
        # Wave activation by Centaur
        scene.append_arrow( *(start_C + start), mark_type=MarkType.Blocked )

        #
        # long --> (3, 2) direction
        # short --> (-2, 1) direction

        rels = [(3, 2), (-2, 1), ]

        arr = GS.gen_steps(start=start, rels=rels, include_prev=True, bounds=scene.board_view.get_position_limits())
        for i, arr in enumerate( arr() ):
            mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *arr, mark_type=mark_type )

        txt = GS.gen_steps(start=start, rels=rels, include_prev=False, bounds=scene.board_view.get_position_limits())
        for i, arr in enumerate( txt() ):
            mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            corner = Corner.UpperRight if i % 2 == 0 else Corner.UpperLeft
            scene.append_text( str(i+1), *arr, corner=corner, mark_type=mark_type )

        #
        # forbidden directions change

        # (-2, 1) is ok, i.e. direction "7", here: 10, 13 --> 8, 14
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-2, 1), ) ) )
        start_X = (10, 13)

        arr = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=True, count=1)
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Illegal )

        txt = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=False, count=1)
        for i, arr in enumerate( txt() ):
            corner = Corner.LowerRight if i > 4 else \
                     Corner.LowerLeft if i > 2 else \
                     Corner.UpperLeft if i > 1 else \
                     Corner.UpperRight
            scene.append_text( str(i+1), *arr, corner=corner, mark_type=MarkType.Illegal )

        return scene

    #
    # Out of board steps

    def scn_hd_10_wave_activated_by_centaur_off_board(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_10_wave_activated_by_centaur_off_board', bt, x=4, y=1)

        start = (17, 3)
        start_C = (13, 2)
        scene.board.set_piece(*start, piece=-PieceType.Wave)
        scene.board.set_piece(*start_C, piece=-PieceType.Centaur)

        #
        # Wave activation by Centaur
        scene.append_arrow( *(start_C + start), mark_type=MarkType.Action )


        #
        # short --> (-2, 1) direction
        # long --> (3, 2) direction

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True) )

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords() ) # long

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords() ) # long

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Legal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Legal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long


        scene.append_text("1", 18, 13, corner=Corner.UpperLeft, mark_type=MarkType.Legal)
        scene.append_text("2", 19, 16, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        return scene

    #
    # Teleporting Wave

    def scn_hd_11_wave_teleport(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_11_wave_teleport', bt, x=4, y=1)

        start_T = (19, 19)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        start = (18, 9)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        start_C = (9, 6)
        scene.board.set_piece(*start_C, piece=PieceType.Centaur)

        #
        # Wave activation
        gen_coords = GS.gen_steps(start=start_C, rels=[(1, -2), (2, 3), ], include_prev=True, count=6)

        for coords in gen_coords():
            scene.append_arrow( *coords )

        #
        # short --> (-2, 1) direction
        # long --> (3, 2) direction

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True) )

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords() ) # long

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords() ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Action ) # short

        scene.append_text("1", 18, 16, corner=Corner.UpperLeft)
        scene.append_text("2", *start_T, corner=Corner.UpperLeft)

        return scene

    #
    # Scout-fields

    def scn_hd_14_scout_fields(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_14_scout_fields', bt, width=5, height=5 )

        start_O = (2, 2)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        rect_O = (0.35, 0.5, 0.65, 0.1)
        coords_O_ = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_O, include_prev=False, count=1 )
        for i, arr in enumerate( coords_O_() ):
            scene.append_text( str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal, rect=rect_O )

        return scene

    #
    # Scout/Movement

    def scn_hd_15_scout_movement(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_15_scout_movement', bt, height=9 )

        start_O_1 = (6, 2)
        scene.board.set_piece( *start_O_1, piece=PieceType.Scout )

        start_p = (5, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_n = (7, 1)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_o_1 = (13, 6)
        scene.board.set_piece( *start_o_1, piece=-PieceType.Scout )

        start_B = (12, 7)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_R = (14, 7)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        #
        # <-- <- O -> -->

        arr = GS.gen_steps( start=start_O_1, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_O_1, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_O_1, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_O_1, -1, -1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_O_1, 1, -1 ), mark_type=MarkType.Action )

        #
        # <-- <- o -> -->

        arr = GS.gen_steps( start=start_o_1, rels=[ (0, -1), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_o_1, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_o_1, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_o_1, -1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_o_1, 1, 1 ), mark_type=MarkType.Action )

        return scene

    def scn_hd_16_scout_capturing(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_16_scout_capturing', bt, width=8, height=4 )

        start_O = (1, 2)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_n = (2, 1)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (5, 1)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        arr = GS.gen_steps( start=start_O, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( arr() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_O, 1, -1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.prepend_pos_rel( 1, -1, start_b ), mark_type=MarkType.Illegal )

        return scene

    #
    # Movement/Forking steps

    def scn_hd_17_scout_forking_steps(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_17_scout_forking_steps', bt, width=7, height=7 )

        start_O = (3, 3)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        adder_R = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_R( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_R( 1,  1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_R( 1, -1, do_advance=False ), mark_type=MarkType.Legal )

        adder_U = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_U(  0, 1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_U(  1, 1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_U( -1, 1, do_advance=False ), mark_type=MarkType.Legal )

        adder_L = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_L( -1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_L( -1,  1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_L( -1, -1, do_advance=False ), mark_type=MarkType.Legal )

        adder_D = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_D(  0, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_D(  1, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_D( -1, -1, do_advance=False ), mark_type=MarkType.Legal )

        return scene

    #
    # Movement/Rerouting Scout

    def scn_hd_20_scout_rerouting(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_20_scout_rerouting', bt, width=8, height=5 )

        start_O = (1, 2)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_b = (4, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        adder_r = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_r( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_r( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_r( 1,  0, do_advance=False ), mark_type=MarkType.Blocked )

        x_roads = GS.add_rel( start_b, -1, 0 )

        # down fork
        adder_d = GS.adder( x_roads, include_prev=True )
        scene.append_arrow( *adder_d( 1, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( 1,  0, do_advance=True ), mark_type=MarkType.Legal )

        # up fork
        adder_u = GS.adder( x_roads, include_prev=True )
        scene.append_arrow( *adder_u( 1,  1, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_u( 1,  0, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_u( 1,  0, do_advance=True ), mark_type=MarkType.Action )

        return scene

    def scn_hd_21_scout_rerouting_first_step(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_21_scout_rerouting_first_step', bt, width=8, height=5 )

        start_O = (1, 2)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_b = (2, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        scene.append_arrow( *GS.append_pos_rel( start_O, 1, 0 ), mark_type=MarkType.Blocked )

        # down fork
        adder_d = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_d( 1, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( 1,  0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_d( 1,  0, do_advance=True ), mark_type=MarkType.Legal )

        # up fork
        adder_u = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder_u( 1, 1, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_u( 1, 0, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_u( 1, 0, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_u( 1, 0, do_advance=True ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_u( 1, 0, do_advance=True ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_O, 1, 0 ), mark_type=MarkType.Blocked )

        return scene

    def scn_hd_22_scout_rerouting_pawn_wall(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_22_scout_rerouting_pawn_wall', bt, width=8, height=6 )

        start_O = (1, 1)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_p_1 = (3, 1)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (4, 2)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        start_p_3 = (5, 3)
        scene.board.set_piece( *start_p_3, piece=-PieceType.Pawn )

        adder = GS.adder( start_O, include_prev=True )
        scene.append_arrow( *adder( 1,  0, do_advance=True ), mark_type=MarkType.Legal )

        scene.append_arrow( *adder( 1,  0, do_advance=False ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder( 1, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( 1,  1, do_advance=True ), mark_type=MarkType.Action )

        scene.append_arrow( *adder( 1,  0, do_advance=False ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder( 1, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( 1,  1, do_advance=True ), mark_type=MarkType.Action )

        scene.append_arrow( *adder( 1,  0, do_advance=False ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder( 1, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( 1,  1, do_advance=True ), mark_type=MarkType.Action )

        scene.append_arrow( *adder( 1,  0, do_advance=True ), mark_type=MarkType.Action )

        return scene

    #
    # Activating Scout

    def scn_hd_23_activating_scout(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_23_activating_scout', bt, height=11 ) # width=8

        start_Q = (18, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_A = (7, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_O = (10, 4)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_A = (11, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_W_B = (6, 4)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_W_C = (17, 4)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        # Q --> W(A)
        gen_Q_WA = GS.gen_steps( start=start_Q, rels=[ (-1, 0), ], include_prev=True, count=11 )
        for i, arr in enumerate( gen_Q_WA() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # W(A) --> O
        gen_WA_O = GS.gen_steps( start=start_W_A, rels=[ (1, 1), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_WA_O() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # O --> W(B)
        gen_O_WB = GS.gen_steps( start=start_O, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_O_WB() ):
            mt_O_WB = MarkType.Action if i == 3 else \
                      MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_O_WB )

        # O --> W(C)
        gen_O_WC = GS.gen_steps( start=start_O, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_O_WC() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # O -->
        gen_O_ = GS.gen_steps( start=start_O, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_O_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # O --> A
        scene.append_arrow( *( start_O + start_A ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeft, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeft, mark_type=MarkType.Action )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperLeft, mark_type=MarkType.Illegal )

        return scene

    #
    # Activating Wave, Pyramid

    def scn_hd_24_scout_activating_wave_step_fields(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_24_scout_activating_wave_step_fields', bt, height=12.7)

        start_W = (9, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_O = (9, 2)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_A = (15, 5)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # O --> W(B)
        gen_O_W = GS.gen_steps( start=start_O, rels=[ (0, 1), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_O_W() ):
            mt_O_W = MarkType.Action if i == 2 else \
                     MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_O_W )

        # W -->
        gen_W_1 = GS.gen_steps( start=start_W, rels=[ (-1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_1() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # W -->
        gen_W_2 = GS.gen_steps( start=start_W, rels=[ (1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_2() ):
            mt_2 = MarkType.Illegal if i == 5 else \
                   MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_2 )

        # W -->
        gen_W_3 = GS.gen_steps( start=start_W, rels=[ (0, 1), ], include_prev=True, count=7 ) # bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_3() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        return scene

    def scn_hd_25_scout_activating_wave_capture_fields(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_25_scout_activating_wave_capture_fields', bt, height=12.7)

        start_W = (9, 9)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_O = (8, 10)
        scene.board.set_piece( *start_O, piece=PieceType.Scout )

        start_A = (5, 5)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        scene.append_arrow( *( start_O + start_W ), mark_type=MarkType.Action )

        # W -->
        gen_W_1 = GS.gen_steps( start=start_W, rels=[ (-1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_1() ):
            mt_1 = MarkType.Action if i == 3 else \
                   MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_1 )

        # W -->
        gen_W_2 = GS.gen_steps( start=start_W, rels=[ (1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_2() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        return scene

    def scn_hd_26_scout_en_passant(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_26_scout_en_passant', bt, width=5, height=10.7)

        start_P = (2, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_o = (3, 4)
        scene.board.set_piece( *start_o, piece=-PieceType.Scout )

        start_p = (1, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # P -->
        gen_P = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=8 )
        for i, arr in enumerate( gen_P() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_o, -1, 1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_p, 1, -1 ), mark_type=MarkType.Action )

        return scene

    #
    # Scouts initial positions

    def scn_hd_39_scout_initial_positions(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_39_scout_initial_positions', bt)

        scene.board.set_piece(4, 0, piece=PieceType.Centaur)

        scene.board.set_piece(2, 3, piece=PieceType.Scout)
        scene.board.set_piece(3, 4, piece=PieceType.Scout)
        scene.board.set_piece(5, 4, piece=PieceType.Scout)
        scene.board.set_piece(6, 3, piece=PieceType.Scout)

        scene.board.set_piece(15, 0, piece=PieceType.Centaur)

        scene.board.set_piece(13, 3, piece=PieceType.Scout)
        scene.board.set_piece(14, 4, piece=PieceType.Scout)
        scene.board.set_piece(16, 4, piece=PieceType.Scout)
        scene.board.set_piece(17, 3, piece=PieceType.Scout)

        scene.board.set_piece(4, 19, piece=-PieceType.Centaur)

        scene.board.set_piece(2, 16, piece=-PieceType.Scout)
        scene.board.set_piece(3, 15, piece=-PieceType.Scout)
        scene.board.set_piece(5, 15, piece=-PieceType.Scout)
        scene.board.set_piece(6, 16, piece=-PieceType.Scout)

        scene.board.set_piece(15, 19, piece=-PieceType.Centaur)

        scene.board.set_piece(13, 16, piece=-PieceType.Scout)
        scene.board.set_piece(14, 15, piece=-PieceType.Scout)
        scene.board.set_piece(16, 15, piece=-PieceType.Scout)
        scene.board.set_piece(17, 16, piece=-PieceType.Scout)

        return scene

    #
    # Grenadier-fields

    def scn_hd_40_grenadier_fields( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_40_grenadier_fields', bt, width=5, height=5 )

        start_G = (2, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        rect_G = (0.35, 0.5, 0.65, 0.1)
        coords_G_ = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_G, include_prev=False, count=1 )
        for i, arr in enumerate( coords_G_() ):
            scene.append_text( str(i+1), *arr, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal, rect=rect_G )

        return scene

    #
    # Grenadier/Movement

    def scn_hd_41_grenadier_movement( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_41_grenadier_movement', bt, x=5.6, y=0.6, width=7.8, height=5.8 )

        start_G = (9, 3)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_Gr_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=2 )
        for i, arr in enumerate( gen_Gu_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_Gl_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=2 )
        for i, arr in enumerate( gen_Gd_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_42_grenadier_movement_transition( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_42_grenadier_movement_transition', bt, width=7, height=3 )

        start_G = (1, 1)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_p = (3, 0)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_Gr_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        return scene

    #
    # Forking steps

    def scn_hd_43_forking_steps( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_43_forking_steps', bt, width=7, height=7 )

        start_G = (3, 3)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        adder_right = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder_right( 1, 0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_right( 1, 1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_right( 1, -1, do_advance=False ), mark_type=MarkType.Illegal )

        adder_up = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder_up( 0, 1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_up( 1, 1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_up( -1, 1, do_advance=False ), mark_type=MarkType.Illegal )

        adder_left = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder_left( -1, 0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_left( -1, 1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_left( -1, -1, do_advance=False ), mark_type=MarkType.Illegal )

        adder_down = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder_down( 0, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_down( 1, -1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_down( -1, -1, do_advance=False ), mark_type=MarkType.Illegal )

        return scene

    #
    # Close quarters

    def scn_hd_44_grenadier_vertical_steps( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_44_grenadier_vertical_steps', bt, width=5, height=7 )

        start_G = (2, 3)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_n = (1, 4)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (3, 1)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        adder_up = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder_up( 1, 1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_up( -1, 1, do_advance=False ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_up( 0, 1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_up( 1, 1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_up( -1, 1, do_advance=False ), mark_type=MarkType.Illegal )

        adder_down = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder_down( 1, -1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_down( -1, -1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder_down( 0, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_down( 1, -1, do_advance=False ), mark_type=MarkType.Action )
        scene.append_arrow( *adder_down( -1, -1, do_advance=False ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_45_grenadier_horizontal_steps( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_45_grenadier_horizontal_steps', bt, height=5 )

        start_G = (9, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_n = (10, 3)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_r = (8, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_p = (8, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_q = (12, 1)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=4 )
        for i, arr in enumerate( gen_Gr_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            mt_q = MarkType.Action if i == 1 else \
                   MarkType.Illegal
            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=mt_q )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=4 )
        for i, arr in enumerate( gen_Gl_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            scene.append_arrow( *GS.add_end_rel( arr, -1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, -1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_46_grenadier_close_quarters_transition( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_46_grenadier_close_quarters_transition', bt, width=7, height=3 )

        start_G = (1, 1)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_p = (0, 0)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_b = (0, 1)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_n = (0, 2)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_q = (6, 0)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        adder = GS.adder( start_G, include_prev=True )
        scene.append_arrow( *adder( 1, 0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( 1, 0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( 1, 0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( 1, 0, do_advance=True ), mark_type=MarkType.Legal )
        # scene.append_arrow( *adder( 1, -1, do_advance=False ), mark_type=MarkType.Action )
        # scene.append_arrow( *adder( 1, 1, do_advance=False ), mark_type=MarkType.Illegal )
        scene.append_arrow( *adder( 1, -1, do_advance=False ), mark_type=MarkType.Action )

        return scene

    def scn_hd_47_grenadier_blocked_steps( self, bt=BoardType.HemerasDawn ):

        # scene = Scene( 'scn_hd_47_grenadier_blocked_steps', bt, width=8, height=5 )
        scene = Scene( 'scn_hd_47_grenadier_blocked_steps', bt, y=0.6, width=8, height=3.8 )

        start_G = (1, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_b = (0, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_r = (0, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_p = (0, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_u = (4, 2)
        scene.board.set_piece( *start_u, piece=-PieceType.Unicorn )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=4 )
        for i, arr in enumerate( gen_Gr_() ):
            mt_arr = MarkType.Blocked if i >= 2 else \
                     MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_arr )

            mt_cpt = MarkType.Blocked if i >= 2 else \
                     MarkType.Illegal
            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=mt_cpt )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=mt_cpt )

        return scene

    def scn_hd_48_grenadier_not_blocked_steps( self, bt=BoardType.HemerasDawn ):

        # scene = Scene( 'scn_hd_48_grenadier_not_blocked_steps', bt, width=8, height=5 )
        scene = Scene( 'scn_hd_48_grenadier_not_blocked_steps', bt, y=0.6, width=8, height=3.8 )

        start_G = (1, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_b = (0, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_r = (0, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_p = (0, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (4, 2)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=4 )
        for i, arr in enumerate( gen_Gr_() ):
            mt_arr = MarkType.Blocked if i == 2 else \
                     MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_arr )

            # mt_cpt = MarkType.Blocked if i >= 2 else \
            #          MarkType.Illegal
            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_49_grenadier_close_quarters_pattern( self, bt=BoardType.HemerasDawn ):

        # scene = Scene( 'scn_hd_49_grenadier_close_quarters_pattern', bt, y=1, height=7 )
        scene = Scene( 'scn_hd_49_grenadier_close_quarters_pattern', bt, y=1.6, height=5.8 )

        start_G = (9, 4)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_n = (10, 5)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_w = (10, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_r = (8, 5)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_p_1 = (8, 3)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_b = (10, 2)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_q = (13, 3)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_Gr_() ):
            mt_w = MarkType.Blocked if i == 0 else \
                   MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_w )

            mt_q = MarkType.Action if i == 2 else \
                   MarkType.Illegal
            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=mt_q )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, arr in enumerate( gen_Gu_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, -1, 1 ), mark_type=MarkType.Illegal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_Gl_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            scene.append_arrow( *GS.add_end_rel( arr, -1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, -1, -1 ), mark_type=MarkType.Illegal )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, arr in enumerate( gen_Gd_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            scene.append_arrow( *GS.add_end_rel( arr, -1, -1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_50_grenadier_activated( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_50_grenadier_activated', bt, width=7, height=5 )

        start_N = (6, 1)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W = (4, 0)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_G = (3, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        scene.append_arrow( *( start_N + start_W ), mark_type=MarkType.Action )
        scene.append_arrow( *( start_W + start_G ), mark_type=MarkType.Action )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_Gr_() ):
            mt_r = MarkType.Legal if i < 1 else \
                   MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mt_r )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=2 )
        for i, arr in enumerate( gen_Gu_() ):
            mt_u = MarkType.Legal if i < 1 else \
                   MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mt_u )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_Gl_() ):
            mt_l = MarkType.Legal if i < 1 else \
                   MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mt_l )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=2 )
        for i, arr in enumerate( gen_Gd_() ):
            mt_d = MarkType.Legal if i < 1 else \
                   MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mt_d )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_51_grenadier_close_quarters_activation( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_51_grenadier_close_quarters_activation', bt, y=2, height=5 )

        start_E = (1, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (7, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_G = (9, 4)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_n = (10, 5)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_w = (10, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_r = (8, 5)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_p_1 = (8, 3)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (10, 2)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        start_q = (13, 3)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        # E --> W
        gen_E_W = GS.gen_steps( start=start_E, rels=[ (2, 1), ], include_prev=True, count=3 )
        for i, arr in enumerate( gen_E_W() ):
            mt_e = MarkType.Action if i == 2 else \
                   MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_e )

        # W --> G
        scene.append_arrow( *( start_W + start_G ), mark_type=MarkType.Action )

        return scene

    def scn_hd_52_grenadier_close_quarters_activated( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_52_grenadier_close_quarters_activated', bt, y=2, height=5 )

        prev_E = (1, 2)
        prev_W = (7, 5)
        prev_G = (9, 4)

        start_E = prev_W # (0, 7)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = prev_G # (3, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_G = prev_G # (9, 4)
        # scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_n = (10, 5)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_w = (10, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_r = (8, 5)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_p_1 = (8, 3)
        scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        start_p_2 = (10, 2)
        scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        start_q = (13, 3)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_Gr_() ):
            mt_r = MarkType.Blocked if i == 0 else \
                   MarkType.Legal if i < 3 else \
                   MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mt_r )

            mt_r_c = MarkType.Illegal if i < 2 else \
                     MarkType.Blocked
            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=mt_r_c )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=mt_r_c )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, arr in enumerate( gen_Gu_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, -1, 1 ), mark_type=MarkType.Illegal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arr in enumerate( gen_Gl_() ):
            mt_l = MarkType.Legal if i < 3 else \
                   MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mt_l )

            mt_lu_c = MarkType.Blocked if i == 0 else \
                      MarkType.Illegal if i < 2 else \
                      MarkType.Blocked
            mt_ld_c = MarkType.Illegal if i < 2 else \
                      MarkType.Blocked
            scene.append_arrow( *GS.add_end_rel( arr, -1, 1 ), mark_type=mt_lu_c )
            scene.append_arrow( *GS.add_end_rel( arr, -1, -1 ), mark_type=mt_ld_c )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, arr in enumerate( gen_Gd_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            scene.append_arrow( *GS.add_end_rel( arr, -1, -1 ), mark_type=MarkType.Illegal )
            scene.append_arrow( *GS.add_end_rel( arr, 1, -1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_53_grenadier_activating_wave_step_field( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_53_grenadier_activating_wave_step_field', bt, x=8, y=9, width=5, height=3 )

        start_G = (9, 10)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_W = (11, 10)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # G --> W
        gen_G_W = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, arr in enumerate( gen_G_W() ):
            mt_g = MarkType.Action if i == 1 else \
                   MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_g )

        return scene

    def scn_hd_54_grenadier_activated_wave_step_field( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_54_grenadier_activated_wave_step_field', bt )

        prev_G = (9, 10)
        prev_W = (11, 10)

        start_G = prev_W # (9, 10)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_W = prev_W # (11, 10)
        # scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_B = (14, 10)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (11, 17)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # W --> (1, 0) -->
        gen_W_r_ = GS.gen_steps( start=start_W, rels=[ (1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_r_() ):
            mt_wr = MarkType.Action if i == 2 else \
                    MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_wr )

        # W --> (0, 1) -->
        gen_W_u_ = GS.gen_steps( start=start_W, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_u_() ):
            mt_wu = MarkType.Blocked if i == 6 else \
                    MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_wu )

        # W --> (-1, 0) -->
        gen_W_l_ = GS.gen_steps( start=start_W, rels=[ (-1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_l_() ):
            mt_wl = MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_wl )

        # W --> (0, -1) -->
        gen_W_d_ = GS.gen_steps( start=start_W, rels=[ (0, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_d_() ):
            mt_wd = MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_wd )

        scene.append_text( "G", *prev_G, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_hd_55_grenadier_activating_wave_capture_field( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_55_grenadier_activating_wave_capture_field', bt, x=7, y=8, width=7, height=4 )

        start_G = (8, 9)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_W = (11, 10)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_n = (7, 10)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        # G --> W
        gen_G_W = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, arr in enumerate( gen_G_W() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

            if i == 1:
                scene.append_arrow( *GS.add_end_rel( arr, 1, 1 ), mark_type=MarkType.Action )

        return scene

    def scn_hd_56_grenadier_activated_wave_capture_field( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_56_grenadier_activated_wave_capture_field', bt )

        prev_G = (8, 9)
        prev_W = (11, 10)
        prev_n = (7, 10)

        start_G = prev_W # (8, 9)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_W = prev_W # (11, 10)
        # scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_n = prev_n # (7, 10)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_B = GS.add_rel( start_W, 3, 3 )
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = GS.add_rel( start_W, -7, 7 )
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # W --> (1, 1) -->
        gen_W_ru_ = GS.gen_steps( start=start_W, rels=[ (1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_ru_() ):
            mt_wru = MarkType.Action if i == 2 else \
                     MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_wru )

        # W --> (-1, 1) -->
        gen_W_lu_ = GS.gen_steps( start=start_W, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_lu_() ):
            mt_wlu = MarkType.Action if i == 6 else \
                    MarkType.Legal
            scene.append_arrow( *arr, mark_type=mt_wlu )

        # W --> (-1, -1) -->
        gen_W_ld_ = GS.gen_steps( start=start_W, rels=[ (-1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_ld_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # W --> (1, -1) -->
        gen_W_rd_ = GS.gen_steps( start=start_W, rels=[ (1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_W_rd_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_text( "G", *prev_G, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_hd_57_grenadier_en_passant( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_57_grenadier_en_passant', bt, width=8, height=10.3 )

        #
        # A

        start_P_A = (1, 1)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_g_A = (2, 5)
        scene.board.set_piece( *start_g_A, piece=-PieceType.Grenadier )

        # P(A) -->
        gen_PA_ = GS.gen_steps( start=start_P_A, rels=[ (0, 1), ], include_prev=True, count=8 )
        for i, arr in enumerate( gen_PA_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        # G(A) -->
        scene.append_arrow( *GS.append_pos_rel( start_g_A, -1, -1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_g_A, -1, 1 ), mark_type=MarkType.Action )

        #
        # B

        start_P_B = (4, 1)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_g_B = (6, 5)
        scene.board.set_piece( *start_g_B, piece=-PieceType.Grenadier )

        start_B = (6, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # G(B) -->
        adder = GS.adder( start_g_B, include_prev=True )
        scene.append_arrow( *adder( -1, 0, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( -1, -1, do_advance=False ), mark_type=MarkType.Action )
        scene.append_arrow( *adder( -1, 1, do_advance=False ), mark_type=MarkType.Action )

        # P(B) -->
        gen_PB_ = GS.gen_steps( start=start_P_B, rels=[ (0, 1), ], include_prev=True, count=8 )
        for i, arr in enumerate( gen_PB_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_text( "A", *start_g_A, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_g_B, corner=Corner.UpperRight, mark_type=MarkType.Legal )

        return scene

    def scn_hd_58_grenadier_en_passant_self_extended( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_58_grenadier_en_passant_self_extended', bt, width=8, height=10.3 )

        #
        # 1

        start_P_1 = (1, 1)
        scene.board.set_piece( *start_P_1, piece=PieceType.Pawn )

        start_g_1 = (2, 8)
        scene.board.set_piece( *start_g_1, piece=-PieceType.Grenadier )

        # P -->
        gen_P_ = GS.gen_steps( start=start_P_1, rels=[ (0, 1), ], include_prev=True, count=8 )
        for i, arr in enumerate( gen_P_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Legal )

        scene.append_text( "1", *start_P_1, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "1", *start_g_1, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        #
        # 2

        prev_P_2 = (5, 1)
        start_P_2 = (5, 7) # (5, 9)
        scene.board.set_piece( *start_P_2, piece=PieceType.Pawn )

        start_g_2 = (6, 8)
        scene.board.set_piece( *start_g_2, piece=-PieceType.Grenadier )

        # --> P
        gen__P_ = GS.gen_steps( start=prev_P_2, rels=[ (0, 1), ], include_prev=True, count=6 ) # count=8 )
        for i, arr in enumerate( gen__P_() ):
            scene.append_arrow( *arr, mark_type=MarkType.Blocked )

        # G -->
        adder = GS.adder( start_g_2, include_prev=True )
        scene.append_arrow( *adder( 0, -1, do_advance=True ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder( -1, -1, do_advance=False ), mark_type=MarkType.Action )

        # scene.append_arrow( *GS.append_pos_rel( start_g_2, -1, -1 ), mark_type=MarkType.Action )
        # scene.append_arrow( *GS.append_pos_rel( start_g_2, -1, 1 ), mark_type=MarkType.Action )

        scene.append_text( "2", *start_P_2, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "2", *start_g_2, corner=Corner.UpperRight, mark_type=MarkType.Action )

        return scene

    #
    # Grenadier/Initial positions

    def scn_hd_59_grenadier_initial_positions(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_59_grenadier_initial_positions', bt)

        #
        # Light

        scene.board.set_piece( 4, 0, piece=PieceType.Centaur )

        scene.board.set_piece( 2, 2, piece=PieceType.Grenadier )
        scene.board.set_piece( 3, 1, piece=PieceType.Grenadier )
        scene.board.set_piece( 5, 1, piece=PieceType.Grenadier )
        scene.board.set_piece( 6, 2, piece=PieceType.Grenadier )

        scene.append_text( "O", 2, 3, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "O", 3, 4, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "O", 5, 4, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "O", 6, 3, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        scene.board.set_piece( 15, 0, piece=PieceType.Centaur )

        scene.board.set_piece( 13, 2, piece=PieceType.Grenadier )
        scene.board.set_piece( 14, 1, piece=PieceType.Grenadier )
        scene.board.set_piece( 16, 1, piece=PieceType.Grenadier )
        scene.board.set_piece( 17, 2, piece=PieceType.Grenadier )

        scene.append_text( "O", 13, 3, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "O", 14, 4, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "O", 16, 4, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "O", 17, 3, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        #
        # Dark

        scene.board.set_piece( 4, 19, piece=-PieceType.Centaur )

        scene.board.set_piece( 2, 17, piece=-PieceType.Grenadier )
        scene.board.set_piece( 3, 18, piece=-PieceType.Grenadier )
        scene.board.set_piece( 5, 18, piece=-PieceType.Grenadier )
        scene.board.set_piece( 6, 17, piece=-PieceType.Grenadier )

        scene.append_text( "o", 2, 16, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "o", 3, 15, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "o", 5, 15, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "o", 6, 16, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        scene.board.set_piece( 15, 19, piece=-PieceType.Centaur )

        scene.board.set_piece( 13, 17, piece=-PieceType.Grenadier )
        scene.board.set_piece( 14, 18, piece=-PieceType.Grenadier )
        scene.board.set_piece( 16, 18, piece=-PieceType.Grenadier )
        scene.board.set_piece( 17, 17, piece=-PieceType.Grenadier )

        scene.append_text( "o", 13, 16, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "o", 14, 15, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "o", 16, 15, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "o", 17, 16, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene
