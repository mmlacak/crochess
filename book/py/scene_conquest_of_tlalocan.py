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


class SceneConquestOfTlalocanMixin:

    #
    # Movement

    def scn_cot_001_shaman_movement(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_001_shaman_movement', bt)

        #
        # light Shaman

        start_lH = (6, 6)
        scene.board.set_piece(*start_lH, piece=PieceType.Shaman)

        # light Shaman, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_lH, include_prev=False, bounds=((2, 2), (10, 10)))

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        # light Shaman, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_lH, include_prev=False, bounds=((4, 4), (8, 8)))

        for i, pos in enumerate( gen_abs_pos_2() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRightFieldMarker)

        #
        # dark Shaman

        start_dH = (17, 17)
        scene.board.set_piece(*start_dH, piece=-PieceType.Shaman)

        # dark Shaman, long jump

        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_dH, include_prev=False, bounds=((13, 13), (21, 21)))

        for i, pos in enumerate( gen_abs_pos_3() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        # dark Shaman, short jump

        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_dH, include_prev=False, bounds=((15, 15), (19, 19)))

        for i, pos in enumerate( gen_abs_pos_4() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        #
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

        #
        # light Shaman 2

        start_lH2 = (17, 6)
        scene.board.set_piece(*start_lH2, piece=PieceType.Shaman)

        # light Shaman 2, long jump

        gen_abs_pos_5 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_lH2, include_prev=False, bounds=((13, 2), (21, 10)))

        for i, pos in enumerate( gen_abs_pos_5() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        # light Shaman 2, short jump

        gen_abs_pos_6 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_lH2, include_prev=False, bounds=((15, 4), (19, 8)))

        for i, pos in enumerate( gen_abs_pos_6() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRightFieldMarker)

        #
        # dark Shaman 2

        start_dH2 = (6, 17)
        scene.board.set_piece(*start_dH2, piece=-PieceType.Shaman)

        # dark Shaman 2, long jump

        gen_abs_pos_7 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_dH2, include_prev=False, bounds=((2, 13), (10, 21)))

        for i, pos in enumerate( gen_abs_pos_7() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        # dark Shaman 2, short jump

        gen_abs_pos_8 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_dH2, include_prev=False, bounds=((4, 15), (8, 19)))

        for i, pos in enumerate( gen_abs_pos_8() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        return scene

    #
    # Light Shaman's step-ply

    def scn_cot_002_light_shaman_step_ply( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_002_light_shaman_step_ply', bt, height=13.4 )

        start = (1, 1)
        scene.board.set_piece( *start, piece=PieceType.Shaman )

        scene.board.set_piece( 9, 5, piece=PieceType.Wave )
        scene.board.set_piece( 19, 10, piece=PieceType.Pyramid )
        scene.board.set_piece( 10, 9, piece=-PieceType.Knight )

        gen_arr = GS.gen_steps( [ (2, 1), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_arr() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Blocked if i > 7 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        #
        # No changing direction, step-fields.

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, -1), (2, 1)] ) )
        gen_arr_2 = GS.gen_multi_steps(multi_rels, start=(5, 3), include_prev=True, count=1)
        for arr_2 in gen_arr_2():
            scene.append_arrow( *arr_2, mark_type=MarkType.Illegal )

        #
        # No changing direction, capture-fields.

        gen_arr_3 = GS.gen_multi_steps( GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=(13, 7), include_prev=True, count=1 )
        for arr_3 in gen_arr_3():
            scene.append_arrow( *arr_3, mark_type=MarkType.Illegal )

        return scene

    def scn_cot_003_light_shaman_step_ply_no_capture( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_003_light_shaman_step_ply_no_capture', bt, width=9.0, height=6.0 )

        start = (1, 1)
        scene.board.set_piece( *start, piece=PieceType.Shaman )
        scene.board.set_piece( 5, 3, piece=-PieceType.Knight )

        gen_arr = GS.gen_steps( [(2, 1), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_arr() ):
            mark_type = MarkType.Blocked if i >= 1 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    #
    # Light Shaman's capture-ply

    def scn_cot_004_light_shaman_capture_ply( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_004_light_shaman_capture_ply', bt, height=9.4 )

        direction = (4, 1)
        opposite_dir = GS.negate( direction )

        start = (1, 1)
        scene.board.set_piece( *start, piece=PieceType.Shaman )

        scene.board.set_piece( *GS.add_step( start, (10, 6) ), piece=-PieceType.Knight )

        coords = GS.gen_steps( [ direction, ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Legal if i < 3 else \
                        MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mark_type )

            if i != 3:
                pos = GS.get_end( arr )
                scene.board.set_piece( *pos, piece=-PieceType.Pawn )

        #
        # No changing direction, step-fields.

        gen_arr_2 = GS.gen_multi_steps( GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=(5, 2), include_prev=True, count=1 )
        for arr_2 in gen_arr_2():
            scene.append_arrow( *arr_2, mark_type=MarkType.Illegal )

        #
        # No changing direction, capture-fields.

        multi_rels_3 = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [ direction, opposite_dir, ] ) )
        gen_arr_3 = GS.gen_multi_steps( multi_rels_3, start=(13, 4), include_prev=True, count=1 )
        for arr_3 in gen_arr_3():
            scene.append_arrow( *arr_3, mark_type=MarkType.Illegal )

        return scene

    def scn_cot_005_light_shaman_capture_ply_passives( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_005_light_shaman_capture_ply_passives', bt, width=9.4, height=3.4 )

        start_H = (0, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W = (4, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A = (8, 2)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        scene.append_arrow( *( start_H + start_W ), mark_type=MarkType.Action )
        scene.append_arrow( *( start_W + start_A ), mark_type=MarkType.Blocked )

        return scene

    #
    # Dark Shaman's step-ply

    def scn_cot_006_dark_shaman_step_ply( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_006_dark_shaman_step_ply', bt, height=9.4 )

        direction = (4, 1)
        opposite_dir = GS.negate( direction )

        start = (1, 1)
        scene.board.set_piece( *start, piece=-PieceType.Shaman )

        scene.board.set_piece( 9, 3, piece=PieceType.Wave )
        scene.board.set_piece( 17, 5, piece=PieceType.Pyramid )
        # scene.board.set_piece( *GS.add_step( start, (10, 6) ), piece=PieceType.Knight )

        coords = GS.gen_steps( [ direction, ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal if i < 3 else \
                        MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mark_type )

        #
        # No changing direction, step-fields.

        # gen_arr_2 = GS.gen_multi_steps( GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=(5, 2), include_prev=True, count=1 )
        # for arr_2 in gen_arr_2():
        #     scene.append_arrow( *arr_2, mark_type=MarkType.Illegal )

        #
        # No changing direction, capture-fields.

        # multi_rels_3 = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [ direction, opposite_dir, ] ) )
        # gen_arr_3 = GS.gen_multi_steps( multi_rels_3, start=(13, 4), include_prev=True, count=1 )
        # for arr_3 in gen_arr_3():
        #     scene.append_arrow( *arr_3, mark_type=MarkType.Illegal )

        return scene

    def scn_cot_007_dark_shaman_step_ply_no_capture( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_007_dark_shaman_step_ply_no_capture', bt, width=7.4, height=10.4 )

        start = (0, 0)
        scene.board.set_piece( *start, piece=-PieceType.Shaman )
        scene.board.set_piece( 4, 6, piece=PieceType.Knight )

        gen_arr = GS.gen_steps( [(2, 3), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_arr() ):
            mark_type = MarkType.Blocked if i >= 1 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    #
    # Dark Shaman's capture-ply

    def scn_cot_008_dark_shaman_capture_ply( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_008_dark_shaman_capture_ply', bt, height=12.4 )

        direction = (2, 1)
        opposite_dir = GS.negate( direction )

        start = (0, 0)
        scene.board.set_piece( *start, piece=-PieceType.Shaman )

        coords = GS.gen_steps( [ direction, ], start=start, include_prev=False, bounds=scene.board_view.get_position_limits() )
        for i, pos in enumerate( coords() ):
            if i != 8:
                scene.board.set_piece( *pos, piece=PieceType.Pawn )

        coords = GS.gen_steps( [ direction, ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Legal if i < 8 else \
                        MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mark_type )

        #
        # No changing direction, step-fields.

        # multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [ opposite_dir, direction ] ) )
        # rel_steps = GS.multiply_all( direction, 2 )
        # start_illegal_steps = GS.add_step( start, rel_steps )
        # gen_pos = GS.gen_multi_steps( multi_rels, start=start_illegal_steps, include_prev=True, count=1 )
        # for pos in gen_pos():
        #     scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        #
        # No changing direction, capture-fields.

        # rel_captures = GS.multiply_all( direction, 6 )
        # start_illegal_captures = GS.add_step( start, rel_captures )
        # scene.board.set_piece( *GS.add_step( start_illegal_captures, (-3, 2) ), piece=PieceType.Knight )

        # gen_pos = GS.gen_multi_steps( GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_illegal_captures, include_prev=True, count=1 )
        # for pos in gen_pos():
        #     scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        return scene

    def scn_cot_009_dark_shaman_capture_ply_passives( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_009_dark_shaman_capture_ply_passives', bt, width=8.4, height=5.4 )

        start_H = (1, 1)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W = (3, 2)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A = (5, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        end_A = (7, 4)

        scene.append_arrow( *( start_H + start_W ), mark_type=MarkType.Action )
        scene.append_arrow( *( start_W + start_A ), mark_type=MarkType.Blocked )
        scene.append_arrow( *( start_A + end_A ), mark_type=MarkType.Blocked )

        return scene

    #
    # Activating Wave

    def scn_cot_010_activating_wave_step_field( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_010_activating_wave_step_field', bt, width=9.4, height=5.4 )

        start_H = (5, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W = (7, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A = (3, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_N = (1, 4)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        scene.append_arrow( *( start_H + start_W ), mark_type=MarkType.Action )

        gen_arr_2 = GS.gen_steps( [ (-2, 1), ], start=start_W, include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_arr_2() ):
            mark_type = MarkType.Blocked if i == 1 else \
                        MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_cot_011_activating_wave_capture_field( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_011_activating_wave_capture_field', bt, width=9.4, height=10.4 )

        start_H = (5, 1)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W = (1, 0)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A = (3, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_N = (7, 9)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        scene.append_arrow( *( start_H + start_W ), mark_type=MarkType.Action )

        gen_arr_2 = GS.gen_steps( [ (2, 3), ], start=start_W, include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )
        for i, arr in enumerate( gen_arr_2() ):
            mark_type = MarkType.Action if i in [ 0, 2 ] else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    #
    # Shaman is semi-transparent

    def scn_cot_012_shaman_transparent_to_own_pieces( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_012_shaman_transparent_to_own_pieces', bt, width=9.4, height=6.4 )

        start_B = (2, 1)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_H = (4, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        coords = GS.gen_steps( start=start_B, rels=[ (1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_cot_013_shaman_not_transparent_to_opponents_pieces( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_013_shaman_not_transparent_to_opponents_pieces', bt, width=9.4, height=6.4 )

        start_b = (2, 1)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_H = (4, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        coords = GS.gen_steps( start=start_b, rels=[ (1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Legal if i < 1 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_cot_014_shaman_transparent_to_opponents_shaman( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_014_shaman_transparent_to_opponents_shaman', bt, width=9.4, height=6.4 )

        start_H = (4, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_h = (0, 1)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        coords = GS.gen_steps( start=start_h, rels=[ (2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    #
    # Unicorn is transparent to Shamans

    def scn_cot_015_unicorn_is_transparent_to_own_shaman( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_015_unicorn_is_transparent_to_own_shaman', bt, width=9.4, height=3.4 )

        start_H = (0, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_U = (4, 1)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        coords = GS.gen_steps( start=start_H, rels=[ (4, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Blocked if i == 0 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_cot_016_unicorn_is_transparent_to_opponents_shaman( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_016_unicorn_is_transparent_to_opponents_shaman', bt, width=9.4, height=3.4 )

        start_h = (1, 0)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        start_U = (3, 1)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        coords = GS.gen_steps( start=start_h, rels=[ (2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Action if i == 0 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_cot_017_unicorn_is_opaque_to_own_pieces( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_017_unicorn_is_opaque_to_own_pieces', bt, width=9.4, height=4.4 )

        start_E = (1, 0)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_U = (5, 2)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        coords = GS.gen_steps( start=start_E, rels=[ (2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Legal if i < 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_cot_018_unicorn_is_opaque_to_opponents_pieces( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_018_unicorn_is_opaque_to_opponents_pieces', bt, width=9.4, height=4.4 )

        start_e = (1, 0)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_U = (5, 2)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        coords = GS.gen_steps( start=start_e, rels=[ (2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=4 )
        for i, arr in enumerate( coords() ):
            mark_type = MarkType.Legal if i < 1 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    #
    # Teleporting Shaman

    def scn_cot_019_teleport_shaman_all(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_019_teleport_shaman_all', bt)

        start_H_A = (9, 17)
        start_T = (0, 23)

        start_T2 = (23, 0)
        start_H_B= (22, 3)
        start_H_C = (15, 4)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)
        scene.board.set_piece(*start_T2, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(3, 21, piece=-PieceType.Bishop)
        scene.board.set_piece(6, 19, piece=-PieceType.Knight)
        scene.board.set_piece(*start_H_A, piece=PieceType.Shaman)
        scene.board.set_piece(*start_H_B, piece=PieceType.Shaman)
        scene.board.set_piece(*start_H_C, piece=PieceType.Shaman)

        # Shaman A
        coords = GS.gen_steps(start=start_H_A, rels=[(-3, 2), ], include_prev=True, count=3)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        # Shaman B
        scene.append_arrow( *(start_H_B + start_T2), mark_type=MarkType.Action )

        # Shaman C
        coords = GS.gen_steps(start=start_H_C, rels=[(2, -1), ], include_prev=True, count=4)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Action if index == 3 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        # portal-fields
        scene.append_text("1", 22, 23, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("2", 22, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 23, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        # Shamans
        scene.append_text("A", *start_H_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("B", *start_H_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("C", *start_H_C, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    #
    # Teleporting Pawn

    def scn_cot_020_teleport_pawn_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_020_teleport_pawn_init', bt)

        start_T = (0, 23)
        start_P = (1, 22)
        start_P_B = (0, 22)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        # Pawns
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)
        scene.board.set_piece(*start_P_B, piece=PieceType.Pawn)

        # scene.append_arrow( *(start_P + start_T), mark_type=MarkType.Action )
        scene.append_arrow( *(start_P_B + start_T), mark_type=MarkType.Action )

        # portal-fields
        scene.append_text("1", 22, 23, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        scene.append_text("2", 22, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 23, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    #
    # Divergence

    # TODO :: reindex

    def scn_cot_030_own_shaman_is_divergent_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_030_own_shaman_is_divergent_init', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (14, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_h = (16, 3)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        start_H = (9, 6)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_e = (12, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_p = (6, 3)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # Q --> H
        coords_Q_H = GS.gen_steps( start=start_Q, rels=[(-1, 1), ], include_prev=True, count=5 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_H() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q --> h
        coords_Q_h = GS.gen_steps( start=start_Q, rels=[(1, 1), ], include_prev=True, count=2 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_h() ):
            mark_type = MarkType.Illegal if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )

        return scene

    def scn_cot_031_own_shaman_is_divergent_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_031_own_shaman_is_divergent_end', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (14, 1)
        # scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_h = (16, 3)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        start_H = (9, 6)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_e = (12, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_p = (6, 3)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # <-- Q --> @ W

        for rel in GS.DEFAULT_KING_REL_MOVES:
            coords_W__Q = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_W__Q() ):
                if rel in [ (-1, -1), (1, 1), ]:
                    mark_type = MarkType.Action if i == 2 else \
                                MarkType.Legal if i < 2 else \
                                MarkType.Blocked
                elif rel == (1, -1):
                    mark_type = MarkType.Illegal if i == 4 else \
                                MarkType.Legal if i < 5 else \
                                MarkType.Blocked
                else:
                    mark_type = MarkType.Legal if i < 5 else \
                                MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, mark_type=MarkType.Illegal, corner=Corner.UpperRightFieldMarker )

        return scene

    #
    # Diverging activated piece

    def scn_cot_032_diverging_activated_piece_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_032_diverging_activated_piece_init', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (1, 13)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W = (6, 8)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_R = (9, 11)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_H_A = (12, 11)
        scene.board.set_piece( *start_H_A, piece=PieceType.Shaman )

        start_H_B = (9, 6)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q, rels=[(1, -1), ], include_prev=True, count=5 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> R
        coords_W_R = GS.gen_steps( start=start_W, rels=[(1, 1), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_W_R() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R --> H(A)
        coords_R_HA = GS.gen_steps( start=start_R, rels=[(1, 0), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_R_HA() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal if i < 4 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R --> H(B)
        coords_R_HB = GS.gen_steps( start=start_R, rels=[(0, -1), ], include_prev=True, count=5 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_R_HB() ):
            mark_type = MarkType.Illegal if i == 4 else \
                        MarkType.Legal if i < 4 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A, mark_type=MarkType.Action, corner=Corner.UpperRight )
        scene.append_text( "B", *start_H_B, mark_type=MarkType.Illegal, corner=Corner.UpperRight )

        return scene

    def scn_cot_033_diverging_activated_piece_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_033_diverging_activated_piece_end', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        prev_Q = (1, 13)
        start_Q = (6, 8)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        prev_W = (6, 8)
        start_W = (9, 11)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        prev_R = (9, 11)
        # scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_H_A = (12, 11)
        scene.board.set_piece( *start_H_A, piece=PieceType.Shaman )

        start_H_B = (9, 6)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        # | <-- R --> |
        for rel in GS.DEFAULT_ROOK_REL_MOVES:
            coords_R_ = GS.gen_steps( start=start_H_A, rels=[rel, ], include_prev=True, bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_R_() ):
                mark_type = MarkType.Legal if i < 2 else \
                            MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A, mark_type=MarkType.Action, corner=Corner.UpperRight )
        scene.append_text( "B", *start_H_B, mark_type=MarkType.Illegal, corner=Corner.UpperRight )

        scene.append_text( "Q", *prev_Q, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    #
    # Diverging Pawn

    def scn_cot_034_diverging_pawn_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_034_diverging_pawn_init', bt)

        # sideways Pawn

        start_P_A = (3, 5)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_H_A = (2, 5)
        scene.board.set_piece( *start_H_A, piece=PieceType.Shaman )

        start_n_A = (1, 6)
        scene.board.set_piece( *start_n_A, piece=-PieceType.Knight )

        scene.append_arrow( *( start_P_A + start_H_A ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_P_A, corner=Corner.UpperRight, mark_type=MarkType.Action )

        # capture Pawn

        start_P_B = (7, 4)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_H_B = (6, 5)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        start_n_B = (7, 6)
        scene.board.set_piece( *start_n_B, piece=-PieceType.Knight )

        scene.append_arrow( *( start_P_B + start_H_B ), mark_type=MarkType.Action )

        scene.append_text( "B", *start_P_B, corner=Corner.UpperRight, mark_type=MarkType.Action )

        # rushing Pawn

        start_P_C = (11, 1)
        scene.board.set_piece( *start_P_C, piece=PieceType.Pawn )

        start_H_C = (11, 5)
        scene.board.set_piece( *start_H_C, piece=PieceType.Shaman )

        start_n_C = (10, 6)
        scene.board.set_piece( *start_n_C, piece=-PieceType.Knight )

        # P(C) --> W
        coords_PC_W = GS.gen_steps( start=start_P_C, rels=[(0, 1), ], include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PC_W() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "C", *start_P_C, corner=Corner.UpperRight, mark_type=MarkType.Action )

        # forward Pawn

        start_P_D = (15, 4)
        scene.board.set_piece( *start_P_D, piece=PieceType.Pawn )

        start_H_D = (15, 5)
        scene.board.set_piece( *start_H_D, piece=PieceType.Shaman )

        start_n_D = (16, 6)
        scene.board.set_piece( *start_n_D, piece=-PieceType.Knight )

        scene.append_arrow( *( start_P_D + start_H_D ), mark_type=MarkType.Action )

        scene.append_text( "D", *start_P_D, corner=Corner.UpperRight, mark_type=MarkType.Action )

        # activated Pawn

        start_Q_E = (22, 11)
        scene.board.set_piece( *start_Q_E, piece=PieceType.Queen )

        start_W_E = (22, 6)
        scene.board.set_piece( *start_W_E, piece=PieceType.Wave )

        start_P_E = (20, 4)
        scene.board.set_piece( *start_P_E, piece=PieceType.Pawn )

        start_H_E = (19, 5)
        scene.board.set_piece( *start_H_E, piece=PieceType.Shaman )

        start_n_E = (20, 6)
        scene.board.set_piece( *start_n_E, piece=-PieceType.Knight )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q_E, rels=[(0, -1), ], include_prev=True, count=5 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> P
        coords_W_P = GS.gen_steps( start=start_W_E, rels=[(-1, -1), ], include_prev=True, count=2 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_W_P() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_arrow( *( start_P_E + start_H_E ), mark_type=MarkType.Action )

        scene.append_text( "E", *start_P_E, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        return scene

    def scn_cot_035_diverging_pawn_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_035_diverging_pawn_end', bt)

        # sideways Pawn

        start_H_A = (2, 5)
        scene.board.set_piece( *start_H_A, piece=PieceType.Shaman )

        start_n_A = (1, 6)
        scene.board.set_piece( *start_n_A, piece=-PieceType.Knight )

        scene.append_arrow( *GS.append_pos_rel( start_H_A, -1, 0 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_A, -1, 1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_H_A, 0, 1 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_A, 1, 1 ), mark_type=MarkType.Illegal )

        scene.append_arrow( *GS.append_pos_rel( start_H_A, 1, 0 ), mark_type=MarkType.Legal )

        scene.append_text( "A", *start_H_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        # capture Pawn

        start_H_B = (6, 5)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        start_n_B = (7, 6)
        scene.board.set_piece( *start_n_B, piece=-PieceType.Knight )

        scene.append_arrow( *GS.append_pos_rel( start_H_B, -1, 0 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_B, -1, 1 ), mark_type=MarkType.Illegal )

        scene.append_arrow( *GS.append_pos_rel( start_H_B, 0, 1 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_B, 1, 1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_H_B, 1, 0 ), mark_type=MarkType.Legal )

        scene.append_text( "B", *start_H_B, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        # rushing Pawn

        start_H_C = (11, 5)
        scene.board.set_piece( *start_H_C, piece=PieceType.Shaman )

        start_n_C = (10, 6)
        scene.board.set_piece( *start_n_C, piece=-PieceType.Knight )

        scene.append_arrow( *GS.append_pos_rel( start_H_C, -1, 0 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_C, -1, 1 ), mark_type=MarkType.Action )

        # P(C) @ H(C) -->
        coords_PC_H = GS.gen_steps( start=start_H_C, rels=[(0, 1), ], include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PC_H() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_C, 1, 1 ), mark_type=MarkType.Illegal )

        scene.append_arrow( *GS.append_pos_rel( start_H_C, 1, 0 ), mark_type=MarkType.Legal )

        scene.append_text( "C", *start_H_C, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        # forward Pawn

        # start_P_D = (15, 4)
        # scene.board.set_piece( *start_P_D, piece=PieceType.Pawn )

        start_H_D = (15, 5)
        scene.board.set_piece( *start_H_D, piece=PieceType.Shaman )

        start_n_D = (16, 6)
        scene.board.set_piece( *start_n_D, piece=-PieceType.Knight )

        scene.append_arrow( *GS.append_pos_rel( start_H_D, -1, 0 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_D, -1, 1 ), mark_type=MarkType.Illegal )

        scene.append_arrow( *GS.append_pos_rel( start_H_D, 0, 1 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_D, 1, 1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_H_D, 1, 0 ), mark_type=MarkType.Legal )

        scene.append_text( "D", *start_H_D, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        # activated Pawn

        prev_Q_E = (22, 11)
        start_Q_E = (22, 6)
        scene.board.set_piece( *start_Q_E, piece=PieceType.Queen )

        # prev_W_E = (22, 6)
        start_W_E = (20, 4)
        scene.board.set_piece( *start_W_E, piece=PieceType.Wave )

        # prev_P_E = (20, 4)
        # scene.board.set_piece( *start_P_E, piece=PieceType.Pawn )

        start_H_E = (19, 5)
        scene.board.set_piece( *start_H_E, piece=PieceType.Shaman )

        start_n_E = (20, 6)
        scene.board.set_piece( *start_n_E, piece=-PieceType.Knight )

        scene.append_arrow( *GS.append_pos_rel( start_H_E, -1, 0 ), mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_H_E, -1, 1 ), mark_type=MarkType.Illegal )

        # scene.append_arrow( *GS.append_pos_rel( start_H_E, 0, 1 ), mark_type=MarkType.Legal )

        # P(E) @ H(E) -->
        coords_PE_H = GS.gen_steps( start=start_H_E, rels=[(0, 1), ], include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PE_H() ):
            mark_type = MarkType.Legal if i == 0 else \
                        MarkType.Illegal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_arrow( *GS.append_pos_rel( start_H_E, 1, 1 ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( start_H_E, 1, 0 ), mark_type=MarkType.Legal )

        scene.append_text( "E", *start_H_E, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        return scene

    #
    # Diverging rushing Pawn

    def scn_cot_036_diverging_rushing_pawn(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_036_diverging_rushing_pawn', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        # stop before

        start_P_A = (4, 1)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_H_A = (4, 3)
        scene.board.set_piece( *start_H_A, piece=PieceType.Shaman )

        # P(A) --> W
        coords_PA_W = GS.gen_steps( start=start_P_A, rels=[(0, 1), ], include_prev=True, count=6 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PA_W() ):
            mark_type = MarkType.Illegal if i == 1 else \
                        MarkType.Legal if i < 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_P_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        # diverging short

        start_P_B = (8, 1)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_H_B = (8, 3)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        # P(B) --> W
        coords_PB_W = GS.gen_steps( start=start_P_B, rels=[(0, 1), ], include_prev=True, count=6 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PB_W() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal if i < 4 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "B", *start_P_B, mark_type=MarkType.Action, corner=Corner.UpperRight )

        # diverging long

        start_P_C = (12, 1)
        scene.board.set_piece( *start_P_C, piece=PieceType.Pawn )

        start_H_C = (12, 5)
        scene.board.set_piece( *start_H_C, piece=PieceType.Shaman )

        # P(C) --> W
        coords_PC_W = GS.gen_steps( start=start_P_C, rels=[(0, 1), ], include_prev=True, count=14 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PC_W() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal if i < 8 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "C", *start_P_C, mark_type=MarkType.Action, corner=Corner.UpperRight )

        # just rushing

        start_P_D = (16, 1)
        scene.board.set_piece( *start_P_D, piece=PieceType.Pawn )

        # P(D) -->
        coords_PD_ = GS.gen_steps( start=start_P_D, rels=[(0, 1), ], include_prev=True, count=6 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_PD_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "D", *start_P_D, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    #
    # Diverging Unicorn

    def scn_cot_037_diverging_unicorn_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_037_diverging_unicorn_init', bt)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_U = (8, 5)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_H = (7, 7)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W = (5, 4)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_N = (4, 9)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_A = (11, 6)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_p = (9, 10)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_b = (10, 9)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        # U --> H
        scene.append_arrow( *( start_U + start_H ), mark_type=MarkType.Action )

        return scene

    def scn_cot_038_diverging_unicorn_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_038_diverging_unicorn_end', bt)
        rect = (0.05, 0.8, 0.65, 0.1)

        prev_U = (8, 5)
        # scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_H = (7, 7)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W = (5, 4)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_N = (4, 9)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_A = (11, 6)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_p = (9, 10)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_b = (10, 9)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        # <-- U @ W(A) -->
        for diverge, rel in enumerate( GS.DEFAULT_UNICORN_REL_LONG_MOVES ):
            coords_P_W = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, count=1 ) # bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_P_W() ):
                mark_type = MarkType.Action if diverge in [ 1, 2, 10, ] else \
                            MarkType.Blocked if diverge in [ 6, 15, ] else \
                            MarkType.Legal
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "U", *prev_U, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )

        return scene

    #
    # Diverging activated Unicorn

    def scn_cot_039_activated_unicorn_divergence_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_039_activated_unicorn_divergence_init', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_E = (6, 13)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (2, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_U = (8, 2)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_H = (10, 5)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_A = (12, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # E --> W
        coords_E_W = GS.gen_steps( start=start_E, rels=[(-1, -2), ], include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_E_W() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> U
        coords_W_U = GS.gen_steps( start=start_W, rels=[(2, -1), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_W_U() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # U --> H
        scene.append_arrow( *( start_U + start_H ), mark_type=MarkType.Action )

        return scene

    def scn_cot_040_activated_unicorn_divergence_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_040_activated_unicorn_divergence_end', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        # prev_E = (6, 13)
        start_E = (2, 5)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        # prev_W = (2, 5)
        start_W = (8, 2)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # prev_U = (8, 2)
        start_U = (10, 5)
        # scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_H = (10, 5)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_A = (12, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # --> U
        # scene.append_arrow( *( prev_U + start_U ), mark_type=MarkType.Blocked )

        # <-- U -->
        for diverge, rel in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
            coords_U_ = GS.gen_steps( start=start_U, rels=[ rel,  ], include_prev=True, count=1 ) # bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_U_() ):
                mark_type = MarkType.Action if diverge == 7 else \
                            MarkType.Legal
                scene.append_arrow( *arrow, mark_type=mark_type )

        # U -->| -->
        start_1 = (8, 4)
        end_1 = (6, 7)

        scene.append_arrow( *( start_1 + end_1 ), mark_type=MarkType.Illegal )

        scene.append_text( "1", *start_1, mark_type=MarkType.Legal, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "2", *end_1, mark_type=MarkType.Illegal, corner=Corner.UpperRight )

        return scene

    #
    # Centaur cannot diverge

    def scn_cot_041_centaur_cannot_diverge(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_041_centaur_cannot_diverge', bt)

        start_C = (6, 3)
        scene.board.set_piece( *start_C, piece=PieceType.Centaur )

        start_H = (13, 13)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        #
        # Centaur's ply
        coords_C_H = GS.gen_steps( start=start_C, rels=[ (3, 2), (-1, 2), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=6 )

        for i, arrow in enumerate( coords_C_H() ):
            mark_type = MarkType.Illegal if i == 4 else \
                        MarkType.Blocked if i > 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Serpent cannot diverge

    def scn_cot_042_serpent_cannot_diverge(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_042_serpent_cannot_diverge', bt, height=5.3 )

        start_S = (4, 1)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_H = (7, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        adr = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, -1), mark_type=MarkType.Illegal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Blocked )

        return scene

    #
    # King cannot diverge

    def scn_cot_043_king_cannot_diverge(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_043_king_cannot_diverge', bt, height=4, width=9 )

        start_K = (4, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_H = (5, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        scene.append_arrow( *( start_K + start_H ), mark_type=MarkType.Illegal )

        return scene

    #
    #  Diverging Shaman

    def scn_cot_044_diverging_stepping_shaman( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_044_diverging_stepping_shaman', bt, x=3, y=8, width=9, height=6 )

        start_H_A_divergent = (10, 12)
        scene.board.set_piece( *start_H_A_divergent, piece=PieceType.Shaman )

        start_H_B = (4, 9)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        # H(B) --> H(A) divergent
        coords_HB_HA = GS.gen_steps( start=start_H_B, rels=[ (2, 1), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_HB_HA() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A_divergent, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "B", *start_H_B, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )

        return scene

    def scn_cot_045_diverging_capturing_shaman( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_045_diverging_capturing_shaman', bt, x=9, y=11, width=9, height=12 )

        start_H_A_divergent = (10, 12)
        scene.board.set_piece( *start_H_A_divergent, piece=PieceType.Shaman )

        start_n = (12, 15)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (14, 18)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_H_C = (16, 21)
        scene.board.set_piece( *start_H_C, piece=PieceType.Shaman )

        # H(C) --> H(A) divergence
        coords_HC_H1 = GS.gen_steps( start=start_H_C, rels=[ (-2, -3), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_HC_H1() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A_divergent, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "C", *start_H_C, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )

        return scene

    #
    # ... into stepping divergence

    def scn_cot_046_diverged_shaman_steps(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_046_diverged_shaman_steps', bt)

        start_H_A_divergent = (9, 7)
        scene.board.set_piece( *start_H_A_divergent, piece=PieceType.Shaman )

        start_n = (11, 3) # (13, 5)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_A = (5, 9)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_W = (13, 5) # (11, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # fixed set
        scene.board.set_piece( 0, 0, piece=PieceType.Star )
        scene.board.set_piece( 23, 23, piece=PieceType.Star )
        scene.board.set_piece( 23, 0, piece=-PieceType.Star )
        scene.board.set_piece( 0, 23, piece=-PieceType.Star )

        # | <-- H(B|C) @ H(A) --> |
        for diverge, rel in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
            coords_H_ = GS.gen_steps( start=start_H_A_divergent, rels=[ rel, ], include_prev=True, bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_H_() ):
                if diverge == 3:
                    mark_type = MarkType.Legal if i < 1 else \
                                MarkType.Blocked
                elif diverge == 6: # 7
                    mark_type = MarkType.Legal if i < 1 else \
                                MarkType.Illegal if i == 1 else \
                                MarkType.Blocked
                elif diverge == 7: # 6
                    mark_type = MarkType.Action if i == 1 else \
                                MarkType.Legal if i < 3 else \
                                MarkType.Blocked
                else:
                    mark_type = MarkType.Legal if i < 3 else \
                                MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A_divergent, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    #
    # ... into capturing divergence

    def scn_cot_047_diverged_shaman_captures(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_047_diverged_shaman_captures', bt)

        start_H_A_divergent = (9, 7)
        scene.board.set_piece( *start_H_A_divergent, piece=PieceType.Shaman )

        # dark Pawns @ H(1) ---> (3, 2)
        coords_p_ = GS.gen_steps( start=start_H_A_divergent, rels=[ (-2, 3), ], include_prev=False, bounds=scene.board_view.get_position_limits() )
        for i, coord in enumerate( coords_p_() ):
            scene.board.set_piece( *coord, piece=-PieceType.Pawn )
            # scene.append_text( str( i+1 ), *coord, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )

        start_n = (12, 5)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_A_D = (15, 3)
        scene.board.set_piece( *start_A_D, piece=PieceType.Pyramid )

        start_A_E = (10, 3)
        scene.board.set_piece( *start_A_E, piece=PieceType.Pyramid )

        start_W = (3, 11)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # fixed set
        scene.board.set_piece( 0, 0, piece=PieceType.Star )
        scene.board.set_piece( 23, 23, piece=PieceType.Star )
        scene.board.set_piece( 23, 0, piece=-PieceType.Star )
        scene.board.set_piece( 0, 23, piece=-PieceType.Star )

        # | <-- H(B|C) @ H(A) --> |
        for diverge, rel in enumerate( GS.DEFAULT_UNICORN_REL_LONG_MOVES ):
            coords_H_ = GS.gen_steps( start=start_H_A_divergent, rels=[ rel, ], include_prev=True, bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_H_() ):
                if diverge == 5:
                    mark_type = MarkType.Legal if i < 3 else \
                                MarkType.Blocked
                # elif diverge == 6:
                #     mark_type = MarkType.Action if i == 0 else \
                #                 MarkType.Legal if i < 2 else \
                #                 MarkType.Blocked
                elif diverge == 12:
                    mark_type = MarkType.Action if i == 0 else \
                                MarkType.Blocked
                elif diverge == 14:
                    mark_type = MarkType.Action if i == 1 else \
                                MarkType.Legal if i < 2 else \
                                MarkType.Blocked
                else:
                    mark_type = MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A_divergent, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_A_D, corner=Corner.UpperRight, mark_type=MarkType.Action )
        scene.append_text( "E", *start_A_E, corner=Corner.UpperRight, mark_type=MarkType.Action )

        return scene

    #
    # ... if activated

    def scn_cot_048_diverging_activated_shaman( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_048_diverging_activated_shaman', bt, width=9, height=11 ) # , x=9, y=11

        start_R = (7, 7)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W = (7, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H_B = (1, 1)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        start_H_A_divergent = (5, 3)
        scene.board.set_piece( *start_H_A_divergent, piece=PieceType.Shaman )

        start_n = (3, 6)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_b = (1, 9)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        # R --> W
        coords_R_W = GS.gen_steps( start=start_R, rels=[ (0, -1), ], include_prev=True, count=6 ) # bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_R_W() ):
            mark_type = MarkType.Action if i == 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> H(B)
        coords_W_HB = GS.gen_steps( start=start_W, rels=[ (-1, 0), ], include_prev=True, count=6 ) # bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_HB() ):
            mark_type = MarkType.Action if i == 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # H(B) --> H(A) divergent
        coords_HB_HA = GS.gen_steps( start=start_H_B, rels=[ (2, 1), ], include_prev=True, count=2 ) # bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_HB_HA() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # H(B) @ H(A) divergent --> ->|
        coords_HB_HA = GS.gen_steps( start=start_H_A_divergent, rels=[ (-2, 3), ], include_prev=True, count=2 ) # bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_HB_HA() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "A", *start_H_A_divergent, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "B", *start_H_B, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )

        return scene

    #
    # ... from opponent's Shaman

    def scn_cot_048_diverging_shaman_from_opponents( self, bt=BoardType.ConquestOfTlalocan ):

        scene = Scene( 'scn_cot_048_diverging_shaman_from_opponents', bt )

        start_H = (2, 0)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_h = (7, 10)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        # dark Pawns @ h ---> (3, 2)
        coords_p_ = GS.gen_steps( start=start_h, rels=[ (3, 2), ], include_prev=False, bounds=scene.board_view.get_position_limits() )

        index = 1
        for i, coord in enumerate( coords_p_() ):

            if i == 2:
                continue # empty field

            scene.board.set_piece( *coord, piece=-PieceType.Pawn )
            scene.append_text( str( index ), *coord, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
            index += 1

        start_W = (11, 8)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A = (15, 6)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # H --> h
        coords_H_h = GS.gen_steps( start=start_H, rels=[(1, 2), ], include_prev=True, count=5 )

        for i, arrow in enumerate( coords_H_h() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # H @ h ---> p p p p
        coords_H_h_ = GS.gen_steps( start=start_h, rels=[(3, 2), ], include_prev=True, count=5 )

        for i, arrow in enumerate( coords_H_h_() ):
            mark_type = MarkType.Action if i < 2 else \
                        MarkType.Illegal if i == 2 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        # H @ h ---> _ W _ A
        coords_H_h_2_ = GS.gen_steps( start=start_h, rels=[(2, -1), ], include_prev=True, count=5 )

        for i, arrow in enumerate( coords_H_h_2_() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Illegal if i == 3 else \
                        MarkType.Legal if i < 3 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Diverging Wave

    def scn_cot_049_wave_divergence_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_049_wave_divergence_init', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (3, 11)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W = (6, 8)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H = (11, 13)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_E = (14, 10)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_P = (11, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (7, 13)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q, rels=[(1, -1), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> w
        coords_W_w = GS.gen_steps( start=start_W, rels=[(1, 1), ], include_prev=True, count=5 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_W_w() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, mark_type=MarkType.Legal, corner=Corner.UpperRight )

        return scene

    def scn_cot_050_wave_divergence_1(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_050_wave_divergence_1', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (3, 11)
        end_Q = (6, 8)
        scene.board.set_piece( *end_Q, piece=PieceType.Queen )

        start_W = (14, 10)
        # scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H = (11, 13)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_E = (14, 10)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_P = (11, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (7, 13)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # <-- W --> @ H

        for rel in GS.DEFAULT_KING_REL_MOVES:
            coords_H__W = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, bounds=scene.board_view.get_position_limits() )

            for i, arrow in enumerate( coords_H__W() ):
                if rel == (-1, -1):
                    mark_type = MarkType.Action if i == 4 else \
                                MarkType.Legal
                elif rel == (1, -1):
                    mark_type = MarkType.Action if i == 2 else \
                                MarkType.Legal
                elif rel == (0, -1):
                    mark_type = MarkType.Action if i == 7 else \
                                MarkType.Legal
                elif rel == (-1, 0):
                    mark_type = MarkType.Illegal if i == 3 else \
                                MarkType.Legal
                else:
                    mark_type = MarkType.Legal
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, mark_type=MarkType.Illegal, corner=Corner.UpperRight )

        return scene

    #
    # Wave cannot diverge, if activated by Unicorn

    def scn_cot_051_wave_cannot_diverge_if_activated_by_unicorn(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_051_wave_cannot_diverge_if_activated_by_unicorn', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_U = (2, 3)
        scene.board.set_piece( *start_U, piece=PieceType.Unicorn )

        start_W = (4, 2)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H = (9, 10)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        # U --> W
        scene.append_arrow( *( start_U + start_W ), mark_type=MarkType.Action )

        # W -->
        rel_1 = (3, 2)
        rel_2 = (-2, 1)
        coords_W_ = GS.gen_steps( start=start_W, rels=[ rel_1, rel_2, ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=5 )

        for i, arrow in enumerate( coords_W_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W @ w | -->
        for divergence, rel in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
            if rel == rel_2:
                continue

            coords_W_w = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, count=1 )

            for i, arrow in enumerate( coords_W_w() ):
                scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        return scene

    #
    # Wave cannot diverge, if activated by Centaur

    def scn_cot_052_wave_cannot_diverge_if_activated_by_centaur(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_052_wave_cannot_diverge_if_activated_by_centaur', bt)

        start_C = (4, 4)
        scene.board.set_piece( *start_C, piece=PieceType.Centaur )

        start_W = (6, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H = (13, 13)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        #
        # Wave activation
        scene.append_arrow( *( start_C + start_W ), mark_type=MarkType.Action )

        #
        # Wave, movement
        coords_WA_WB = GS.gen_steps( start=start_W, rels=[ (3, 2), (-1, 2), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # count=6 )

        for i, arrow in enumerate( coords_WA_WB() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        #
        # Illegal divergence
        for diverge, rel in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
            coords_WA__WB_ = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, count=1 )

            for i, arrow in enumerate( coords_WA__WB_() ):
                if diverge != 2:
                    scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        return scene

    #
    # Wave cannot diverge, if activated by Serpent

    def scn_cot_053_wave_cannot_diverge_if_activated_by_serpent(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_053_wave_cannot_diverge_if_activated_by_serpent', bt)

        start_S = (3, 5)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_W = (5, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H = (11, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        #
        # Wave activation
        adr = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adr(-1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Action )
        # scene.append_arrow( *adr(1, -1, do_advance=False), mark_type=MarkType.Illegal )

        #
        # Wave, movement
        coords_W_ = GS.gen_steps( start=start_W, rels=[ (1, -1), (1, 1), ], include_prev=True, count=12 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_W_() ):
            # mark_type = MarkType.Illegal if i == 5 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        #
        # Illegal divergence
        adr_H = GS.adder( start_H, include_prev=True )
        scene.append_arrow( *adr_H(-1, 1, do_advance=False), mark_type=MarkType.Illegal )
        scene.append_arrow( *adr_H(1, 1, do_advance=False), mark_type=MarkType.Illegal )

        return scene

    #
    # Multiple divergences

    def scn_cot_054_multiple_divergences(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_054_multiple_divergences', bt)

        start_Q = (3, 17)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_H_A = (16, 4)
        scene.board.set_piece( *start_H_A, piece=PieceType.Shaman )

        start_H_B = (16, 11)
        scene.board.set_piece( *start_H_B, piece=PieceType.Shaman )

        start_c = (20, 15)
        scene.board.set_piece( *start_c, piece=-PieceType.Centaur )

        start_s = (7, 20)
        scene.board.set_piece( *start_s, piece=-PieceType.Serpent )

        # Q --> H(A)
        coords_Q_HA = GS.gen_steps( start=start_Q, rels=[ (1, -1), ], include_prev=True, count=13 )
        for i, arrow in enumerate( coords_Q_HA() ):
            mark_type = MarkType.Action if i == 12 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q @ H(A) --> H(B)
        coords_Q_HA_HB = GS.gen_steps( start=start_H_A, rels=[ (0, 1), ], include_prev=True, count=7 )
        for i, arrow in enumerate( coords_Q_HA_HB() ):
            mark_type = MarkType.Action if i == 6 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q @ H(B) --> c
        coords_Q_HB_c = GS.gen_steps( start=start_H_B, rels=[ (1, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_Q_HB_c() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q @ H(B) --> s
        coords_Q_HB_s = GS.gen_steps( start=start_H_B, rels=[ (-1, 1), ], include_prev=True, count=9 )
        for i, arrow in enumerate( coords_Q_HB_s() ):
            mark_type = MarkType.Blocked if i > 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_H_A, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "B", *start_H_B, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker )

        return scene

    #
    # Diverging opponent's pieces

    def scn_cot_055_diverging_opponents_pieces(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_055_diverging_opponents_pieces', bt)

        start_Q = (3, 17)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W = (15, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_w = (18, 8)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_e = (14, 12)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_H = (11, 18)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_h = (18, 14)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q, rels=[ (1, -1), ], include_prev=True, count=12 )
        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 11 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> w
        coords_W_w = GS.gen_steps( start=start_W, rels=[ (1, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_W_w() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # w --> g
        coords_w_g = GS.gen_steps( start=start_w, rels=[ (-1, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_w_g() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # g --> H
        coords_g_H = GS.gen_steps( start=start_e, rels=[ (-1, 2), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_g_H() ):
            mark_type = MarkType.Illegal if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # g --> h
        coords_g_h = GS.gen_steps( start=start_e, rels=[ (2, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_g_h() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Diverging to start
    # ... illegal, if first in cascade

    def scn_cot_056_diverging_first_piece(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_056_diverging_first_piece', bt, width=8, height=8 )

        start_Q = (1, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_H = (3, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        # Q --> H
        coords_Q_H = GS.gen_steps( start=start_Q, rels=[ (1, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_Q_H() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_cot_057_diverged_first_piece_illegal(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_057_diverged_first_piece_illegal', bt, width=8, height=8 )

        prev_Q = (1, 1)
        prev_H = (3, 3)

        # start_Q = prev_H
        # scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_H = prev_H
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        # Q @ H --> Q
        for rel in GS.DEFAULT_KING_REL_MOVES:
            coords_Q_H__Q = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, count=2 )

            for i, arrow in enumerate( coords_Q_H__Q() ):
                if rel == (-1, -1):
                    mark_type = MarkType.Illegal if i == 1 else \
                                MarkType.Legal if i < 2 else \
                                MarkType.Blocked
                else:
                    mark_type = MarkType.Legal if i < 2 else \
                                MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *prev_Q, corner=Corner.UpperLeft, mark_type=MarkType.Illegal )

        return scene

    # ... legal, if activated

    def scn_cot_058_diverging_activated_piece(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_058_diverging_activated_piece', bt, width=8, height=8 )

        start_R = (6, 6)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W = (6, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_Q = (1, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_H = (3, 3)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        # R --> W
        coords_R_W = GS.gen_steps( start=start_R, rels=[ (0, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_R_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> Q
        coords_W_Q = GS.gen_steps( start=start_W, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_W_Q() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q --> H
        coords_Q_H = GS.gen_steps( start=start_Q, rels=[ (1, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_Q_H() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_cot_059_diverged_activated_piece_legal(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_059_diverged_activated_piece_legal', bt, width=8, height=8 )

        prev_R = (6, 6)
        prev_W = (6, 1)
        prev_Q = (1, 1)
        prev_H = (3, 3)

        start_R = prev_W
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W = prev_Q
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_Q = prev_H
        # scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_H = prev_H
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        # Q @ H --> Q
        for rel in GS.DEFAULT_KING_REL_MOVES:
            coords_Q_H__Q = GS.gen_steps( start=start_H, rels=[ rel, ], include_prev=True, count=3 )

            for i, arrow in enumerate( coords_Q_H__Q() ):
                if rel == (-1, -1):
                    mark_type = MarkType.Action if i == 1 else \
                                MarkType.Legal if i < 3 else \
                                MarkType.Blocked
                else:
                    mark_type = MarkType.Legal if i < 3 else \
                                MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "Q", *prev_Q, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        return scene


    #
    # Trance-journey

    def scn_cot_070_trance_fields(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_070_trance_fields', bt, width=5, height=5 )

        start_H = (2, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        rect_H = (0.35, 0.5, 0.65, 0.1)
        coords_H_ = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_H, include_prev=False, count=1 )
        for i, pos in enumerate( coords_H_() ):
            scene.append_text( str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked, rect=rect_H )

        return scene

    def scn_cot_071_entrancement_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_071_entrancement_init', bt, width=9, height=9 )

        start_h = (1, 3)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        start_H = (6, 4)
        end_H = (2, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        # H -->
        coords_H_ = GS.gen_steps( start=start_H, rels=[ (-2, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_H_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_arrow( *( end_H + start_h ), mark_type=MarkType.Illegal )

        scene.append_text( "T", *end_H, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Illegal )

        return scene

    def scn_cot_072_entrancement_step(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_072_entrancement_step', bt, width=9, height=9 )

        start_h = (1, 3)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        start_H = (2, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        scene.append_arrow( *( start_H + start_h ), mark_type=MarkType.Action )

        scene.append_text( "T", *start_H, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        return scene

    def scn_cot_073_entrancement_activated(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_073_entrancement_activated', bt, width=9, height=9 )

        start_h = (1, 3)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        start_N = (2, 7)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W = (4, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_H = (2, 2)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        scene.append_arrow( *( start_N + start_W ), mark_type=MarkType.Legal )

        # W --> H
        coords_W_H = GS.gen_steps( start=start_W, rels=[ (-1, -2), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_W_H() ):
            # mark_type = MarkType.Action if i == 1 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_arrow( *( start_H + start_h ), mark_type=MarkType.Action )

        scene.append_text( "T", *start_H, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        return scene

    #
    # Entrancement cascade

    def scn_cot_074_entrancement_repositioning(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_074_entrancement_repositioning', bt, width=9, height=12)

        start_B = (2, 10)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_1 = (7, 5)
        scene.board.set_piece( *start_W_1, piece=PieceType.Wave )

        start_H = (6, 4)
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W_2 = (2, 2)
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_R = (4, 6)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W_3 = (2, 6)
        scene.board.set_piece( *start_W_3, piece=PieceType.Wave )

        start_h = (1, 3)
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        # B --> W(1)
        coords_B_W1 = GS.gen_steps( start=start_B, rels=[ (1, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_B_W1() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(1) --> H
        scene.append_arrow( *( start_W_1 + start_H ), mark_type=MarkType.Action )

        # H --> W(2)
        coords_H_W2 = GS.gen_steps( start=start_H, rels=[ (-2, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_H_W2() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(2) --> R
        coords_W2_R = GS.gen_steps( start=start_W_2, rels=[ (1, 2), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_W2_R() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R --> W(3)
        coords_R_W3 = GS.gen_steps( start=start_R, rels=[ (-1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_R_W3() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "1", *start_W_1, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "2", *start_W_2, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "3", *start_W_3, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        return scene

    def scn_cot_075_entrancement_cascade(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene( 'scn_cot_075_entrancement_cascade', bt, width=9, height=12)

        prev_B = (2, 10)
        prev_W_1 = (7, 5)
        prev_H = (6, 4)
        prev_W_2 = (2, 2)
        prev_R = (4, 6)
        prev_W_3 = (2, 6)
        prev_h = (1, 3)

        start_B = prev_W_1
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_1 = prev_H
        scene.board.set_piece( *start_W_1, piece=PieceType.Wave )

        start_H = prev_W_2
        scene.board.set_piece( *start_H, piece=PieceType.Shaman )

        start_W_2 = prev_R
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_R = prev_W_3
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W_3 = prev_W_3
        # scene.board.set_piece( *start_W_3, piece=PieceType.Wave )

        start_h = prev_h
        scene.board.set_piece( *start_h, piece=-PieceType.Shaman )

        # B --> W(1)
        coords_B_W1 = GS.gen_steps( end=start_B, rels=[ (1, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_B_W1() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(1) --> H
        scene.append_arrow( *( prev_W_1 + prev_H ), mark_type=MarkType.Blocked )

        # H --> W(2)
        coords_H_W2 = GS.gen_steps( end=start_H, rels=[ (-2, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_H_W2() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(2) --> R
        coords_W2_R = GS.gen_steps( end=start_W_2, rels=[ (1, 2), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_W2_R() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # R --> W(3)
        coords_R_W3 = GS.gen_steps( end=start_R, rels=[ (-1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_R_W3() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        scene.append_text( "1", *start_W_1, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "2", *start_W_2, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        # scene.append_text( "3", *start_W_3, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )

        # W(3) --> H
        coords_W3_H = GS.gen_steps( start=start_W_3, rels=[ (0, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_W3_H() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # H --> h
        scene.append_arrow( *( start_H + start_h ), mark_type=MarkType.Action )

        return scene

    #
    # Movement

    def scn_cot_076_knight_directions(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_076_knight_directions', bt, width=9, height=12)

        scene.append_text("S", 6, 6, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        # left
        scene.board.set_piece(5, 9, piece=PieceType.Knight)

        scene.append_arrow( 5, 9, 4+0.5, 9+0.5, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False ) # left
        scene.append_arrow( *GS.add_to_all( (4, 9, 3, 8), 0.5 ), mark_type=MarkType.Blocked, start_pointer=False, end_pointer=True ) # left down
        scene.append_arrow( *GS.add_to_all( (4, 9, 3, 10), 0.5 ), mark_type=MarkType.Blocked, start_pointer=False, end_pointer=True ) # left up

        scene.append_text("L", 3, 8, corner=Corner.LowerLeft, mark_type=MarkType.Blocked)
        scene.append_text("R", 3, 10, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # down
        scene.board.set_piece(2, 7, piece=-PieceType.Knight)

        scene.append_arrow( 2, 7, 2+0.5, 6+0.5, mark_type=MarkType.Illegal, start_pointer=False, end_pointer=False ) # down
        scene.append_arrow( *GS.add_to_all( (2, 6, 1, 5), 0.5 ), mark_type=MarkType.Illegal, start_pointer=False, end_pointer=True ) # down left
        scene.append_arrow( *GS.add_to_all( (2, 6, 3, 5), 0.5 ), mark_type=MarkType.Illegal, start_pointer=False, end_pointer=True ) # down right

        scene.append_text("L", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Illegal)
        scene.append_text("R", 1, 5, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        # right
        scene.board.set_piece(4, 4, piece=PieceType.Knight)

        scene.append_arrow( 4, 4, 5+0.5, 4+0.5, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False ) # right
        scene.append_arrow( *GS.add_to_all( (5, 4, 6, 3), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # right down
        scene.append_arrow( *GS.add_to_all( (5, 4, 6, 5), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # right up

        scene.append_text("L", 6, 5, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("R", 6, 3, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        # up
        scene.board.set_piece(7, 6, piece=-PieceType.Knight)

        scene.append_arrow( 7, 6, 7+0.5, 7+0.5, mark_type=MarkType.Action, start_pointer=False, end_pointer=False ) # up
        scene.append_arrow( *GS.add_to_all( (7, 7, 6, 8), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up left
        scene.append_arrow( *GS.add_to_all( (7, 7, 8, 8), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up right

        scene.append_text("L", 6, 8, corner=Corner.UpperLeft, mark_type=MarkType.Action)
        scene.append_text("R", 8, 8, corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_cot_077_stop_sign_pattern(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_077_stop_sign_pattern', bt, width=9, height=12)

        start = (6, 6)
        scene.append_text("S", *start, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        scene.append_arrow( *GS.add_to_all( (6, 6, 7, 6), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
        scene.append_arrow( *GS.add_to_all( (7, 6, 8, 7), 0.5 ), mark_type=MarkType.Legal ) # right-up

        scene.append_text("1", 8, 7, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        scene.append_arrow( *GS.add_to_all( (8, 7, 8, 8), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
        scene.append_arrow( *GS.add_to_all( (8, 8, 7, 9), 0.5 ), mark_type=MarkType.Action ) # left-up

        scene.append_text("2", 7, 9, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *GS.add_to_all( (7, 9, 6, 9), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
        scene.append_arrow( *GS.add_to_all( (6, 9, 5, 8), 0.5 ), mark_type=MarkType.Blocked ) # left-down

        scene.append_text("3", 5, 8, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        scene.append_arrow( *GS.add_to_all( (5, 8, 5, 7), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
        scene.append_arrow( *GS.add_to_all( (5, 7, 6, 6), 0.5 ), mark_type=MarkType.Illegal ) # right-down

        return scene

    def append_broken_arrow(self, scene, start, rel, outward_arrows=True, bounds=None, count=None, is_with_field_marker=False, rect=None):

        rels = GS.gen_shaman_rel_legs(rel) # , count=count)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True, bounds=bounds) )
        corners = GS.gen_next( GS.gen_shaman_corners(rel, is_with_field_marker=is_with_field_marker) )

        if outward_arrows:
            sp1 = False
            ep1 = False
            sp2 = False
            ep2 = True
        else:
            sp1 = True
            ep1 = False
            sp2 = False
            ep2 = False

        def _append_broken_arrow(text=None, mark_type=MarkType.Legal, full_length=False):
            x0, y0, x1, y1 = leg_1 = coords()
            x2, y2, x3, y3 = leg_2 = coords()

            assert x1 == x2
            assert y1 == y2

            if full_length:
                scene.append_arrow( *GS.add_to_all( leg_1, 0.5 ), mark_type=mark_type, start_pointer=sp1, end_pointer=ep1 ) # start
                scene.append_arrow( *GS.add_to_all( leg_2, 0.5 ), mark_type=mark_type, start_pointer=sp2, end_pointer=ep2 ) # end, diagonal
            else:
                scene.append_arrow( x0, y0, x1+0.5, y1+0.5, mark_type=mark_type, start_pointer=sp1, end_pointer=ep1 ) # start
                scene.append_arrow( x2+0.5, y2+0.5, x3, y3, mark_type=mark_type, start_pointer=sp2, end_pointer=ep2 ) # end, diagonal

            if text is not None:
                scene.append_text(text, x3, y3, corner=corners(), mark_type=mark_type, rect=rect)

        return _append_broken_arrow

    def scn_cot_080_stop_sign_pattern_unwind(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_080_stop_sign_pattern_unwind', bt, width=9, height=12)

        start = (6, 6)
        rel = (2, 1)

        scene.append_text("S", *start, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        aba = self.append_broken_arrow(scene, start, rel, bounds=scene.board_view.get_position_limits(), count=8)

        aba("1", mark_type=MarkType.Legal)
        aba("2", mark_type=MarkType.Action)
        aba("3", mark_type=MarkType.Blocked)
        aba("4", mark_type=MarkType.Illegal)

        return scene

    def scn_cot_081_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_081_stop_sign_pattern_full', bt)

        start = (5, 11)
        # scene.board.set_piece(*start, piece=PieceType.Shaman)

        scene.append_field_marker(*start, mark_type=MarkType.Illegal) # , mark_type=MarkType.Blocked
        scene.append_text("S", *start, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(4):
            aba(str(4 * i + 1), mark_type=MarkType.Legal)
            aba(str(4 * i + 2), mark_type=MarkType.Action)
            aba(str(4 * i + 3), mark_type=MarkType.Blocked)
            aba(str(4 * i + 4), mark_type=MarkType.Illegal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(4):
            aba(str(4 * i + 1), mark_type=MarkType.Legal)
            aba(str(4 * i + 2), mark_type=MarkType.Action)
            aba(str(4 * i + 3), mark_type=MarkType.Blocked)
            aba(str(4 * i + 4), mark_type=MarkType.Illegal)

        return scene

    def scn_cot_082_light_shaman_trance_journey(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_082_light_shaman_trance_journey', bt)

        start = (5, 11)
        scene.board.set_piece(*start, piece=PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_cot_083_light_shaman_trance_journey_offset(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_083_light_shaman_trance_journey_offset', bt, x=-7, y=-1)

        start = (5, 11)
        scene.board.set_piece(*start, piece=PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Legal if i < 5 else MarkType.Blocked
            aba(str(i + 1), mark_type=mark_type)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # mark_type = MarkType.Legal if i < 5 else MarkType.Blocked
            # aba(str(i + 1), mark_type=mark_type)

        return scene

    def scn_cot_084_dark_shaman_trance_journey(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_084_dark_shaman_trance_journey', bt)

        start = (5, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=True)

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=True)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Action)

        return scene

    def scn_cot_085_displacement_fields(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_085_displacement_fields', bt)

        start = (12, 11)
        scene.board.set_piece(*start, piece=PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*pos, mark_type=MarkType.Blocked)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_cot_086_light_light_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_086_light_light_shaman_interaction_start', bt)

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
        scene.board.set_piece(4, 17, piece=PieceType.Knight)
        scene.board.set_piece(12, 11, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = (3, 11)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        # scene.append_arrow( *((-1, 1) + startT), mark_type=MarkType.Illegal )
        # scene.append_arrow( *((-1, 9) + startK), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-7.5, 8.5) + startT), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-1.5, 10.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("4", *startK, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)
        scene.replace_text("9", *startT, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_087_light_light_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_087_light_light_shaman_interaction_end', bt)

        start = (4, 12)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(8, 13, piece=PieceType.Knight)
        scene.board.set_piece(9, 16, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = start
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        startH1 = (6, 23)
        scene.board.set_piece(*startH1, piece=PieceType.Shaman)
        scene.append_text("T", *startH1, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        # right arm Knight's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(4, 17), include_prev=False, count=1)

        # i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            # scene.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Action)
            # i += 1

        # right arm Pawn's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(12, 11), include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            # scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text(str(i), *pos, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
            i += 1

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_cot_088_dark_light_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_088_dark_light_shaman_interaction_start', bt)

        start = (4, 12)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(4, 17, piece=PieceType.Knight)
        scene.board.set_piece(12, 11, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = (5, 11)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        # scene.append_arrow( *((-1, 1) + startT), mark_type=MarkType.Illegal )
        # scene.append_arrow( *((-1, 9) + startK), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-7.5, 8.5) + startT), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-1.5, 10.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("4", *startK, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)
        scene.replace_text("9", *startT, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_089_dark_light_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_089_dark_light_shaman_interaction_end', bt)

        start = (4, 12)

        startH0 = (6, 23)
        scene.board.set_piece(*startH0, piece=PieceType.Shaman)
        scene.append_text("T", *startH0, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        # scene.board.set_piece(4, 17, piece=PieceType.Knight)
        # scene.board.set_piece(12, 11, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = start
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(, is_with_field_marker=False scene, start, rel, count=24, is_with_field_marker=False )

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_cot_090_dark_dark_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_090_dark_dark_shaman_interaction_start', bt)
        rect = ( 0.02, 0.79, 0.65, 0.05 ) # left, top, right, bottom

        start = (4, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(*startP, piece=-PieceType.Pawn)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startH = (3, 12)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        # for i in range(16):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Legal)

        scene.replace_arrow( *(startP + (8.5, 23.5)), mark_type=MarkType.Illegal, end_pointer=False )
        scene.replace_arrow( *((8.5, 23.5) + startT), mark_type=MarkType.Illegal )

        scene.replace_arrow( *((-0.5, 3.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("6", *startT, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)
        scene.replace_text("8", *startK, corner=Corner.LowerRight, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_091_dark_dark_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_091_dark_dark_shaman_interaction_end', bt)
        rect = ( 0.02, 0.79, 0.65, 0.05 ) # left, top, right, bottom

        start = (4, 11)

        startH0 = (2, 17)
        scene.board.set_piece(*startH0, piece=-PieceType.Shaman)
        scene.append_text("T", *startH0, corner=Corner.LowerRight, mark_type=MarkType.Action)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(5, 3, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startH = start
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        # for i in range(16):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        return scene

    def scn_cot_092_dark_dark_shaman_double_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_092_dark_dark_shaman_double_interaction_start', bt)
        rect = ( 0.02, 0.79, 0.65, 0.05 ) # left, top, right, bottom

        start = (4, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(*startP, piece=-PieceType.Pawn)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startH = (3, 12)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Legal)

        scene.replace_arrow( *(startP + (8.5, 23.5)), mark_type=MarkType.Illegal, end_pointer=False )
        scene.replace_arrow( *((8.5, 23.5) + startT), mark_type=MarkType.Illegal )

        scene.replace_arrow( *((-0.5, 3.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("6", *startT, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)
        scene.replace_text("8", *startK, corner=Corner.LowerRight, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_093_dark_dark_shaman_double_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_093_dark_dark_shaman_double_interaction_end', bt)
        rect = ( 0.02, 0.79, 0.65, 0.05 ) # left, top, right, bottom

        start = (4, 11)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startH = start
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Blocked)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        return scene

    def scn_cot_094_light_dark_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_094_light_dark_shaman_interaction_start', bt)
        rect = ( 0.02, 0.79, 0.65, 0.05 ) # left, top, right, bottom

        start = (4, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(*startP, piece=-PieceType.Pawn)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startH = (3, 12)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        # for i in range(10):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=False, rect=rect )

        for i in range(12):
            mark_type = MarkType.Illegal if i in [ 5, 7 ] else \
                        MarkType.Legal

            # \TODO :: fix text color
            aba(str(12 - i), mark_type=mark_type)

        return scene

    def scn_cot_095_light_dark_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_095_light_dark_shaman_interaction_end', bt)

        start = (4, 11)

        startH0 = (2, 17)
        scene.board.set_piece(*startH0, piece=-PieceType.Shaman)
        scene.append_text("T", *startH0, corner=Corner.LowerRight, mark_type=MarkType.Action)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(5, 3, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(13, 14, piece=-PieceType.Pawn)
        scene.board.set_piece(12, 11, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startH = start
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action)

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=True )

        # for i in range(10):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow( scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=True )

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        # down arm Knight's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(10, 13), include_prev=False, count=1)

        # i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            # scene.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Action)
            # i += 1

        # down arm Pawn's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(16, 15), include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            # scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text(str(i), *pos, mark_type=MarkType.Legal, corner=Corner.LowerLeftFieldMarker)
            i += 1

        return scene

    def scn_cot_096_backward_displacement_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_096_backward_displacement_start', bt)

        start = (15, 12)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperLeft)

        startH = (14, 11)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperLeft)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=True )

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # backward displacement

        start_B = (15, 17)
        scene.board.set_piece(*start_B, piece=-PieceType.Bishop)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=start_B, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)
            i += 1

        return scene

    def scn_cot_097_backward_displacement_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_097_backward_displacement_end', bt)

        startH2 = (15, 17)
        scene.board.set_piece(*startH2, piece=PieceType.Shaman)
        scene.append_text("T", *startH2, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        start = (15, 12)

        startH = start
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=False )

        for i in range(16):
            mark_type = MarkType.Blocked if i < 2 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        #
        # backward displacement

        start_B = (17, 13)
        scene.board.set_piece(*start_B, piece=-PieceType.Bishop)

        return scene

    def scn_cot_098_forward_displacement_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_098_forward_displacement_start', bt)

        start = (20, 7)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        startH = (19, 8)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( *(startH + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, start, rel, count=24, is_with_field_marker=True )

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # forward displacement

        startR = (14, 9)
        scene.board.set_piece(*startR, piece=-PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startR, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i), *pos, mark_type=MarkType.Action, corner=Corner.LowerRightFieldMarker)
            i += 1

        return scene

    def scn_cot_099_forward_displacement_step_2(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_099_forward_displacement_step_2', bt)

        start = (14, 9)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        startH = (20, 7)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow( scene, startH, rel, count=24, is_with_field_marker=True )

        for i in range(16):
            mark_type = MarkType.Blocked if i < 3 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        #
        # forward displacement

        startR = (8, 11)
        scene.board.set_piece(*startR, piece=-PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startR, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i), *pos, mark_type=MarkType.Action, corner=Corner.LowerRightFieldMarker)
            i += 1

        return scene

    def scn_cot_100_forward_displacement_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_100_forward_displacement_end', bt, x=-5, y=1)

        start = (8, 11)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        startH = (20, 7)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, startH, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            mark_type = MarkType.Blocked if i < 7 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        #
        # forward displacement

        startR = (2, 13)
        scene.board.set_piece(*startR, piece=-PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startR, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            x, y = pos
            mark_type = MarkType.Action if x >= 0 else MarkType.Illegal
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i), *pos, mark_type=mark_type, corner=Corner.LowerRightFieldMarker)
            i += 1

        return scene

    def place_troopers_around_pieces(self, scene, piece_type):

        pt = PieceType(piece_type)
        b = scene.board
        row, dy, ddy, p, o, g = (0, 3, 1, PieceType.Pawn, PieceType.Scout, PieceType.Grenadier) if pt.is_light() else \
                                (b.get_height() - 1, -3, -1, -PieceType.Pawn, -PieceType.Scout, -PieceType.Grenadier)

        for i in range( b.get_width() ):
            if b.get_piece(i, row) == pt:
                adder = GS.adder((i, row), include_prev=False)
                b.set_piece_safe(*adder(-1, ddy), piece=g)
                b.set_piece_safe(*adder(-1, ddy), piece=g)

                b.set_piece_safe(*adder(0, ddy), piece=o)
                b.set_piece_safe(*adder(1, ddy), piece=o)
                b.set_piece_safe(*adder(2, 0), piece=o)
                b.set_piece_safe(*adder(1, -ddy), piece=o)

                b.set_piece_safe(*adder(0, -ddy), piece=g)
                b.set_piece_safe(*adder(-1, -ddy), piece=g)

        return scene

    def scn_cot_110_troopers_initial_positions(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_110_troopers_initial_positions', bt)

        scene.board.set_piece(5, 0, piece=PieceType.Centaur)
        scene.board.set_piece(18, 0, piece=PieceType.Centaur)

        scene.board.set_piece(5, 23, piece=-PieceType.Centaur)
        scene.board.set_piece(18, 23, piece=-PieceType.Centaur)

        self.place_troopers_around_pieces(scene, PieceType.Centaur)
        self.place_troopers_around_pieces(scene, -PieceType.Centaur)


        scene.board.set_piece(10, 0, piece=PieceType.Shaman)
        scene.board.set_piece(13, 0, piece=PieceType.Shaman)

        scene.board.set_piece(10, 23, piece=-PieceType.Shaman)
        scene.board.set_piece(13, 23, piece=-PieceType.Shaman)

        self.place_troopers_around_pieces(scene, PieceType.Shaman)
        self.place_troopers_around_pieces(scene, -PieceType.Shaman)

        return scene


    #
    # Trance-journey cannot be blocked.

    # def scn_cot_091_trance_journey_failed(self, bt=BoardType.ConquestOfTlalocan):

    #     scene = Scene('scn_cot_091_trance_journey_failed', bt)

    #     start_b = (23, 0)
    #     adder = GS.adder(start_b)
    #     scene.board.set_piece(*start_b, piece=-PieceType.Bishop)

    #     start_I = adder(-1, 1) # (-1, 1)
    #     scene.board.set_piece(*start_I, piece=PieceType.Starchild)

    #     start_i = adder(1, 1) # (0, 2)
    #     scene.board.set_piece(*start_i, piece=-PieceType.Starchild)

    #     scene.append_arrow( *(start_i + start_I), mark_type=MarkType.Action )
    #     scene.append_arrow( *(start_I + start_b), mark_type=MarkType.Action )

    #     # scene.board.set_piece(25, 5, piece=PieceType.Star)
    #     # scene.board.set_piece(1, 8, piece=PieceType.Star)
    #     # scene.board.set_piece(5, 5, piece=-PieceType.Star)
    #     # scene.board.set_piece(7, 6, piece=-PieceType.Star)

    #     # scene.board.set_piece(11, 3, piece=PieceType.King)
    #     # scene.board.set_piece(13, 4, piece=-PieceType.King)

    #     # scene.board.set_piece(17, 1, piece=PieceType.Monolith)
    #     # scene.board.set_piece(19, 2, piece=-PieceType.Monolith)

    #     #
    #     # right arm

    #     rel = (2, 1)
    #     aba = self.append_broken_arrow(scene, start_b, rel, count=32, is_with_field_marker=True)

    #     for i in range(32):
    #         aba(str(i + 1), mark_type=MarkType.Blocked) # Legal)

    #     #
    #     # left arm

    #     rel = (-2, -1)
    #     aba = self.append_broken_arrow(scene, start_b, rel, count=32, is_with_field_marker=True)

    #     for i in range(32):
    #         aba(str(i + 1), mark_type=MarkType.Blocked) # Action)

    #     return scene


    #
    # test methods

    def test_cot_121_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('test_cot_121_stop_sign_pattern_full', bt, x=-50, y=-50, width=128, height=128)

        start = (11, 11) # (11, 11)


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
    ss.render_example( scene,
                       scene.test_cot_121_stop_sign_pattern_full,
                       board_types=[ BoardType.ConquestOfTlalocan, ],
                       path_prefix='temp/')
                       # , enforce_cot_in_bw=True)


if __name__ == '__main__':
    test_big_pattern()
