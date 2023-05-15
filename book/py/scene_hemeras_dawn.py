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

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Legal if i < 4 else MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        return scene

    def scn_hd_02_centaur_opposite_color(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_02_centaur_opposite_color', bt, width=11, height=11)

        start = (5, 5)
        scene.board.set_piece(*start, piece=PieceType.Centaur)

        # Centaur, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_LONG_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Legal if i < 8 else MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

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

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Blocked if i == 1 else MarkType.Legal if i < 4 else MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

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

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Blocked if i == 1 else MarkType.Legal if i < 8 else MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

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
        for i, pos in enumerate( arr() ):
            mark_type = MarkType.Blocked if i > 6 else MarkType.Action if i % 2 == 0 else MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        txt = GS.gen_steps(start=start, rels=rels, include_prev=False, bounds=scene.board_view.get_position_limits())
        for i, pos in enumerate( txt() ):
            mark_type = MarkType.Blocked if i > 6 else MarkType.Action if i % 2 == 0 else MarkType.Legal
            scene.append_text( str(i+1), *pos, mark_type=mark_type )

        #
        # forbidden directions change

        # (-1, 2) is ok, i.e. direction "7", here: 12, 11 --> 11, 13
        # multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_CENTAUR_SHORT_REL_MOVES, to_remove=((-1, 2), ) ) )
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-1, 2), ) ) )
        start_X = (12, 11)

        arr = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=True, count=1)
        for i, pos in enumerate( arr() ):
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        txt = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=False, count=1)
        for i, pos in enumerate( txt() ):
            corner = Corner.LowerRight if i > 4 else \
                     Corner.LowerLeft if i > 2 else \
                     Corner.UpperLeft if i > 1 else \
                     Corner.UpperRight
            scene.append_text( str(i+1), *pos, corner=corner, mark_type=MarkType.Illegal )

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

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Legal if i < 8 else MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

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

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

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
        for i, pos in enumerate( arr() ):
            mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=mark_type )

        txt = GS.gen_steps(start=start, rels=rels, include_prev=False, bounds=scene.board_view.get_position_limits())
        for i, pos in enumerate( txt() ):
            mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            corner = Corner.UpperRight if i % 2 == 0 else Corner.UpperLeft
            scene.append_text( str(i+1), *pos, corner=corner, mark_type=mark_type )

        #
        # forbidden directions change

        # (-2, 1) is ok, i.e. direction "7", here: 10, 13 --> 8, 14
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-2, 1), ) ) )
        start_X = (10, 13)

        arr = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=True, count=1)
        for i, pos in enumerate( arr() ):
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        txt = GS.gen_multi_steps(multi_rels, start=start_X, include_prev=False, count=1)
        for i, pos in enumerate( txt() ):
            corner = Corner.LowerRight if i > 4 else \
                     Corner.LowerLeft if i > 2 else \
                     Corner.UpperLeft if i > 1 else \
                     Corner.UpperRight
            scene.append_text( str(i+1), *pos, corner=corner, mark_type=MarkType.Illegal )

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
        for i, pos in enumerate( coords_O_() ):
            scene.append_text( str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal, rect=rect_O )

        return scene

    #
    # Scout/Movement

    def scn_hd_15_scout_movement(self, bt=BoardType.HemerasDawn):

        scene = Scene( 'scn_hd_15_scout_movement', bt )

        start_O_1 = (7, 7)
        scene.board.set_piece( *start_O_1, piece=PieceType.Scout )

        # start_p_1 = (6, 3)
        # scene.board.set_piece( *start_p_1, piece=-PieceType.Pawn )

        # start_p_2 = (8, 3)
        # scene.board.set_piece( *start_p_2, piece=-PieceType.Pawn )

        start_o_1 = (13, 13)
        scene.board.set_piece( *start_o_1, piece=-PieceType.Scout )

        # start_P_1 = (12, 17)
        # scene.board.set_piece( *start_P_1, piece=PieceType.Pawn )

        # start_P_2 = (14, 17)
        # scene.board.set_piece( *start_P_2, piece=PieceType.Pawn )

        #
        # <-- <- O -> -->

        arr = GS.gen_steps( start=start_O_1, rels=[ (0, 1), ], include_prev=True, count=3 )
        for i, pos in enumerate( arr() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_O_1, rels=[ (-1, 0), ], include_prev=True, count=2 )
        for i, pos in enumerate( arr() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_O_1, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, pos in enumerate( arr() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_O_1, -1, -1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_O_1, 1, -1 ), mark_type=MarkType.Illegal )

        #
        # <-- <- o -> -->

        arr = GS.gen_steps( start=start_o_1, rels=[ (0, -1), ], include_prev=True, count=3 )
        for i, pos in enumerate( arr() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_o_1, rels=[ (-1, 0), ], include_prev=True, count=2 )
        for i, pos in enumerate( arr() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        arr = GS.gen_steps( start=start_o_1, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, pos in enumerate( arr() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_o_1, -1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_o_1, 1, 1 ), mark_type=MarkType.Illegal )

        #
        # % \TODO :: decide on Scout movement
        #
        for a in [0, 19]:
            for b in range( 20 ):
                scene.append_text( "TODO", a, b, mark_type=MarkType.Action )
                scene.append_text( "TODO", b, a, mark_type=MarkType.Action )
        #
        # % \TODO :: decide on Scout movement
        #

        return scene

    #
    # Scout Pawns

    def scn_hd_16_scout_initial_positions(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_16_scout_initial_positions', bt)

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

    def scn_hd_17_grenadier_fields( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_17_grenadier_fields', bt, width=5, height=5 )

        start_G = (2, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        rect_G = (0.35, 0.5, 0.65, 0.1)
        coords_G_ = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_G, include_prev=False, count=1 )
        for i, pos in enumerate( coords_G_() ):
            scene.append_text( str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal, rect=rect_G )

        return scene

    #
    # Grenadier/Movement

    def scn_hd_18_grenadier_movement( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_18_grenadier_movement', bt, width=7, height=5 )

        start_G = (3, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=3 )
        for i, pos in enumerate( gen_Gr_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=2 )
        for i, pos in enumerate( gen_Gu_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=3 )
        for i, pos in enumerate( gen_Gl_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=2 )
        for i, pos in enumerate( gen_Gd_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ) , mark_type=MarkType.Illegal )

        return scene

    def scn_hd_19_grenadier_extended_steps( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_19_grenadier_extended_steps', bt, width=11, height=5 )

        start_G = (5, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_n = (6, 3)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, pos in enumerate( gen_Gr_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gu_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, pos in enumerate( gen_Gl_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gd_() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        return scene

    def scn_hd_20_grenadier_capture_fields( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_20_grenadier_capture_fields', bt, width=5, height=5 )

        start_G = (2, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ) , mark_type=MarkType.Illegal )

        return scene

    def scn_hd_21_grenadier_extended_capture_fields( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_21_grenadier_extended_capture_fields', bt, width=5, height=5 )

        start_G = (2, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_r = (3, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gr_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gu_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gl_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gd_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        return scene

    def scn_hd_22_grenadier_extended_captures( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_22_grenadier_extended_captures', bt, width=5, height=5 )

        start_G = (2, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_r = (3, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_b = (0, 1)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        gen_Gr_ = GS.gen_steps( start=start_G, rels=[ (1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gr_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gu_ = GS.gen_steps( start=start_G, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gu_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gl_ = GS.gen_steps( start=start_G, rels=[ (-1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gl_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Action )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_Gd_ = GS.gen_steps( start=start_G, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_Gd_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_G, 1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, -1, -1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G, 1, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_hd_23_grenadier_blocked_capture( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_23_grenadier_blocked_capture', bt, width=5, height=5 )

        start_G = (1, 2)
        scene.board.set_piece( *start_G, piece=PieceType.Grenadier )

        start_p = (2, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_u = (3, 3)
        scene.board.set_piece( *start_u, piece=-PieceType.Unicorn )

        coords = GS.append_pos_rel( start_G, 1, 0 )
        scene.append_arrow( *coords, mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.add_end_rel( coords, 1, 1 ) , mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.add_end_rel( coords, 1, -1 ) , mark_type=MarkType.Blocked )

        return scene

    #
    # \TODO :: DELETE

    def scn_hd_98_grenadier_captures( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_98_grenadier_captures', bt, height=5 )

        start_G_A = (2, 2)
        scene.board.set_piece( *start_G_A, piece=PieceType.Grenadier )

        start_G_B = (6, 2)
        scene.board.set_piece( *start_G_B, piece=PieceType.Grenadier )

        start_r = (7, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_G_C = (12, 2)
        scene.board.set_piece( *start_G_C, piece=PieceType.Grenadier )

        start_n = (13, 3)
        scene.board.set_piece( *start_n, piece=-PieceType.Rook )

        start_b = (10, 1)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_G_D = (16, 2)
        scene.board.set_piece( *start_G_D, piece=PieceType.Grenadier )

        start_p = (17, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_u = (18, 3)
        scene.board.set_piece( *start_u, piece=-PieceType.Unicorn )

        #
        # <-- <- G(A) -> -->

        scene.append_arrow( *GS.append_pos_rel( start_G_A, 1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_A, -1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_A, -1, -1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_A, 1, -1 ) , mark_type=MarkType.Illegal )

        #
        # <-- <- G(B) -> -->

        gen_GBr_ = GS.gen_steps( start=start_G_B, rels=[ (1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GBr_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GBu_ = GS.gen_steps( start=start_G_B, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GBu_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GBl_ = GS.gen_steps( start=start_G_B, rels=[ (-1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GBl_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GBd_ = GS.gen_steps( start=start_G_B, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GBd_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        #
        # <-- <- G(C) -> -->

        gen_GCr_ = GS.gen_steps( start=start_G_C, rels=[ (1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCr_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GCu_ = GS.gen_steps( start=start_G_C, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCu_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GCl_ = GS.gen_steps( start=start_G_C, rels=[ (-1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCl_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Action )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GCd_ = GS.gen_steps( start=start_G_C, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCd_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_G_C, 1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G_C, -1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_C, -1, -1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_C, 1, -1 ), mark_type=MarkType.Illegal )

        #
        # <-- <- G(D) -> -->

        coords = GS.append_pos_rel( start_G_D, 1, 0 )
        scene.append_arrow( *coords, mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.add_end_rel( coords, 1, 1 ) , mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.add_end_rel( coords, 1, -1 ) , mark_type=MarkType.Blocked )

        scene.append_text( "A", *start_G_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_G_B, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_G_C, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "D", *start_G_D, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )

        return scene

    def scn_hd_99_grenadier_movement( self, bt=BoardType.HemerasDawn ):

        scene = Scene( 'scn_hd_99_grenadier_movement', bt )

        start_G_A = (6, 4)
        scene.board.set_piece( *start_G_A, piece=PieceType.Grenadier )

        start_G_B = (14, 8)
        scene.board.set_piece( *start_G_B, piece=PieceType.Grenadier )

        start_n = (15, 9)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_G_C = (4, 14)
        scene.board.set_piece( *start_G_C, piece=PieceType.Grenadier )

        start_r = (5, 15)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_G_D = (14, 14)
        scene.board.set_piece( *start_G_D, piece=PieceType.Grenadier )

        start_p = (15, 14)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        #
        # <-- <- G(A) -> -->

        gen_GAr_ = GS.gen_steps( start=start_G_A, rels=[ (1, 0), ], include_prev=True, count=3 )
        for i, pos in enumerate( gen_GAr_() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GAu_ = GS.gen_steps( start=start_G_A, rels=[ (0, 1), ], include_prev=True, count=2 )
        for i, pos in enumerate( gen_GAu_() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GAl_ = GS.gen_steps( start=start_G_A, rels=[ (-1, 0), ], include_prev=True, count=3 )
        for i, pos in enumerate( gen_GAl_() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GAd_ = GS.gen_steps( start=start_G_A, rels=[ (0, -1), ], include_prev=True, count=2 )
        for i, pos in enumerate( gen_GAd_() ):
            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_G_A, 1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_A, -1, 1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_A, -1, -1 ) , mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_A, 1, -1 ) , mark_type=MarkType.Illegal )

        #
        # <-- <- G(B) -> -->

        gen_GBr_ = GS.gen_steps( start=start_G_B, rels=[ (1, 0), ], include_prev=True, count=5 )
        for i, pos in enumerate( gen_GBr_() ):
            # if i == 0:
            #     scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
            #     scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GBu_ = GS.gen_steps( start=start_G_B, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GBu_() ):
            # if i == 0:
            #     scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
            #     scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GBl_ = GS.gen_steps( start=start_G_B, rels=[ (-1, 0), ], include_prev=True, count=5 )
        for i, pos in enumerate( gen_GBl_() ):
            # if i == 0:
            #     scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )
            #     scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GBd_ = GS.gen_steps( start=start_G_B, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GBd_() ):
            # if i == 0:
            #     scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )
            #     scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        # scene.append_arrow( *GS.append_pos_rel( start_G_B, 1, 1 ) , mark_type=MarkType.Illegal )
        # scene.append_arrow( *GS.append_pos_rel( start_G_B, -1, 1 ) , mark_type=MarkType.Illegal )
        # scene.append_arrow( *GS.append_pos_rel( start_G_B, -1, -1 ) , mark_type=MarkType.Illegal )
        # scene.append_arrow( *GS.append_pos_rel( start_G_B, 1, -1 ) , mark_type=MarkType.Illegal )

        #
        # <-- <- G(C) -> -->

        gen_GCr_ = GS.gen_steps( start=start_G_C, rels=[ (1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCr_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GCu_ = GS.gen_steps( start=start_G_C, rels=[ (0, 1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCu_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, 1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GCl_ = GS.gen_steps( start=start_G_C, rels=[ (-1, 0), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCl_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, 1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        gen_GCd_ = GS.gen_steps( start=start_G_C, rels=[ (0, -1), ], include_prev=True, count=1 )
        for i, pos in enumerate( gen_GCd_() ):
            if i == 0:
                scene.append_arrow( *GS.add_end_rel( pos, -1, -1 ), mark_type=MarkType.Illegal )
                scene.append_arrow( *GS.add_end_rel( pos, 1, -1 ), mark_type=MarkType.Illegal )

            # mark_type = MarkType.Legal if i % 2 == 0 else MarkType.Action
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        scene.append_arrow( *GS.append_pos_rel( start_G_C, 1, 1 ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_G_C, -1, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_C, -1, -1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( start_G_C, 1, -1 ), mark_type=MarkType.Illegal )

        #
        # <-- <- G(D) -> -->

        coords = GS.append_pos_rel( start_G_D, 1, 0 )
        scene.append_arrow( *coords, mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.add_end_rel( coords, 1, 1 ) , mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.add_end_rel( coords, 1, -1 ) , mark_type=MarkType.Blocked )

        scene.append_text( "A", *start_G_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_G_B, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_G_C, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "D", *start_G_D, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )

        return scene
