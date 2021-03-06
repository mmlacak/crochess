#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneHemerasDawnMixin:

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


    def scn_hd_12_scout_pawns(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_12_scout_pawns', bt)

        scene.board.set_piece(4, 0, piece=PieceType.Centaur)

        scene.board.set_piece(2, 3, piece=PieceType.Pawn)
        scene.board.set_piece(3, 4, piece=PieceType.Pawn)
        scene.board.set_piece(5, 4, piece=PieceType.Pawn)
        scene.board.set_piece(6, 3, piece=PieceType.Pawn)

        scene.board.set_piece(15, 0, piece=PieceType.Centaur)

        scene.board.set_piece(13, 3, piece=PieceType.Pawn)
        scene.board.set_piece(14, 4, piece=PieceType.Pawn)
        scene.board.set_piece(16, 4, piece=PieceType.Pawn)
        scene.board.set_piece(17, 3, piece=PieceType.Pawn)

        scene.board.set_piece(4, 19, piece=-PieceType.Centaur)

        scene.board.set_piece(2, 16, piece=-PieceType.Pawn)
        scene.board.set_piece(3, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(5, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(6, 16, piece=-PieceType.Pawn)

        scene.board.set_piece(15, 19, piece=-PieceType.Centaur)

        scene.board.set_piece(13, 16, piece=-PieceType.Pawn)
        scene.board.set_piece(14, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(16, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(17, 16, piece=-PieceType.Pawn)

        return scene
