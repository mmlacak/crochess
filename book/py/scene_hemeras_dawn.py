#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


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

        scene = Scene('scn_hd_01_centaur_same_color', bt, width=7, height=7)

        start = (3, 3)
        scene.board.set_piece(*start, piece=PieceType.Centaur)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, bounds=scene.board_view.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_hd_02_centaur_opposite_color(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_02_centaur_opposite_color', bt)

        start = (6, 6)
        scene.board.set_piece(*start, piece=PieceType.Centaur)

        # Centaur, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start, include_prev=False, bounds=((2, 2), (10, 10)))

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, bounds=((4, 4), (8, 8)))

        for i, pos in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

        return scene

    def scn_hd_03_centaur_multi_step(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_03_centaur_multi_step', bt)

        start = (3, 2)
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
        arr = GS.gen_next( GS.gen_steps(start=start, rels=rels, include_prev=True) )
        txt = GS.gen_next( GS.gen_steps(start=start, rels=rels, include_prev=False) )

        #
        # choose directions

        # short
        scene.append_arrow( *arr(), mark_type=MarkType.Action )
        scene.append_text("1", *txt(), mark_type=MarkType.Action)

        # long
        scene.append_arrow( *arr(), mark_type=MarkType.Action )
        scene.append_text("2", *txt(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # follow directions

        # short
        scene.append_arrow( *arr() )
        scene.append_text("3", *txt())

        # long
        scene.append_arrow( *arr() )
        scene.append_text("4", *txt(), corner=Corner.UpperRight)

        # short
        scene.append_arrow( *arr() )
        scene.append_text("5", *txt())

        # long
        scene.append_arrow( *arr() )
        scene.append_text("6", *txt(), corner=Corner.UpperRight)

        # short
        scene.append_arrow( *arr() )
        scene.append_text("7", *txt())

        # long
        scene.append_arrow( *arr() )
        scene.append_text("8", *txt(), corner=Corner.UpperRight)

        # short
        scene.append_arrow( *arr(), mark_type=MarkType.Action )
        scene.append_text("9", *txt(), mark_type=MarkType.Action)

        # long
        scene.append_arrow( *arr(), mark_type=MarkType.Blocked )
        scene.append_text("10", *txt(), mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

        # short
        scene.append_arrow( *arr(), mark_type=MarkType.Blocked )
        scene.append_text("11", *txt(), mark_type=MarkType.Blocked)

        #
        # forbidden directions change

        # (-1, 2) is ok, i.e. direction "7", here: 12, 11 --> 11, 13
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-1, 2), ) ) )
        startK = (12, 11)

        arr = GS.gen_next( GS.gen_multi_steps(multi_rels, start=startK, include_prev=True, count=1) )
        txt = GS.gen_next( GS.gen_multi_steps(multi_rels, start=startK, include_prev=False, count=1) )

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7a", *txt(), mark_type=MarkType.Illegal, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7b", *txt(), mark_type=MarkType.Illegal, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7c", *txt(), mark_type=MarkType.Illegal, corner=Corner.UpperLeft)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7d", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerLeft)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7e", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerLeft)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7f", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerRightFieldMarker)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7g", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerRightFieldMarker)

        return scene


    def scn_hd_04_centaur_off_board(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_04_centaur_off_board', bt, x=4, y=1)

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


    def scn_hd_05_wave_activation_by_centaur(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_05_wave_activation_by_centaur', bt)

        start = (3, 2)
        start_C = (7, 1)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(*start_C, piece=PieceType.Centaur)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 8, piece=PieceType.Pawn)
        scene.board.set_piece(8, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(14, 16, piece=-PieceType.Wave)

        #
        # Wave activation by Centaur
        scene.append_arrow( *(start_C + start), mark_type=MarkType.Legal )

        #
        # short --> (-1, 2) direction
        # long --> (4, 1) direction

        rels = [(-1, 2), (4, 1), ]
        arr = GS.gen_next( GS.gen_steps(start=start, rels=rels, include_prev=True) )
        txt = GS.gen_next( GS.gen_steps(start=start, rels=rels, include_prev=False) )

        #
        # choose directions

        # short
        scene.append_arrow( *arr(), mark_type=MarkType.Action )
        scene.append_text("1", *txt(), mark_type=MarkType.Action)

        # long
        scene.append_arrow( *arr(), mark_type=MarkType.Action )
        scene.append_text("2", *txt(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # follow directions

        # short
        scene.append_arrow( *arr() )
        scene.append_text("3", *txt())

        # long
        scene.append_arrow( *arr() )
        scene.append_text("4", *txt(), corner=Corner.UpperRight)

        # short
        scene.append_arrow( *arr() )
        scene.append_text("5", *txt())

        # long
        scene.append_arrow( *arr() )
        scene.append_text("6", *txt(), corner=Corner.UpperRight)

        # short
        scene.append_arrow( *arr() )
        scene.append_text("7", *txt())

        # long
        scene.append_arrow( *arr() )
        scene.append_text("8", *txt(), corner=Corner.UpperRight)

        # short
        scene.append_arrow( *arr(), mark_type=MarkType.Action )
        scene.append_text("9", *txt(), mark_type=MarkType.Action)

        # long
        scene.append_arrow( *arr() )
        scene.append_text("10", *txt(), corner=Corner.UpperRightFieldMarker)

        # short
        scene.append_arrow( *arr() )
        scene.append_text("11", *txt())

        #
        # forbidden directions change

        # (-1, 2) is ok, i.e. direction "7", here: 12, 11 --> 11, 13
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-1, 2), ) ) )
        startK = (12, 11)

        arr = GS.gen_next( GS.gen_multi_steps(multi_rels, start=startK, include_prev=True, count=1) )
        txt = GS.gen_next( GS.gen_multi_steps(multi_rels, start=startK, include_prev=False, count=1) )

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7a", *txt(), mark_type=MarkType.Illegal, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7b", *txt(), mark_type=MarkType.Illegal, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7c", *txt(), mark_type=MarkType.Illegal, corner=Corner.UpperLeft)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7d", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerLeft)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7e", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerLeft)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7f", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerRightFieldMarker)

        scene.append_arrow( *arr(), mark_type=MarkType.Illegal )
        scene.append_text("7g", *txt(), mark_type=MarkType.Illegal, corner=Corner.LowerRightFieldMarker)

        return scene


    def scn_hd_06_wave_activated_by_centaur_off_board(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_06_wave_activated_by_centaur_off_board', bt, x=4, y=1)

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


    def scn_hd_07_wave_teleport(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_07_wave_teleport', bt, x=4, y=1)

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


    def scn_hd_08_umbrella_pawns(self, bt=BoardType.HemerasDawn):

        scene = Scene('scn_hd_08_umbrella_pawns', bt)

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
