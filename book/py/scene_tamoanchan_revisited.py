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


class SceneTamoanchanRevisitedMixin:

    def scn_tr_01_serpent_diagonals(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_01_serpent_diagonals', bt, width=8, height=8)

        start = (2, 2)
        # scene.append_text('S', *start, mark_type=MarkType.Blocked)

        scene.board.set_piece(*start, piece=PieceType.Serpent)

        scene.append_arrow( 2, 2, 1, 3 )
        scene.append_text('A', 1, 3, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 2, 2, 3, 1 )
        scene.append_text('A', 3, 1, corner=Corner.LowerRightFieldMarker)

        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Action )
        scene.append_text('B', 3, 3, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( 2, 2, 1, 1, mark_type=MarkType.Action )
        scene.append_text('B', 1, 1, mark_type=MarkType.Action, corner=Corner.LowerLeftFieldMarker)

        return scene

    def scn_tr_02_serpent_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_02_serpent_1', bt, width=8, height=8)

        start = (2, 2)
        scene.append_text('S', *start, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        scene.board.set_piece(3, 3, piece=PieceType.Serpent)

        scene.append_arrow( 3, 3, 2, 4 )
        scene.append_text('A', 2, 4, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 3, 3, 4, 2 )
        scene.append_text('A', 4, 2, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_tr_03_serpent_2(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_03_serpent_2', bt, width=8, height=8)

        start = (2, 2)
        scene.append_text('S', *start, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        scene.append_text('1', 3, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        scene.board.set_piece(2, 4, piece=PieceType.Serpent)

        scene.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Action )
        scene.append_text('B', 3, 5, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 2, 4, 1, 3, mark_type=MarkType.Action )
        scene.append_text('B', 1, 3, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_tr_04_serpent_3(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_04_serpent_3', bt, width=8, height=8)

        start = (2, 2)
        scene.append_text('S', *start, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        scene.append_text('1', 3, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        scene.append_text('2', 2, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        scene.board.set_piece(3, 5, piece=PieceType.Serpent)

        scene.append_arrow( 3, 5, 2, 6 )
        scene.append_text('A', 2, 6, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 3, 5, 4, 4 )
        scene.append_text('A', 4, 4, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_tr_05_serpent_end(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_05_serpent_end', bt, width=8, height=8)

        start = (2, 2)
        scene.append_text('S', *start, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        scene.append_text('1', 3, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        scene.append_text('2', 2, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        scene.append_text('3', 3, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 5, 4, 4, mark_type=MarkType.Blocked )

        scene.append_text('4', 4, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 4, 4, 5, 5, mark_type=MarkType.Blocked )

        scene.append_text('5', 5, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 5, 5, 6, 4, mark_type=MarkType.Blocked )

        scene.append_text('6', 6, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 6, 4, 5, 3, mark_type=MarkType.Blocked )

        scene.append_text('7', 5, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 5, 3, 6, 2, mark_type=MarkType.Blocked )

        scene.append_text('8', 6, 2, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        scene.board.set_piece(6, 2, piece=PieceType.Serpent)

        return scene

    def scn_tr_06_serpent_neighbors(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_06_serpent_neighbors', bt, width=8, height=8)

        start = (2, 2)
        scene.board.set_piece(*start, piece=PieceType.Serpent)
        scene.board.set_piece(1, 2, piece=PieceType.Wave)

        scene.append_arrow( 2, 2, 3, 2 )
        scene.append_arrow( 2, 2, 2, 3 )
        scene.append_arrow( 2, 2, 1, 2, mark_type=MarkType.Blocked )
        scene.append_arrow( 2, 2, 2, 1 )

        scene.append_field_marker(3, 2)
        scene.append_field_marker(2, 3)
        scene.append_field_marker(1, 2, mark_type=MarkType.Blocked)
        scene.append_field_marker(2, 1)

        return scene

    def scn_tr_07_serpent_loop_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_07_serpent_loop_1', bt, width=8, height=8)

        start = (2, 2)
        scene.append_text('S', *start, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        scene.append_text('1', 3, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        scene.append_text('2', 2, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        scene.board.set_piece(2, 6, piece=PieceType.Pyramid)
        scene.board.set_piece(3, 5, piece=PieceType.Serpent)

        scene.append_text('3', 3, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 5, 2, 6, mark_type=MarkType.Action )

        scene.append_text('4', 2, 6, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 3, 5, 4, 4 )

        return scene

    def scn_tr_08_serpent_loop_end(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_08_serpent_loop_end', bt, width=8, height=8)

        start = (2, 2)
        scene.append_text('S', *start, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        scene.append_text('1', 3, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        # scene.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        scene.append_text('2', 2, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        # scene.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        scene.append_text('3', 3, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 3, 5, 4, 4, mark_type=MarkType.Blocked )

        scene.append_text('4', 4, 4, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_arrow( 4, 4, 3, 3, mark_type=MarkType.Action )

        scene.append_text('5', 3, 3, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)
        scene.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Action )

        scene.append_text('6', 2, 4, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)
        scene.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Action )

        scene.append_text('7', 3, 5, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)
        scene.append_arrow( 3, 5, 2, 6, mark_type=MarkType.Blocked )

        scene.append_text('8', 2, 6, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        scene.board.set_piece(2, 6, piece=PieceType.Serpent)

        return scene

    def scn_tr_09_serpent_out_of_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_09_serpent_out_of_board', bt, x=4, y=1)

        start = (19, 4)
        scene.board.set_piece(*start, piece=PieceType.Serpent)

        scene.append_arrow( 19, 4, 20, 5 )
        scene.append_arrow( 20, 5, 21, 4 )

        scene.append_arrow( 21, 4, 22, 5, mark_type=MarkType.Illegal )

        scene.append_arrow( 22, 5, 21, 6, mark_type=MarkType.Illegal )
        scene.append_text('1', 21, 6, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 21, 6, 22, 7, mark_type=MarkType.Illegal )

        scene.append_arrow( 22, 7, 21, 8, mark_type=MarkType.Illegal )
        scene.append_text('2', 21, 8, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( 21, 8, 20, 7, mark_type=MarkType.Illegal )
        scene.append_text('3', 20, 7, mark_type=MarkType.Illegal, corner=Corner.LowerLeftFieldMarker)

        scene.append_arrow( 20, 7, 19, 8, mark_type=MarkType.Illegal )
        scene.append_text('4', 19, 8, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_tr_10_teleport_serpent_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_10_teleport_serpent_1', bt)

        start_S = (4, 19)
        start_T = (0, 21)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(21, 21, piece=PieceType.Star)
        scene.board.set_piece(21, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        scene.append_arrow( *(start_S + (3, 18)), mark_type=MarkType.Legal )
        scene.append_arrow( 3, 18, 2, 19, mark_type=MarkType.Legal )
        scene.append_arrow( 2, 19, 1, 18, mark_type=MarkType.Legal )
        scene.append_arrow( 1, 18, 0, 19, mark_type=MarkType.Legal )
        scene.append_arrow( 0, 19, 1, 20, mark_type=MarkType.Legal )
        scene.append_arrow( 1, 20, *start_T, mark_type=MarkType.Action )

        scene.append_text("1", 20, 21, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("2", 20, 20, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        scene.append_text("3", 21, 20, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Action)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_tr_11_teleport_serpent_2(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_11_teleport_serpent_2', bt)

        start_S = (0, 20)
        start_T = (0, 21)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(21, 21, piece=PieceType.Star)
        scene.board.set_piece(21, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        scene.append_arrow( *(start_S + start_T), mark_type=MarkType.Action )
        scene.append_arrow( *(start_S + (1, 20)), mark_type=MarkType.Legal )
        scene.append_arrow( *(start_S + (0, 19)), mark_type=MarkType.Legal )

        scene.append_text("1", 20, 21, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        scene.append_text("2", 20, 20, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 21, 20, corner=Corner.LowerLeft, mark_type=MarkType.Action)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Action)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_tr_12_serpent_activating_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_12_serpent_activating_wave', bt, width=8, height=8)

        scene.board.set_piece(1, 1, piece=PieceType.Serpent)
        scene.board.set_piece(4, 4, piece=PieceType.Wave)

        scene.append_arrow( 1, 1, 2, 2, mark_type=MarkType.Action )
        scene.append_arrow( 2, 2, 1, 3, mark_type=MarkType.Action )
        scene.append_arrow( 1, 3, 2, 4, mark_type=MarkType.Action )
        scene.append_arrow( 2, 4, 3, 3, mark_type=MarkType.Action )
        scene.append_arrow( 3, 3, 4, 4, mark_type=MarkType.Action )

        return scene

    def scn_tr_13_serpent_activated_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_13_serpent_activated_wave', bt, width=8, height=8)

        start = (4, 4)
        scene.board.set_piece(*start, piece=PieceType.Serpent)

        #
        # right diagonal == <1, 1> || <-1, -1>

        scene.append_arrow( *(start + (5, 5)), mark_type=MarkType.Action )
        scene.append_arrow( *(start + (3, 3)), mark_type=MarkType.Action )

        scene.append_text('A', 5, 5, mark_type=MarkType.Action, corner=Corner.UpperRight)
        scene.append_text('A', 3, 3, mark_type=MarkType.Action, corner=Corner.LowerLeft)

        #
        # left diagonal == <1, -1> || <-1, 1>

        scene.append_arrow( *(start + (5, 3)), mark_type=MarkType.Legal )
        scene.append_arrow( *(start + (3, 5)), mark_type=MarkType.Legal )

        scene.append_text('B', 5, 3, mark_type=MarkType.Legal, corner=Corner.LowerRight)
        scene.append_text('B', 3, 5, mark_type=MarkType.Legal, corner=Corner.UpperLeft)

        return scene

    def scn_tr_14_serpent_activated_wave_step_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_14_serpent_activated_wave_step_1', bt, width=8, height=8)

        start_S = (4, 4)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        start = (5, 3)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        scene.append_arrow( *(start_S + start), mark_type=MarkType.Blocked )

        #
        # right diagonal == <1, 1> || <-1, -1>

        scene.append_arrow( *(start + (6, 4)), mark_type=MarkType.Action )
        scene.append_arrow( *(start + (4, 2)), mark_type=MarkType.Action )

        scene.append_text('A', 6, 4, mark_type=MarkType.Action, corner=Corner.UpperRight)
        scene.append_text('A', 4, 2, mark_type=MarkType.Action, corner=Corner.LowerLeft)

        #
        # left diagonal == <1, -1> || <-1, 1>

        # scene.append_arrow( *(start + (5, 3)), mark_type=MarkType.Legal )
        # scene.append_arrow( *(start + (3, 5)), mark_type=MarkType.Legal )

        # scene.append_text('B', 5, 3, mark_type=MarkType.Legal, corner=Corner.LowerRight)
        # scene.append_text('B', 3, 5, mark_type=MarkType.Legal, corner=Corner.UpperLeft)

        return scene

    def scn_tr_15_serpent_activated_wave_ply(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_15_serpent_activated_wave_ply', bt)

        start = (4, 4)
        scene.board.set_piece(*start, piece=PieceType.Serpent)

        coords = GS.gen_steps(start=start, rels=[(1, -1), (1, 1), ], include_prev=True, count=18)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Legal if index % 2 == 0 else MarkType.Action
            if index < 17:
                scene.append_arrow( *coord, mark_type=mark_type )

        scene.append_text('B', 5, 3, corner=Corner.LowerRight, mark_type=MarkType.Legal)
        scene.append_text('A', 6, 4, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # illegal direction change

        step_6 = (10, 4)
        scene.append_text('6', *step_6, corner=Corner.UpperRight, mark_type=MarkType.Action)

        step_7 = (11, 3)
        scene.append_text('B', *step_7, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        step_7i = (9, 5)
        scene.append_arrow( *(step_6 + step_7i), mark_type=MarkType.Illegal )
        scene.append_text('B', *step_7i, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)

        return scene

    def scn_tr_16_wave_out_of_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_16_wave_out_of_board', bt, x=4, y=1)

        scene.board.set_piece(18, 5, piece=PieceType.Serpent)

        scene.append_arrow( 18, 5, 19, 4, mark_type=MarkType.Action )
        scene.append_arrow( 19, 4, 20, 5, mark_type=MarkType.Action )
        scene.append_arrow( 20, 5, 21, 4, mark_type=MarkType.Action )

        start = (21, 4)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        #
        # arrows

        coords = GS.gen_steps(start=start, rels=[(1, 1), (-1, 1), ], include_prev=True, count=18)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Illegal if (index % 2 == 0) or (index >= 17) else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        #
        # enumeration text

        coords = GS.gen_steps(start=start, rels=[(1, 1), (-1, 1), ], include_prev=False, count=18)

        for index, coord in enumerate( coords() ):
            if (index % 2 != 0) and (index < 17):
                i = int( (index + 1) // 2 )
                scene.append_text(str(i), *coord, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        #
        # diagonals, from activation example

        scene.append_text('A', 22, 5, corner=Corner.UpperRight, mark_type=MarkType.Illegal)
        scene.append_text('B', 21, 6, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        return scene

    def scn_tr_17_off_board_teleport_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_17_off_board_teleport_wave', bt, x=4, y=1)

        scene.board.set_piece(21, 21, piece=PieceType.Star)
        scene.board.set_piece(18, 6, piece=PieceType.Serpent)

        scene.append_arrow( 18, 6, 19, 5, mark_type=MarkType.Action )
        scene.append_arrow( 19, 5, 20, 6, mark_type=MarkType.Action )
        scene.append_arrow( 20, 6, 21, 5, mark_type=MarkType.Action )

        start = (21, 5)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        #
        # arrows

        coords = GS.gen_steps(start=start, rels=[(1, 1), (-1, 1), ], include_prev=True, count=16)

        for index, coord in enumerate( coords() ):
            # mark_type = MarkType.Illegal if (index % 2 == 0) or (index >= 17) else MarkType.Legal
            mark_type = MarkType.Legal
            if index == 15:
                mark_type = MarkType.Action
            elif (index % 2 == 0) or (index >= 17):
                mark_type = MarkType.Illegal
            scene.append_arrow( *coord, mark_type=mark_type )

        #
        # diagonals

        scene.append_text('A', 22, 6, corner=Corner.UpperRight, mark_type=MarkType.Illegal)
        scene.append_text('B', 21, 7, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        scene.append_text('B', 21, 21, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        return scene

    def scn_tr_18_teleported_wave_on_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_18_teleported_wave_on_board', bt, x=-4, y=-1)

        scene.board.set_piece(0, 0, piece=PieceType.Star)

        scene.board.set_piece(0, 6, piece=-PieceType.Pyramid)
        scene.board.set_piece(1, 15, piece=PieceType.Pawn)

        start = (0, 0)

        #
        # arrows

        coords = GS.gen_steps(start=start, rels=[(1, 1), (-1, 1), ], include_prev=True, count=21)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Legal
            if index == 5:
                mark_type = MarkType.Blocked
            elif index == 14:
                mark_type = MarkType.Action
            scene.append_arrow( *coord, mark_type=mark_type )

        #
        # diagonals

        scene.append_text('A', 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text('B', 0, 2, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        scene.append_text('B', 0, 10, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        scene.append_text('9', 1, 9, corner=Corner.UpperRight, mark_type=MarkType.Action)
        scene.append_arrow( 1, 9, 2, 8, mark_type=MarkType.Illegal )
        scene.append_text('B', 2, 8, corner=Corner.LowerRight, mark_type=MarkType.Illegal)

        return scene

    def scn_tr_19_on_board_teleport_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_19_on_board_teleport_wave', bt, x=4, y=1)

        scene.board.set_piece(21, 21, piece=PieceType.Star)
        scene.board.set_piece(17, 7, piece=PieceType.Serpent)

        scene.append_arrow( 17, 7, 18, 6, mark_type=MarkType.Action )
        scene.append_arrow( 18, 6, 19, 7, mark_type=MarkType.Action )
        scene.append_arrow( 19, 7, 20, 6, mark_type=MarkType.Action )

        start = (20, 6)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        #
        # arrows

        coords = GS.gen_steps(start=start, rels=[(1, 1), (-1, 1), ], include_prev=True, count=15)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Action if index == 14 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        #
        # diagonals

        scene.append_text('A', 21, 7, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text('B', 20, 8, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        scene.append_text('A', 21, 21, corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_tr_20_teleported_wave_off_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_20_teleported_wave_off_board', bt, x=-4, y=-1)

        scene.board.set_piece(0, 0, piece=PieceType.Star)

        scene.board.set_piece(0, 6, piece=-PieceType.Pyramid)
        scene.board.set_piece(0, 16, piece=PieceType.Pawn)

        start = (0, 0)

        #
        # arrows

        coords = GS.gen_steps(start=start, rels=[(-1, 1), (1, 1), ], include_prev=True, count=21)

        for index, coord in enumerate( coords() ):
            if index == 5:
                mark_type = MarkType.Blocked
            elif index == 15:
                mark_type = MarkType.Action
            else:
                mark_type = MarkType.Illegal if (index % 2 == 0) else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        #
        # enumeration text

        coords = GS.gen_steps(start=start, rels=[(-1, 1), (1, 1), ], include_prev=False, count=20)

        for index, coord in enumerate( coords() ):
            if (index % 2 != 0): # (index < 17):
                i = int( (index + 1) // 2 )
                mark_type = MarkType.Legal
                if index == 5:
                    mark_type = MarkType.Blocked
                elif index == 15:
                    mark_type = MarkType.Action
                scene.append_text(str(i), *coord, mark_type=mark_type, corner=Corner.UpperLeftFieldMarker)

        #
        # diagonals

        scene.append_text('B', -1, 1, mark_type=MarkType.Illegal, corner=Corner.UpperLeft)
        scene.append_text('A', 0, 2, mark_type=MarkType.Legal, corner=Corner.UpperRight)

        return scene
