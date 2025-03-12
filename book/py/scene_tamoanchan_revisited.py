#!/usr/bin/env -S python3 -B
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


class SceneTamoanchanRevisitedMixin:

    #
    # Movement

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

    def scn_tr_06_serpent_loop_illegal(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_06_serpent_loop_illegal', bt, width=8, height=8)

        start = (2, 2)
        scene.board.set_piece( *start, piece=PieceType.Serpent )

        adr = GS.adder( start, include_prev=True )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, -1), mark_type=MarkType.Illegal )

        scene.append_text( "S", *start, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )

        adr_2 = GS.adder( start, include_prev=False )
        scene.append_text( "1", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_2(1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )

        return scene

    #
    # Static move is illegal

    def scn_tr_07_static_move_is_illegal(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_07_static_move_is_illegal', bt, width=8, height=8)

        start_S = (3, 3)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        pos_1 = (2, 4)
        pos_2 = (3, 5)
        pos_3 = (4, 4)

        scene.append_arrow( *( start_S + pos_1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_1 + pos_2 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_2 + pos_3 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_3 + start_S ), mark_type=MarkType.Illegal )

        scene.append_text( "1", *pos_1, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "2", *pos_2, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "3", *pos_3, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)

        scene.append_text( "S", *start_S, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        return scene

    #
    # Static Serpent is illegal

    def scn_tr_08_static_piece_is_illegal(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_08_static_piece_is_illegal', bt, width=8, height=8)

        start_Q = (5, 6)
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        start_W = (5, 1)
        end_W = (1, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        start_S = (2, 4)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q, rels=[(0, -1), ], include_prev=True, count=5 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> S
        coords_W_S = GS.gen_steps( start=start_W, rels=[(-1, 1), ], include_prev=True, count=3 ) # bounds=scene.board_view.get_position_limits() )

        for i, arrow in enumerate( coords_W_S() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        pos_1 = (1, 5)
        pos_2 = (2, 6)
        pos_3 = (3, 5)

        scene.append_arrow( *( start_S + pos_1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_1 + pos_2 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_2 + pos_3 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_3 + start_S ), mark_type=MarkType.Illegal )

        scene.append_arrow( *( start_S + end_W ), mark_type=MarkType.Illegal )

        scene.append_text( "1", *pos_1, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "2", *pos_2, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "3", *pos_3, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)

        scene.append_text( "S", *start_S, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "W", *end_W, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        return scene

    #
    # Revisiting fields, loops

    def scn_tr_09_serpent_loop_init(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_09_serpent_loop_init', bt)

        start_Q = (1, 20)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_A = (1, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_S = (7, 7)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_W_B = (7, 11)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_B = (7, 15)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_C = (9, 13)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_A = (13, 9)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen_Q_WA = GS.gen_steps( start=start_Q, rels=[(0, -1), ], include_prev=True, count=19 )
        for index, coords in enumerate( gen_Q_WA() ):
            mark_type = MarkType.Action if index == 18 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_WA_S = GS.gen_steps( start=start_W_A, rels=[(1, 1), ], include_prev=True, count=6 )
        for index, coords in enumerate( gen_WA_S() ):
            mark_type = MarkType.Action if index == 5 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        adr_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Action )

        adr_WB = GS.adder( start_W_B, include_prev=True )
        scene.append_arrow( *adr_WB(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_WB(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_WB(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_WB(1, 1), mark_type=MarkType.Action )

        gen_B_WC = GS.gen_steps( start=start_B, rels=[(1, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_B_WC() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_text( "Q", *start_Q, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "S", *start_S, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "A", *start_W_A, mark_type=MarkType.Legal, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Legal, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "C", *start_W_C, mark_type=MarkType.Legal, corner=Corner.UpperRightFieldMarker )

        adr_S_2 = GS.adder( start_S, include_prev=False )
        scene.append_text( "1", *adr_S_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_S_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_S_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_S_2(-1, 1), mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker )

        return scene

    def scn_tr_10_serpent_loop_step(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_10_serpent_loop_step', bt)

        prev_Q = (1, 20)
        prev_W_A = (1, 1)
        prev_S = (7, 7)
        prev_W_B = (7, 11)
        prev_B = (7, 15)
        prev_W_C = (9, 13)

        start_Q = prev_W_A # (1, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_A = prev_S # (7, 7)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_S = prev_W_B # (7, 11)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_W_B = prev_B # (7, 15)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_B = prev_W_C # (9, 13)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # start_W_C = (9, 13)
        # scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_A = (13, 9)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen_Q_WA = GS.gen_steps( end=start_Q, rels=[(0, -1), ], include_prev=True, count=19 )
        for index, coords in enumerate( gen_Q_WA() ):
            # mark_type = MarkType.Action if index == 18 else \
            #             MarkType.Blocked
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        gen_WA_S = GS.gen_steps( end=start_W_A, rels=[(1, 1), ], include_prev=True, count=6 )
        for index, coords in enumerate( gen_WA_S() ):
            # mark_type = MarkType.Action if index == 5 else \
            #             MarkType.Legal
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        adr_S = GS.adder( prev_S, include_prev=True )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Blocked )

        adr_WB = GS.adder( prev_W_B, include_prev=True )
        scene.append_arrow( *adr_WB(-1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_WB(1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_WB(-1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_WB(1, 1), mark_type=MarkType.Blocked )

        gen_B_WC = GS.gen_steps( end=start_B, rels=[(1, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_B_WC() ):
            # mark_type = MarkType.Action if index == 1 else \
            #             MarkType.Legal
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        gen_WC_S = GS.gen_steps( start=prev_W_C, rels=[(-1, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_WC_S() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_text( "Q", *prev_Q, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "S", *prev_S, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "A", *start_W_A, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )
        # scene.append_text( "C", *start_W_C, mark_type=MarkType.Legal, corner=Corner.UpperRightFieldMarker )

        adr_S_2 = GS.adder( prev_S, include_prev=False )
        scene.append_text( "1", *adr_S_2(1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_S_2(-1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_S_2(1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_S_2(-1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )

        return scene

    def scn_tr_11_serpent_loop_end(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_11_serpent_loop_end', bt)

        prev_Q = (1, 20)
        prev_W_A = (1, 1)
        prev_S = (7, 7)
        prev_W_B = (7, 11)
        prev_B = (7, 15)
        prev_W_C = (9, 13)

        start_Q = prev_W_A # (1, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_A = prev_S # (7, 7)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_S = prev_W_B # (7, 11)
        # scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_W_B = prev_B # (7, 15)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_B = prev_W_C # (9, 13)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_C = start_S # (7, 11)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_A = (13, 9)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        adr_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Action )

        scene.append_text( "Q", *prev_Q, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "S", *prev_S, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "A", *start_W_A, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "B", *start_W_B, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "C", *start_W_C, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )

        adr_S_2 = GS.adder( prev_S, include_prev=False )
        scene.append_text( "1", *adr_S_2(1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_S_2(-1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_S_2(1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_S_2(-1, 1), mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )

        return scene

    #
    # Different paths, momentum

    def scn_tr_12_serpent_path_short(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_12_serpent_path_short', bt, width=8, height=8)

        scene.board.set_piece( 2, 6, piece=PieceType.Pyramid )

        start = (2, 2)
        scene.board.set_piece( *start, piece=PieceType.Serpent )

        adr = GS.adder( start, include_prev=True )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Action )

        scene.append_text( "S", *start, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )

        adr_2 = GS.adder( start, include_prev=False )
        scene.append_text( "1", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_2(-1, 1), mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker )

        return scene

    def scn_tr_13_serpent_path_long(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_13_serpent_path_long', bt, width=8, height=8)

        scene.board.set_piece( 2, 6, piece=PieceType.Pyramid )

        start = (2, 2)
        scene.board.set_piece( *start, piece=PieceType.Serpent )

        adr = GS.adder( start, include_prev=True )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Action )

        scene.append_text( "S", *start, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )

        adr_2 = GS.adder( start, include_prev=False )
        scene.append_text( "1", *adr_2(1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "5", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "6", *adr_2(-1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "7", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "8", *adr_2(1, 1), mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker )

        return scene

    def scn_tr_14_serpent_path_longer(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_14_serpent_path_longer', bt, width=8, height=8)

        scene.board.set_piece( 2, 6, piece=PieceType.Pyramid )

        start = (2, 2)
        scene.board.set_piece( *start, piece=PieceType.Serpent )

        adr = GS.adder( start, include_prev=True )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr(1, 1), mark_type=MarkType.Action )

        scene.append_text( "S", *start, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )

        adr_2 = GS.adder( start, include_prev=False )
        scene.append_text( "1", *adr_2(1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "3", *adr_2(1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "4", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "5", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "6", *adr_2(1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "7", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "8", *adr_2(-1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "9", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "10", *adr_2(-1, -1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "11", *adr_2(-1, 1), mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "12", *adr_2(1, 1), mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker )

        return scene

    #
    # Step limit, momentum

    def scn_tr_15_serpent_step_limit(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_15_serpent_step_limit', bt)

        start_Q = (1, 20)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_A = (1, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_S = (3, 3)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_A = (13, 11)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen_Q_WA = GS.gen_steps( start=start_Q, rels=[(0, -1), ], include_prev=True, count=19 )
        for index, coords in enumerate( gen_Q_WA() ):
            mark_type = MarkType.Action if index == 18 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_WA_S = GS.gen_steps( start=start_W_A, rels=[(1, 1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_WA_S() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        adr_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )

        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )

        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )

        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Blocked )

        scene.append_arrow( *adr_S(1, -1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_S(1, 1), mark_type=MarkType.Blocked )
        scene.append_arrow( *adr_S(-1, 1), mark_type=MarkType.Blocked )

        return scene

    def scn_tr_16_serpent_step_limit_momentum(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_16_serpent_step_limit_momentum', bt)

        start_Q = (1, 20)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_A = (1, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_S = (3, 3)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_A = (17, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen_Q_WA = GS.gen_steps( start=start_Q, rels=[(0, -1), ], include_prev=True, count=19 )
        for index, coords in enumerate( gen_Q_WA() ):
            mark_type = MarkType.Action if index == 18 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_WA_S = GS.gen_steps( start=start_W_A, rels=[(1, 1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_WA_S() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_S_A = GS.gen_steps( start=start_S, rels=[(1, -1), (1, 1), ], include_prev=True, count=14 )
        for index, coords in enumerate( gen_S_A() ):
            mark_type = MarkType.Action if index == 13 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_A_ = GS.gen_steps( start=start_A, rels=[(0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen_A_() ):
            mark_type = MarkType.Legal if index < 5 else \
                        MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Color-changing move

    def scn_tr_17_serpent_neighbors(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_17_serpent_neighbors', bt, width=8, height=8)

        start = (2, 2)
        scene.board.set_piece(*start, piece=PieceType.Serpent)
        scene.board.set_piece(1, 2, piece=PieceType.Wave)

        scene.append_arrow( 2, 2, 3, 2 )
        scene.append_arrow( 2, 2, 2, 3 )
        scene.append_arrow( 2, 2, 1, 2, mark_type=MarkType.Illegal )
        scene.append_arrow( 2, 2, 2, 1 )

        scene.append_field_marker(3, 2)
        scene.append_field_marker(2, 3)
        scene.append_field_marker(1, 2, mark_type=MarkType.Illegal)
        scene.append_field_marker(2, 1)

        return scene

    def scn_tr_18_cascade_serpent_neighbors(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_18_cascade_serpent_neighbors', bt, width=8, height=8)

        start_B = (6, 2)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W = (4, 4)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start = (2, 2)
        scene.board.set_piece( *start, piece=PieceType.Serpent )
        scene.board.set_piece( 1, 2, piece=PieceType.Wave )

        gen_B_W = GS.gen_steps( start=start_B, rels=[(-1, 1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_B_W() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_W_S = GS.gen_steps( start=start_W, rels=[(-1, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_W_S() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_arrow( 2, 2, 3, 2 )
        scene.append_arrow( 2, 2, 2, 3 )
        scene.append_arrow( 2, 2, 1, 2, mark_type=MarkType.Illegal )
        scene.append_arrow( 2, 2, 2, 1 )

        scene.append_field_marker(3, 2)
        scene.append_field_marker(2, 3)
        scene.append_field_marker(1, 2, mark_type=MarkType.Illegal)
        scene.append_field_marker(2, 1)

        return scene

    #
    # Displacing Pawns

    def scn_tr_19_displacement_init(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_19_displacement_init', bt, width=8, height=8)

        start_S = (1, 3)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (3, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        gen_S_ = GS.gen_steps( start=start_S, rels=[ (1, 1), (1, -1), ], include_prev=True, count=3 )
        for index, coords in enumerate( gen_S_() ):
            mark_type = MarkType.Action if index == 2 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_tr_20_displacement_step_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_20_displacement_step_1', bt, width=8, height=8)

        prev_p = (4, 4)
        prev_S = (1, 3)

        start_S = prev_p
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        # start_p = (4, 4)
        # scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (3, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        scene.append_arrow( *GS.append_pos_rel( prev_p, 1, 0 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_pos_rel( prev_p, 0, 1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_pos_rel( prev_p, -1, 0 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( prev_p, 0, -1 ), mark_type=MarkType.Legal )

        scene.append_field_marker( *GS.add_rel( prev_p, 1, 0 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_rel( prev_p, 0, 1 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_rel( prev_p, -1, 0 ), mark_type=MarkType.Illegal )
        scene.append_field_marker( *GS.add_rel( prev_p, 0, -1 ), mark_type=MarkType.Legal )

        return scene

    def scn_tr_21_displacement_step_2(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_21_displacement_step_2', bt, width=8, height=8)

        prev_p = (4, 4)
        prev_P = (5, 3)
        prev_S = (1, 3)

        start_p = (5, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (3, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_S = prev_P
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        scene.append_arrow( *( prev_p + start_S ), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_pos_rel( prev_P, 1, 0 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_pos_rel( prev_P, 0, 1 ), mark_type=MarkType.Illegal )
        scene.append_arrow( *GS.append_pos_rel( prev_P, -1, 0 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_pos_rel( prev_P, 0, -1 ), mark_type=MarkType.Legal )

        scene.append_field_marker( *GS.add_rel( prev_P, 1, 0 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_rel( prev_P, 0, 1 ), mark_type=MarkType.Illegal )
        scene.append_field_marker( *GS.add_rel( prev_P, -1, 0 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_rel( prev_P, 0, -1 ), mark_type=MarkType.Legal )

        return scene

    def scn_tr_22_displacement_end(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_22_displacement_end', bt, width=8, height=8)

        prev_p = (4, 4)
        prev_P = (5, 3)
        prev_S = (1, 3)

        start_p = (5, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (3, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_S = (7, 3)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_P = (5, 2)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        gen_S_ = GS.gen_steps( end=start_S, rels=[ (1, -1), (1, 1), ], include_prev=True, count=6 )
        for index, coords in enumerate( gen_S_() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

        scene.append_text( "1", *prev_p, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text( "2", *prev_P, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker )

        return scene

    def scn_tr_23_displacement_activated(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_23_displacement_activated', bt, width=8, height=8)

        start_R = (5, 1)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W = (1, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_S = (1, 3)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (3, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        gen_R_W = GS.gen_steps( start=start_R, rels=[ (-1, 0), ], include_prev=True, count=4 )
        for index, coords in enumerate( gen_R_W() ):
            mark_type = MarkType.Action if index == 3 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_W_S = GS.gen_steps( start=start_W, rels=[ (0, 1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_W_S() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_S_ = GS.gen_steps( start=start_S, rels=[ (1, 1), (1, -1), ], include_prev=True, count=4 )
        for index, coords in enumerate( gen_S_() ):
            mark_type = MarkType.Action if index == 3 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Out-of-board steps

    def scn_tr_24_serpent_out_of_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_24_serpent_out_of_board', bt, x=4, y=1)

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

    #
    # Teleporting Serpent

    def scn_tr_25_teleport_serpent_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_25_teleport_serpent_1', bt)

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

    def scn_tr_26_teleport_serpent_2(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_26_teleport_serpent_2', bt)

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

    #
    # ... Pawn-sacrifice move

    def scn_tr_27_pawn_sacrifice_init( self, bt=BoardType.TamoanchanRevisited ):

        scene = Scene( 'scn_tr_27_pawn_sacrifice_init', bt, width=8.3, height=12.3 )

        start_S = (6, 2)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_A = (1, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_P = (1, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        adder = GS.adder( start_A, include_prev=False )
        adder( 1, 1 ) # empty field
        scene.board.set_piece( *adder( -1, 1 ), piece=PieceType.Pawn ) # light Pawn A
        scene.append_text( "A", *adder( 0, 0 ), corner=Corner.UpperLeft, mark_type=MarkType.Action )

        adder( 1, 1 ) # empty field
        scene.board.set_piece( *adder( -1,  1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder(  1,  1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder( -1,  1, do_advance=False ), piece=-PieceType.Bishop )

        adder( 1,  1 ) # empty field
        scene.board.set_piece( *adder( 1, -1 ), piece=-PieceType.Wave )
        adder( 1,  1 ) # empty field
        scene.board.set_piece( *adder(  1, -1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder(  1, -1 ), piece=PieceType.Pawn ) # light Pawn B
        scene.append_text( "B", *adder( 0, 0 ), corner=Corner.UpperLeft, mark_type=MarkType.Action )

        coords_S_A = GS.gen_steps( start=start_S, rels=[ (-1, 1), (-1, -1), ], include_prev=True, count=5 )
        for i, arr in enumerate( coords_S_A() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        coords_A_P = GS.gen_steps( start=start_A, rels=[ (0, -1), ], include_prev=True, count=2 )
        for i, arr in enumerate( coords_A_P() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arr, mark_type=mark_type )

        return scene

    def scn_tr_28_pawn_sacrifice_steps_1( self, bt=BoardType.TamoanchanRevisited ):

        scene = Scene( 'scn_tr_28_pawn_sacrifice_steps_1', bt, width=8.3, height=12.3 )

        prev_S = (6, 2)
        prev_A = (1, 3)
        prev_P = (1, 1)

        start_S = prev_A
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_A = prev_P
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # start_P = (1, 1)
        # scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        adder = GS.adder( start_S, include_prev=False )
        adder( 1, 1 ) # empty field
        scene.board.set_piece( *adder( -1,  1 ), piece=PieceType.Pawn ) # light Pawn A
        scene.append_text( "A", *adder( 0, 0 ), corner=Corner.UpperLeft, mark_type=MarkType.Action )

        adder( 1, 1 ) # empty field
        scene.board.set_piece( *adder( -1,  1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder(  1,  1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder( -1,  1, do_advance=False ), piece=-PieceType.Bishop )

        adder( 1,  1 ) # empty field
        scene.board.set_piece( *adder( 1, -1 ), piece=-PieceType.Wave )
        adder( 1,  1 ) # empty field
        scene.board.set_piece( *adder(  1, -1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder(  1, -1 ), piece=PieceType.Pawn ) # light Pawn B
        scene.append_text( "B", *adder( 0, 0 ), corner=Corner.UpperRight, mark_type=MarkType.Action )

        adder_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Action ) # light Pawn
        # scene.append_arrow( *adder_S( -1,  0, do_advance=False ), mark_type=MarkType.Action ) # light Pawn displacement

        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S( -1,  1, do_advance=False ), mark_type=MarkType.Illegal ) # dark Bishop

        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked ) # dark Wave
        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Action ) # light Pawn
        # scene.append_arrow( *adder_S(  0,  1 ), mark_type=MarkType.Action ) # light Pawn displacement

        scene.append_field_marker( *start_S, mark_type=MarkType.Illegal )

        return scene

    def scn_tr_29_pawn_sacrifice_end( self, bt=BoardType.TamoanchanRevisited ):

        scene = Scene( 'scn_tr_29_pawn_sacrifice_end', bt, width=8.3, height=12.3 )

        prev_S = (1, 3)
        prev_A = (1, 1)

        start_S = (7, 7)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_A = prev_A
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # start_P = (1, 1)
        # scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start = GS.get_start
        end = GS.get_end

        adder_S = GS.adder( prev_S, include_prev=True )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Blocked ) # light Pawn
        # scene.append_arrow( *adder_S( -1,  0, do_advance=False ), mark_type=MarkType.Blocked ) # light Pawn displacement

        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Blocked ) # dark Pawn
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked ) # dark Pawn

        scene.append_arrow( *adder_S( 0, 1, do_advance=False ), mark_type=MarkType.Action ) # dark Pawn displaced
        scene.board.set_piece( *end( adder_S( 0, 1, do_advance=False ) ), piece=-PieceType.Pawn )

        # scene.append_arrow( *adder_S( -1,  1, do_advance=False ), mark_type=MarkType.Illegal ) # dark Bishop
        scene.board.set_piece( *end( adder_S( -1,  1, do_advance=False ) ), piece=-PieceType.Bishop )

        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked ) # dark Wave
        scene.board.set_piece( *end( adder_S( 0, 0, do_advance=False ) ), piece=-PieceType.Wave )

        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked ) # dark Pawn
        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Blocked ) # light Pawn

        scene.append_arrow( *adder_S( 0, 1 ), mark_type=MarkType.Action ) # light Pawn displacement
        scene.board.set_piece( *end( adder_S( 0, 0, do_advance=False ) ), piece=PieceType.Pawn ) # light Pawn B
        scene.append_text( "B", *end( adder_S( 0, 0, do_advance=False ) ), corner=Corner.UpperRight, mark_type=MarkType.Action )

        scene.append_field_marker( *prev_S, mark_type=MarkType.Blocked )

        return scene

    def scn_tr_30_pawn_sacrifice_alt_end( self, bt=BoardType.TamoanchanRevisited ):

        scene = Scene( 'scn_tr_30_pawn_sacrifice_alt_end', bt, width=8.3, height=12.3 )

        prev_S = (6, 2)
        prev_A = (1, 3)
        prev_P = (1, 1)

        start_S = prev_A
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        start_A = prev_P
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid ) # A

        # start_P = (1, 1)
        # scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        adder = GS.adder( start_S, include_prev=False )
        adder( 1, 1 ) # empty field
        scene.board.set_piece( *adder( -1,  1 ), piece=PieceType.Pawn ) # light Pawn A
        scene.append_text( "A", *adder( 0, 0 ), corner=Corner.UpperLeft, mark_type=MarkType.Action )

        adder( 1, 1 ) # empty field
        scene.board.set_piece( *adder( -1,  1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder(  1,  1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder( -1,  1, do_advance=False ), piece=-PieceType.Bishop )

        adder( 1,  1 ) # empty field
        scene.board.set_piece( *adder( 1, -1 ), piece=PieceType.Pyramid )
        adder( 1,  1 ) # empty field
        scene.board.set_piece( *adder(  1, -1 ), piece=-PieceType.Pawn )
        scene.board.set_piece( *adder(  1, -1 ), piece=PieceType.Pawn ) # light Pawn B
        scene.append_text( "B", *adder( 0, 0 ), corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        adder_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Action ) # light Pawn
        # scene.append_arrow( *adder_S( -1,  0, do_advance=False ), mark_type=MarkType.Action ) # light Pawn displacement

        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S( -1,  1, do_advance=False ), mark_type=MarkType.Illegal ) # dark Bishop

        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Action ) # light Pyramid B

        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Blocked )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Blocked ) # dark Pawn
        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Blocked ) # light Pawn
        # scene.append_arrow( *adder_S(  0,  1 ), mark_type=MarkType.Blocked ) # light Pawn displacement

        scene.append_field_marker( *start_S, mark_type=MarkType.Illegal )

        return scene

    #
    # Checking opponent's King

    def scn_tr_31_checking_king_pawns( self, bt=BoardType.TamoanchanRevisited ):

        scene = Scene( 'scn_tr_31_checking_king_pawns', bt, x=13.7, y=9.7, width=8.3, height=12.3 )

        start_k = (16, 19)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        scene.board.set_piece( 14, 19, piece=-PieceType.Pawn )
        scene.board.set_piece( 15, 18, piece=-PieceType.Pawn )
        scene.board.set_piece( 16, 18, piece=-PieceType.Pawn )
        scene.board.set_piece( 17, 18, piece=-PieceType.Pawn )
        scene.board.set_piece( 18, 17, piece=-PieceType.Pawn )
        scene.board.set_piece( 19, 17, piece=-PieceType.Pawn )
        scene.board.set_piece( 20, 18, piece=-PieceType.Pawn )
        scene.board.set_piece( 17, 16, piece=-PieceType.Pawn )
        scene.board.set_piece( 19, 19, piece=-PieceType.Pawn )

        start_S = (17, 12)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        adder_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )

        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S( -1,  0, do_advance=False ), mark_type=MarkType.Blocked )

        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S( -1,  0, do_advance=False ), mark_type=MarkType.Blocked )

        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Action ) # dark Pawn
        scene.append_arrow( *adder_S(  1,  0, do_advance=False ), mark_type=MarkType.Blocked )

        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1, -1 ), mark_type=MarkType.Blocked ) # dark King

        return scene

    def scn_tr_32_checking_king_figures( self, bt=BoardType.TamoanchanRevisited ):

        scene = Scene( 'scn_tr_32_checking_king_figures', bt, x=13.7, y=9.7, width=8.3, height=12.3 )

        start_k = (16, 19)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        scene.board.set_piece( 14, 19, piece=-PieceType.Pawn )
        scene.board.set_piece( 15, 18, piece=-PieceType.Scout )
        scene.board.set_piece( 16, 18, piece=-PieceType.Knight )
        scene.board.set_piece( 17, 18, piece=-PieceType.Bishop )
        scene.board.set_piece( 18, 17, piece=-PieceType.Grenadier )
        scene.board.set_piece( 19, 17, piece=-PieceType.Scout )
        scene.board.set_piece( 20, 18, piece=-PieceType.Grenadier )
        scene.board.set_piece( 17, 16, piece=-PieceType.Scout )
        scene.board.set_piece( 19, 19, piece=-PieceType.Pawn )

        start_S = (17, 12)
        scene.board.set_piece( *start_S, piece=PieceType.Serpent )

        adder_S = GS.adder( start_S, include_prev=True )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1, -1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )

        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S(  1,  1 ), mark_type=MarkType.Legal )

        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1, -1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1,  1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_S( -1, -1 ), mark_type=MarkType.Illegal ) # dark King

        # scene.append_text( "N", 19, 16, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        return scene

    #
    # Activating Wave

    def scn_tr_33_serpent_activating_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_33_serpent_activating_wave', bt, width=8, height=8)

        scene.board.set_piece(1, 1, piece=PieceType.Serpent)
        scene.board.set_piece(4, 4, piece=PieceType.Wave)

        scene.append_arrow( 1, 1, 2, 2, mark_type=MarkType.Action )
        scene.append_arrow( 2, 2, 1, 3, mark_type=MarkType.Action )
        scene.append_arrow( 1, 3, 2, 4, mark_type=MarkType.Action )
        scene.append_arrow( 2, 4, 3, 3, mark_type=MarkType.Action )
        scene.append_arrow( 3, 3, 4, 4, mark_type=MarkType.Action )

        return scene

    def scn_tr_34_serpent_activated_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_34_serpent_activated_wave', bt, width=8, height=8)

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

    def scn_tr_35_serpent_activated_wave_step_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_35_serpent_activated_wave_step_1', bt, width=8, height=8)

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

    def scn_tr_36_serpent_activated_wave_ply(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_36_serpent_activated_wave_ply', bt)

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

    #
    # Out-of-board steps

    def scn_tr_37_wave_out_of_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_37_wave_out_of_board', bt, x=4, y=1)

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

    #
    # Teleporting Wave

    def scn_tr_38_off_board_teleport_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_38_off_board_teleport_wave', bt, x=4, y=1)

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

    def scn_tr_39_teleported_wave_on_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_39_teleported_wave_on_board', bt, x=-4, y=-1)

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

    def scn_tr_40_on_board_teleport_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_40_on_board_teleport_wave', bt, x=4, y=1)

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

    def scn_tr_41_teleported_wave_off_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_41_teleported_wave_off_board', bt, x=-4, y=-1)

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
