#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from util import in_range
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
    # Static piece is legal

    def scn_tr_08_static_piece_is_legal(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_08_static_piece_is_legal', bt, width=8, height=8)

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
        scene.append_arrow( *( pos_3 + start_S ), mark_type=MarkType.Action )

        scene.append_arrow( *( start_S + end_W ), mark_type=MarkType.Action )

        scene.append_text( "1", *pos_1, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "2", *pos_2, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "3", *pos_3, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)

        scene.append_text( "S", *start_S, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "W", *end_W, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        return scene

    #
    # Static loop is legal

    def scn_tr_9_static_loop_is_legal(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_9_static_loop_is_legal', bt, width=8, height=8)

        start_S = (3, 3)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        pos_1 = (2, 4)
        pos_2 = (3, 5)
        pos_3 = (4, 4)
        pos_5 = (4, 2)

        scene.append_arrow( *( start_S + pos_1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_1 + pos_2 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_2 + pos_3 ), mark_type=MarkType.Legal )
        scene.append_arrow( *( pos_3 + start_S ), mark_type=MarkType.Illegal )
        scene.append_arrow( *( start_S + pos_5 ), mark_type=MarkType.Legal )

        scene.append_text( "1", *pos_1, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "2", *pos_2, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "3", *pos_3, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
        scene.append_text( "5", *pos_5, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)

        scene.append_text( "S", *start_S, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        return scene

    #
    # Revisiting fields, loops

    def scn_tr_10_serpent_loop_init(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_10_serpent_loop_init', bt, width=8, height=8)

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

        return scene

    def scn_tr_11_serpent_loop_unwinded(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_11_serpent_loop_unwinded', bt, width=8, height=8)

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

    #
    # Color-changing move

    def scn_tr_11_serpent_neighbors(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_11_serpent_neighbors', bt, width=8, height=8)

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

    def scn_tr_12_cascade_serpent_neighbors(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_12_cascade_serpent_neighbors', bt, width=8, height=8)

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
    # Displacements while moving

    def scn_tr_13_displacement_init(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_13_displacement_init', bt, width=8, height=8)

        start_S = (1, 3)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        start_p = (4, 4)
        scene.board.set_piece(*start_p, piece=-PieceType.Pawn)

        start_w = (4, 3)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        gen_S_ = GS.gen_steps( start=start_S, rels=[ (1, 1), (1, -1), ], include_prev=True, count=3 )
        for index, coords in enumerate( gen_S_() ):
            mark_type = MarkType.Action if index == 2 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_tr_14_displacement_step(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_14_displacement_step', bt, width=8, height=8)

        prev_p = (4, 4)
        start_p = (4, 4)
        # scene.board.set_piece(*start_p, piece=-PieceType.Pawn)

        start_w = (4, 3)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        prev_S = (1, 3)
        start_S = prev_p
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        scene.append_arrow( *GS.append_tpl_rel( start_p, 1, 0 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_tpl_rel( start_p, 0, 1 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_tpl_rel( start_p, -1, 0 ), mark_type=MarkType.Legal )
        scene.append_arrow( *GS.append_tpl_rel( start_p, 0, -1 ), mark_type=MarkType.Illegal )

        scene.append_text( "S", *prev_S, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )
        scene.append_text( "P", *prev_p, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )

        scene.append_field_marker( *GS.add_tpl( start_p, 1, 0 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_tpl( start_p, 0, 1 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_tpl( start_p, -1, 0 ), mark_type=MarkType.Legal )
        scene.append_field_marker( *GS.add_tpl( start_p, 0, -1 ), mark_type=MarkType.Illegal )

        return scene

    def scn_tr_15_displacement_end(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_15_displacement_end', bt, width=8, height=8)

        prev_p = (4, 4)
        start_p = (4, 5)
        scene.board.set_piece(*start_p, piece=-PieceType.Pawn)

        start_w = (4, 3)
        scene.board.set_piece(*start_w, piece=-PieceType.Wave)

        prev_S = (1, 3)
        start_S = prev_p
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        gen_S_ = GS.gen_steps( start=prev_S, rels=[ (1, 1), (1, -1), ], include_prev=True, count=6 )
        for index, coords in enumerate( gen_S_() ):
            mark_type = MarkType.Blocked if index <= 2 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_text( "S", *prev_S, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )
        scene.append_text( "P", *prev_p, mark_type=MarkType.Blocked, corner=Corner.UpperLeft )

        return scene

    #
    # Out-of-board steps

    def scn_tr_16_serpent_out_of_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_16_serpent_out_of_board', bt, x=4, y=1)

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

    def scn_tr_17_teleport_serpent_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_17_teleport_serpent_1', bt)

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

    def scn_tr_18_teleport_serpent_2(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_18_teleport_serpent_2', bt)

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

    def scn_tr_19_pawn_sacrifice_init(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_19_pawn_sacrifice_init', bt)

        start_S = (17, 13)
        start_A = (12, 14)
        start_P = (12, 10)

        scene.board.set_piece(*start_S, piece=PieceType.Serpent)
        scene.board.set_piece(*start_A, piece=PieceType.Pyramid)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        adder = GS.adder(start_A, include_prev=False)
        adder(1, 1) # empty field
        scene.board.set_piece(*adder(-1, 1), piece=-PieceType.Pawn)
        scene.board.set_piece(*adder(1, 1), piece=-PieceType.Bishop)
        scene.board.set_piece(*adder(-1, 1), piece=-PieceType.Pawn)

        for i in range(8, 22):
            if i > 8:
                scene.board.set_piece(i, 20, piece=-PieceType.Pawn)
            if i not in [10, 12]:
                scene.board.set_piece(i, 19, piece=-PieceType.Pawn)

        coords = GS.gen_steps(start=start_S, rels=[(-1, 1), (-1, -1), ], include_prev=True, count=5)
        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Action if index == 4 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        coords2 = GS.gen_steps(start=start_A, rels=[(0, -1), ], include_prev=True, count=4)
        for index, coord in enumerate( coords2() ):
            mark_type = MarkType.Action if index == 3 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        return scene

    def scn_tr_20_pawn_sacrifice_end(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_20_pawn_sacrifice_end', bt)

        start_S = (12, 14)
        start_A = (12, 10)

        scene.board.set_piece(*start_S, piece=PieceType.Serpent)
        scene.board.set_piece(*start_A, piece=PieceType.Pyramid)

        scene.append_field_marker( *start_S, mark_type=MarkType.Illegal )

        adder = GS.adder(start_S, include_prev=False)
        adder(1, 1) # empty field
        scene.board.set_piece(*adder(-1, 1), piece=-PieceType.Pawn)
        scene.board.set_piece(*adder(1, 1), piece=-PieceType.Bishop)
        scene.board.set_piece(*adder(-1, 1), piece=-PieceType.Pawn)

        for i in range(8, 22):
            if i > 8:
                scene.board.set_piece(i, 20, piece=-PieceType.Pawn)
            if i not in [10, 12]:
                scene.board.set_piece(i, 19, piece=-PieceType.Pawn)

        adder_2 = GS.adder(start_S, include_prev=True)
        scene.append_arrow( *adder_2(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_2(1, 1), mark_type=MarkType.Action )
        scene.append_arrow( *adder_2(-1, 1), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_2(1, 1), mark_type=MarkType.Action )
        scene.append_arrow( *adder_2(1, -1, do_advance=False), mark_type=MarkType.Illegal )

        scene.append_arrow( *adder_2(-1, 1), mark_type=MarkType.Action )
        scene.append_arrow( *adder_2(1, 1), mark_type=MarkType.Action )
        scene.append_arrow( *adder_2(1, -1), mark_type=MarkType.Action )
        scene.append_arrow( *adder_2(1, 1), mark_type=MarkType.Action )

        return scene

    #
    # Activating Wave

    def scn_tr_21_serpent_activating_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_21_serpent_activating_wave', bt, width=8, height=8)

        scene.board.set_piece(1, 1, piece=PieceType.Serpent)
        scene.board.set_piece(4, 4, piece=PieceType.Wave)

        scene.append_arrow( 1, 1, 2, 2, mark_type=MarkType.Action )
        scene.append_arrow( 2, 2, 1, 3, mark_type=MarkType.Action )
        scene.append_arrow( 1, 3, 2, 4, mark_type=MarkType.Action )
        scene.append_arrow( 2, 4, 3, 3, mark_type=MarkType.Action )
        scene.append_arrow( 3, 3, 4, 4, mark_type=MarkType.Action )

        return scene

    def scn_tr_22_serpent_activated_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_22_serpent_activated_wave', bt, width=8, height=8)

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

    def scn_tr_23_serpent_activated_wave_step_1(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_23_serpent_activated_wave_step_1', bt, width=8, height=8)

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

    def scn_tr_24_serpent_activated_wave_ply(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_24_serpent_activated_wave_ply', bt)

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

    def scn_tr_25_wave_out_of_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_25_wave_out_of_board', bt, x=4, y=1)

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

    def scn_tr_26_off_board_teleport_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_26_off_board_teleport_wave', bt, x=4, y=1)

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

    def scn_tr_27_teleported_wave_on_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_27_teleported_wave_on_board', bt, x=-4, y=-1)

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

    def scn_tr_28_on_board_teleport_wave(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_28_on_board_teleport_wave', bt, x=4, y=1)

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

    def scn_tr_29_teleported_wave_off_board(self, bt=BoardType.TamoanchanRevisited):

        scene = Scene('scn_tr_29_teleported_wave_off_board', bt, x=-4, y=-1)

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
