#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneSimpleMixin:

    #
    # Croatian Ties 14

    def scn_ct14_01_sideways_pawns( self, bt=BoardType.Croatian_14 ):

        scene = Scene( 'scn_ct14_01_sideways_pawns', bt, width=5.3, height=8.3 ) # x=1.0, y=0.0,

        start_P = (2, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_B = (2, 0)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_p = (2, 7)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        adder_P = GS.adder( start_P, include_prev=True )
        scene.append_arrow( *adder_P( -1, 0, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_P( 0, 1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_P( 1, 0, do_advance=False ), mark_type=MarkType.Legal )

        adder_p = GS.adder( start_p, include_prev=True )
        scene.append_arrow( *adder_p( -1,  0, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_p( 0, -1, do_advance=False ), mark_type=MarkType.Legal )
        scene.append_arrow( *adder_p( 1,  0, do_advance=False ), mark_type=MarkType.Legal )

        return scene

    #
    # Classical Chess 14 mirrored

    def scn_c14m_01_castling_init( self, bt=BoardType.Classic_14_Mirrored ):

        scene = Scene( 'scn_c14m_01_castling_init', bt, height=1.3 )

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_K = (2, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_B = (3, 0)
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_R_C = (5, 0)
        scene.board.set_piece( *start_R_C, piece=PieceType.Rook )

        start_R_D = (11, 0)
        scene.board.set_piece( *start_R_D, piece=PieceType.Rook )

        scene.append_text( "K", *start_K, corner=Corner.UpperRight, mark_type=MarkType.Action ) # UpperRightFieldMarker

        # K --> R(A)
        start_K_RA = GS.gen_steps( start=start_K, rels=[ (-1, 0), ], include_prev=False, count=2 )
        for i, coord in enumerate( start_K_RA() ):
            mark_type = MarkType.Legal if i <= 1 else \
                        MarkType.Blocked
            scene.append_text( str( i+1 ), *coord, corner=Corner.UpperRight, mark_type=mark_type ) # UpperRightFieldMarker

        scene.append_text( "1", *start_R_B, corner=Corner.UpperRight, mark_type=MarkType.Legal ) # UpperRightFieldMarker

        scene.append_text( "A", *start_R_A, corner=Corner.LowerLeft, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_R_B, corner=Corner.LowerLeft, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_R_C, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_R_D, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_c14m_02_castled_right_1( self, bt=BoardType.Classic_14_Mirrored ):

        scene = Scene( 'scn_c14m_02_castled_right_1', bt, height=1.3 )

        prev_K = (2, 0)
        prev_R_B = (3, 0)

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_K = prev_R_B
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_B = prev_K
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_R_C = (5, 0)
        scene.board.set_piece( *start_R_C, piece=PieceType.Rook )

        start_R_D = (11, 0)
        scene.board.set_piece( *start_R_D, piece=PieceType.Rook )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked ) # UpperRightFieldMarker

        scene.append_text( "A", *start_R_A, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_R_B, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_R_C, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_R_D, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_c14m_03_castled_left_2( self, bt=BoardType.Classic_14_Mirrored ):

        scene = Scene( 'scn_c14m_03_castled_left_2', bt, height=1.3 )

        prev_K = (2, 0)
        prev_R_A = (0, 0)

        start_R_A = (1, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_K = prev_R_A
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_B = (3, 0)
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_R_C = (5, 0)
        scene.board.set_piece( *start_R_C, piece=PieceType.Rook )

        start_R_D = (11, 0)
        scene.board.set_piece( *start_R_D, piece=PieceType.Rook )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked ) # UpperRightFieldMarker

        scene.append_text( "A", *start_R_A, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_R_B, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_R_C, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_R_D, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_c14m_04_castled_left_1( self, bt=BoardType.Classic_14_Mirrored ):

        scene = Scene( 'scn_c14m_04_castled_left_1', bt, height=1.3 )

        prev_K = (2, 0)
        prev_R_A = (0, 0)

        start_R_A = prev_K
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_K = (1, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_B = (3, 0)
        scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_R_C = (5, 0)
        scene.board.set_piece( *start_R_C, piece=PieceType.Rook )

        start_R_D = (11, 0)
        scene.board.set_piece( *start_R_D, piece=PieceType.Rook )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked ) # UpperRightFieldMarker

        scene.append_text( "A", *start_R_A, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_R_B, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_R_C, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_R_D, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_c14m_05_castling_right_3( self, bt=BoardType.Classic_14_Mirrored ):

        scene = Scene( 'scn_c14m_05_castling_right_3', bt, height=1.3 )

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_K = (2, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_C = (5, 0)
        scene.board.set_piece( *start_R_C, piece=PieceType.Rook )

        start_R_D = (11, 0)
        scene.board.set_piece( *start_R_D, piece=PieceType.Rook )

        scene.append_text( "K", *start_K, corner=Corner.UpperRight, mark_type=MarkType.Action ) # UpperRightFieldMarker

        # K --> R(C)
        start_K_RA = GS.gen_steps( start=start_K, rels=[ (1, 0), ], include_prev=False, count=3 )
        for i, coord in enumerate( start_K_RA() ):
            mark_type = MarkType.Legal if i <= 2 else \
                        MarkType.Blocked
            scene.append_text( str( i+1 ), *coord, corner=Corner.UpperRight, mark_type=mark_type ) # UpperRightFieldMarker

        scene.append_text( "A", *start_R_A, corner=Corner.LowerLeft, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_R_C, corner=Corner.LowerLeft, mark_type=MarkType.Legal )
        scene.append_text( "D", *start_R_D, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_c14m_06_castling_right_9( self, bt=BoardType.Classic_14_Mirrored ):

        scene = Scene( 'scn_c14m_06_castling_right_9', bt, height=1.3 )

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        start_K = (2, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_D = (11, 0)
        scene.board.set_piece( *start_R_D, piece=PieceType.Rook )

        scene.append_text( "K", *start_K, corner=Corner.UpperRight, mark_type=MarkType.Action ) # UpperRightFieldMarker

        # K --> R(C)
        start_K_RA = GS.gen_steps( start=start_K, rels=[ (1, 0), ], include_prev=False, count=9 )
        for i, coord in enumerate( start_K_RA() ):
            mark_type = MarkType.Legal if i <= 9 else \
                        MarkType.Blocked
            scene.append_text( str( i+1 ), *coord, corner=Corner.UpperRight, mark_type=mark_type ) # UpperRightFieldMarker

        scene.append_text( "A", *start_R_A, corner=Corner.LowerLeft, mark_type=MarkType.Legal )
        scene.append_text( "D", *start_R_D, corner=Corner.LowerLeft, mark_type=MarkType.Legal )

        return scene
