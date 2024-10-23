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


class SceneNineteenMixin:

    #
    # Portal-fields

    def scn_n_01_portal_fields(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_01_portal_fields', bt)
        rect = (0.10, 0.75, 0.6, 0.15)

        start_T_1 = (0, 0)
        start_T_2 = (17, 17)
        start_T_3 = (17, 0)
        start_T_4 = (0, 17)

        scene.board.set_piece(*start_T_1, piece=PieceType.Star)
        scene.board.set_piece(*start_T_2, piece=PieceType.Star)
        scene.board.set_piece(*start_T_3, piece=-PieceType.Star)
        scene.board.set_piece(*start_T_4, piece=-PieceType.Star)

        #
        # King
        start_K = (8, 8)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        rect_K = (0.35, 0.5, 0.65, 0.1)
        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_K, include_prev=False, count=1) # , bounds=((1, 3), (3, 5)))

        for i, pos in enumerate( gen_abs_pos_K() ):
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked, rect=rect_K)

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_1, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board(*pos):
                scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=rect)

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_2, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board(*pos):
                scene.append_text(str(i+1), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Action, rect=rect)

        #
        # Star 3
        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_3, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_3() ):
            if scene.board.is_on_board(*pos):
                scene.append_text(str(i+1), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Legal, rect=rect)

        #
        # Star 4
        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_4, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_4() ):
            if scene.board.is_on_board(*pos):
                scene.append_text(str(i+1), *pos, corner=Corner.LowerRight, mark_type=MarkType.Legal, rect=rect)

        return scene

    #
    # Teleporting pieces

    def scn_n_02_teleport_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_02_teleport_init', bt)
        rect = (0.10, 0.75, 0.6, 0.15)

        start_B = (3, 14)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W = (0, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_T_1 = (0, 0)
        start_T_2 = (17, 17)
        start_T_3 = (17, 0)
        start_T_4 = (0, 17)

        scene.board.set_piece( *start_T_1, piece=PieceType.Star )
        scene.board.set_piece( *start_T_2, piece=PieceType.Star )
        scene.board.set_piece( *start_T_3, piece=-PieceType.Star )
        scene.board.set_piece( *start_T_4, piece=-PieceType.Star )

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps( start=start_B, rels=[(-1, 1), ], include_prev=True ) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_1, include_prev=False, count=1 )

        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board( *pos ):
                scene.append_text( str( i+1 ), *pos, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=rect )

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_2, include_prev=False, count=1 )

        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board( *pos ):
                scene.append_text( str( i+1 ), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Action, rect=rect )

        return scene

    #
    # Teleportation blocked

    def scn_n_03_teleport_move_2(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_03_teleport_move_2', bt)
        rect = (0.10, 0.75, 0.75, 0.15)

        start_T_1 = (0, 0)
        start_T_2 = (17, 17)
        start_T_3 = (17, 0)
        start_T_4 = (0, 17)

        scene.board.set_piece( *start_T_1, piece=PieceType.Star )
        scene.board.set_piece( *start_T_2, piece=PieceType.Star )
        scene.board.set_piece( *start_T_3, piece=-PieceType.Star )
        scene.board.set_piece( *start_T_4, piece=-PieceType.Star )

        scene.board.set_piece(0, 1, piece=-PieceType.Wave)
        scene.board.set_piece(1, 1, piece=PieceType.Pawn)
        scene.board.set_piece(1, 0, piece=PieceType.Rook)

        scene.board.set_piece(17, 16, piece=-PieceType.Pawn)
        scene.board.set_piece(16, 16, piece=-PieceType.Pawn)

        scene.board.set_piece(16, 17, piece=PieceType.Bishop)

        start_R = (0, 12)
        scene.board.set_piece(*start_R, piece=-PieceType.Rook)

        # Rook, direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start_R, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_1, include_prev=False, count=1 )

        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board( *pos ):
                scene.append_text( str( i+1 ), *pos, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=rect )

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_T_2, include_prev=False, count=1 )

        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board( *pos ):
                scene.append_text( str( i+1 ), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Action, rect=rect )

        return scene

    #
    # Teleporting Wave

    def scn_n_04_teleport_move_3(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_04_teleport_move_3', bt)

        start_W = (6, 14)
        start_E = (8, 10)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(4, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(5, 6, piece=-PieceType.Pawn)

        scene.board.set_piece(15, 1, piece=PieceType.Pawn)

        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)
        scene.board.set_piece(11, 3, piece=PieceType.Pyramid)

        # Pegasus, direction <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start_E, rels=[(-1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # Wave, direction <-2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start_W, rels=[(-2, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        return scene

    def scn_n_05_teleport_end(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_05_teleport_end', bt)

        start_W = (17, 0)
        start_E = (6, 14)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(15, 1, piece=PieceType.Pawn)
        scene.board.set_piece(4, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(5, 6, piece=-PieceType.Pawn)

        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)
        scene.board.set_piece(11, 3, piece=PieceType.Pyramid)

        gen_coords = GS.gen_steps(start=start_W, rels=[(-2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits())
        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Legal
            if index in [0, 2]:
                mark_type = MarkType.Action
            elif index == 5:
                mark_type = MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Teleporting Wave blocked

    def scn_n_06_teleport_wave_blocked(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_06_teleport_wave_blocked', bt)

        start_W = (17, 0)
        start_E = (6, 14)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(4, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(*start_E, piece=PieceType.Pegasus)

        gen_coords = GS.gen_steps( start=start_W, rels=[(-2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen_coords() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

            pos = GS.get_end( coords )
            scene.board.set_piece( *pos, piece=-PieceType.Pawn )

        return scene

    #
    # Teleporting off-board

    def scn_n_07_teleport_wave_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_07_teleport_wave_init', bt, x=4, y=1, reverse_off_board_field_colors=True)

        start_T = (17, 17) # (13, 16)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        start = (16, 7) # (12, 6)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        start_U = (12, 6) # (8, 5)
        scene.board.set_piece(*start_U, piece=PieceType.Unicorn)

        #
        # Wave activation
        scene.append_arrow( *(start_U + start), mark_type=MarkType.Action ) # short

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

        scene.append_arrow( *coords() ) # short

        # scene.append_text("1", 12, 13, corner=Corner.UpperLeft)
        scene.append_text("1", 16, 14, corner=Corner.UpperLeft)
        scene.append_text("2", *start_T, corner=Corner.UpperLeft)

        return scene

    def scn_n_08_teleport_wave_end(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_08_teleport_wave_end', bt, x=-4, y=-1, reverse_off_board_field_colors=True)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        scene.board.set_piece(4, 5, piece=PieceType.Pyramid)
        scene.board.set_piece(4, 12, piece=-PieceType.Pawn)

        start = start_T

        #
        # long --> (3, 2) direction
        # short --> (-2, 1) direction

        gen_coords = GS.gen_steps(start=start, rels=[(3, 2), (-2, 1), ], include_prev=True, bounds=((0, 0), (19, 19)))

        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Legal
            if index == 2:
                mark_type = MarkType.Action
            elif index == 7:
                mark_type = MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Emerging off-board

    def scn_n_09_teleport_wave_2_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_09_teleport_wave_2_init', bt, x=4, y=1, reverse_off_board_field_colors=True)

        start_T = (17, 17) # (13, 16)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        start = (14, 8) # (10, 7)
        scene.board.set_piece(*start, piece=-PieceType.Wave)

        start_U = (10, 7) # (6, 6)
        scene.board.set_piece(*start_U, piece=-PieceType.Unicorn)

        #
        # Wave activation
        scene.append_arrow( *(start_U + start), mark_type=MarkType.Action ) # short

        #
        # short --> (-2, 1) direction
        # long --> (3, 2) direction

        gen_coords = GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True, count=6)

        for coords in gen_coords():
            scene.append_arrow( *coords )

        return scene

    def scn_n_10_teleport_wave_2_end(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_10_teleport_wave_2_end', bt, x=-4, y=-1, reverse_off_board_field_colors=True)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        scene.board.set_piece(3, 9, piece=PieceType.Pyramid)
        scene.board.set_piece(4, 12, piece=-PieceType.Pawn)

        start = start_T

        #
        # long --> (3, 2) direction
        # short --> (-2, 1) direction

        gen_coords = GS.gen_steps(start=start, rels=[(-2, 1), (3, 2), ], include_prev=True, bounds=((-4, -1), (15, 17)))

        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Legal
            if index in [0, 2]:
                mark_type = MarkType.Illegal
            elif index == 5:
                mark_type = MarkType.Blocked
            elif index == 7:
                mark_type = MarkType.Action
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Teleporting Pawn

    def scn_n_11_teleport_pawns_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_11_teleport_pawns_init', bt)

        start_P1 = (0, 16)
        start_T = (0, 17)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(*start_P1, piece=PieceType.Pawn)

        scene.append_arrow( *(start_P1 + start_T), mark_type=MarkType.Action )

        scene.append_text("1", 16, 17, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        scene.append_text("2", 16, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 17, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("a", 0, 17, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    def scn_n_12_teleport_pawns_step_1(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_12_teleport_pawns_step_1', bt)

        start_P2 = (1, 16)
        start_T = (0, 17)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(*start_P2, piece=PieceType.Pawn)

        scene.append_arrow( *(start_P2 + start_T), mark_type=MarkType.Action )

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    def scn_n_13_teleport_pawns_end(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_13_teleport_pawns_end', bt)

        start_P2 = (1, 1)
        start_T = (0, 17)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(*start_P2, piece=PieceType.Pawn)

        gen_coords = GS.gen_steps( start=start_P2, rels=[(0, 1), ], include_prev=True, count=7 )
        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Legal if index == 0 else \
                        MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Teleporting Bishop

    def scn_n_14_teleport_bishop(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_14_teleport_bishop', bt)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        start_B = (3, 14)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start_B, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        scene.append_text("1", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("2", 16, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        return scene

    #
    # King cannot teleport

    def scn_n_15_king_cannot_teleport( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_15_king_cannot_teleport', bt )

        start_K = (1, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        # fixed set
        start_T_A = (0, 0)
        scene.board.set_piece( *start_T_A, piece=PieceType.Star )
        scene.board.set_piece( 17, 17, piece=PieceType.Star )
        scene.board.set_piece( 17, 0, piece=-PieceType.Star )
        scene.board.set_piece( 0, 17, piece=-PieceType.Star )

        scene.append_arrow( *( start_K + start_T_A ), mark_type=MarkType.Illegal )

        return scene

    #
    # Sideways Pawns

    def scn_n_16_sideways_pawn_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_16_sideways_pawn_init', bt)

        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W_D = (6, 3)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        # right
        start_A_A = (11, 3)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_w = (15, 3)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # forward
        start_A_B = (6, 7)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        start_N = (6, 10)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # diagonal, forward-right
        start_A_C = (12, 9)
        scene.board.set_piece( *start_A_C, piece=PieceType.Pyramid )

        start_W_E = (16, 13) # (6, 14)
        scene.board.set_piece( *start_W_E, piece=PieceType.Wave )

        # unreachable
        start_B = (10, 15)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        scene.append_arrow( *( GS.append_pos_rel( start_P, -1, 0 ) ) )
        scene.append_arrow( *( start_P + start_W_D ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_A_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_A_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_A_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "E", *start_W_E, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_n_17_sideways_pawn_activated_wave(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_17_sideways_pawn_activated_wave', bt)

        prev_W_D = (6, 3)
        start_P = prev_W_D
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        # right
        start_A_A = (11, 3)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_w = (15, 3)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # forward
        start_A_B = (6, 7)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        start_N = (6, 10)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # diagonal, forward-right
        start_A_C = (12, 9)
        scene.board.set_piece( *start_A_C, piece=PieceType.Pyramid )

        start_W_E = (16, 13) # (6, 14)
        scene.board.set_piece( *start_W_E, piece=PieceType.Wave )

        # unreachable
        start_B = (10, 15)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # W --> (left)
        coords_Wl_ = GS.gen_steps( start=prev_W_D, rels=[(-1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wl_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W --> (forward, left)
        coords_Wfl_ = GS.gen_steps( start=prev_W_D, rels=[(-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wfl_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W --> (forward)
        coords_Wf_ = GS.gen_steps( start=prev_W_D, rels=[(0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wf_() ):
            mark_type = MarkType.Blocked if i == 3 else \
                        MarkType.Action if i == 6 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (forward, right)
        coords_Wfr_ = GS.gen_steps( start=prev_W_D, rels=[(1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wfr_() ):
            mark_type = MarkType.Blocked if i == 5 else \
                        MarkType.Action if i == 9 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (right)
        coords_Wr_ = GS.gen_steps( start=prev_W_D, rels=[(1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wr_() ):
            mark_type = MarkType.Blocked if i == 4 else \
                        MarkType.Action if i == 8 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> *
        start_ = (14, 11)

        # W --> * --> (forward)
        coords_Wf_ = GS.gen_steps( start=start_, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_Wf_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        # W --> * --> (forward, left)
        coords_Wfl_ = GS.gen_steps( start=start_, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_Wfl_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_A_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_A_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_A_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        # scene.append_text( "D", *start_W_D, corner=Corner.UpperLeft, mark_type=MarkType.Action )
        scene.append_text( "E", *start_W_E, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        scene.append_text( "F", *start_, corner=Corner.LowerRight, mark_type=MarkType.Illegal )

        return scene

    #
    # Activating Wave

    def scn_n_18_capture_pawn_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_18_capture_pawn_init', bt)

        start_W_D = (6, 3)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_P = (7, 2)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        # right
        start_A_A = (11, 3)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_w = (15, 3)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # forward
        start_A_B = (6, 7)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        start_N = (6, 10)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # diagonal, forward-right
        start_A_C = (12, 9)
        scene.board.set_piece( *start_A_C, piece=PieceType.Pyramid )

        start_W_E = (16, 13) # (6, 14)
        scene.board.set_piece( *start_W_E, piece=PieceType.Wave )

        # unreachable
        start_B = (10, 15)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        scene.append_arrow( *( start_P + start_W_D ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_A_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_A_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_A_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "E", *start_W_E, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_n_19_capture_pawn_activated_wave(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_19_capture_pawn_activated_wave', bt)

        prev_W_D = (6, 3)
        start_P = prev_W_D
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        # right
        start_A_A = (11, 3)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_w = (15, 3)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # forward
        start_A_B = (6, 7)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        start_N = (6, 10)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # diagonal, forward-right
        start_A_C = (12, 9)
        scene.board.set_piece( *start_A_C, piece=PieceType.Pyramid )

        start_W_E = (16, 13) # (6, 14)
        scene.board.set_piece( *start_W_E, piece=PieceType.Wave )

        # unreachable
        start_B = (10, 15)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # W --> (left)
        coords_Wl_ = GS.gen_steps( start=prev_W_D, rels=[(-1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wl_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W --> (forward, left)
        coords_Wfl_ = GS.gen_steps( start=prev_W_D, rels=[(-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wfl_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W --> (forward)
        coords_Wf_ = GS.gen_steps( start=prev_W_D, rels=[(0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wf_() ):
            mark_type = MarkType.Blocked if i == 3 else \
                        MarkType.Action if i == 6 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (forward, right)
        coords_Wfr_ = GS.gen_steps( start=prev_W_D, rels=[(1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wfr_() ):
            mark_type = MarkType.Action if i in [5, 9] else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (right)
        coords_Wr_ = GS.gen_steps( start=prev_W_D, rels=[(1, 0), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Wr_() ):
            mark_type = MarkType.Blocked if i == 4 else \
                        MarkType.Action if i == 8 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> *
        start_ = (14, 11)

        # W --> * --> (forward)
        coords_Wf_ = GS.gen_steps( start=start_, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_Wf_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        # W --> * --> (forward, left)
        coords_Wfl_ = GS.gen_steps( start=start_, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_Wfl_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_A_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_A_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_A_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        # scene.append_text( "D", *start_W_D, corner=Corner.UpperLeft, mark_type=MarkType.Action )
        scene.append_text( "E", *start_W_E, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        scene.append_text( "F", *start_, corner=Corner.LowerRight, mark_type=MarkType.Illegal )

        return scene

    #
    # Activating Pyramid

    def scn_n_18_sideways_pawn_does_not_activate_pyramid(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_18_sideways_pawn_does_not_activate_pyramid', bt)

        # top, dark pieces

        start_p = (5, 13)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_a = (6, 13)
        scene.board.set_piece( *start_a, piece=-PieceType.Pyramid )

        scene.append_arrow( *( start_p + start_a ), mark_type=MarkType.Illegal )

        # bottom, light pieces

        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W_A = (6, 3)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (8, 3)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_A = (10, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        scene.append_arrow( *( start_P + start_W_A ), mark_type=MarkType.Action )

        gen_WA_WB = GS.gen_steps( start=start_W_A, rels=[(1, 0), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_WA_WB() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_WB_A = GS.gen_steps( start=start_W_B, rels=[(1, 0), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_WB_A() ):
            mark_type = MarkType.Illegal if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_n_19_sideways_pawns_cascade_pyramids(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_19_sideways_pawns_cascade_pyramids', bt)

        # top, dark pieces

        start_r = (16, 15)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_w_A = (5, 15)
        scene.board.set_piece( *start_w_A, piece=-PieceType.Wave )

        start_p = (5, 13)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w_B = (6, 13)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_b = (11, 13)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_w_C = (13, 11)
        scene.board.set_piece( *start_w_C, piece=-PieceType.Wave )

        start_a = (15, 13)
        scene.board.set_piece( *start_a, piece=-PieceType.Pyramid )

        gen_r_wA = GS.gen_steps( start=start_r, rels=[(-1, 0), ], include_prev=True, count=11 )
        for index, coords in enumerate( gen_r_wA() ):
            mark_type = MarkType.Action if index == 10 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_wA_p = GS.gen_steps( start=start_w_A, rels=[(0, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_wA_p() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_arrow( *( start_p + start_w_B ), mark_type=MarkType.Action )

        gen_wB_b = GS.gen_steps( start=start_w_B, rels=[(1, 0), ], include_prev=True, count=5 )
        for index, coords in enumerate( gen_wB_b() ):
            mark_type = MarkType.Action if index == 4 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_b_wC = GS.gen_steps( start=start_b, rels=[(1, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_b_wC() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_wC_a = GS.gen_steps( start=start_w_C, rels=[(1, 1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_wC_a() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        # bottom, light pieces

        start_R = (16, 5)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W_A = (5, 5)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W_B = (6, 3)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_B = (11, 3)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (13, 1)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen_R_WA = GS.gen_steps( start=start_R, rels=[(-1, 0), ], include_prev=True, count=11 )
        for index, coords in enumerate( gen_R_WA() ):
            mark_type = MarkType.Action if index == 10 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_WA_P = GS.gen_steps( start=start_W_A, rels=[(0, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_WA_P() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_arrow( *( start_P + start_W_B ), mark_type=MarkType.Action )

        gen_WB_B = GS.gen_steps( start=start_W_B, rels=[(1, 0), ], include_prev=True, count=5 )
        for index, coords in enumerate( gen_WB_B() ):
            mark_type = MarkType.Action if index == 4 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_B_A = GS.gen_steps( start=start_B, rels=[(1, -1), ], include_prev=True, count=2 )
        for index, coords in enumerate( gen_B_A() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Pawn ranks, rows

    def scn_n_21_pawn_ranks(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_21_pawn_ranks', bt)

        scene.append_arrow(0.7, 16.5, 17.3, 16.5, mark_type=MarkType.Blocked, start_pointer=True, end_pointer=True)
        scene.append_arrow(0.7, 15.5, 17.3, 15.5, mark_type=MarkType.Illegal, start_pointer=True, end_pointer=True)

        scene.append_arrow(0.7, 2.5, 17.3, 2.5, mark_type=MarkType.Action, start_pointer=True, end_pointer=True)
        scene.append_arrow(0.7, 1.5, 17.3, 1.5, mark_type=MarkType.Legal, start_pointer=True, end_pointer=True)

        for i in range(0, 19):
            scene.append_text(str(i + 1), 0, i, mark_type=MarkType.Blocked)

        return scene

    #
    # Promotion, only one Queen

    def scn_n_22_only_one_queen(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_22_only_one_queen', bt)

        start_Q = (12, 15)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_B = (3, 11)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (11, 3)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        start_q = (7, 3)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        gen_B_A = GS.gen_steps( start=start_B, rels=[(1, -1), ], include_prev=True, count=8 )
        for index, coords in enumerate( gen_B_A() ):
            mark_type = MarkType.Action if index == 7 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen_A_q = GS.gen_steps( start=start_A, rels=[(-1, 0), ], include_prev=True, count=4 )
        for index, coords in enumerate( gen_A_q() ):
            mark_type = MarkType.Action if index == 3 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    #
    # Castling

    def scn_n_23_new_castling_init( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_23_new_castling_init', bt, height=2.3 )

        start_K = (9, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R = (1, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_n = (7, 1)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        for i in range( 7, 2, -1 ):
            mark_type = MarkType.Illegal if i == 5 else \
                        MarkType.Legal
            scene.append_text( str(8 - i), i, 0, corner=Corner.UpperLeft, mark_type=mark_type )

        scene.append_text( "K", 9, 0, corner=Corner.UpperRight, mark_type=MarkType.Illegal )
        # scene.append_text( "B", 5, 0, corner=Corner.UpperRight, mark_type=MarkType.Illegal )

        return scene

    def scn_n_24_new_castling_end( self, bt=BoardType.Nineteen ):

        scene = Scene( 'scn_n_24_new_castling_end', bt, height=2.3 )

        start_K = (4, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R = (5, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_n = (7, 1)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        for i in range( 7, 2, -1 ):
            mark_type = MarkType.Illegal if i == 5 else \
                        MarkType.Legal
            scene.append_text( str(8 - i), i, 0, corner=Corner.UpperLeft, mark_type=mark_type )

        scene.append_text( "K", 9, 0, corner=Corner.UpperRight, mark_type=MarkType.Illegal )
        # scene.append_text( "B", 5, 0, corner=Corner.UpperRight, mark_type=MarkType.Illegal )

        return scene
