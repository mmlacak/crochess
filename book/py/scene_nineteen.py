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

    def scn_n_04_teleport_move_3(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_04_teleport_move_3', bt)

        start_W = (6, 14)
        start_G = (8, 10)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(4, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(5, 6, piece=-PieceType.Pawn)

        scene.board.set_piece(15, 1, piece=PieceType.Pawn)

        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
        scene.board.set_piece(11, 3, piece=PieceType.Pyramid)

        # Pegasus, direction <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start_G, rels=[(-1, 2), ], include_prev=True) )
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
        start_G = (6, 14)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(15, 1, piece=PieceType.Pawn)
        scene.board.set_piece(4, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(5, 6, piece=-PieceType.Pawn)

        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)
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

    def scn_n_06_teleport_wave_blocked(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_06_teleport_wave_blocked', bt)

        start_W = (17, 0)
        start_G = (6, 14)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(4, 15, piece=-PieceType.Pawn)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        gen_coords = GS.gen_steps( start=start_W, rels=[(-2, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen_coords() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked )

            pos = GS.get_end( coords )
            scene.board.set_piece( *pos, piece=-PieceType.Pawn )

        return scene

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

    def scn_n_11_teleport_pawns_init(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_11_teleport_pawns_init', bt)

        start_P1 = (0, 16)
        # start_P2 = (1, 16)
        start_T = (0, 17)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(*start_P1, piece=PieceType.Pawn)
        # scene.board.set_piece(*start_P2, piece=PieceType.Pawn)

        scene.append_arrow( *(start_P1 + start_T), mark_type=MarkType.Action )

        scene.append_text("1", 16, 17, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        scene.append_text("2", 16, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 17, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        # scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        # scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        # scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        scene.append_text("a", 0, 17, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    def scn_n_12_teleport_pawns_step_1(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_12_teleport_pawns_step_1', bt)

        # start_P1 = (16, 16)
        start_P2 = (1, 16)
        start_T = (0, 17)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        # scene.board.set_piece(*start_P1, piece=PieceType.Pawn)
        scene.board.set_piece(*start_P2, piece=PieceType.Pawn)

        scene.append_arrow( *(start_P2 + start_T), mark_type=MarkType.Action )

        # scene.append_text("1", 16, 17, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        # # scene.append_text("2", 16, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        # scene.append_text("3", 17, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # scene.append_field_marker(*start_P1, mark_type=MarkType.Action)

        # scene.append_text("a", 0, 0, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        return scene

    def scn_n_13_teleport_pawns_end(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_13_teleport_pawns_end', bt)

        # start_P1 = (16, 16)
        start_P2 = (1, 1)
        start_T = (0, 17)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        # scene.board.set_piece(*start_P1, piece=PieceType.Pawn)
        scene.board.set_piece(*start_P2, piece=PieceType.Pawn)

        # scene.append_arrow( *(start_P2 + (1, 2)), mark_type=MarkType.Legal )

        gen_coords = GS.gen_steps( start=start_P2, rels=[(0, 1), ], include_prev=True, count=7 )
        for index, coords in enumerate( gen_coords() ):
            mark_type = MarkType.Legal if index == 0 else \
                        MarkType.Blocked
            scene.append_arrow( *coords, mark_type=mark_type )

        # scene.append_field_marker(*start_P1, mark_type=MarkType.Action)

        # scene.append_text("a", 0, 0, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        return scene

    def scn_n_14_teleport_bishop(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_14_teleport_bishop', bt)

        start_B = (3, 14)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(17, 17, piece=PieceType.Star)
        scene.board.set_piece(17, 0, piece=-PieceType.Star)
        scene.board.set_piece(0, 17, piece=-PieceType.Star)

        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        # Bishop, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start_B, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        scene.append_text("1", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("2", 16, 16, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        return scene

    def scn_n_15_pawn_ranks(self, bt=BoardType.Nineteen):

        scene = Scene('scn_n_15_pawn_ranks', bt)

        scene.append_arrow(0.7, 16.5, 17.3, 16.5, mark_type=MarkType.Blocked, start_pointer=True, end_pointer=True)
        scene.append_arrow(0.7, 15.5, 17.3, 15.5, mark_type=MarkType.Illegal, start_pointer=True, end_pointer=True)

        scene.append_arrow(0.7, 2.5, 17.3, 2.5, mark_type=MarkType.Action, start_pointer=True, end_pointer=True)
        scene.append_arrow(0.7, 1.5, 17.3, 1.5, mark_type=MarkType.Legal, start_pointer=True, end_pointer=True)

        for i in range(0, 19):
            scene.append_text(str(i + 1), 0, i, mark_type=MarkType.Blocked)

        return scene
