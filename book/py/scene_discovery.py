#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneDiscoveryMixin:

    def scn_d_01_knight_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_01_knight_steps', bt, width=7, height=7)

        start_N = (3, 3)
        scene.board.set_piece(*start_N, piece=-PieceType.Knight)

        scene.append_arrow( 3, 3, 3+0.5, 4+0.5, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False ) # up
        scene.append_arrow( *GS.add_to_all( (3, 4, 2, 5), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # up left
        scene.append_arrow( *GS.add_to_all( (3, 4, 4, 5), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up right

        scene.append_text("L", 2, 5, corner=Corner.UpperLeft, mark_type=MarkType.Legal)
        scene.append_text("R", 4, 5, corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_d_02_monolith_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_02_monolith_steps', bt, width=7, height=7)

        start_M = (3, 3)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        #
        # left steps

        coords = GS.gen_multi_steps(GS.DEFAULT_MONOLITH_MULTI_REL_LEFT_MOVES, start=start_M, include_prev=False, count=1)

        for index, pos in enumerate( coords() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text("L", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal)

        #
        # right steps

        coords = GS.gen_multi_steps(GS.DEFAULT_MONOLITH_MULTI_REL_RIGHT_MOVES, start=start_M, include_prev=False, count=1)

        for index, pos in enumerate( coords() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text("R", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action)

        return scene

    def scn_d_03_monolith_step_1(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_03_monolith_step_1', bt, width=7, height=7)

        start_M = (3, 3)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        end = (4, 5)
        scene.append_text("R", *end, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        step = start_M + end
        scene.append_arrow( *step, mark_type=MarkType.Action )

        return scene

    def scn_d_04_monolith_step_2(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_04_monolith_step_2', bt)

        start = (3, 3)
        start_M = (4, 5) # rel == (1, 2) --> right step
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_W = (3, 7)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(5, 8, piece=PieceType.Bishop)
        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)

        scene.append_text("S", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action)
        scene.append_arrow( *(start + start_M), mark_type=MarkType.Action )

        #
        # left steps

        coords = GS.gen_multi_steps(GS.DEFAULT_MONOLITH_MULTI_REL_LEFT_MOVES, start=start_M, include_prev=True, count=1)

        for index, pos in enumerate( coords() ):
            mark_type = MarkType.Blocked if index == 1 else MarkType.Legal
            scene.append_field_marker(*pos[ 2 : ], mark_type=mark_type)
            # scene.append_text("L", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)
            scene.append_arrow( *pos, mark_type=mark_type )

        return scene

    def scn_d_05_monolith_step_3(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_05_monolith_step_3', bt)

        start = (3, 3)
        start_2 = (4, 5) # rel == (1, 2) --> right step
        start_M = (6, 6) # rel == (2, 1) --> left step
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        start_W = (3, 7)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(5, 8, piece=PieceType.Bishop)
        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)

        scene.append_text("S", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action)
        scene.append_arrow( *(start + start_2), mark_type=MarkType.Action )
        scene.append_arrow( *(start_2 + start_M), mark_type=MarkType.Legal )

        #
        # left steps

        coords = GS.gen_multi_steps(GS.DEFAULT_MONOLITH_MULTI_REL_RIGHT_MOVES, start=start_M, include_prev=True, count=1)

        for index, pos in enumerate( coords() ):
            scene.append_field_marker(*pos[ 2 : ], mark_type=MarkType.Action)
            # scene.append_text("L", *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal)
            scene.append_arrow( *pos, mark_type=MarkType.Action )

        return scene

    def scn_d_06_teleport_via_monolith(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_06_teleport_via_monolith', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.board.set_piece(0, 7, piece=PieceType.Wave)

        #
        # Monolith 1
        start_M1 = (1, 6)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        #
        # Bishop
        start_B = (4, 3)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        coords = GS.gen_steps([(-1, 1), ], start=start_B, include_prev=True, count=3)
        for index, pos in enumerate( coords() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # King
        start_K = (2, 7)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        scene.append_arrow( *(start_K + start_M1), mark_type=MarkType.Illegal )

        #
        # Monolith 2
        start_M2 = (22, 17)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M2, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT1, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT2, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        #
        # Star 3
        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT3, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_3() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Legal)

        #
        # Star 4
        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT4, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_4() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_07_teleport_via_star(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_07_teleport_via_star', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.board.set_piece(0, 7, piece=PieceType.Wave)
        scene.board.set_piece(2, 7, piece=PieceType.King)

        #
        # Bishop
        start_B = (4, 19)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        coords = GS.gen_steps([(-1, 1), ], start=start_B, include_prev=True, count=4)
        for index, pos in enumerate( coords() ):
            mark_type = MarkType.Action if index == 3 else MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # Monolith 1
        start_M1 = (1, 6)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M1, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            if i not in [1, 3]:
                scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Monolith 2
        start_M2 = (22, 17)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M2, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 1
        gen_abs_pos_1 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT1, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_1() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        #
        # Star 2
        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=startT2, include_prev=False, count=1)

        j = 0
        for i, pos in enumerate( gen_abs_pos_2() ):
            if scene.board.is_on_board(*pos):
                j += 1
                scene.append_text(str(j), *pos, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        return scene

    def scn_d_08_teleport_wave_via_star(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_08_teleport_wave_via_star', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        scene.board.set_piece(1, 6, piece=PieceType.Monolith)
        scene.board.set_piece(22, 17, piece=PieceType.Monolith)

        scene.board.set_piece(17, 3, piece=PieceType.Bishop)

        #
        # Wave
        start_W = (6, 20)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Pegasus
        start_G = (4, 16)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        gen_abs_pos_G = GS.gen_steps([(1, 2), ], start=start_G, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_G() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=startT3, include_prev=True, bounds=scene.board.get_position_limits()) # , count=3)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *startT4, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *startT3, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_09_teleport_wave_via_monolith(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_09_teleport_wave_via_monolith', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (9, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(5, 8, piece=PieceType.Queen)
        scene.board.set_piece(16, 20, piece=PieceType.Bishop)

        #
        # Pegasus
        start_G = (17, 7)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        gen_abs_pos_G = GS.gen_steps([(-1, -2), ], start=start_G, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_G() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (15, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=7)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Blocked if i > 2 else MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_10_teleported_wave_blocked(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_10_teleported_wave_blocked', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (9, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(5, 8, piece=PieceType.Queen)

        #
        # Pawns
        gen_abs_pos_P = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_P() ):
            piece = -PieceType.King if i == 5 else -PieceType.Pawn
            scene.board.set_piece(*pos, piece=piece)

        #
        # Pegasus
        start_G = (17, 7)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        gen_abs_pos_G = GS.gen_steps([(-1, -2), ], start=start_G, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_G() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (15, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=7)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Blocked if i > 2 else MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            scene.append_arrow(*pos, mark_type=MarkType.Blocked)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_11_wave_teleported_off_board(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_11_wave_teleported_off_board', bt, x=-4.0, y=1.0)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (1, 6)
        start_M2 = (1, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        #
        # Wave
        start_W = (7, 3)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Pegasus
        start_G = (9, 7)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        gen_abs_pos_G = GS.gen_steps([(-1, -2), ], start=start_G, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_G() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            scene.append_arrow(*pos, mark_type=MarkType.Blocked)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_12_wave_teleport_on_and_off_board(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_12_wave_teleport_on_and_off_board', bt, x=-4.0, y=1.0)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (0, 6)
        start_M2 = (0, 15)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(0, 19, piece=PieceType.Bishop)

        #
        # Serpent
        start_S = (3, 3)
        scene.board.set_piece(*start_S, piece=PieceType.Serpent)

        gen_abs_pos_W = GS.gen_steps([(-1, -1), (-1, 1), ], start=start_S, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave
        start_W = (0, 2)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-1, 1), (1, 1), ], start=start_W, include_prev=True, count=4)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Legal
            if i == 3:
                mark_type = MarkType.Action
            elif i % 2 == 0:
                mark_type = MarkType.Illegal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-1, 1), (1, 1), ], start=start_M2, include_prev=True, count=10)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Illegal if i > 8 or i % 2 == 0 else MarkType.Legal
            if i in [3, 7, ]:
                mark_type = MarkType.Action
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_13_teleporting_wave_cascade(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_13_teleporting_wave_cascade', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (1, 4)
        start_M2 = (12, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(17, 3, piece=PieceType.Bishop)

        #
        # Wave
        start_W = (7, 1)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        gen_abs_pos_W = GS.gen_steps([(-2, 1), ], start=start_W, include_prev=True, count=3)

        for i, pos in enumerate( gen_abs_pos_W() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Pegasus
        start_G = (9, 5)
        scene.board.set_piece(*start_G, piece=PieceType.Pegasus)

        gen_abs_pos_G = GS.gen_steps([(-1, -2), ], start=start_G, include_prev=True, count=2)

        for i, pos in enumerate( gen_abs_pos_G() ):
            mark_type = MarkType.Action if i == 1 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=True, count=6) # bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 5 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Wave, teleported
        gen_abs_pos_Wt = GS.gen_steps([(-2, 1), ], start=startT3, include_prev=True, bounds=scene.board.get_position_limits()) # , count=3)

        for i, pos in enumerate( gen_abs_pos_Wt() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        scene.append_text("A", *start_M1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("B", *start_M2, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("C", *startT4, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("D", *startT3, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        return scene

    def scn_d_14_monolith_shaman_interaction(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_14_monolith_shaman_interaction', bt, x=-4, y=0)

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
        scene.board.set_piece(4, 17, piece=PieceType.Monolith)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        startW = (3, 10)
        scene.board.set_piece(*startW, piece=PieceType.Wave)

        startH = (5, 9)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        scene.append_arrow( *(startH + startW), mark_type=MarkType.Action )
        scene.append_arrow( *(startW + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Illegal if i in [1, 3, 7] else MarkType.Legal
            # aba(str(i + 1), mark_type=mark_type)
            aba(None, mark_type=mark_type)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_d_15_syzygy_explain(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_15_syzygy_explain', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (17, 2)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        start_M2 = (2, 5)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.append_arrow( *GS.append_tpl_rel(start_M1, 1, -2), mark_type=MarkType.Action )

        scene.append_arrow( *GS.append_tpl_rel(start_M2, -2, 1), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_tpl_rel(start_M2, 1, -2), mark_type=MarkType.Action )

        gen = GS.gen_steps( [(1, 0), ], startT1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Legal, end_pointer=False )

        gen = GS.gen_steps( [(1, 1), ], startT1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Illegal, end_pointer=False )

        gen = GS.gen_steps( [(0, 1), ], startT1, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for index, coords in enumerate( gen() ):
            scene.append_arrow( *coords, mark_type=MarkType.Blocked, end_pointer=False )

        return scene

    def scn_d_16_syzygy_2_stars_init(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_16_syzygy_2_stars_init', bt)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(22, 1, piece=PieceType.Pawn)
        scene.board.set_piece(20, 3, piece=PieceType.King)
        scene.board.set_piece(19, 14, piece=PieceType.Knight)
        scene.board.set_piece(16, 7, piece=PieceType.Bishop)
        scene.board.set_piece(15, 8, piece=PieceType.Wave)

        scene.board.set_piece(1, 22, piece=-PieceType.Pawn)
        scene.board.set_piece(3, 20, piece=-PieceType.King)
        scene.board.set_piece(4, 11, piece=-PieceType.Queen)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        start_M = (13, 13)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(1, 6, piece=PieceType.Monolith)

        end_M = (12, 11)
        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Action )

        return scene

    def scn_d_17_syzygy_2_stars_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_17_syzygy_2_stars_steps', bt)

        start_T = (23, 0)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(22, 1, piece=PieceType.Pawn)
        scene.board.set_piece(20, 3, piece=PieceType.King)
        scene.board.set_piece(19, 14, piece=PieceType.Knight)
        scene.board.set_piece(16, 7, piece=PieceType.Bishop)
        scene.board.set_piece(15, 8, piece=PieceType.Wave)

        scene.board.set_piece(1, 22, piece=-PieceType.Pawn)
        scene.board.set_piece(3, 20, piece=-PieceType.King)
        scene.board.set_piece(4, 11, piece=-PieceType.Queen)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        start_M = (12, 11)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(1, 6, piece=PieceType.Monolith)

        #
        # diagonal arrows
        gen_abs_pos = GS.gen_steps([(-1, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        #
        # field markers
        gen_abs_pos_1 = GS.gen_steps([(-1, 1)], start=start_T, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_1() ):
            mark_type = None
            if i in [0, 21]:
                mark_type = MarkType.Blocked
            elif i in [2, 10, 19, 22]:
                mark_type = MarkType.Illegal
            elif i in [15, ]:
                mark_type = MarkType.Legal
            elif i in [6, 7]:
                mark_type = MarkType.Action

            if mark_type is not None:
                scene.append_field_marker( *pos, mark_type=mark_type )

        scene.append_field_marker( *start_T, mark_type=MarkType.Illegal )

        return scene

    def scn_d_18_syzygy_2_monoliths_init(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_18_syzygy_2_monoliths_init', bt)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(16, 9, piece=PieceType.Bishop)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        scene.board.set_piece(21, 7, piece=PieceType.Knight)

        start_M = (13, 6)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(6, 2, piece=PieceType.Monolith)

        end_M = (12, 4)
        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Action )

        return scene

    def scn_d_19_syzygy_2_monoliths_steps(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_19_syzygy_2_monoliths_steps', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(16, 9, piece=PieceType.Bishop)
        scene.board.set_piece(7, 16, piece=-PieceType.Rook)

        scene.board.set_piece(21, 7, piece=PieceType.Knight)

        start_M = (12, 4)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(6, 2, piece=PieceType.Monolith)

        #
        # diagonal arrows
        gen_abs_pos = GS.gen_steps([(3, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        #
        # texts, field markers
        gen_abs_pos_1 = GS.gen_steps([(3, 1)], start=start_T, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_1() ):
            scene.append_text(str(i+2), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

            mark_type = None
            if i in [1, 3, ]:
                mark_type = MarkType.Illegal
            elif i in [6, ]:
                mark_type = MarkType.Action

            if mark_type is not None:
                scene.append_field_marker( *pos, mark_type=mark_type )

        scene.append_text("1", *start_T, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_field_marker( *start_T, mark_type=MarkType.Illegal )

        return scene

    def scn_d_20_syzygy_reentering_same_move(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_20_syzygy_reentering_same_move', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        start_M = (4, 2)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)
        scene.board.set_piece(22, 11, piece=PieceType.Monolith)

        #
        # diagonal arrows
        gen_abs_pos = GS.gen_steps([(2, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        adder = GS.adder( start_M, include_prev=True )
        scene.append_arrow( *adder(1, 2), mark_type=MarkType.Action )
        scene.append_arrow( *adder(2, 1), mark_type=MarkType.Action )
        scene.append_arrow( *adder(-1, -2), mark_type=MarkType.Action )

        return scene

    def scn_d_21_syzygy_reentering_independent(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_21_syzygy_reentering_independent', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)

        start_T1 = (0, 23)
        scene.board.set_piece(*start_T1, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        start_M = (14, 14)
        scene.board.set_piece(*start_M, piece=PieceType.Monolith)

        #
        # diagonal SW-NE arrows
        gen_abs_pos = GS.gen_steps([(1, 1)], start=start_T, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False )

        #
        # diagonal NW-SE arrows
        gen_abs_pos = GS.gen_steps([(1, -1)], start=start_T1, include_prev=True, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False )

        adder = GS.adder( start_M, include_prev=True )
        scene.append_arrow( *adder(-1, 2), mark_type=MarkType.Action )
        scene.append_arrow( *adder(-1, -2), mark_type=MarkType.Action )
        scene.append_arrow( *adder(-2, -1), mark_type=MarkType.Action )

        return scene

    def scn_d_22_syzygy_in_oppo_figure_row(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_22_syzygy_in_oppo_figure_row', bt)

        start_T = (0, 0)
        scene.board.set_piece(*start_T, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        # start_R = (7, 23)
        # scene.board.set_piece( *start_R, piece=-PieceType.Rook )

        end_M = (12, 23)
        scene.board.set_piece(*end_M, piece=PieceType.Monolith)

        start_M = (11, 21)
        scene.append_arrow( *(start_M + end_M), mark_type=MarkType.Blocked )

        start_P = (16, 23)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        scene.append_field_marker( *start_P, mark_type=MarkType.Action )

        return scene
