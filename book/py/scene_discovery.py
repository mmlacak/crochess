#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


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

        start_M1 = (1, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(16, 20, piece=PieceType.Bishop)

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

        start_M1 = (1, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        #
        # Pawns
        gen_abs_pos_P = GS.gen_steps([(-2, 1), ], start=start_M2, include_prev=False, bounds=scene.board.get_position_limits())

        for i, pos in enumerate( gen_abs_pos_P() ):
            piece = -PieceType.King if i == 5 else -PieceType.Pawn
            scene.board.set_piece(*pos, piece=piece)

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

    def scn_d_12_teleporting_monolith_via_star(self, bt=BoardType.Discovery):

        scene = Scene('scn_d_12_teleporting_monolith_via_star', bt)

        startT1 = (0, 0)
        startT2 = (23, 23)
        startT3 = (23, 0)
        startT4 = (0, 23)

        scene.board.set_piece(*startT1, piece=PieceType.Star)
        scene.board.set_piece(*startT2, piece=PieceType.Star)
        scene.board.set_piece(*startT3, piece=-PieceType.Star)
        scene.board.set_piece(*startT4, piece=-PieceType.Star)

        start_M1 = (1, 6)
        start_M2 = (22, 17)

        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)
        scene.board.set_piece(*start_M2, piece=PieceType.Monolith)

        scene.board.set_piece(0, 7, piece=PieceType.Wave)
        scene.board.set_piece(2, 7, piece=PieceType.King)

        #
        # Monolith 2
        rels = [(1, 2), (-1, 2), (1, 2), ]
        gen_abs_pos_G = GS.gen_steps(rels, start=start_M2, include_prev=True, count=len(rels))

        for i, pos in enumerate( gen_abs_pos_G() ):
            mark_type = MarkType.Action if i == 2 else MarkType.Legal
            scene.append_arrow(*pos, mark_type=mark_type)

        #
        # Monolith 1
        start_M1 = (1, 6)
        scene.board.set_piece(*start_M1, piece=PieceType.Monolith)

        gen_abs_pos_K = GS.gen_multi_steps(GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_M1, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_K() ):
            if i not in [1, 3]:
                scene.append_text(str(i+1), *pos, corner=Corner.UpperRight, mark_type=MarkType.Legal)

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
