#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2017 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneMirandasVeilMixin:

    def scn_mv_01_move_wave_init(self, bt=BoardType.MirandasVeil):
        # move_wave_init

        scene = Scene('scn_mv_01_move_wave_init', bt)

        scene.board.set_piece(1, 1, piece=PieceType.Wave)
        scene.board.set_piece(0, 3, piece=PieceType.Knight)

        scene.append_arrow(0, 3, 1, 1, mark_type=MarkType.Action)

        # direction <1, 2>
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)
        scene.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        scene.board.set_piece(5, 9, piece=-PieceType.Queen)
        scene.board.set_piece(7, 13, piece=PieceType.Bishop)

        # direction <2, 1>
        scene.board.set_piece(3, 2, piece=PieceType.King)
        scene.board.set_piece(5, 3, piece=PieceType.Rook)
        scene.board.set_piece(7, 4, piece=PieceType.Pyramid)
        scene.board.set_piece(9, 5, piece=-PieceType.King)
        scene.board.set_piece(13, 7, piece=-PieceType.Wave)

        return scene

    def scn_mv_02_move_wave_activated(self, bt=BoardType.MirandasVeil):
        # move_wave_activated

        scene = Scene('scn_mv_02_move_wave_activated', bt)

        start = (1, 1)
        scene.board.set_piece(*start, piece=PieceType.Knight)

        scene.append_arrow( *(start + (0, 3)) ) # 1, 1, 0, 3
        scene.append_arrow( *(start + (3, 0)) ) # 1, 1, 3, 0

        # direction <1, 2>
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)
        scene.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        scene.board.set_piece(5, 9, piece=-PieceType.Queen)
        scene.board.set_piece(7, 13, piece=PieceType.Bishop)

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )

        middle_1 = (6, 11)
        multi_steps = GS.convert_single_step_into_multi_rels( GS.remove(GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=[(1, 2), (-1, -2)]) )
        gen_pos = GS.gen_multi_steps(multi_steps, start=middle_1, include_prev=True, bounds=((4, 9), (8, 13)))

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        scene.append_text("5", *middle_1, mark_type=MarkType.Blocked)

        multi_steps = GS.convert_single_step_into_multi_rels( GS.remove(GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=[(1, 2), (-1, -2)]) )
        gen_pos = GS.gen_next( GS.gen_multi_steps(multi_steps, start=middle_1, include_prev=False, bounds=((4, 9), (8, 13))) )

        scene.append_text("5a", *gen_pos(), mark_type=MarkType.Blocked, corner=Corner.UpperLeft)
        scene.append_text("5b", *gen_pos(), mark_type=MarkType.Blocked, corner=Corner.UpperLeft)
        scene.append_text("5c", *gen_pos(), mark_type=MarkType.Blocked, corner=Corner.UpperLeft)
        scene.append_text("5d", *gen_pos(), mark_type=MarkType.Blocked, corner=Corner.UpperLeft)
        scene.append_text("5e", *gen_pos(), mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)
        scene.append_text("5f", *gen_pos(), mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

        # direction <2, 1>
        scene.board.set_piece(3, 2, piece=PieceType.King)
        scene.board.set_piece(5, 3, piece=PieceType.Rook)
        scene.board.set_piece(7, 4, piece=PieceType.Pyramid)
        scene.board.set_piece(9, 5, piece=-PieceType.King)
        scene.board.set_piece(13, 7, piece=-PieceType.Wave)

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )

        # middle_2 = (11, 6)
        # multi_steps = GS.convert_single_step_into_multi_rels( GS.remove(GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=[(2, 1), (-2, -1)]) )
        # gen_pos = GS.gen_multi_steps(multi_steps, start=middle_2, include_prev=True, bounds=((9, 4), (13, 8)))

        # for pos in gen_pos():
        #     scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        # scene.append_text("2", *middle_2, mark_type=MarkType.Blocked)

        return scene

    def scn_mv_03_move_wave_finished(self, bt=BoardType.MirandasVeil):
        # move_wave_finished

        scene = Scene('scn_mv_03_move_wave_finished', bt)

        scene.board.set_piece(1, 1, piece=PieceType.Knight)

        # direction <1, 2>
        start = (7, 13)
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)
        scene.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        scene.board.set_piece(5, 9, piece=-PieceType.Queen)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_BISHOP_MULTI_REL_MOVES, start=start, include_prev=True, bounds=((6, 12), (8, 14)))

        for pos in gen_abs_pos():
            scene.append_arrow(*pos)

        gen_pos = GS.gen_multi_steps(GS.convert_single_step_into_multi_rels([(1, 2), ]), end=start, include_prev=True, count=6)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        # direction <2, 1>
        scene.board.set_piece(3, 2, piece=PieceType.King)
        scene.board.set_piece(5, 3, piece=PieceType.Rook)
        scene.board.set_piece(7, 4, piece=PieceType.Pyramid)
        scene.board.set_piece(9, 5, piece=-PieceType.King)
        scene.board.set_piece(13, 7, piece=-PieceType.Wave)

        return scene

    def scn_mv_04_wave_cascading_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_04_wave_cascading_init', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (7, 4)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        # B --> W(A)
        coords_B_WA = GS.gen_next( GS.gen_steps(start=start_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_B_WA() )
        scene.append_arrow( *coords_B_WA() )
        scene.append_arrow( *coords_B_WA(), mark_type=MarkType.Action )

        # W(A) --> W(B)
        coords_WA_WB = GS.gen_next( GS.gen_steps(start=start_W_A, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WA_WB() )
        scene.append_arrow( *coords_WA_WB() )
        scene.append_arrow( *coords_WA_WB(), mark_type=MarkType.Action )

        # W(B) --> N
        coords_WB_N = GS.gen_steps(start=start_W_B, rels=[(-1, 1), ], include_prev=True, count=7)
        for step in coords_WB_N():
            scene.append_arrow( *step )

        # labels
        scene.append_text( "A", *start_W_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_05_wave_cascading_steps(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_05_wave_cascading_steps', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (7, 4)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_N = (3, 8)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W_C = (5, 9)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (13, 5)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        # B --> W(A)
        coords_B_WA = GS.gen_next( GS.gen_steps(start=start_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_B_WA() )
        scene.append_arrow( *coords_B_WA() )
        scene.append_arrow( *coords_B_WA(), mark_type=MarkType.Action )

        # W(A) --> W(B)
        coords_WA_WB = GS.gen_next( GS.gen_steps(start=start_W_A, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WA_WB() )
        scene.append_arrow( *coords_WA_WB() )
        scene.append_arrow( *coords_WA_WB(), mark_type=MarkType.Action )

        # W(B) --> N
        coords_WB_N = GS.gen_next( GS.gen_steps(start=start_W_B, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N(), mark_type=MarkType.Action )

        # N --> W(C)
        scene.append_arrow( *(start_N + start_W_C), mark_type=MarkType.Action )

        # W(C) --> W(D)
        coords_WC_WD = GS.gen_next( GS.gen_steps(start=start_W_C, rels=[(2, -1), ], include_prev=True) )
        scene.append_arrow( *coords_WC_WD() )
        scene.append_arrow( *coords_WC_WD() )
        scene.append_arrow( *coords_WC_WD() )
        scene.append_arrow( *coords_WC_WD(), mark_type=MarkType.Action )

        # W(D) --> P
        coords_WD_P = GS.gen_next( GS.gen_steps(start=start_W_D, rels=[(-1, -2), ], include_prev=True) )
        scene.append_arrow( *coords_WD_P() )
        scene.append_arrow( *coords_WD_P() )

        # labels
        scene.append_text( "A", *start_W_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_06_wave_cascading_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_06_wave_cascading_end', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_W_B = (7, 4)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_N = (3, 8)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W_C = (5, 9)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (13, 5)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_P = (11, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        # B --> W(A)
        coords_B_WA = GS.gen_next( GS.gen_steps(start=start_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_B_WA() )
        scene.append_arrow( *coords_B_WA() )
        scene.append_arrow( *coords_B_WA(), mark_type=MarkType.Action )

        # W(A) --> W(B)
        coords_WA_WB = GS.gen_next( GS.gen_steps(start=start_W_A, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WA_WB() )
        scene.append_arrow( *coords_WA_WB() )
        scene.append_arrow( *coords_WA_WB(), mark_type=MarkType.Action )

        # W(B) --> N
        coords_WB_N = GS.gen_next( GS.gen_steps(start=start_W_B, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N(), mark_type=MarkType.Action )

        # N --> W(C)
        scene.append_arrow( *(start_N + start_W_C), mark_type=MarkType.Action )

        # W(C) --> W(D)
        coords_WC_WD = GS.gen_next( GS.gen_steps(start=start_W_C, rels=[(2, -1), ], include_prev=True) )
        scene.append_arrow( *coords_WC_WD() )
        scene.append_arrow( *coords_WC_WD() )
        scene.append_arrow( *coords_WC_WD() )
        scene.append_arrow( *coords_WC_WD(), mark_type=MarkType.Action )

        # W(D) --> P
        coords_WD_P = GS.gen_next( GS.gen_steps(start=start_W_D, rels=[(-1, -2), ], include_prev=True) )
        scene.append_arrow( *coords_WD_P() )
        scene.append_arrow( *coords_WD_P(), mark_type=MarkType.Action )

        # P -->
        coords_P = GS.gen_next( GS.gen_steps(start=start_P, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords_P() )
        scene.append_arrow( *coords_P() )
        scene.append_arrow( *coords_P(), mark_type=MarkType.Blocked )

        # labels
        scene.append_text( "A", *start_W_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        # momentum received
        scene.append_text( "3", *start_W_A, corner=Corner.LowerLeft, mark_type=MarkType.Action )
        scene.append_text( "3", *start_W_B, corner=Corner.LowerLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "3", *start_N, corner=Corner.LowerLeft, mark_type=MarkType.Action )
        scene.append_text( "2", *start_W_C, corner=Corner.LowerLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "2", *start_W_D, corner=Corner.LowerLeft, mark_type=MarkType.Action )
        scene.append_text( "2", *start_P, corner=Corner.LowerLeft, mark_type=MarkType.Action )

        return scene

    def scn_mv_07_wave_no_momentum_no_activating(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_07_wave_no_momentum_no_activating', bt)

        scene.board.set_piece( 8, 6, piece=PieceType.Bishop )
        scene.board.set_piece( 7, 5, piece=PieceType.Wave )
        scene.board.set_piece( 6, 6, piece=PieceType.Knight )
        scene.board.set_piece( 5, 4, piece=PieceType.Wave )
        scene.board.set_piece( 3, 5, piece=PieceType.Pawn )
        scene.board.set_piece( 1, 6, piece=-PieceType.Wave )

        scene.append_arrow( 8, 6, 7, 5, mark_type=MarkType.Legal )
        scene.append_arrow( 7, 5, 6, 6, mark_type=MarkType.Legal )
        scene.append_arrow( 6, 6, 5, 4, mark_type=MarkType.Action )
        scene.append_arrow( 5, 4, 3, 5, mark_type=MarkType.Illegal )
        scene.append_arrow( 3, 5, 1, 6, mark_type=MarkType.Legal )

        scene.append_text( "A", 7, 5, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", 5, 4, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_08_wave_no_activating_blocked_piece(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_08_wave_no_activating_blocked_piece', bt)

        scene.board.set_piece(6, 6, piece=PieceType.Knight)
        scene.board.set_piece(5, 4, piece=PieceType.Wave)
        scene.board.set_piece(3, 6, piece=PieceType.Bishop)
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)

        scene.append_arrow( 6, 6, 5, 4, mark_type=MarkType.Action )
        scene.append_arrow( 5, 4, 3, 5, mark_type=MarkType.Illegal )
        scene.append_arrow( 3, 5, 1, 6, mark_type=MarkType.Legal )

        scene.append_arrow( 3, 5, 2, 6, mark_type=MarkType.Blocked )
        scene.append_arrow( 3, 5, 3, 6, mark_type=MarkType.Blocked )
        scene.append_arrow( 3, 5, 4, 6, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_09_wave_blocked_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_09_wave_blocked_init', bt)

        scene.board.set_piece(9, 9, piece=-PieceType.Rook)
        scene.board.set_piece(9, 4, piece=-PieceType.Wave)
        scene.board.set_piece(11, 4, piece=PieceType.Wave)
        scene.board.set_piece(11, 0, piece=PieceType.Rook)
        scene.board.set_piece(15, 0, piece=PieceType.Wave)

        start = (15, 1)
        scene.board.set_piece(*start, piece=-PieceType.Wave)

        # horizontal Pawns
        coords = GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=False, count=15)
        for step in coords():
            scene.board.set_piece(*step, piece=PieceType.Pawn)

        # vertical Pawns + Queen
        coords = GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=False, count=14)
        for i, step in enumerate(coords()):
            piece = PieceType.Knight if i < 2 else \
                    PieceType.Bishop if i < 4 else \
                    PieceType.Rook if i < 6 else \
                    PieceType.Unicorn if i < 8 else \
                    PieceType.Pegasus if i < 10 else \
                    PieceType.Pyramid if i < 12 else \
                    PieceType.Queen
            scene.board.set_piece(*step, piece=piece)

        # arrows
        scene.append_arrow( 9, 9, 9, 4, mark_type=MarkType.Legal )
        scene.append_arrow( 9, 4, 11, 4, mark_type=MarkType.Legal )
        scene.append_arrow( 11, 4, 11, 0, mark_type=MarkType.Legal )
        scene.append_arrow( 11, 0, 15, 0, mark_type=MarkType.Legal )
        scene.append_arrow( 15, 0, 15, 1, mark_type=MarkType.Action )

        scene.append_text( "A", 9, 4, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "1", 11, 4, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "2", 15, 0, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_10_wave_blocked_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_10_wave_blocked_end', bt)

        scene.board.set_piece(9, 4, piece=-PieceType.Rook)
        scene.board.set_piece(11, 4, piece=-PieceType.Wave)
        scene.board.set_piece(11, 0, piece=PieceType.Wave)
        scene.board.set_piece(15, 0, piece=PieceType.Rook)

        start = (15, 1)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        # horizontal Pawns
        coords = GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=False, count=15)
        for step in coords():
            scene.board.set_piece(*step, piece=PieceType.Pawn)

        # vertical Pawns + Queen
        coords = GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=False, count=14)
        for i, step in enumerate(coords()):
            piece = PieceType.Knight if i < 2 else \
                    PieceType.Bishop if i < 4 else \
                    PieceType.Rook if i < 6 else \
                    PieceType.Unicorn if i < 8 else \
                    PieceType.Pegasus if i < 10 else \
                    PieceType.Pyramid if i < 12 else \
                    PieceType.Queen
            scene.board.set_piece(*step, piece=piece)

        # horizontal arrows
        coords = GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True, count=15)
        for step in coords():
            scene.append_arrow(*step, mark_type=MarkType.Blocked)

        # vertical arrows
        coords = GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True, count=14)
        for step in coords():
            scene.append_arrow(*step, mark_type=MarkType.Blocked)

        scene.append_arrow(*(start + (15, 0)), mark_type=MarkType.Blocked)

        scene.append_text( "A", 11, 4, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "1", 11, 0, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "2", 15, 1, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )

        return scene

    #
    # cascading

    def scn_mv_11_cascading_rook(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_rook

        scene = Scene('scn_mv_11_cascading_rook', bt, width=9, height=9)

        start = (5, 7)
        scene.board.set_piece(*start, piece=PieceType.Rook)
        scene.board.set_piece(5, 3, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Wave)
        scene.board.set_piece(3, 5, piece=PieceType.Queen)

        scene.append_text("1", 5, 3, mark_type=MarkType.Blocked)
        scene.append_text("2", 3, 3, mark_type=MarkType.Blocked)

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        return scene

    def scn_mv_12_cascading_wave_1(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_1

        scene = Scene('scn_mv_12_cascading_wave_1', bt, width=9, height=9)

        start = (5, 3)
        scene.board.set_piece(*start, piece=PieceType.Rook)
        scene.board.set_piece(3, 3, piece=PieceType.Wave)
        scene.board.set_piece(3, 5, piece=PieceType.Queen)

        scene.append_text("2", 3, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_13_cascading_wave_2(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_2

        scene = Scene('scn_mv_13_cascading_wave_2', bt, width=9, height=9)

        start = (3, 3)
        scene.board.set_piece(5, 3, piece=PieceType.Rook)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(3, 5, piece=PieceType.Queen)

        scene.append_text("1", 3, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_14_cascading_rook_2nd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_rook_b

        scene = Scene('scn_mv_14_cascading_rook_2nd_time', bt, width=9, height=9)

        start = (5, 3)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Wave)
        scene.board.set_piece(3, 5, piece=PieceType.Queen)

        scene.append_text("1", 3, 3, mark_type=MarkType.Blocked)
        scene.append_text("2", 5, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_15_cascading_wave_1_2nd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_1_b

        scene = Scene('scn_mv_15_cascading_wave_1_2nd_time', bt, width=9, height=9)

        start = (3, 3)
        scene.board.set_piece(3, 5, piece=PieceType.Queen)
        scene.board.set_piece(*start, piece=PieceType.Rook)
        scene.board.set_piece(5, 3, piece=PieceType.Wave)

        scene.append_text("2", 5, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_16_cascading_queen(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_queen

        scene = Scene('scn_mv_16_cascading_queen', bt, width=9, height=9)

        start = (3, 5)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Rook)
        scene.board.set_piece(5, 3, piece=PieceType.Wave)

        scene.append_text("1", 3, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )
        scene.append_text("2", 5, 3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_17_cascading_wave_2_2nd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_2_b

        scene = Scene('scn_mv_17_cascading_wave_2_2nd_time', bt, width=9, height=9)

        start = (5, 3)
        scene.board.set_piece(3, 5, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Rook)
        scene.board.set_piece(*start, piece=PieceType.Queen)

        scene.append_text("1", 3, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_18_cascading_wave_1_3rd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_1_c

        scene = Scene('scn_mv_18_cascading_wave_1_3rd_time', bt, width=9, height=9)

        start = (3, 5)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Rook)
        scene.board.set_piece(5, 3, piece=PieceType.Queen)

        scene.append_text("2", 3, 5, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_19_cascading_end(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_end

        scene = Scene('scn_mv_19_cascading_end', bt, width=9, height=9)

        scene.board.set_piece(3, 5, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Rook)
        scene.board.set_piece(3, 1, piece=PieceType.Wave)
        scene.board.set_piece(5, 3, piece=PieceType.Queen)

        scene.append_text("1", 3, 1, mark_type=MarkType.Blocked)
        scene.append_text("2", 3, 5, mark_type=MarkType.Blocked)

        return scene

    #
    # cascading opponent

    def scn_mv_20_casc_oppo_light_queen(self, bt=BoardType.MirandasVeil):
        # move_wave_opponent_light_queen

        scene = Scene('scn_mv_20_casc_oppo_light_queen', bt, width=9, height=9)

        start = (5, 6)
        scene.board.set_piece(*start, piece=PieceType.Queen)
        scene.board.set_piece(5, 3, piece=PieceType.Wave)
        scene.board.set_piece(3, 3, piece=-PieceType.Wave)
        scene.board.set_piece(3, 5, piece=-PieceType.Queen)

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        return scene

    def scn_mv_21_casc_oppo_light_wave(self, bt=BoardType.MirandasVeil):
        # move_wave_opponent_light_wave

        scene = Scene('scn_mv_21_casc_oppo_light_wave', bt, width=9, height=9)

        start = (5, 3)
        scene.board.set_piece(3, 5, piece=-PieceType.Queen)
        scene.board.set_piece(3, 3, piece=-PieceType.Wave)
        scene.board.set_piece(*start, piece=PieceType.Queen)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_22_casc_oppo_dark_wave(self, bt=BoardType.MirandasVeil):
        # move_wave_opponent_dark_wave

        scene = Scene('scn_mv_22_casc_oppo_dark_wave', bt, width=9, height=9)

        start = (3, 3)
        scene.board.set_piece(3, 5, piece=-PieceType.Queen)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(5, 3, piece=PieceType.Queen)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_23_casc_oppo_dark_queen(self, bt=BoardType.MirandasVeil):
        # move_wave_opponent_dark_queen

        scene = Scene('scn_mv_23_casc_oppo_dark_queen', bt, width=9, height=9)

        start = (3, 5)
        scene.board.set_piece(*start, piece=-PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Wave)
        scene.board.set_piece(5, 3, piece=PieceType.Queen)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        return scene

    def scn_mv_24_casc_oppo_end(self, bt=BoardType.MirandasVeil):
        # move_wave_opponent_end

        scene = Scene('scn_mv_24_casc_oppo_end', bt, width=9, height=9)

        start = (3, 5)
        scene.board.set_piece(3, 5, piece=-PieceType.Wave)
        scene.board.set_piece(3, 3, piece=PieceType.Wave)
        scene.board.set_piece(5, 3, piece=-PieceType.Queen)

        return scene

    #
    # activating pawn, with rushing ability

    def scn_mv_25_activating_rush_pawn_init(self, bt=BoardType.MirandasVeil):
        # move_wave_activating_pawn_init

        scene = Scene('scn_mv_25_activating_rush_pawn_init', bt) # , width=5, height=8)

        #
        # 1 - momentum smaller than rush

        startW1 = (4, 1)
        startR1 = (4, 5)
        startP1 = (2, 1)

        scene.board.set_piece(1, 2, piece=-PieceType.Knight)
        scene.board.set_piece(*startP1, piece=PieceType.Pawn)
        scene.board.set_piece(*startW1, piece=PieceType.Wave)
        scene.board.set_piece(*startR1, piece=PieceType.Rook)

        # Rook, direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startR1, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # Wave, direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startW1, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        scene.append_text("1", *startP1, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        #
        # 2 - momentum larger than rush

        startW2 = (14, 1)
        startR2 = (14, 14)
        startP2 = (12, 1)

        scene.board.set_piece(11, 2, piece=-PieceType.Rook)
        scene.board.set_piece(*startP2, piece=PieceType.Pawn)
        scene.board.set_piece(*startW2, piece=PieceType.Wave)
        scene.board.set_piece(*startR2, piece=PieceType.Rook)

        # Rook, direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startR2, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # Wave, direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startW2, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        scene.append_text("2", *startP2, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        return scene

    def scn_mv_26_activating_rush_pawn_end(self, bt=BoardType.MirandasVeil):
        # move_wave_activating_pawn_end

        scene = Scene('scn_mv_26_activating_rush_pawn_end', bt)

        #
        # 1 - momentum smaller than rush

        startP1 = (2, 1)

        scene.board.set_piece(1, 2, piece=-PieceType.Knight)
        scene.board.set_piece(*startP1, piece=PieceType.Wave)
        scene.board.set_piece(4, 1, piece=PieceType.Rook)

        # Pawn, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP1, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # Pawn, direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP1, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # Pawn, direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP1, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        scene.append_text("1", *startP1, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        #
        # 2 - momentum larger than rush

        startP2 = (12, 1)

        scene.board.set_piece(11, 2, piece=-PieceType.Rook)
        scene.board.set_piece(*startP2, piece=PieceType.Wave)
        scene.board.set_piece(14, 1, piece=PieceType.Rook)

        # Pawn, direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # Pawn, direction <1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # Pawn, direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        scene.append_text("2", *startP2, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        return scene

    #
    # activating by pawn

    def scn_mv_27_wave_activation_by_step_pawn(self, bt=BoardType.MirandasVeil):
        # move_wave_activation_by_pawn

        scene = Scene('scn_mv_27_wave_activation_by_step_pawn', bt)

        #
        # step-fields 1, Pawn 1
        start_P1 = (2, 2)
        scene.board.set_piece(*start_P1, piece=PieceType.Pawn)
        scene.board.set_piece(2, 3, piece=PieceType.Wave)

        start_W = (1, 5)
        start_P3 = (1, 9)
        scene.board.set_piece(2, 7, piece=-PieceType.Pawn)
        scene.board.set_piece(2, 11, piece=PieceType.Knight)
        scene.board.set_piece(*start_W, piece=-PieceType.Wave)
        scene.board.set_piece(*start_P3, piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps(start=start_P1, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action)

        scene.append_text("1", *start_P1, mark_type=MarkType.Blocked, corner=Corner.UpperRight)
        scene.append_text("3", *start_P3, mark_type=MarkType.Blocked, corner=Corner.UpperRight)

        #
        # step-fields 2, Pawn 2
        start_P2 = (13, 1)
        scene.board.set_piece(*start_P2, piece=PieceType.Pawn)
        scene.board.set_piece(13, 4, piece=PieceType.Wave)

        scene.board.set_piece(13, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(13, 13, piece=PieceType.Bishop)

        coords = GS.gen_next( GS.gen_steps(start=start_P2, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        scene.append_text("2", *start_P2, mark_type=MarkType.Blocked, corner=Corner.UpperRight)

        return scene

    def scn_mv_28_wave_activated_by_step_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_28_wave_activated_by_step_pawn', bt)

        #
        # step-fields 1, Pawn 1
        start_P1 = (2, 3)
        scene.board.set_piece(*start_P1, piece=PieceType.Pawn)

        start_W = (1, 5)
        start_P3 = (1, 9)
        scene.board.set_piece(2, 7, piece=-PieceType.Pawn)
        scene.board.set_piece(2, 11, piece=PieceType.Knight)
        scene.board.set_piece(*start_W, piece=-PieceType.Wave)
        scene.board.set_piece(*start_P3, piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps(start=start_P1, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        scene.append_arrow(2, 4, 1, 5, mark_type=MarkType.Illegal)
        scene.append_arrow(2, 4, 3, 5, mark_type=MarkType.Illegal)
        scene.append_arrow(2, 8, 1, 9, mark_type=MarkType.Illegal)
        scene.append_arrow(2, 8, 3, 9, mark_type=MarkType.Illegal)

        scene.append_text("1", *start_P1, mark_type=MarkType.Blocked, corner=Corner.UpperRight)
        scene.append_text("3", *start_P3, mark_type=MarkType.Blocked, corner=Corner.UpperRight)

        #
        # step-fields 2, Pawn 2
        start_P2 = (13, 4)
        scene.board.set_piece(*start_P2, piece=PieceType.Pawn)

        scene.board.set_piece(13, 9, piece=-PieceType.Pawn)
        scene.board.set_piece(13, 13, piece=PieceType.Bishop)

        coords = GS.gen_next( GS.gen_steps(start=start_P2, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        scene.append_text("2", *start_P2, mark_type=MarkType.Blocked, corner=Corner.UpperRight)

        return scene

    def scn_mv_29_wave_activation_by_capture_pawn(self, bt=BoardType.MirandasVeil):
        # move_wave_activation_by_pawn

        scene = Scene('scn_mv_29_wave_activation_by_capture_pawn', bt)

        #
        # capture-fields
        start_P = (5, 6)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)
        scene.board.set_piece(4, 7, piece=PieceType.Wave)

        scene.board.set_piece(1, 10, piece=PieceType.Bishop)
        scene.board.set_piece(3, 8, piece=-PieceType.Rook)
        scene.board.set_piece(2, 13, piece=-PieceType.Wave)

        scene.board.set_piece(6, 12, piece=PieceType.Pegasus)
        scene.board.set_piece(8, 11, piece=-PieceType.Pyramid)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        coords = GS.gen_next( GS.gen_steps(start=start_P, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_P, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal )

        return scene

    def scn_mv_30_wave_activated_by_capture_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_30_wave_activated_by_capture_pawn', bt)

        #
        # capture-fields
        start_W = (4, 7)
        scene.board.set_piece(*start_W, piece=PieceType.Pawn)

        start = (6, 9)

        scene.board.set_piece(1, 10, piece=PieceType.Bishop)
        scene.board.set_piece(3, 8, piece=-PieceType.Rook)
        scene.board.set_piece(2, 13, piece=-PieceType.Wave)

        scene.board.set_piece(6, 12, piece=PieceType.Pegasus)
        scene.board.set_piece(8, 11, piece=-PieceType.Pyramid)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        coords = GS.gen_next( GS.gen_steps(start=start_W, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )

        coords = GS.gen_next( GS.gen_steps(start=start_W, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )
        scene.append_arrow( *coords(), mark_type=MarkType.Legal )

        coords = GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True, count=6)
        for step in coords():
            scene.append_arrow(*step, mark_type=MarkType.Illegal)

        coords = GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True, count=6)
        for step in coords():
            scene.append_arrow(*step, mark_type=MarkType.Illegal)

        scene.append_text("A", *start, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        return scene


    def scn_mv_31_wave_same_color(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_31_wave_same_color', bt, y=1, width=7, height=7)

        start = (3, 4)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal)

        return scene

    def scn_mv_32_wave_opposite_color(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_32_wave_opposite_color', bt, width=11, height=11)

        start = (5, 5)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        # Unicorn, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_LONG_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action)

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

        return scene


    def scn_mv_33_wave_activation_by_unicorn_first_step(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_33_wave_activation_by_unicorn_first_step', bt)

        start = (6, 3)
        start_U = (2, 4)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(*start_U, piece=PieceType.Unicorn)

        scene.board.set_piece(7, 5, piece=PieceType.Pawn)
        scene.board.set_piece(7, 6, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)

        scene.board.set_piece(7, 8, piece=-PieceType.Pawn)
        scene.board.set_piece(8, 8, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 8, piece=-PieceType.Pawn)

        scene.board.set_piece(9, 13, piece=-PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Blocked if i == 1 else MarkType.Legal
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        scene.append_arrow( *(start_U + start), mark_type=MarkType.Action )

        return scene

    def scn_mv_34_wave_activation_by_unicorn_second_step(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_34_wave_activation_by_unicorn_second_step', bt)

        start = (6, 3)
        start_W = (5, 5)
        start_U = (2, 4)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)
        scene.board.set_piece(*start_U, piece=PieceType.Unicorn)

        scene.board.set_piece(7, 5, piece=PieceType.Pawn)
        scene.board.set_piece(7, 6, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)

        scene.board.set_piece(7, 8, piece=-PieceType.Pawn)
        scene.board.set_piece(8, 8, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 8, piece=-PieceType.Pawn)

        scene.board.set_piece(9, 13, piece=-PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_LONG_MULTI_REL_MOVES, start=start_W, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            mark_type = MarkType.Blocked if i == 2 else MarkType.Action
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=mark_type)

        scene.append_arrow( *(start_U + start), mark_type=MarkType.Blocked )
        scene.append_arrow( *(start + start_W), mark_type=MarkType.Legal )

        return scene

    def scn_mv_35_wave_activation_by_unicorn_complete(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_35_wave_activation_by_unicorn_complete', bt)

        start = (6, 3)
        start_U = (2, 4)
        scene.board.set_piece(*start, piece=PieceType.Wave)
        scene.board.set_piece(*start_U, piece=PieceType.Unicorn)

        scene.board.set_piece(7, 5, piece=PieceType.Pawn)
        scene.board.set_piece(7, 6, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)

        scene.board.set_piece(7, 8, piece=-PieceType.Pawn)
        scene.board.set_piece(8, 8, piece=-PieceType.Pawn)
        scene.board.set_piece(9, 8, piece=-PieceType.Pawn)

        scene.board.set_piece(9, 13, piece=-PieceType.Wave)

        #
        # Wave activation by Unicorn
        scene.append_arrow( *(start_U + start), mark_type=MarkType.Blocked )

        #
        # short --> (-1, 2) direction
        # long --> (3, 2) direction

        rels = [(-1, 2), (3, 2), ]

        arr = GS.gen_steps(start=start, rels=rels, include_prev=True, bounds=scene.board_view.get_position_limits())
        for i, pos in enumerate( arr() ):
            mark_type = MarkType.Legal if i % 2 == 0 else \
                        MarkType.Action
            scene.append_arrow( *pos, mark_type=mark_type )

        txt = GS.gen_steps(start=start, rels=rels, include_prev=False, bounds=scene.board_view.get_position_limits())
        for i, pos in enumerate( txt() ):
            mark_type = MarkType.Legal if i % 2 == 0 else \
                        MarkType.Action
            corner = Corner.UpperRight if i % 2 == 0 else Corner.UpperLeft
            scene.append_text( str(i+1), *pos, corner=corner, mark_type=mark_type )

        #
        # forbidden directions change

        # (-1, 2) is ok, i.e. direction "7", here: 10, 13 --> 8, 14
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, to_remove=((-1, 2), ) ) )
        start_X = (10, 11)

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


    def scn_mv_36_wave_off_board(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_36_wave_off_board', bt, x=4, y=1, reverse_off_board_field_colors=False)

        rect = (0.05, 1.0, 0.6, 0.45)

        start = (14, 2)
        scene.board.set_piece(*start, piece=-PieceType.Wave)

        start_U = (10, 1)
        scene.board.set_piece(*start_U, piece=-PieceType.Unicorn)

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
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # short
        scene.append_arrow( *coords(), mark_type=MarkType.Illegal ) # long

        scene.append_text("1", 14, 9, corner=Corner.UpperLeft)
        scene.append_text("2", 15, 12, corner=Corner.UpperLeft)

        return scene


    def scn_mv_37_activating_pyramid_by_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_37_activating_pyramid_by_pawn', bt)

        #
        # left side

        left_R = (6, 14)
        scene.board.set_piece(*left_R, piece=PieceType.Rook)

        left_W_1 = (6, 1)
        scene.board.set_piece(*left_W_1, piece=PieceType.Wave)

        left_P = (2, 1)
        scene.board.set_piece(*left_P, piece=PieceType.Pawn)

        left_W_2 = (1, 2)
        scene.board.set_piece(*left_W_2, piece=PieceType.Wave)

        left_A = (4, 5)
        scene.board.set_piece(*left_A, piece=PieceType.Pyramid)

        #
        # R --> W1

        gen = GS.gen_steps( [(0, -1), ], left_R, include_prev=True, count=13 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index >= 12 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        #
        # W1 --> P

        coords = GS.gen_next( GS.gen_steps(start=left_W_1, rels=[(-1, 0), ], include_prev=True) )

        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # P --> W2

        scene.append_arrow( *(left_P + left_W_2), mark_type=MarkType.Action )

        #
        # W2 --> A

        gen_2 = GS.gen_steps( [(1, 1), ], left_W_2, include_prev=True, count=3 )
        for index, coords in enumerate( gen_2() ):
            mark_type = MarkType.Action if index == 2 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_text("1", *left_W_1, corner=Corner.LowerLeft)
        scene.append_text("2", *left_W_2, corner=Corner.LowerLeft)


        #
        # right side

        right_R = (14, 14)
        scene.board.set_piece(*right_R, piece=PieceType.Rook)

        right_W_1 = (14, 1)
        scene.board.set_piece(*right_W_1, piece=PieceType.Wave)

        right_P = (12, 1)
        scene.board.set_piece(*right_P, piece=PieceType.Pawn)

        right_W_2 = (12, 3)
        scene.board.set_piece(*right_W_2, piece=PieceType.Wave)

        right_A = (12, 6)
        scene.board.set_piece(*right_A, piece=PieceType.Pyramid)

        #
        # R --> W1

        gen_3 = GS.gen_steps( [(0, -1), ], right_R, include_prev=True, count=13 )
        for index, coords in enumerate( gen_3() ):
            mark_type = MarkType.Action if index >= 12 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        #
        # W1 --> P

        coords_3 = GS.gen_next( GS.gen_steps(start=right_W_1, rels=[(-1, 0), ], include_prev=True) )

        scene.append_arrow( *coords_3() )
        scene.append_arrow( *coords_3(), mark_type=MarkType.Action )

        #
        # P --> W2

        coords_4 = GS.gen_next( GS.gen_steps(start=right_P, rels=[(0, 1), ], include_prev=True) )

        scene.append_arrow( *coords_4() )
        scene.append_arrow( *coords_4(), mark_type=MarkType.Action )

        #
        # W2 --> A

        gen_4 = GS.gen_steps( [(0, 1), ], right_W_2, include_prev=True, count=3 )
        for index, coords in enumerate( gen_4() ):
            mark_type = MarkType.Illegal if index == 2 else \
                        MarkType.Action if index == 9 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        scene.append_text("4", *right_W_1, corner=Corner.LowerLeft)
        scene.append_text("5", *right_W_2, corner=Corner.LowerLeft)

        return scene


    def scn_mv_38_activating_pyramid_cascade_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_38_activating_pyramid_cascade_pawn', bt)

        #
        # left side

        left_R = (6, 14)
        scene.board.set_piece(*left_R, piece=PieceType.Rook)

        left_W_1 = (6, 1)
        scene.board.set_piece(*left_W_1, piece=PieceType.Wave)

        left_P = (2, 1)
        scene.board.set_piece(*left_P, piece=PieceType.Pawn)

        left_W_2 = (1, 2)
        scene.board.set_piece(*left_W_2, piece=PieceType.Wave)

        left_N = (3, 4)
        scene.board.set_piece(*left_N, piece=PieceType.Knight)

        left_W_3 = (1, 5)
        scene.board.set_piece(*left_W_3, piece=PieceType.Wave)

        left_A = (3, 9)
        scene.board.set_piece(*left_A, piece=PieceType.Pyramid)

        #
        # R --> W1

        gen = GS.gen_steps( [(0, -1), ], left_R, include_prev=True, count=13 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index >= 12 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        #
        # W1 --> P

        coords = GS.gen_next( GS.gen_steps(start=left_W_1, rels=[(-1, 0), ], include_prev=True) )

        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # P --> W2

        scene.append_arrow( *(left_P + left_W_2), mark_type=MarkType.Action )

        #
        # W2 --> N

        coords_2 = GS.gen_next( GS.gen_steps(start=left_W_2, rels=[(1, 1), ], include_prev=True) )

        scene.append_arrow( *coords_2() )
        scene.append_arrow( *coords_2(), mark_type=MarkType.Action )

        #
        # N --> W3

        coords_3 = GS.gen_next( GS.gen_steps(start=left_N, rels=[(-2, 1), ], include_prev=True) )

        scene.append_arrow( *coords_3(), mark_type=MarkType.Action )

        #
        # W3 --> A

        coords_4 = GS.gen_next( GS.gen_steps(start=left_W_3, rels=[(1, 2), ], include_prev=True) )

        scene.append_arrow( *coords_4() )
        scene.append_arrow( *coords_4(), mark_type=MarkType.Action )

        scene.append_text("1", *left_W_1, corner=Corner.LowerLeft)
        scene.append_text("2", *left_W_2, corner=Corner.LowerLeft)
        scene.append_text("3", *left_W_3, corner=Corner.LowerLeft)


        #
        # right side

        right_R = (14, 14)
        scene.board.set_piece(*right_R, piece=PieceType.Rook)

        right_W_1 = (14, 1)
        scene.board.set_piece(*right_W_1, piece=PieceType.Wave)

        right_P = (12, 1)
        scene.board.set_piece(*right_P, piece=PieceType.Pawn)

        right_W_2 = (12, 3)
        scene.board.set_piece(*right_W_2, piece=PieceType.Wave)

        right_N = (12, 6)
        scene.board.set_piece(*right_N, piece=PieceType.Knight)

        right_W_3 = (10, 7)
        scene.board.set_piece(*right_W_3, piece=PieceType.Wave)

        right_A = (12, 11)
        scene.board.set_piece(*right_A, piece=PieceType.Pyramid)

        #
        # R --> W1

        gen_3 = GS.gen_steps( [(0, -1), ], right_R, include_prev=True, count=13 )
        for index, coords in enumerate( gen_3() ):
            mark_type = MarkType.Action if index >= 12 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        #
        # W1 --> P

        coords_5 = GS.gen_next( GS.gen_steps(start=right_W_1, rels=[(-1, 0), ], include_prev=True) )

        scene.append_arrow( *coords_5() )
        scene.append_arrow( *coords_5(), mark_type=MarkType.Action )

        #
        # P --> W2

        coords_6 = GS.gen_next( GS.gen_steps(start=right_P, rels=[(0, 1), ], include_prev=True) )

        scene.append_arrow( *coords_6() )
        scene.append_arrow( *coords_6(), mark_type=MarkType.Action )

        #
        # W2 --> N

        coords_7 = GS.gen_next( GS.gen_steps(start=right_W_2, rels=[(0, 1), ], include_prev=True) )

        scene.append_arrow( *coords_7() )
        scene.append_arrow( *coords_7() )
        scene.append_arrow( *coords_7(), mark_type=MarkType.Action )

        #
        # N --> W3

        coords_8 = GS.gen_next( GS.gen_steps(start=right_N, rels=[(-2, 1), ], include_prev=True) )

        scene.append_arrow( *coords_8(), mark_type=MarkType.Action )

        #
        # W3 --> A

        coords_9 = GS.gen_next( GS.gen_steps(start=right_W_3, rels=[(1, 2), ], include_prev=True) )

        scene.append_arrow( *coords_9() )
        scene.append_arrow( *coords_9(), mark_type=MarkType.Action )

        scene.append_text("4", *right_W_1, corner=Corner.LowerLeft)
        scene.append_text("5", *right_W_2, corner=Corner.LowerLeft)
        scene.append_text("6", *right_W_3, corner=Corner.LowerLeft)

        return scene


    def scn_mv_39_activated_piece_check_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_39_activated_piece_check_init', bt, height=9.7)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_k = (3, 6)
        scene.board.set_piece(*start_k, piece=-PieceType.King)

        start_W_A = GS.add_tpl( start_k, 4, 2 )
        scene.board.set_piece(*start_W_A, piece=PieceType.Wave)

        start_R_1 = GS.add_tpl( start_k, 2, 2 )
        scene.board.set_piece(*start_R_1, piece=PieceType.Rook)

        start_W_C = GS.add_tpl( start_k, 0, 2 )
        scene.board.set_piece(*start_W_C, piece=PieceType.Wave)

        start_R_2 = GS.add_tpl( start_k, -2, 2 )
        scene.board.set_piece(*start_R_2, piece=PieceType.Rook)

        start_Q = GS.add_tpl( start_W_A, 5, -5 )
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        #
        # Q --> W 1

        coords = GS.gen_steps(start=start_Q, rels=[(-1, 1), ], include_prev=True, count=5)
        for i, pos in enumerate( coords() ):
            mark_type = MarkType.Action if i >= 4 else \
                        MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # W 1 --> R 1

        coords = GS.gen_steps(start=start_W_A, rels=[(-1, 0), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            mark_type = MarkType.Action if i >= 1 else \
                        MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # R 1 --> W 2

        coords = GS.gen_steps(start=start_R_1, rels=[(-1, 0), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            mark_type = MarkType.Action if i >= 1 else \
                        MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # W 2 --> R 2

        coords = GS.gen_steps(start=start_W_C, rels=[(-1, 0), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            mark_type = MarkType.Action if i >= 1 else \
                        MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # R 2 -->

        coords = GS.gen_steps(start=start_R_2, rels=[(0, -1), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            scene.append_arrow( *pos )

        #
        # labels

        scene.append_text("A", *start_W_A, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("B", *start_R_1, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("C", *start_W_C, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("D", *start_R_2, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        scene.append_text("R", *GS.add_tpl(start_R_2, 0, -2), corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # Q labels

        coords_2 = GS.gen_steps(start=GS.add(start_Q, (-1, 1)), rels=[(1, -1), ], include_prev=False, count=3)
        for i, pos_2 in enumerate( coords_2() ):
            mark_type = MarkType.Action if i > 0 else \
                        MarkType.Legal
            scene.append_text( "Q"+str(i+1), *pos_2, corner=Corner.UpperRight, mark_type=mark_type, rect=rect )

        return scene


    def scn_mv_40_activated_piece_check_cascade(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_40_activated_piece_check_cascade', bt, height=9.7)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_k = (3, 6)
        scene.board.set_piece(*start_k, piece=-PieceType.King)

        start_Q = GS.add_tpl( start_k, 4, 2 )
        scene.board.set_piece(*start_Q, piece=PieceType.Queen)

        start_W_A = GS.add_tpl( start_k, 2, 2 )
        scene.board.set_piece(*start_W_A, piece=PieceType.Wave)

        start_R_1 = GS.add_tpl( start_k, 0, 2 )
        scene.board.set_piece(*start_R_1, piece=PieceType.Rook)

        start_W_C = GS.add_tpl( start_k, -2, 2 )
        scene.board.set_piece(*start_W_C, piece=PieceType.Wave)

        start_R_2 = GS.add_tpl( start_k, -2, 0 )
        scene.board.set_piece(*start_R_2, piece=PieceType.Rook)

        #
        # -- > Q

        coords = GS.gen_steps(end=start_Q, rels=[(-1, 1), ], include_prev=True, count=5)
        for i, pos in enumerate( coords() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        #
        # Q -- > W 1

        coords = GS.gen_steps(end=start_W_A, rels=[(-1, 0), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        #
        # W 1 --> R 1

        coords = GS.gen_steps(end=start_R_1, rels=[(-1, 0), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        #
        # R 1 --> W 2

        coords = GS.gen_steps(end=start_W_C, rels=[(-1, 0), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        #
        # W 2 --> R 2

        coords = GS.gen_steps(end=start_R_2, rels=[(0, -1), ], include_prev=True, count=2)
        for i, pos in enumerate( coords() ):
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        #
        # R 2 --> k -->

        coords = GS.gen_steps(start=start_R_2, rels=[(1, 0), ], include_prev=True, count=3)
        for i, pos in enumerate( coords() ):
            mark_type = MarkType.Action if i > 0 else \
                        MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        #
        # labels

        scene.append_text("A", *start_W_A, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("B", *start_R_1, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("C", *start_W_C, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("D", *start_R_2, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        scene.append_text("R", *start_R_2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        #
        # Q labels

        coords_2 = GS.gen_steps(start=GS.add_tpl(start_Q, 4, -4), rels=[(1, -1), ], include_prev=False, count=3)
        for i, pos_2 in enumerate( coords_2() ):
            mark_type = MarkType.Action if i > 0 else \
                        MarkType.Legal
            scene.append_text( "Q"+str(i+1), *pos_2, corner=Corner.UpperRight, mark_type=mark_type, rect=rect )

        #
        # R 2 labels

        coords_2 = GS.gen_steps(start=start_R_2, rels=[(1, 0), ], include_prev=False, count=3)
        for i, pos_2 in enumerate( coords_2() ):
            mark_type = MarkType.Action if i > 0 else \
                        MarkType.Legal
            scene.append_text( "R"+str(i+1), *pos_2, corner=Corner.UpperRight, mark_type=mark_type, rect=rect )

        return scene
