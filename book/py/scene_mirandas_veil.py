#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2017 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import enum
from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneMirandasVeilMixin:

    #
    # Activation

    def scn_mv_001_wave_activation_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_001_wave_activation_init', bt)

        start_E = (1, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (6, 12)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # G --> W
        coords_G_W = GS.gen_steps( start=start_E, rels=[(1, 2), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_G_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_mv_002_wave_activated(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_002_wave_activated', bt)

        start_E = (6, 12)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        arr = GS.gen_multi_steps( GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_E, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, pos in enumerate( arr() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        return scene

    #
    # Activating pieces

    def scn_mv_003_pawn_pass_through(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_003_pawn_pass_through', bt)

        start_E = (1, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (6, 12)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_p = (8, 8)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # G --> W
        coords_G_W = GS.gen_steps( start=start_E, rels=[(1, 2), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_G_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --p-->
        coords_W_p = GS.gen_steps( start=start_W, rels=[(1, -2), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_W_p() ):
            mark_type = MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_mv_004_wave_activating_rook(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_004_wave_activating_rook', bt)

        start_E = (1, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (6, 12)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_p = (8, 8)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_R = (9, 6)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        # G --> W
        coords_G_W = GS.gen_steps( start=start_E, rels=[(1, 2), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_G_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W -p-> R

        coords_W_p_R = GS.gen_steps( start=start_W, rels=[(1, -2), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_W_p_R() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_mv_005_rook_activated(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_005_rook_activated', bt)

        start_E = (6, 12)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (9, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_p = (8, 8)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        for rel in GS.DEFAULT_ROOK_REL_MOVES:
            coords_R = GS.gen_steps( start=start_W, rels=[rel, ], include_prev=True, bounds=scene.board_view.get_position_limits() )
            for i, arrow in enumerate( coords_R() ):
                mark_type = MarkType.Legal if i < 5 else \
                            MarkType.Blocked
                scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_mv_006_rook_captures(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_006_rook_captures', bt)

        start_E = (1, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_W = (6, 12)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_p = (8, 8)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_R = (9, 6)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_n = (9, 3)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_P = (11, 6)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_b = (13, 6)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_e = (9, 14)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        # G --> W
        coords_G_W = GS.gen_steps( start=start_E, rels=[(1, 2), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_G_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W -p-> R
        coords_W_p_R = GS.gen_steps( start=start_W, rels=[(1, -2), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_W_p_R() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R --> n
        coords_R_n = GS.gen_steps( start=start_R, rels=[(0, -1), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_R_n() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Blocked if i >= 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R --|P .. b
        coords_R_n = GS.gen_steps( start=start_R, rels=[(1, 0), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_R_n() ):
            mark_type = MarkType.Blocked if i >= 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R -->|-- g
        coords_R_n = GS.gen_steps( start=start_R, rels=[(0, 1), ], include_prev=True, count=9 )
        for i, arrow in enumerate( coords_R_n() ):
            mark_type = MarkType.Blocked if i >= 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Wave is transparent

    def scn_mv_007_wave_is_transparent(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_007_wave_is_transparent', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (14, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_w = (12, 3)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_W = (9, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_e = (6, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_p = (4, 11)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # Q --> W --> w --> g -->| p
        coords_Q_W_w_g_p = GS.gen_steps( start=start_Q, rels=[(-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_Q_W_w_g_p() ):
            mark_type = MarkType.Action if i in [ 1, 4, 7 ] else \
                        MarkType.Legal if i < 7 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_mv_008_wave_cant_be_pinned(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_008_wave_cant_be_pinned', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_e = (9, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_K = (5, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_W = (7, 5)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # g --> K
        coords_g_K = GS.gen_steps( start=start_e, rels=[(-1, -2), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_g_K() ):
            mark_type = MarkType.Illegal if i == 3 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Castling is not blocked by Wave

    def scn_mv_009_wave_no_block_castling_king(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_009_wave_no_block_castling_king', bt, height=1.7 )

        start_K = (8, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        # start_R_B = (15, 0)
        # scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_W = (5, 0)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # |<-- K
        coords = GS.gen_steps( start=start_K, rels=[(-1, 0), ], include_prev=True, count=6 )
        for i, step in enumerate( coords() ):
            if i == 0:
                continue

            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal

            scene.append_arrow( *step, mark_type=mark_type )

        # <<- <-- K
        coords = GS.gen_steps( start=start_K, rels=[(-1, 0), ], include_prev=False, count=6 )
        for i, pos in enumerate( coords() ):
            if i == 0:
                continue

            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal

            scene.append_text( str( i ), *pos, corner=Corner.UpperLeft, mark_type=mark_type )

        scene.append_text( "K", *start_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        # scene.append_text( "A", *start_R_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        # scene.append_text( "B", *start_R_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_010_wave_no_block_castling_rook(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_010_wave_no_block_castling_rook', bt, height=1.7 )

        prev_K = (8, 0)
        start_K = (3, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        # start_R_B = (15, 0)
        # scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_W = (5, 0)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # R -->|
        coords = GS.gen_steps( start=start_R_A, rels=[(1, 0), ], include_prev=True, count=4 )
        for i, step in enumerate( coords() ):
            mark_type = MarkType.Blocked if i == 2 else \
                        MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *step, mark_type=mark_type )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        # scene.append_text( "A", *start_R_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        # scene.append_text( "B", *start_R_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_011_wave_block_castling_rook(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_011_wave_block_castling_rook', bt, height=1.7 )

        prev_K = (8, 0)
        start_K = (4, 0)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_R_A = (0, 0)
        scene.board.set_piece( *start_R_A, piece=PieceType.Rook )

        # start_R_B = (15, 0)
        # scene.board.set_piece( *start_R_B, piece=PieceType.Rook )

        start_W = (5, 0)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        # R -->|
        coords = GS.gen_steps( start=start_R_A, rels=[(1, 0), ], include_prev=True, count=5 )
        for i, step in enumerate( coords() ):
            mark_type = MarkType.Blocked if i == 3 else \
                        MarkType.Illegal if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *step, mark_type=mark_type )

        scene.append_text( "K", *prev_K, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        # scene.append_text( "A", *start_R_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        # scene.append_text( "B", *start_R_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    #
    # Single-step pieces and transparency

    def scn_mv_012_pawn_blocked_by_opponents_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_012_pawn_blocked_by_opponents_wave', bt, width=6.3, height=8.3 )

        #
        # Pawn A

        start_P_A = (0, 2)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_W_C = (0, 3)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (1, 3)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        scene.append_arrow( *( start_P_A + start_W_C ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_W_C, 0, 1 ), mark_type=MarkType.Illegal )

        scene.append_arrow( *( start_P_A + start_W_D ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_W_D, 1, 1 ), mark_type=MarkType.Illegal )

        #
        # Pawn B

        start_P_B = (5, 5)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_w_E = (5, 6)
        scene.board.set_piece( *start_w_E, piece=-PieceType.Wave )

        start_w_F = (4, 6)
        scene.board.set_piece( *start_w_F, piece=-PieceType.Wave )

        scene.append_arrow( *( start_P_B + start_w_E ), mark_type=MarkType.Blocked )
        scene.append_arrow( *GS.append_pos_rel( start_w_E, 0, 1 ), mark_type=MarkType.Illegal )

        scene.append_arrow( *( start_P_B + start_w_F ), mark_type=MarkType.Action )
        scene.append_arrow( *GS.append_pos_rel( start_w_F, -1, 1 ), mark_type=MarkType.Illegal )


        scene.append_text( "A", *start_P_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_P_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_013_pawn_not_blocked_by_opponents_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_013_pawn_not_blocked_by_opponents_wave', bt, width=6.3, height=8.3 )

        #
        # Pawn A

        start_P_A = (1, 1)
        scene.board.set_piece( *start_P_A, piece=PieceType.Pawn )

        start_W_C = (1, 4)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        # P(A) -->
        coords = GS.gen_steps( start=start_P_A, rels=[ (0, 1), ], include_prev=True, count=6 )
        for i, step in enumerate( coords() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *step, mark_type=mark_type )

        #
        # Pawn B

        start_P_B = (4, 1)
        scene.board.set_piece( *start_P_B, piece=PieceType.Pawn )

        start_w_E = (4, 4)
        scene.board.set_piece( *start_w_E, piece=-PieceType.Wave )

        # P(B) -->
        coords = GS.gen_steps( start=start_P_B, rels=[ (0, 1), ], include_prev=True, count=6 )
        for i, step in enumerate( coords() ):
            mark_type = MarkType.Blocked if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *step, mark_type=mark_type )


        scene.append_text( "A", *start_P_A, corner=Corner.UpperLeft, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_P_B, corner=Corner.UpperRight, mark_type=MarkType.Legal )

        return scene

    #
    # Piece blocked

    def scn_mv_014_wave_no_activating_blocked_piece(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_014_wave_no_activating_blocked_piece', bt)

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

    #
    # Movement

    def scn_mv_015_bishop_activating_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_015_bishop_activating_wave', bt)

        start_B = (3, 6)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_W = (7, 10)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        # B --> W
        coords_B_W = GS.gen_steps( start=start_B, rels=[(1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_B_W() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    def scn_mv_016_wave_activated_by_bishop(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_016_wave_activated_by_bishop', bt)

        start_B = (7, 10)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_P = (5, 12)
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)

        start_n = (10, 7)
        scene.board.set_piece(*start_n, piece=-PieceType.Knight)

        # W --> (1, 1)
        coords_W_0 = GS.gen_steps( start=start_B, rels=[(1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_0() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W --> (-1, 1)
        coords_W_1 = GS.gen_steps( start=start_B, rels=[(-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_1() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (-1, -1)
        coords_W_2 = GS.gen_steps( start=start_B, rels=[(-1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_2() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        # W --> (1, -1)
        coords_W_3 = GS.gen_steps( start=start_B, rels=[(1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, arrow in enumerate( coords_W_3() ):
            mark_type = MarkType.Blocked if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Activated by Knight

    def scn_mv_017_knight_activating_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_017_knight_activating_wave', bt)

        start_N = (3, 6)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        start_W = (5, 7)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        # N --> W
        scene.append_arrow( *( start_N + start_W ), mark_type=MarkType.Action )

        return scene

    def scn_mv_018_wave_activated_by_knight(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_018_wave_activated_by_knight', bt)

        start_N = (5, 7)
        scene.board.set_piece(*start_N, piece=PieceType.Knight)

        # W <-<-<--- * --->->->
        arr = GS.gen_multi_steps( GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_N, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, pos in enumerate( arr() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        return scene

    #
    # Activated by King

    def scn_mv_019_king_activating_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_019_king_activating_wave', bt)

        start_K = (3, 6)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        start_W = (4, 7)
        scene.board.set_piece(*start_W, piece=PieceType.Wave)

        # N --> W
        scene.append_arrow( *( start_K + start_W ), mark_type=MarkType.Action )

        return scene

    def scn_mv_020_wave_activated_by_king(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_020_wave_activated_by_king', bt)

        start_K = (4, 7)
        scene.board.set_piece(*start_K, piece=PieceType.King)

        # W <-<-<--- * --->->->
        arr = GS.gen_multi_steps( GS.DEFAULT_KING_MULTI_REL_MOVES, start=start_K, include_prev=True, bounds=scene.board_view.get_position_limits() )
        for i, pos in enumerate( arr() ):
            scene.append_arrow( *pos, mark_type=MarkType.Legal )

        return scene

    #
    # Activated by stepping Pawn

    def scn_mv_021_wave_activation_by_step_pawn( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_021_wave_activation_by_step_pawn', bt )

        start_P = (5, 2)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (5, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A_A = (2, 6)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_A_B = (5, 9)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        scene.board.set_piece( 7, 5, piece=-PieceType.Pawn )
        scene.board.set_piece( 5, 11, piece=PieceType.Knight )
        scene.board.set_piece( 5, 14, piece=-PieceType.Wave )

        start_B = (9, 13)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # P(A) --> W
        scene.append_arrow( *( start_P + start_W ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_A_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )
        scene.append_text( "B", *start_A_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    def scn_mv_022_wave_activated_by_step_pawn( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_022_wave_activated_by_step_pawn', bt )

        # prev_P_A = (5, 2)
        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        prev_W = (5, 3)
        start_W = prev_W
        # scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A_A = (2, 6)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_A_B = (5, 9)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        scene.board.set_piece( 7, 5, piece=-PieceType.Pawn )
        scene.board.set_piece( 5, 11, piece=PieceType.Knight )
        scene.board.set_piece( 5, 14, piece=-PieceType.Wave )

        start_B = (9, 13)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # W --> (right)
        coords_W_1_ = GS.gen_steps( start=start_W, rels=[ (1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_1_() ):
            mark_type = MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (forward)
        coords_W_2_ = GS.gen_steps( start=start_W, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_2_() ):
            mark_type = MarkType.Blocked if i == 5 else \
                        MarkType.Action if i in [ 7, 10 ] else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (left)
        coords_W_3_ = GS.gen_steps( start=start_W, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_3_() ):
            mark_type = MarkType.Blocked if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> *
        start_ = (12, 10)

        # W --> * --> (forward)
        coords_W_4_ = GS.gen_steps( start=start_, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_4_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        # W --> * --> (left)
        coords_W_5_ = GS.gen_steps( start=start_, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_5_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_A_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )
        scene.append_text( "B", *start_A_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    #
    # Activated by capturing Pawn

    def scn_mv_023_wave_activation_by_capture_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_023_wave_activation_by_capture_pawn', bt)

        start_P = (4, 2)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W = (5, 3)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A_A = (2, 6)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_A_B = (5, 9)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        scene.board.set_piece( 7, 5, piece=-PieceType.Pawn )
        scene.board.set_piece( 5, 11, piece=PieceType.Knight )
        scene.board.set_piece( 5, 14, piece=-PieceType.Wave )

        start_B = (9, 13)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # P(A) --> W
        scene.append_arrow( *( start_P + start_W ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_A_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )
        scene.append_text( "B", *start_A_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    def scn_mv_024_wave_activated_by_capture_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_024_wave_activated_by_capture_pawn', bt)

        # prev_P_A = (5, 2)
        start_P = (5, 3)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        prev_W = (5, 3)
        start_W = prev_W
        # scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A_A = (2, 6)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_A_B = (5, 9)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        scene.board.set_piece( 7, 5, piece=-PieceType.Pawn )
        scene.board.set_piece( 5, 11, piece=PieceType.Knight )
        scene.board.set_piece( 5, 14, piece=-PieceType.Wave )

        start_B = (9, 13)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # W --> (right)
        coords_W_1_ = GS.gen_steps( start=start_W, rels=[ (1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_1_() ):
            mark_type = MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (forward)
        coords_W_2_ = GS.gen_steps( start=start_W, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_2_() ):
            mark_type = MarkType.Blocked if i == 5 else \
                        MarkType.Action if i in [ 7, 10 ] else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> (left)
        coords_W_3_ = GS.gen_steps( start=start_W, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_3_() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> *
        start_ = (12, 10)

        # W --> * --> (forward)
        coords_W_4_ = GS.gen_steps( start=start_, rels=[ (0, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_4_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        # W --> * --> (left)
        coords_W_5_ = GS.gen_steps( start=start_, rels=[ (-1, 1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=4 )
        for i, arrow in enumerate( coords_W_5_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_A_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )
        scene.append_text( "B", *start_A_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    #
    # Activated by Unicorn

    def scn_mv_025_wave_same_color(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_025_wave_same_color', bt, y=1, width=7, height=7)

        start = (3, 4)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal)

        return scene

    def scn_mv_026_wave_opposite_color(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_026_wave_opposite_color', bt, width=11, height=11)

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

    def scn_mv_027_wave_activation_by_unicorn_first_step(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_027_wave_activation_by_unicorn_first_step', bt)

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

    def scn_mv_028_wave_activation_by_unicorn_second_step(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_028_wave_activation_by_unicorn_second_step', bt)

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

    def scn_mv_029_wave_activation_by_unicorn_complete(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_029_wave_activation_by_unicorn_complete', bt)

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

    def scn_mv_030_wave_off_board(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_030_wave_off_board', bt, x=4, y=1, reverse_off_board_field_colors=False)

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

    #
    # Cascading Waves

    def scn_mv_031_wave_cascading_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_031_wave_cascading_init', bt)

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

    def scn_mv_032_wave_cascading_steps(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_032_wave_cascading_steps', bt)

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

    def scn_mv_033_wave_cascading_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_033_wave_cascading_end', bt)

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

    def scn_mv_034_wave_no_momentum_no_activating(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_034_wave_no_momentum_no_activating', bt)

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

    def scn_mv_035_single_step_piece_momentum(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_035_single_step_piece_momentum', bt)

        start_B = (6, 2)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (2, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_N = (4, 8)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W_B = (6, 7)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_R = (10, 9)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        # B --> W(A)
        coords_B_WA = GS.gen_steps( start=start_B, rels=[(-1, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_B_WA() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> N
        coords_WA_N = GS.gen_steps( start=start_W_A, rels=[(1, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_WA_N() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # N --> W(B)
        scene.append_arrow( *( start_N + start_W_B ), mark_type=MarkType.Action )

        # W(B) --> R
        coords_WB_R = GS.gen_steps( start=start_W_B, rels=[(2, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_WB_R() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )

        # <--<--R-->-->
        arr = GS.gen_multi_steps( GS.DEFAULT_ROOK_MULTI_REL_MOVES, start=start_R, include_prev=True, count=4 ) # bounds=scene.board_view.get_position_limits() #
        for i, pos in enumerate( arr() ):
            mark_type = MarkType.Blocked if ( i % 4 ) >= 3 else \
                        MarkType.Legal
            scene.append_arrow( *pos, mark_type=mark_type )

        # momentum received
        scene.append_text( "4", *start_W_A, corner=Corner.LowerRightFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "4", *start_N, corner=Corner.LowerRight, mark_type=MarkType.Action )
        scene.append_text( "3", *start_W_B, corner=Corner.LowerRight, mark_type=MarkType.Action )
        scene.append_text( "3", *start_R, corner=Corner.LowerRight, mark_type=MarkType.Action )

        return scene

    #
    # Activating Pawn

    def scn_mv_036_activating_rush_pawn_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_036_activating_rush_pawn_init', bt) # , width=5, height=8)

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

    def scn_mv_037_activating_rush_pawn_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_037_activating_rush_pawn_end', bt)

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
    # Activating Pyramid

    def scn_mv_038_not_activating_pyramid_by_wave( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_038_not_activating_pyramid_by_wave', bt, width=6.3, height=9.3 )

        start_B = (1, 1)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W = (4, 4)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A = (2, 6)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen = GS.gen_steps( [(1, 1), ], start_B, include_prev=True, count=3 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 2 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen = GS.gen_steps( [(-1, 1), ], start_W, include_prev=True, count=4 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Illegal if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        return scene

    def scn_mv_039_activating_pyramid_cascade_pawn(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_039_activating_pyramid_cascade_pawn', bt, width=6.3, height=9.3 )

        start_B = (1, 1)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W = (4, 4)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_P = (2, 6)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_A = (3, 7)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        gen = GS.gen_steps( [(1, 1), ], start_B, include_prev=True, count=3 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 2 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        gen = GS.gen_steps( [(-1, 1), ], start_W, include_prev=True, count=2 )
        for index, coords in enumerate( gen() ):
            mark_type = MarkType.Action if index == 1 else \
                        MarkType.Legal
            scene.append_arrow( *coords, mark_type=mark_type )

        coords = GS.gen_next( GS.gen_steps(start=start_P, rels=[(1, 1), ], include_prev=True) )
        # scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        # scene.append_arrow( *coords() )

        return scene

    #
    # Reactivating pieces

    def scn_mv_043_reactivating_piece_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_043_reactivating_piece_init', bt)

        start_R = (2, 1)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W_A = (7, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_N = (7, 4)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W_B = (5, 5)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_B = (8, 11)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_C = (11, 8)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_E = (9, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        # R --> W(A)
        coords_R_WA = GS.gen_steps( start=start_R, rels=[(1, 0), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_R_WA() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> N
        coords_WA_N = GS.gen_steps( start=start_W_A, rels=[(0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WA_N() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # N --> W(B)
        scene.append_arrow( *( start_N + start_W_B ), mark_type=MarkType.Action )

        # W(B) --> B
        coords_WB_N = GS.gen_steps( start=start_W_B, rels=[(1, 2), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WB_N() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # B --> W(C)
        coords_B_WC = GS.gen_steps( start=start_B, rels=[(1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_B_WC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_044_reactivating_piece_steps(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_044_reactivating_piece_steps', bt)

        start_R_prev = (2, 1)

        start_R = (7, 1)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W_A = (7, 4)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_N = (5, 5)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        start_W_B = (8, 11)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_B = (11, 8)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_E = (9, 2)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        # --> R
        coords_R_WA = GS.gen_steps( end=start_R, rels=[(1, 0), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_R_WA() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # R --> W(A)
        coords_WA_N = GS.gen_steps( end=start_W_A, rels=[(0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WA_N() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(A) --> N
        scene.append_arrow( *( start_W_A + start_N ), mark_type=MarkType.Blocked )

        # N --> W(B)
        coords_WB_N = GS.gen_steps( end=start_W_B, rels=[(1, 2), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WB_N() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(B) --> B
        coords_B_WC = GS.gen_steps( end=start_B, rels=[(1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_B_WC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        # W(C) is "in the air"
        # scene.append_text( "C", *start_W_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        # R, starting position, previous scene
        scene.append_text( "R", *start_R_prev, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        scene.append_text( "a", *start_R, corner=Corner.UpperLeft, mark_type=MarkType.Illegal )

        # W(C) --> W(A)
        coords_WC_WA = GS.gen_steps( start=start_B, rels=[(-1, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_WC_WA() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> G
        coords_WA_w = GS.gen_steps( start=start_W_A, rels=[(1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_WA_w() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # G --->
        coords_w_ = GS.gen_steps( start=start_E, rels=[(2, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_w_() ):
            mark_type = MarkType.Legal if i == 0 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        return scene

    #
    # Cascading pinned piece

    def scn_mv_045_pinned_piece_cascaded_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_045_pinned_piece_cascaded_init', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_e = (9, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_K = (5, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_B = (10, 14)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_1 = (4, 8)
        scene.board.set_piece( *start_W_1, piece=PieceType.Wave )

        start_Q = (7, 5)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_2 = (11, 5)
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_N = (8, 2)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # g --> K
        coords_g_K = GS.gen_steps( start=start_e, rels=[(-1, -2), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_g_K() ):
            mark_type = MarkType.Legal if i == 0 else \
                        MarkType.Action if i == 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text("A", *start_W_1, corner=Corner.UpperLeft)
        scene.append_text("B", *start_W_2, corner=Corner.UpperRight)

        return scene

    def scn_mv_046_pinned_piece_cascaded_1(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_046_pinned_piece_cascaded_1', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_e = (9, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_K = (5, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_B = (10, 14)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_1 = (4, 8)
        scene.board.set_piece( *start_W_1, piece=PieceType.Wave )

        start_Q = (7, 5)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_2 = (11, 5)
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_N = (8, 2)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # B --> W 1
        coords_B_W1 = GS.gen_steps( start=start_B, rels=[(-1, -1), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_B_W1() ):
            mark_type = MarkType.Action if i == 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W 1 --> Q
        coords_W1_Q = GS.gen_steps( start=start_W_1, rels=[(1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_W1_Q() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q --> W 2
        coords_Q_W2 = GS.gen_steps( start=start_Q, rels=[(1, 0), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_Q_W2() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W 2 --> N
        coords_W2_N = GS.gen_steps( start=start_W_2, rels=[(-1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_W2_N() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W 2 --> N
        scene.append_arrow( *( GS.append_pos_rel( start_N, -2, 1 ) ), mark_type=MarkType.Action )

        scene.append_text("A", *start_W_1, corner=Corner.UpperLeft)
        scene.append_text("B", *start_W_2, corner=Corner.UpperRight)

        return scene

    def scn_mv_047_pinned_piece_cascaded_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_047_pinned_piece_cascaded_end', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_e = (9, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_K = (5, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_B = (4, 8) # (10, 14)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_1 = (7, 5) # (4, 8)
        scene.board.set_piece( *start_W_1, piece=PieceType.Wave )

        start_Q = (11, 5) # (7, 5)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_2 = (8, 2) # (11, 5)
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_N = (6, 3) # (8, 2)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # g --> K
        coords_g_K = GS.gen_steps( start=start_e, rels=[(-1, -2), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_g_K() ):
            mark_type = MarkType.Legal if i == 0 else \
                        MarkType.Action if i in [1, 2] else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text("A", *start_W_1, corner=Corner.UpperLeft)
        scene.append_text("B", *start_W_2, corner=Corner.UpperRight)

        return scene

    def scn_mv_048_pinned_piece_cascaded_2(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_048_pinned_piece_cascaded_2', bt) # , height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_e = (9, 9)
        scene.board.set_piece( *start_e, piece=-PieceType.Pegasus )

        start_K = (5, 1)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        start_Q = (7, 5)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_2 = (11, 5)
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_N = (8, 2)
        scene.board.set_piece( *start_N, piece=PieceType.Knight )

        # Q --> W 2
        coords_Q_W2 = GS.gen_steps( start=start_Q, rels=[(1, 0), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_Q_W2() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W 2 --> N
        coords_W2_N = GS.gen_steps( start=start_W_2, rels=[(-1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_W2_N() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W 2 --> N
        scene.append_arrow( *( GS.append_pos_rel( start_N, -2, 1 ) ), mark_type=MarkType.Action )

        scene.append_text("B", *start_W_2, corner=Corner.UpperRight)

        return scene

    #
    # Activating piece, check, and checkmate

    def scn_mv_049_activating_piece_check_init( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_049_activating_piece_check_init', bt, width=6.3, height=8.3 )
        rect = (0.05, 0.8, 0.65, 0.1)

        start_Q = (1, 1)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W = (1, 6)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_R = (3, 4)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_k = (5, 0)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> R
        coords_W_R = GS.gen_steps( start=start_W, rels=[ (1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_W_R() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R -->|
        coords_R_ = GS.gen_steps( start=start_R, rels=[ (0, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_R_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        return scene

    def scn_mv_050_activating_piece_check_end( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_050_activating_piece_check_end', bt, width=6.3, height=8.3 )
        rect = (0.05, 0.8, 0.65, 0.1)

        prev_Q = (1, 1)
        prev_W = (1, 6)
        prev_R = (3, 4)
        prev_k = (5, 0)

        start_Q = prev_W
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W = prev_R
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_R = (3, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_k = prev_k
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=prev_Q, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_Q_W() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W --> R
        coords_W_R = GS.gen_steps( start=prev_W, rels=[ (1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_W_R() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # R -->|
        coords_R_ = GS.gen_steps( start=prev_R, rels=[ (0, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_R_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # R --> k
        coords_R_k = GS.gen_steps( start=start_R, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_R_k() ):
            # mark_type = MarkType.Illegal if i == 1 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        return scene


    #
    # Cascade check, checkmate

    def scn_mv_051_cascaded_piece_check_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_051_cascaded_piece_check_init', bt, height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (9, 12)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_Q = (12, 9)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_B = (12, 6)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_W_C = (9, 6)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D = (9, 9)
        scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_k = (12, 2)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        start_p = (7, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # B --> W(A)
        coords_B_WA = GS.gen_steps( start=start_B, rels=[(1, 1), ], include_prev=True, count=8 )
        for i, arrow in enumerate( coords_B_WA() ):
            mark_type = MarkType.Action if i == 7 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> Q
        coords_WA_Q = GS.gen_steps( start=start_W_A, rels=[(1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WA_Q() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q --> W(B)
        coords_Q_WB = GS.gen_steps( start=start_Q, rels=[(0, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_Q_WB() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(B) --> W(C)
        coords_WB_WC = GS.gen_steps( start=start_W_B, rels=[(-1, 0), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WB_WC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(C) --> W(D)
        coords_WC_WD = GS.gen_steps( start=start_W_C, rels=[(0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WC_WD() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_052_cascaded_piece_check_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_052_cascaded_piece_check_end', bt, height=13.3) # , y=0.7, height=12.5)
        rect = (0.05, 0.8, 0.65, 0.1)

        start_B_prev = (1, 4)
        start_B = (9, 12)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (12, 9)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_Q = (12, 6)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W_B = (9, 6)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_W_C = (9, 9)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_W_D_prev = (9, 9)
        # start_W_D = (9, 9)
        # scene.board.set_piece( *start_W_D, piece=PieceType.Wave )

        start_k = (12, 2)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        start_p = (7, 1)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # B --> W(A)
        coords_B_WA = GS.gen_steps( end=start_B, rels=[(1, 1), ], include_prev=True, count=8 )
        for i, arrow in enumerate( coords_B_WA() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(A) --> Q
        coords_WA_Q = GS.gen_steps( end=start_W_A, rels=[(1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WA_Q() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # Q --> W(B)
        coords_Q_WB = GS.gen_steps( end=start_Q, rels=[(0, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_Q_WB() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(B) --> W(C)
        coords_WB_WC = GS.gen_steps( end=start_W_B, rels=[(-1, 0), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WB_WC() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(C) --> W(D)
        coords_WC_WD = GS.gen_steps( end=start_W_C, rels=[(0, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WC_WD() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        # scene.append_text( "D", *start_W_D, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        # W(D) --> Q
        coords_WD_Q = GS.gen_steps( start=start_W_D_prev, rels=[(1, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_WD_Q() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # Q --> p
        coords_Q_p = GS.gen_steps( start=start_Q, rels=[(-1, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_Q_p() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

            pos = GS.get_end( arrow )
            scene.append_text( str( i+1 ), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    #
    # Cascading opponent

    def scn_mv_065_wave_cascading_opponent(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_065_wave_cascading_opponent', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_w_B = (7, 4)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_n = (3, 8)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_w_C = (5, 9)
        scene.board.set_piece( *start_w_C, piece=-PieceType.Wave )

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
        coords_WB_N = GS.gen_next( GS.gen_steps(start=start_w_B, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N(), mark_type=MarkType.Action )

        # N --> W(C)
        scene.append_arrow( *(start_n + start_w_C), mark_type=MarkType.Action )

        # W(C) --> W(D)
        coords_WC_WD = GS.gen_next( GS.gen_steps(start=start_w_C, rels=[(2, -1), ], include_prev=True) )
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
        scene.append_text( "B", *start_w_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_w_C, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_W_D, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        # momentum received
        # scene.append_text( "3", *start_W_A, corner=Corner.LowerLeft, mark_type=MarkType.Action )
        # scene.append_text( "3", *start_w_B, corner=Corner.LowerLeftFieldMarker, mark_type=MarkType.Action )
        # scene.append_text( "3", *start_n, corner=Corner.LowerLeft, mark_type=MarkType.Action )
        # scene.append_text( "2", *start_w_C, corner=Corner.LowerLeftFieldMarker, mark_type=MarkType.Action )
        # scene.append_text( "2", *start_W_D, corner=Corner.LowerLeft, mark_type=MarkType.Action )
        # scene.append_text( "2", *start_P, corner=Corner.LowerLeft, mark_type=MarkType.Action )

        return scene

    def scn_mv_066_cascaded_opponent_capturing(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_066_cascaded_opponent_capturing', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_w_B = (7, 4)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_n = (3, 8)
        scene.board.set_piece( *start_n, piece=-PieceType.Knight )

        start_W_C = (5, 9)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_w_D = (13, 5)
        scene.board.set_piece( *start_w_D, piece=-PieceType.Wave )

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
        coords_WB_N = GS.gen_next( GS.gen_steps(start=start_w_B, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N() )
        scene.append_arrow( *coords_WB_N(), mark_type=MarkType.Action )

        # N --> W(C)
        scene.append_arrow( *(start_n + start_W_C), mark_type=MarkType.Action )

        # labels
        scene.append_text( "A", *start_W_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "D", *start_w_D, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_067_cascaded_opponent_promoting(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_067_cascaded_opponent_promoting', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_w_B = (7, 4)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_p = (10, 1)
        end_p = (10, 0)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

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

        # W(B) --> p
        coords_WB_a = GS.gen_next( GS.gen_steps(start=start_w_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_a() )
        scene.append_arrow( *coords_WB_a() )
        scene.append_arrow( *coords_WB_a(), mark_type=MarkType.Action )

        # p --->
        scene.append_arrow( *( start_p + end_p ), mark_type=MarkType.Action )

        # labels
        scene.append_text( "A", *start_W_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    #
    # Cascade self-checkmate

    def scn_mv_068_cascade_self_checkmate_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_068_cascade_self_checkmate_init', bt)

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_A = (4, 1)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_w_B = (7, 4)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_r = (10, 1)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_K = (12, 14)
        scene.board.set_piece( *start_K, piece=PieceType.King )

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

        # W(B) --> r
        coords_WB_a = GS.gen_next( GS.gen_steps(start=start_w_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_a() )
        scene.append_arrow( *coords_WB_a() )
        scene.append_arrow( *coords_WB_a(), mark_type=MarkType.Action )

        # r --->
        coords_r_ = GS.gen_next( GS.gen_steps(start=start_r, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords_r_() )
        scene.append_arrow( *coords_r_() )

        # labels
        scene.append_text( "A", *start_W_A, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_069_cascade_self_checkmate_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_069_cascade_self_checkmate_end', bt)

        end_B = (4, 1)
        scene.board.set_piece( *end_B, piece=PieceType.Bishop )

        end_W_A = (7, 4)
        scene.board.set_piece( *end_W_A, piece=PieceType.Wave )

        end_w_B = (10, 1)
        scene.board.set_piece( *end_w_B, piece=-PieceType.Wave )

        end_r = (12, 1)
        scene.board.set_piece( *end_r, piece=-PieceType.Rook )

        start_K = (12, 14)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        # B --> W(A)
        coords_B_WA = GS.gen_next( GS.gen_steps(end=end_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_B_WA(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_B_WA(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_B_WA(), mark_type=MarkType.Blocked )

        # W(A) --> W(B)
        coords_WA_WB = GS.gen_next( GS.gen_steps(end=end_W_A, rels=[(1, 1), ], include_prev=True) )
        scene.append_arrow( *coords_WA_WB(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_WA_WB(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_WA_WB(), mark_type=MarkType.Blocked )

        # W(B) --> r
        coords_WB_a = GS.gen_next( GS.gen_steps(end=end_w_B, rels=[(1, -1), ], include_prev=True) )
        scene.append_arrow( *coords_WB_a(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_WB_a(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_WB_a(), mark_type=MarkType.Blocked )

        # r --->
        coords_r_ = GS.gen_next( GS.gen_steps(end=end_r, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords_r_(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords_r_(), mark_type=MarkType.Blocked )

        # r --> K
        coords_r_K = GS.gen_steps( start=end_r, rels=[(0, 1), ], include_prev=True, count=13 )
        for i, arrow in enumerate( coords_r_K() ):
            mark_type = MarkType.Illegal if i == 12 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # labels
        scene.append_text( "A", *end_W_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked )
        scene.append_text( "B", *end_w_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked )

        return scene

    #
    # Double checkmate

    def scn_mv_070_cascade_double_checkmate_init( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_070_cascade_double_checkmate_init', bt )

        start_k = (0, 1)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        start_E = (5, 3)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        start_Q = (1, 14)
        scene.board.set_piece( *start_Q, piece=PieceType.Queen )

        start_W = (1, 1)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_w = (7, 1)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_r = (11, 5)
        end_r = (14, 5)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_K = (14, 3)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        # Q --> W
        coords_Q_W = GS.gen_steps( start=start_Q, rels=[(0, -1), ], include_prev=True, count=13 )
        for i, arrow in enumerate( coords_Q_W() ):
            mark_type = MarkType.Action if i == 12 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> w
        coords_W_w = GS.gen_steps( start=start_W, rels=[(1, 0), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_W_w() ):
            mark_type = MarkType.Action if i == 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # w --> r
        coords_w_r = GS.gen_steps( start=start_w, rels=[(1, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_w_r() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # r -->
        coords_r_ = GS.gen_steps( start=start_r, rels=[(1, 0), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_r_() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # labels
        scene.append_text( "Q", *start_Q, corner=Corner.UpperRight, mark_type=MarkType.Legal )
        scene.append_text( "R", *end_r, corner=Corner.UpperRight, mark_type=MarkType.Action )

        return scene

    def scn_mv_071_cascade_double_checkmate_end( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_071_cascade_double_checkmate_end', bt )

        start_k = (0, 1)
        scene.board.set_piece( *start_k, piece=-PieceType.King )

        start_E = (5, 3)
        scene.board.set_piece( *start_E, piece=PieceType.Pegasus )

        prev_Q = (1, 14)
        prev_W = (1, 1)
        prev_w = (7, 1)
        prev_r = (11, 5)

        end_Q = prev_W
        scene.board.set_piece( *end_Q, piece=PieceType.Queen )

        end_W = prev_w
        scene.board.set_piece( *end_W, piece=PieceType.Wave )

        end_w = prev_r
        scene.board.set_piece( *end_w, piece=-PieceType.Wave )

        end_r = (14, 5)
        scene.board.set_piece( *end_r, piece=-PieceType.Rook )

        start_K = (14, 3)
        scene.board.set_piece( *start_K, piece=PieceType.King )

        # Q --> W
        coords_Q_W = GS.gen_steps( end=end_Q, rels=[(0, -1), ], include_prev=True, count=13 )
        for i, arrow in enumerate( coords_Q_W() ):
            # mark_type = MarkType.Action if i == 12 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W --> w
        coords_W_w = GS.gen_steps( end=end_W, rels=[(1, 0), ], include_prev=True, count=6 )
        for i, arrow in enumerate( coords_W_w() ):
            # mark_type = MarkType.Action if i == 5 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # w --> r
        coords_w_r = GS.gen_steps( end=end_w, rels=[(1, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_w_r() ):
            # mark_type = MarkType.Action if i == 3 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # r -->
        coords_r_ = GS.gen_steps( end=end_r, rels=[(1, 0), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_r_() ):
            # mark_type = MarkType.Action if i == 1 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # labels
        scene.append_text( "Q", *prev_Q, corner=Corner.UpperRight, mark_type=MarkType.Illegal )
        scene.append_text( "R", *end_r, corner=Corner.UpperRight, mark_type=MarkType.Illegal )

        return scene

    #
    # Wave blocked

    def scn_mv_072_wave_blocked_init(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_072_wave_blocked_init', bt)

        start_r = (9, 9)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        start_w_A = (9, 4)
        scene.board.set_piece( *start_w_A, piece=-PieceType.Wave )

        start_W_1 = (11, 4)
        scene.board.set_piece( *start_W_1, piece=PieceType.Wave )

        start_R = (11, 0)
        scene.board.set_piece( *start_R, piece=PieceType.Rook )

        start_W_2 = (15, 0)
        scene.board.set_piece( *start_W_2, piece=PieceType.Wave )

        start_w_B = (15, 1)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        # horizontal Pawns
        coords = GS.gen_steps( start=start_w_B, rels=[(-1, 0), ], include_prev=False, count=15 )
        for step in coords():
            scene.board.set_piece( *step, piece=PieceType.Pawn )

        # vertical Pawns + Queen
        coords = GS.gen_steps( start=start_w_B, rels=[(0, 1), ], include_prev=False, count=14 )
        for i, step in enumerate( coords() ):
            piece = PieceType.Knight if i < 2 else \
                    PieceType.Bishop if i < 4 else \
                    PieceType.Rook if i < 6 else \
                    PieceType.Unicorn if i < 8 else \
                    PieceType.Pegasus if i < 10 else \
                    PieceType.Pyramid if i < 12 else \
                    PieceType.Queen
            scene.board.set_piece( *step, piece=piece )

        # r --> w(A)
        coords_r_wA = GS.gen_steps( start=start_r, rels=[(0, -1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( coords_r_wA() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # w(A) --> W(1)
        coords_wA_W1 = GS.gen_steps( start=start_w_A, rels=[(1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( coords_wA_W1() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(1) --> R
        coords_r_wA = GS.gen_steps( start=start_W_1, rels=[(0, -1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_r_wA() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # R --> W(2)
        coords_R_W2 = GS.gen_steps( start=start_R, rels=[(1, 0), ], include_prev=True, count=4 )
        for i, arrow in enumerate( coords_R_W2() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(2) --> w(B)
        scene.append_arrow( *( start_W_2 + start_w_B ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_w_A, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "1", *start_W_1, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "2", *start_W_2, corner=Corner.LowerLeft, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_073_wave_blocked_end(self, bt=BoardType.MirandasVeil):

        scene = Scene('scn_mv_073_wave_blocked_end', bt)

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
    # Activating opponent's Wave

    def scn_mv_078_activating_opponents_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_078_activating_opponents_wave', bt )

        start_p = (4, 14)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = (5, 13)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_W = (5, 10)
        scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A_A = (2, 7)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_A_B = (5, 6)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        scene.board.set_piece( 7, 8, piece=-PieceType.Pyramid )
        scene.board.set_piece( 13, 2, piece=-PieceType.Pawn )
        scene.board.set_piece( 5, 4, piece=PieceType.Knight )
        scene.board.set_piece( 5, 1, piece=-PieceType.Wave )

        start_B = (9, 2)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # p --> w
        scene.append_arrow( *( start_p + start_w ), mark_type=MarkType.Action )

        # w --> W
        coords_w_W = GS.gen_steps( start=start_w, rels=[ (0, -1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( coords_w_W() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "A", *start_A_A, mark_type=MarkType.Blocked, corner=Corner.UpperRight )
        scene.append_text( "B", *start_A_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )

        return scene

    def scn_mv_079_activated_opponents_wave(self, bt=BoardType.MirandasVeil):

        scene = Scene( 'scn_mv_079_activated_opponents_wave', bt )

        prev_p = (4, 14)
        prev_w = (5, 13)
        prev_W = (5, 10)

        start_p = prev_w
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = prev_W
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # start_W = (5, 10)
        # scene.board.set_piece( *start_W, piece=PieceType.Wave )

        start_A_A = (2, 7)
        scene.board.set_piece( *start_A_A, piece=PieceType.Pyramid )

        start_A_B = (5, 6)
        scene.board.set_piece( *start_A_B, piece=PieceType.Pyramid )

        scene.board.set_piece( 7, 8, piece=-PieceType.Pyramid )
        scene.board.set_piece( 13, 2, piece=-PieceType.Pawn )
        scene.board.set_piece( 5, 4, piece=PieceType.Knight )
        scene.board.set_piece( 5, 1, piece=-PieceType.Wave )

        start_B = (9, 2)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # W -->| (right-down)
        coords_W_rd_ = GS.gen_steps( start=prev_W, rels=[ (1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=3 )
        for i, arrow in enumerate( coords_W_rd_() ):
            mark_type = MarkType.Blocked if i in [1, 7] else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W -->| (down)
        coords_W_d_ = GS.gen_steps( start=prev_W, rels=[ (0, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=3 )
        for i, arrow in enumerate( coords_W_d_() ):
            mark_type = MarkType.Blocked if i == 3 else \
                        MarkType.Action if i in [5, 8] else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W -->| (left-down)
        coords_W_ld_ = GS.gen_steps( start=prev_W, rels=[ (-1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=3 )
        for i, arrow in enumerate( coords_W_ld_() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W --> * (right-down)
        start_ = (11, 4)

        # W --> * --> (left-down)
        coords_W__ld_ = GS.gen_steps( start=start_, rels=[ (-1, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=3 )
        for i, arrow in enumerate( coords_W__ld_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        # W --> * --> (down)
        coords_W__d_ = GS.gen_steps( start=start_, rels=[ (0, -1), ], include_prev=True, bounds=scene.board_view.get_position_limits() ) # , count=3 )
        for i, arrow in enumerate( coords_W__d_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_A_A, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker )
        scene.append_text( "B", *start_A_B, mark_type=MarkType.Blocked, corner=Corner.UpperRight )
        scene.append_text( "C", *start_, mark_type=MarkType.Illegal, corner=Corner.UpperRight )

        return scene

    #
    # En passant turned capture

    def scn_mv_080_en_passant_rushing_cascade( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_080_en_passant_rushing_cascade', bt, height=7.3, width=6.3 )

        start_P = (3, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 5) # 7)
        end_p = (3, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (3, 5) # 7)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (3, 6)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (1, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # P --> W(A)
        start_P_WA = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=4 ) # 6 )
        for i, arrow in enumerate( start_P_WA() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> B
        scene.append_arrow( *( start_W_A + start_B ), mark_type=MarkType.Action )

        # B --> A
        start_B_A = GS.gen_steps( start=start_B, rels=[ (-1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_B_A() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "E", *end_p, corner=Corner.UpperLeft, mark_type=MarkType.Legal )

        return scene

    def scn_mv_081_en_passant_turning_capture( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_081_en_passant_turning_capture', bt, height=7.3, width=6.3 )

        prev_P = (3, 1)
        prev_p = (4, 5)
        prev_W_A = (3, 5)
        prev_B = (3, 6)
        prev_A = (1, 4)

        start_P = (3, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 5)
        end_p = (3, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (3, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # start_A = (3, 4)
        # scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # P --> W(A)
        start_P_WA = GS.gen_steps( end=start_P, rels=[ (0, 1), ], include_prev=True, count=4 ) # 6 )
        for i, arrow in enumerate( start_P_WA() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(A) --> B
        scene.append_arrow( *( prev_W_A + prev_B ), mark_type=MarkType.Blocked )

        # B --> A
        start_B_A = GS.gen_steps( end=start_B, rels=[ (-1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_B_A() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # A -->
        start_A_ = GS.gen_steps( start=prev_A, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_A_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "E", *end_p, corner=Corner.UpperLeft, mark_type=MarkType.Legal )

        return scene

    def scn_mv_082_en_passant_turned_capture( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_082_en_passant_turned_capture', bt, height=7.3, width=6.3 )

        prev_P = (3, 1)
        prev_p = (4, 5)
        prev_W_A = (3, 5)
        prev_B = (3, 6)
        prev_A = (1, 4)

        start_P = (3, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 5)
        end_p = (3, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (3, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (3, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # p --> [P]
        scene.append_arrow( *( start_p + end_p ), mark_type=MarkType.Action )

        scene.append_text( "E", *end_p, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        return scene

    def scn_mv_083_en_passant_wave_captured( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_083_en_passant_wave_captured', bt, height=7.3, width=6.3 )

        field_E = (3, 4)

        start_P = (3, 1)
        end_P = (3, 6)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_p = (4, 5)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = field_E
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        # P --> W(A) -->
        start_P_WA_ = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=5 ) # 6 )
        for i, arrow in enumerate( start_P_WA_() ):
            # mark_type = MarkType.Blocked if i == 2 else \
            #             MarkType.Legal
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # p --> [P]
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Action )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Action )
        scene.append_text( "P", *start_P, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    #
    # En passant blocked

    def scn_mv_084_rushing_cascade_opponent( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_084_rushing_cascade_opponent', bt, height=8.3, width=6.3 )

        start_P = (4, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (5, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (4, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (4, 7)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_B = (2, 5)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_w = (3, 4)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_r = (2, 3)
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        field_E = (4, 3)

        # P --> W(A)
        start_P_WA = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( start_P_WA() ):
            mark_type = MarkType.Action if i == 4 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> B
        scene.append_arrow( *( start_W_A + start_B ), mark_type=MarkType.Action )

        # B --> W(B)
        start_B_WB = GS.gen_steps( start=start_B, rels=[ (-1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_B_WB() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(B) --> w
        scene.append_arrow( *( start_W_B + start_w ), mark_type=MarkType.Action )

        # w --> r
        scene.append_arrow( *( start_w + start_r ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Action )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Legal )

        return scene

    def scn_mv_085_blocking_en_passant( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_085_blocking_en_passant', bt, height=8.3, width=6.3 )

        prev_P = (4, 1)
        prev_p = (5, 4)
        prev_W_A = (4, 6)
        prev_B = (4, 7)
        prev_W_B = (2, 5)
        prev_w = (3, 4)
        prev_r = (2, 3)

        start_P = prev_W_A
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (5, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = prev_B
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = prev_W_B
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_B = prev_w
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_w = prev_r
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # start_r = (2, 3)
        # scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        field_E = (4, 3)

        # P --> W(A)
        start_P_WA = GS.gen_steps( start=prev_P, rels=[ (0, 1), ], include_prev=True, count=5 )
        for i, arrow in enumerate( start_P_WA() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(A) --> B
        scene.append_arrow( *( prev_W_A + prev_B ), mark_type=MarkType.Blocked )

        # B --> W(B)
        start_B_WB = GS.gen_steps( start=prev_B, rels=[ (-1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_B_WB() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(B) --> w
        scene.append_arrow( *( prev_W_B + prev_w ), mark_type=MarkType.Blocked )

        # w --> r
        scene.append_arrow( *( prev_w + prev_r ), mark_type=MarkType.Blocked )

        # r --> [E]
        start_e_E = GS.gen_steps( start=prev_r, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_e_E() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Legal )

        return scene

    def scn_mv_086_blocked_en_passant( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_086_blocked_en_passant', bt, height=8.3, width=6.3 )

        prev_P = (4, 1)
        prev_p = (5, 4)
        prev_W_A = (4, 6)
        prev_B = (4, 7)
        prev_W_B = (2, 5)
        prev_w = (3, 4)
        prev_r = (2, 3)

        start_P = prev_W_A
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (5, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = prev_B
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = prev_W_B
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_B = prev_w
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_w = prev_r
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        field_E = (4, 3)

        start_r = field_E
        scene.board.set_piece( *start_r, piece=-PieceType.Rook )

        # p --> [E]
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Illegal )

        return scene

    #
    # En passant denied

    def scn_mv_087_en_passant_denied_init( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_087_en_passant_denied_init', bt, height=7.3, width=6.3 )

        field_E = (3, 3)

        start_P = (3, 1)
        rush_P = (3, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W_A = rush_P
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (2, 6)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_B = (1, 5)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_W_C = (2, 4)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # P --> W(A)
        start_P_WA = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( start_P_WA() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> B
        scene.append_arrow( *( start_W_A + start_B ), mark_type=MarkType.Action )

        # B --> W(B)
        scene.append_arrow( *( start_B + start_W_B ), mark_type=MarkType.Action )

        # W(B) --> W(C)
        scene.append_arrow( *( start_W_B + start_W_C ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_088_en_passant_denied_pawn_activated( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_088_en_passant_denied_pawn_activated', bt, height=7.3, width=6.3 )

        prev_P = (3, 1)
        prev_W_A = (3, 5)
        prev_B = (2, 6)
        prev_W_B = (1, 5)
        prev_W_C = (2, 4)

        field_E = (3, 3)

        start_P = (3, 5)
        end_P = (3, 6)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W_A = (2, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (1, 5)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_B = (2, 4)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_W_C = (2, 4)
        # scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # # P --> W(A)
        # start_P_WA = GS.gen_steps( start=prev_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        # for i, arrow in enumerate( start_P_WA() ):
        #     scene.append_arrow( *arrow, mark_type=MarkType.Blocked )
        #
        # # W(A) --> B
        # scene.append_arrow( *( prev_W_A + prev_B ), mark_type=MarkType.Blocked )
        #
        # # B --> W(B)
        # scene.append_arrow( *( prev_B + prev_W_B ), mark_type=MarkType.Blocked )
        #
        # # W(B) --> W(C)
        # scene.append_arrow( *( prev_W_B + prev_W_C ), mark_type=MarkType.Blocked )

        # W(C) --> P
        scene.append_arrow( *( start_W_C + start_P ), mark_type=MarkType.Action )

        # P -->
        scene.append_arrow( *( start_P + end_P ), mark_type=MarkType.Legal )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        # scene.append_text( "C", *start_W_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene

    def scn_mv_089_en_passant_denied_end( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_089_en_passant_denied_end', bt, height=7.3, width=6.3 )

        field_E = (3, 3)

        prev_P = (3, 5)
        start_P = (3, 6)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_W_A = (2, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (1, 5)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_W_B = (2, 4)
        scene.board.set_piece( *start_W_B, piece=PieceType.Wave )

        start_W_C = (3, 5)
        scene.board.set_piece( *start_W_C, piece=PieceType.Wave )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        # p --> *
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Illegal )

        scene.append_text( "A", *start_W_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_W_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_W_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Illegal )

        # scene.append_field_marker( *prev_P, mark_type=MarkType.Illegal )

        return scene

    #
    # Activation after en passant

    def scn_mv_090_activation_after_en_passant_init( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_090_activation_after_en_passant_init', bt, height=6.3, width=6.3 ) # , height=7.3, width=6.3 )

        field_E = (3, 3)

        start_P = (3, 1)
        end_P = (3, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = field_E
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # P --> w -->
        start_P_w_ = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( start_P_w_() ):
            mark_type = MarkType.Blocked if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Action )
        scene.append_text( "P", *start_P, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "R", *end_P, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_091_activation_after_en_passant_end( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_091_activation_after_en_passant_end', bt, height=6.3, width=6.3 ) # , height=7.3, width=6.3 )

        field_E = (3, 3)

        start_P = (3, 1)
        end_P = (3, 5)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_p = (4, 4)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w = field_E
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        # P --> w -->
        start_P_w_ = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=4 )
        for i, arrow in enumerate( start_P_w_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # p --> [P]
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Action )

        # w -->
        start_w_ = GS.gen_steps( start=start_w, rels=[ (1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_w_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Action )
        scene.append_text( "P", *start_P, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
        scene.append_text( "R", *end_P, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    #
    # En passant not blocked

    def scn_mv_092_en_passant_not_blocked_init( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_092_en_passant_not_blocked_init', bt, height=7.3, width=6.3 )

        start_P = (3, 1)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 3)
        end_p = (3, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (3, 5) # 7)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (3, 6)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (1, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # P --> W(A)
        start_P_WA = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=4 ) # 6 )
        for i, arrow in enumerate( start_P_WA() ):
            mark_type = MarkType.Action if i == 3 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # W(A) --> B
        scene.append_arrow( *( start_W_A + start_B ), mark_type=MarkType.Action )

        # B --> A
        start_B_A = GS.gen_steps( start=start_B, rels=[ (-1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_B_A() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        scene.append_text( "E", *end_p, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )

        return scene

    def scn_mv_093_en_passant_not_blocked_step_2( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_093_en_passant_not_blocked_step_2', bt, height=7.3, width=6.3 )

        prev_P = (3, 1)
        prev_p = (4, 5)
        prev_W_A = (3, 5)
        prev_B = (3, 6)
        prev_A = (1, 4)

        start_P = (3, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 3)
        end_p = (3, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (3, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        # start_A = (3, 4)
        # scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # P --> W(A)
        start_P_WA = GS.gen_steps( end=start_P, rels=[ (0, 1), ], include_prev=True, count=4 ) # 6 )
        for i, arrow in enumerate( start_P_WA() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # W(A) --> B
        scene.append_arrow( *( prev_W_A + prev_B ), mark_type=MarkType.Blocked )

        # B --> A
        start_B_A = GS.gen_steps( end=start_B, rels=[ (-1, -1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_B_A() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # A -->
        start_A_ = GS.gen_steps( start=prev_A, rels=[ (1, 0), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_A_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Legal )

        scene.append_text( "E", *end_p, corner=Corner.UpperLeft, mark_type=MarkType.Legal )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene

    def scn_mv_094_en_passant_not_blocked_end( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_094_en_passant_not_blocked_end', bt, height=7.3, width=6.3 )

        prev_P = (3, 1)
        prev_p = (4, 5)
        prev_W_A = (3, 5)
        prev_B = (3, 6)
        prev_A = (1, 4)

        start_P = (3, 5)
        scene.board.set_piece( *start_P, piece=PieceType.Pawn )

        start_p = (4, 3)
        end_p = (3, 2)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_W_A = (3, 6)
        scene.board.set_piece( *start_W_A, piece=PieceType.Wave )

        start_B = (1, 4)
        scene.board.set_piece( *start_B, piece=PieceType.Bishop )

        start_A = (3, 4)
        scene.board.set_piece( *start_A, piece=PieceType.Pyramid )

        # p --> *
        scene.append_arrow( *( start_p + end_p ), mark_type=MarkType.Action )

        scene.append_text( "E", *end_p, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        scene.append_field_marker( *start_P, mark_type=MarkType.Legal )

        return scene

    #
    # En passant legal

    # todo :: not used
    # def scn_mv_095_en_passant_legal_init( self, bt=BoardType.MirandasVeil ):
    #
    #     scene = Scene( 'scn_mv_095_en_passant_legal_init', bt, height=8.3, width=6.3 )
    #
    #     start_P = (1, 1)
    #     end_P = (1, 7)
    #     scene.board.set_piece( *start_P, piece=PieceType.Pawn )
    #
    #     start_b = (3, 0)
    #     scene.board.set_piece( *start_b, piece=-PieceType.Bishop )
    #
    #     start_w = (5, 2)
    #     scene.board.set_piece( *start_w, piece=-PieceType.Wave )
    #
    #     start_p = (2, 5)
    #     scene.board.set_piece( *start_p, piece=-PieceType.Pawn )
    #
    #     field_E = (1, 4)
    #
    #     # P -->
    #     start_P_ = GS.gen_steps( start=start_P, rels=[ (0, 1), ], include_prev=True, count=6 )
    #     for i, arrow in enumerate( start_P_() ):
    #         scene.append_arrow( *arrow, mark_type=MarkType.Legal )
    #
    #     scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Blocked )
    #
    #     scene.append_field_marker( *start_P, mark_type=MarkType.Action )
    #
    #     return scene

    def scn_mv_096_en_passant_legal_end( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_096_en_passant_legal_end', bt, height=8.3, width=6.3 )

        start_P = (1, 1)
        end_P = (1, 7)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_b = (3, 0)
        scene.board.set_piece( *start_b, piece=-PieceType.Bishop )

        start_w = (5, 2)
        scene.board.set_piece( *start_w, piece=-PieceType.Wave )

        start_p = (2, 5)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        field_E = (1, 4)

        # P -->
        start_P_ = GS.gen_steps( end=end_P, rels=[ (0, 1), ], include_prev=True, count=6 )
        for i, arrow in enumerate( start_P_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # b --> w
        start_b_w = GS.gen_steps( start=start_b, rels=[ (1, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_b_w() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # w --> p
        start_w_p = GS.gen_steps( start=start_w, rels=[ (-1, 1), ], include_prev=True, count=3 )
        for i, arrow in enumerate( start_w_p() ):
            mark_type = MarkType.Action if i == 2 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # p --> *
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Action )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Action )

        scene.append_field_marker( *end_P, mark_type=MarkType.Legal )

        return scene

    #
    # En passant illegal

    def scn_mv_097_en_passant_illegal_init( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_097_en_passant_illegal_init', bt, width=6.3 ) # , height=7.3, width=6.3 )

        start_P = (1, 1)
        end_P = (1, 7)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_p = (2, 14)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w_A = (2, 8)
        scene.board.set_piece( *start_w_A, piece=-PieceType.Wave )

        start_w_B = (2, 7)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_w_C = (2, 6)
        scene.board.set_piece( *start_w_C, piece=-PieceType.Wave )

        start_q = (3, 7)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        # P -->
        start_P_ = GS.gen_steps( end=end_P, rels=[ (0, 1), ], include_prev=True, count=6 )
        for i, arrow in enumerate( start_P_() ):
            scene.append_arrow( *arrow, mark_type=MarkType.Blocked )

        # p -->
        start_p_ = GS.gen_steps( start=start_p, rels=[ (0, -1), ], include_prev=True, count=6 )
        for i, arrow in enumerate( start_p_() ):
            mark_type = MarkType.Action if i == 5 else \
                        MarkType.Legal
            scene.append_arrow( *arrow, mark_type=mark_type )

        # w(A) --> q
        scene.append_arrow( *( start_w_A + start_q ), mark_type=MarkType.Action )

        # q --> w(C)
        scene.append_arrow( *( start_q + start_w_C ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_w_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_w_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_field_marker( *end_P, mark_type=MarkType.Legal )

        return scene

    def scn_mv_098_en_passant_illegal_pawn_activated( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_098_en_passant_illegal_pawn_activated', bt, y=5.7, height=3.6, width=6.3 ) # , height=7.3, width=6.3 )

        end_P = (1, 7)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_p = (2, 8)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w_A = (3, 7)
        scene.board.set_piece( *start_w_A, piece=-PieceType.Wave )

        start_w_B = (2, 7)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_w_C = (2, 6)
        # scene.board.set_piece( *start_w_C, piece=-PieceType.Wave )

        start_q = (2, 6)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        # w(C) --> p
        start_wC_p = GS.gen_steps( start=start_w_C, rels=[ (0, 1), ], include_prev=True, count=2 )
        for i, arrow in enumerate( start_wC_p() ):
            mark_type = MarkType.Action if i == 1 else \
                        MarkType.Blocked
            scene.append_arrow( *arrow, mark_type=mark_type )


        scene.append_text( "A", *start_w_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        # scene.append_text( "C", *start_w_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_field_marker( *end_P, mark_type=MarkType.Legal )
        scene.append_field_marker( *start_p, mark_type=MarkType.Legal )

        return scene

    def scn_mv_099_en_passant_illegal_queen_reactivated( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_099_en_passant_illegal_queen_reactivated', bt, y=5.7, height=3.6, width=6.3 ) # , height=7.3, width=6.3 )

        end_P = (1, 7)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_p = (2, 8)
        # scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w_A = (3, 7)
        scene.board.set_piece( *start_w_A, piece=-PieceType.Wave )

        start_w_B = (2, 7)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_w_C = (2, 8)
        scene.board.set_piece( *start_w_C, piece=-PieceType.Wave )

        start_q = (2, 6)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        # p --> w(B)
        scene.append_arrow( *( start_p + start_w_B ), mark_type=MarkType.Action )

        # w(B) --> q
        scene.append_arrow( *( start_w_B + start_q ), mark_type=MarkType.Action )

        # q --> w(A)
        scene.append_arrow( *( start_q + start_w_A ), mark_type=MarkType.Action )

        scene.append_text( "A", *start_w_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_w_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_field_marker( *end_P, mark_type=MarkType.Legal )

        return scene

    def scn_mv_100_en_passant_illegal_pawn_reactivated( self, bt=BoardType.MirandasVeil ):

        scene = Scene( 'scn_mv_100_en_passant_illegal_pawn_reactivated', bt, y=5.7, height=3.6, width=6.3 ) # , height=7.3, width=6.3 )

        field_E = (1, 6)

        end_P = (1, 7)
        scene.board.set_piece( *end_P, piece=PieceType.Pawn )

        start_p = (2, 7)
        scene.board.set_piece( *start_p, piece=-PieceType.Pawn )

        start_w_A = (3, 7)
        # scene.board.set_piece( *start_w_A, piece=-PieceType.Wave )

        start_w_B = (2, 6)
        scene.board.set_piece( *start_w_B, piece=-PieceType.Wave )

        start_w_C = (2, 8)
        scene.board.set_piece( *start_w_C, piece=-PieceType.Wave )

        start_q = (3, 7)
        scene.board.set_piece( *start_q, piece=-PieceType.Queen )

        # w(A) --> p
        scene.append_arrow( *( start_w_A + start_p ), mark_type=MarkType.Action )

        # p --> *
        scene.append_arrow( *( start_p + field_E ), mark_type=MarkType.Illegal )

        # scene.append_text( "A", *start_w_A, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "B", *start_w_B, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )
        scene.append_text( "C", *start_w_C, corner=Corner.UpperLeftFieldMarker, mark_type=MarkType.Legal )

        scene.append_text( "E", *field_E, corner=Corner.UpperLeft, mark_type=MarkType.Illegal )

        scene.append_field_marker( *end_P, mark_type=MarkType.Legal )

        return scene
