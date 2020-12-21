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


class SceneConquestOfTlalocanMixin:

    def scn_cot_01_shaman_movement(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_01_shaman_movement', bt)

        #
        # light Shaman

        start_lH = (6, 6)
        scene.board.set_piece(*start_lH, piece=PieceType.Shaman)

        # light Shaman, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_lH, include_prev=False, bounds=((2, 2), (10, 10)))

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        # light Shaman, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_lH, include_prev=False, bounds=((4, 4), (8, 8)))

        for i, pos in enumerate( gen_abs_pos_2() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRightFieldMarker)

        #
        # dark Shaman

        start_dH = (17, 17)
        scene.board.set_piece(*start_dH, piece=-PieceType.Shaman)

        # dark Shaman, long jump

        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_dH, include_prev=False, bounds=((13, 13), (21, 21)))

        for i, pos in enumerate( gen_abs_pos_3() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        # dark Shaman, short jump

        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_dH, include_prev=False, bounds=((15, 15), (19, 19)))

        for i, pos in enumerate( gen_abs_pos_4() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        #
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

        #
        # light Shaman 2

        start_lH2 = (17, 6)
        scene.board.set_piece(*start_lH2, piece=PieceType.Shaman)

        # light Shaman 2, long jump

        gen_abs_pos_5 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_lH2, include_prev=False, bounds=((13, 2), (21, 10)))

        for i, pos in enumerate( gen_abs_pos_5() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        # light Shaman 2, short jump

        gen_abs_pos_6 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_lH2, include_prev=False, bounds=((15, 4), (19, 8)))

        for i, pos in enumerate( gen_abs_pos_6() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperRightFieldMarker)

        #
        # dark Shaman 2

        start_dH2 = (6, 17)
        scene.board.set_piece(*start_dH2, piece=-PieceType.Shaman)

        # dark Shaman 2, long jump

        gen_abs_pos_7 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_dH2, include_prev=False, bounds=((2, 13), (10, 21)))

        for i, pos in enumerate( gen_abs_pos_7() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        # dark Shaman 2, short jump

        gen_abs_pos_8 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_dH2, include_prev=False, bounds=((4, 15), (8, 19)))

        for i, pos in enumerate( gen_abs_pos_8() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        return scene

    #
    # light Shaman's plies

    def scn_cot_02_light_shaman_step_ply(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_02_light_shaman_step_ply', bt)

        start = (3, 9)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.board.set_piece(9, 6, piece=PieceType.Pyramid)
        scene.board.set_piece(5, 5, piece=PieceType.Wave)
        scene.board.set_piece(7, 17, piece=-PieceType.Bishop)
        scene.board.set_piece(15, 19, piece=-PieceType.Knight)

        # (2, 1) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(2, 1), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        # (-1, 2) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-1, 2), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        # (-2, 1) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-2, 1), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        # (-2, -1) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-2, -1), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        # (-1, -2) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-1, -2), ], start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        # (1, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, -1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, -1), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (1, -2) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, -2), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, -1), (2, 1)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=(9, 12), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=(17, 16), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        return scene

    def scn_cot_03_light_shaman_capture_ply(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_03_light_shaman_capture_ply', bt)

        start = (3, 9)
        scene.board.set_piece(*start, piece=PieceType.Shaman)

        start_W1 = (2, 5)
        scene.board.set_piece(*start_W1, piece=PieceType.Wave)
        scene.append_text("1", *start_W1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_A1 = (4, 5)
        scene.board.set_piece(*start_A1, piece=PieceType.Pyramid)
        scene.append_text("1", *start_A1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_W2 = (7, 3)
        scene.board.set_piece(*start_W2, piece=PieceType.Wave)
        scene.append_text("2", *start_W2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_A2 = (9, 5)
        scene.board.set_piece(*start_A2, piece=PieceType.Pyramid)
        scene.append_text("2", *start_A2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # (4, 1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=PieceType.Wave)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (-1, -4) ----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(-1, -4), ], start=start, include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (1, -4) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, -4), ], start=start, include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(3, 2), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(3, 2), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        scene.board.set_piece(*GS.add(start, (13, 13)), piece=-PieceType.Knight)

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=GS.add(start, (9, 6)), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(-3, -2), (3, 2)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=GS.add(start, (15, 10)), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (2, 3) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        coords() # leave empty
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # empty -------------------------------------------------------------------------------------------------------------------

        scene.board.set_piece(*GS.add(start, (8, -2)), piece=-PieceType.Pawn)
        scene.board.set_piece(*GS.add(start, (12, -3)), piece=-PieceType.Pawn)

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(-1, -4), (1, -4), (3, 2), (4, 1), (2, 3), ] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        return scene

    #
    # dark Shaman's plies

    def scn_cot_04_dark_shaman_step_ply(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_04_dark_shaman_step_ply', bt)

        start = (3, 9)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)

        start_W = (7, 3)
        scene.board.set_piece(*start_W, piece=-PieceType.Wave)

        start_A = (9, 5)
        scene.board.set_piece(*start_A, piece=-PieceType.Pyramid)

        start_B = (7, 15)
        scene.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_N = (5, 17)
        scene.board.set_piece(*start_N, piece=-PieceType.Knight)

        # (1, 4) ----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, 4), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, 3) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, -3) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, -3), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, -2) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(3, -2), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        scene.board.set_piece(*GS.add(start, (8, 8)), piece=PieceType.Knight)

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=GS.add(start, (9, 6)), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(-3, -2), (3, 2)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=GS.add(start, (15, 10)), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # empty -------------------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(1, 4), (2, 3), (2, -3), (3, -2)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        return scene

    def scn_cot_05_dark_shaman_capture_ply(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_05_dark_shaman_capture_ply', bt)

        start = (3, 9)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.board.set_piece(1, 13, piece=-PieceType.Pyramid)
        scene.board.set_piece(1, 5, piece=-PieceType.Wave)

        # (2, 1) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 1), ], start=start, include_prev=False) )
        for i in range( 8 ):
            scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pyramid)

        coords = GS.gen_next( GS.gen_steps([(2, 1), ], start=start, include_prev=True) )
        for i in range( 8 ):
            scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        scene.board.set_piece(*GS.add(start, (5, 5)), piece=PieceType.Knight)

        # (-2, 1) ------------------------------------------------------------------------------------------------------------------

        scene.board.set_piece(1, 10, piece=-PieceType.Wave)

        scene.append_arrow( 3, 9, 1, 10, mark_type=MarkType.Action )

        # (-2, -1) ------------------------------------------------------------------------------------------------------------------

        scene.board.set_piece(1, 8, piece=-PieceType.Pyramid)

        scene.append_arrow( 3, 9, 1, 8, mark_type=MarkType.Action )

        # (1, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        coords() # keep empty
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        for i in range(5):
            scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, -1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, -1), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pyramid)
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        coords() # keep empty
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(2, -1), ], start=start, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        for i in range(6):
            scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (1, -2) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, -2), ], start=start, include_prev=False) )
        coords() # keep empty
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, -1), (2, 1)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=(9, 12), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=(17, 16), include_prev=True, count=1)

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Illegal )

        # empty -------------------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, 1), (-2, -1), (2, 1), (1, 2), (2, -1)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos, mark_type=MarkType.Blocked )

        return scene

    #
    # Wave activation

    def scn_cot_06_wave_activated(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_06_wave_activated', bt)

        start = (3, 9)

        start_W1 = (2, 5)
        scene.board.set_piece(*start_W1, piece=PieceType.Wave)
        scene.append_text("1", *start_W1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_A1 = (4, 5)
        scene.board.set_piece(*start_A1, piece=PieceType.Pyramid)
        scene.append_text("1", *start_A1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_W2 = (7, 3)
        scene.board.set_piece(*start_W2, piece=PieceType.Wave)
        scene.append_text("2", *start_W2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_A2 = (9, 5)
        scene.board.set_piece(*start_A2, piece=PieceType.Pyramid)
        scene.append_text("2", *start_A2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # (4, 1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=False) )
        scene.append_text("1", *coords(), mark_type=MarkType.Blocked) # (4, 10), captured Pawn
        scene.append_text("2", *coords(), mark_type=MarkType.Blocked) # captured Pawn
        scene.board.set_piece(*coords(), piece=PieceType.Shaman) # (15, 12)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=PieceType.Pawn)

        start_W = (15, 12)
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(4, 1), (2, 3)] ) ) # (-4, -1),
        gen_pos = GS.gen_multi_steps(multi_rels, start=start_W, include_prev=True, bounds=scene.board_view.get_position_limits())

        for pos in gen_pos():
            scene.append_arrow( *pos )

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start_W, include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start_W, include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(3, 2), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)

        scene.board.set_piece(*GS.add(start, (13, 13)), piece=-PieceType.Knight)

        # (2, 3) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=False) )
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)
        coords() # leave empty
        scene.board.set_piece(*coords(), piece=-PieceType.Pawn)

        # empty -------------------------------------------------------------------------------------------------------------------

        scene.board.set_piece(*GS.add(start, (8, -2)), piece=-PieceType.Pawn)
        scene.board.set_piece(*GS.add(start, (12, -3)), piece=-PieceType.Pawn)

        return scene

    #
    # teleporting Shaman

    def scn_cot_07_teleport_shaman_all(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_07_teleport_shaman_all', bt)

        start_H_A = (9, 17)
        start_T = (0, 23)

        start_T2 = (23, 0)
        start_H_B= (22, 3)
        start_H_C = (15, 4)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)
        scene.board.set_piece(*start_T2, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        scene.board.set_piece(3, 21, piece=-PieceType.Bishop)
        scene.board.set_piece(6, 19, piece=-PieceType.Knight)
        scene.board.set_piece(*start_H_A, piece=PieceType.Shaman)
        scene.board.set_piece(*start_H_B, piece=PieceType.Shaman)
        scene.board.set_piece(*start_H_C, piece=PieceType.Shaman)

        # Shaman A
        coords = GS.gen_steps(start=start_H_A, rels=[(-3, 2), ], include_prev=True, count=3)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Action if index == 2 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        # Shaman B
        scene.append_arrow( *(start_H_B + start_T2), mark_type=MarkType.Action )

        # Shaman C
        coords = GS.gen_steps(start=start_H_C, rels=[(2, -1), ], include_prev=True, count=4)

        for index, coord in enumerate( coords() ):
            mark_type = MarkType.Action if index == 3 else MarkType.Legal
            scene.append_arrow( *coord, mark_type=mark_type )

        # portal-fields
        scene.append_text("1", 22, 23, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("2", 22, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 23, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Legal)

        # Shamans
        scene.append_text("A", *start_H_A, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("B", *start_H_B, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("C", *start_H_C, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    def scn_cot_08_teleport_pawn_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_08_teleport_pawn_init', bt)

        start_T = (0, 23)
        start_P = (1, 22)
        start_P_B = (0, 22)

        # fixed set
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(*start_T, piece=-PieceType.Star)

        # Pawns
        scene.board.set_piece(*start_P, piece=PieceType.Pawn)
        scene.board.set_piece(*start_P_B, piece=PieceType.Pawn)

        # scene.append_arrow( *(start_P + start_T), mark_type=MarkType.Action )
        scene.append_arrow( *(start_P_B + start_T), mark_type=MarkType.Action )

        # portal-fields
        scene.append_text("1", 22, 23, corner=Corner.LowerLeft, mark_type=MarkType.Action)
        scene.append_text("2", 22, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)
        scene.append_text("3", 23, 22, corner=Corner.LowerLeft, mark_type=MarkType.Legal)

        scene.append_text("4", 0, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("5", 1, 1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)
        scene.append_text("6", 1, 0, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    #
    # trance-journey

    def scn_cot_09_trance_journey_init(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_09_trance_journey_init', bt, width=9, height=12)

        start_H1 = (4, 9)
        scene.board.set_piece(*start_H1, piece=PieceType.Shaman)
        scene.append_text("1", *start_H1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_W2 = (2, 5)
        scene.board.set_piece(*start_W2, piece=PieceType.Wave)

        start_H2 = (6, 3)
        scene.board.set_piece(*start_H2, piece=PieceType.Shaman)
        scene.append_text("2", *start_H2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_W3 = (4, 7)
        scene.board.set_piece(*start_W3, piece=PieceType.Wave)

        start_w1 = (6, 6)
        scene.board.set_piece(*start_w1, piece=-PieceType.Wave)

        start_h1 = (7, 8)
        scene.board.set_piece(*start_h1, piece=-PieceType.Shaman)
        scene.append_text("3", *start_h1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        coords = GS.gen_next( GS.gen_steps(start=start_H1, rels=[(-1, -2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_W2, rels=[(2, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_H2, rels=[(-1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_W3, rels=[(2, -1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_w1, rels=[(1, 2), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        return scene

    def scn_cot_10_knight_directions(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_10_knight_directions', bt, width=9, height=12)

        scene.append_text("S", 6, 6, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        # left
        scene.board.set_piece(5, 9, piece=PieceType.Knight)

        scene.append_arrow( 5, 9, 4+0.5, 9+0.5, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False ) # left
        scene.append_arrow( *GS.add_to_all( (4, 9, 3, 8), 0.5 ), mark_type=MarkType.Blocked, start_pointer=False, end_pointer=True ) # left down
        scene.append_arrow( *GS.add_to_all( (4, 9, 3, 10), 0.5 ), mark_type=MarkType.Blocked, start_pointer=False, end_pointer=True ) # left up

        scene.append_text("L", 3, 8, corner=Corner.LowerLeft, mark_type=MarkType.Blocked)
        scene.append_text("R", 3, 10, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # down
        scene.board.set_piece(2, 7, piece=-PieceType.Knight)

        scene.append_arrow( 2, 7, 2+0.5, 6+0.5, mark_type=MarkType.Illegal, start_pointer=False, end_pointer=False ) # down
        scene.append_arrow( *GS.add_to_all( (2, 6, 1, 5), 0.5 ), mark_type=MarkType.Illegal, start_pointer=False, end_pointer=True ) # down left
        scene.append_arrow( *GS.add_to_all( (2, 6, 3, 5), 0.5 ), mark_type=MarkType.Illegal, start_pointer=False, end_pointer=True ) # down right

        scene.append_text("L", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Illegal)
        scene.append_text("R", 1, 5, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        # right
        scene.board.set_piece(4, 4, piece=PieceType.Knight)

        scene.append_arrow( 4, 4, 5+0.5, 4+0.5, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False ) # right
        scene.append_arrow( *GS.add_to_all( (5, 4, 6, 3), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # right down
        scene.append_arrow( *GS.add_to_all( (5, 4, 6, 5), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # right up

        scene.append_text("L", 6, 5, corner=Corner.UpperRight, mark_type=MarkType.Legal)
        scene.append_text("R", 6, 3, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        # up
        scene.board.set_piece(7, 6, piece=-PieceType.Knight)

        scene.append_arrow( 7, 6, 7+0.5, 7+0.5, mark_type=MarkType.Action, start_pointer=False, end_pointer=False ) # up
        scene.append_arrow( *GS.add_to_all( (7, 7, 6, 8), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up left
        scene.append_arrow( *GS.add_to_all( (7, 7, 8, 8), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up right

        scene.append_text("L", 6, 8, corner=Corner.UpperLeft, mark_type=MarkType.Action)
        scene.append_text("R", 8, 8, corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene


    def scn_cot_11_stop_sign_pattern(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_11_stop_sign_pattern', bt, width=9, height=12)

        start = (6, 6)
        scene.append_text("S", *start, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        scene.append_arrow( *GS.add_to_all( (6, 6, 7, 6), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
        scene.append_arrow( *GS.add_to_all( (7, 6, 8, 7), 0.5 ), mark_type=MarkType.Legal ) # right-up

        scene.append_text("1", 8, 7, corner=Corner.LowerRight, mark_type=MarkType.Legal)

        scene.append_arrow( *GS.add_to_all( (8, 7, 8, 8), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
        scene.append_arrow( *GS.add_to_all( (8, 8, 7, 9), 0.5 ), mark_type=MarkType.Action ) # left-up

        scene.append_text("2", 7, 9, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *GS.add_to_all( (7, 9, 6, 9), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
        scene.append_arrow( *GS.add_to_all( (6, 9, 5, 8), 0.5 ), mark_type=MarkType.Blocked ) # left-down

        scene.append_text("3", 5, 8, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        scene.append_arrow( *GS.add_to_all( (5, 8, 5, 7), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
        scene.append_arrow( *GS.add_to_all( (5, 7, 6, 6), 0.5 ), mark_type=MarkType.Illegal ) # right-down

        return scene

    def append_broken_arrow(self, scene, start, rel, outward_arrows=True, bounds=None, count=None, is_with_field_marker=True, rect=None):

        rels = GS.gen_shaman_rel_legs(rel) # , count=count)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True, bounds=bounds) )
        corners = GS.gen_next( GS.gen_shaman_corners(rel, is_with_field_marker=is_with_field_marker) )

        if outward_arrows:
            sp1 = False
            ep1 = False
            sp2 = False
            ep2 = True
        else:
            sp1 = True
            ep1 = False
            sp2 = False
            ep2 = False

        def _append_broken_arrow(text=None, mark_type=MarkType.Legal, full_length=False):
            x0, y0, x1, y1 = leg_1 = coords()
            x2, y2, x3, y3 = leg_2 = coords()

            assert x1 == x2
            assert y1 == y2

            if full_length:
                scene.append_arrow( *GS.add_to_all( leg_1, 0.5 ), mark_type=mark_type, start_pointer=sp1, end_pointer=ep1 ) # start
                scene.append_arrow( *GS.add_to_all( leg_2, 0.5 ), mark_type=mark_type, start_pointer=sp2, end_pointer=ep2 ) # end, diagonal
            else:
                scene.append_arrow( x0, y0, x1+0.5, y1+0.5, mark_type=mark_type, start_pointer=sp1, end_pointer=ep1 ) # start
                scene.append_arrow( x2+0.5, y2+0.5, x3, y3, mark_type=mark_type, start_pointer=sp2, end_pointer=ep2 ) # end, diagonal

            if text is not None:
                scene.append_text(text, x3, y3, corner=corners(), mark_type=mark_type, rect=rect)

        return _append_broken_arrow

    def scn_cot_12_stop_sign_pattern_unwind(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_12_stop_sign_pattern_unwind', bt, width=9, height=12)

        start = (6, 6)
        rel = (2, 1)

        scene.append_text("S", *start, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        aba = self.append_broken_arrow(scene, start, rel, bounds=scene.board_view.get_position_limits(), count=8)

        aba("1", mark_type=MarkType.Legal)
        aba("2", mark_type=MarkType.Action)
        aba("3", mark_type=MarkType.Blocked)
        aba("4", mark_type=MarkType.Illegal)

        return scene

    def scn_cot_13_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_13_stop_sign_pattern_full', bt)

        start = (5, 11)
        # scene.board.set_piece(*start, piece=PieceType.Shaman)

        scene.append_field_marker(*start, mark_type=MarkType.Illegal) # , mark_type=MarkType.Blocked
        scene.append_text("S", *start, mark_type=MarkType.Illegal, corner=Corner.UpperLeftFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(4):
            aba(str(4 * i + 1), mark_type=MarkType.Legal)
            aba(str(4 * i + 2), mark_type=MarkType.Action)
            aba(str(4 * i + 3), mark_type=MarkType.Blocked)
            aba(str(4 * i + 4), mark_type=MarkType.Illegal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(4):
            aba(str(4 * i + 1), mark_type=MarkType.Legal)
            aba(str(4 * i + 2), mark_type=MarkType.Action)
            aba(str(4 * i + 3), mark_type=MarkType.Blocked)
            aba(str(4 * i + 4), mark_type=MarkType.Illegal)

        return scene

    def scn_cot_14_light_shaman_trance_journey(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_14_light_shaman_trance_journey', bt)

        start = (5, 11)
        scene.board.set_piece(*start, piece=PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(scene, start, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_cot_15_light_shaman_trance_journey_offset(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_15_light_shaman_trance_journey_offset', bt, x=-7, y=-1)

        start = (5, 11)
        scene.board.set_piece(*start, piece=PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Legal if i < 5 else MarkType.Blocked
            aba(str(i + 1), mark_type=mark_type)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # mark_type = MarkType.Legal if i < 5 else MarkType.Blocked
            # aba(str(i + 1), mark_type=mark_type)

        return scene

    def scn_cot_16_dark_shaman_trance_journey(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_16_dark_shaman_trance_journey', bt)

        start = (5, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=True)

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24, is_with_field_marker=True)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Action)

        return scene

    def scn_cot_17_displacement_fields(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_17_displacement_fields', bt)

        start = (12, 11)
        scene.board.set_piece(*start, piece=PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, count=1)

        for i, pos in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*pos, mark_type=MarkType.Blocked)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_cot_18_light_light_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_18_light_light_shaman_interaction_start', bt)

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
        scene.board.set_piece(4, 17, piece=PieceType.Knight)
        scene.board.set_piece(12, 11, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

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
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        # scene.append_arrow( *((-1, 1) + startT), mark_type=MarkType.Illegal )
        # scene.append_arrow( *((-1, 9) + startK), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-7.5, 8.5) + startT), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-1.5, 10.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("4", *startK, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)
        scene.replace_text("9", *startT, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_19_light_light_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_19_light_light_shaman_interaction_end', bt)

        start = (4, 12)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(8, 13, piece=PieceType.Knight)
        scene.board.set_piece(9, 16, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = (3, 10)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        startH1 = (6, 23)
        scene.board.set_piece(*startH1, piece=PieceType.Shaman)
        scene.append_text("T", *startH1, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        # right arm Knight's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(4, 17), include_prev=False, count=1)

        # i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            # scene.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Action)
            # i += 1

        # right arm Pawn's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(12, 11), include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            # scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text(str(i), *pos, mark_type=MarkType.Legal, corner=Corner.UpperLeftFieldMarker)
            i += 1

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_cot_20_dark_light_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_20_dark_light_shaman_interaction_start', bt)

        start = (4, 12)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(4, 17, piece=PieceType.Knight)
        scene.board.set_piece(12, 11, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startW1 = (5, 11)
        scene.board.set_piece(*startW1, piece=-PieceType.Wave)

        startW2 = (3, 8)
        scene.board.set_piece(*startW2, piece=PieceType.Wave)

        startH = (8, 9)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + startW1), mark_type=MarkType.Action )
        scene.append_arrow( *(startW1 + startW2), mark_type=MarkType.Action )
        scene.append_arrow( *(startW2 + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        # scene.append_arrow( *((-1, 1) + startT), mark_type=MarkType.Illegal )
        # scene.append_arrow( *((-1, 9) + startK), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-7.5, 8.5) + startT), mark_type=MarkType.Illegal )
        scene.replace_arrow( *((-1.5, 10.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("4", *startK, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)
        scene.replace_text("9", *startT, corner=Corner.LowerLeft, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_21_dark_light_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_21_dark_light_shaman_interaction_end', bt)

        start = (4, 12)

        startH0 = (6, 23)
        scene.board.set_piece(*startH0, piece=PieceType.Shaman)
        scene.append_text("T", *startH0, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        startT = (0, 0)
        scene.board.set_piece(*startT, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        scene.board.set_piece(*startK, piece=PieceType.King)
        # scene.board.set_piece(4, 17, piece=PieceType.Knight)
        # scene.board.set_piece(12, 11, piece=-PieceType.Pawn)
        scene.board.set_piece(18, 9, piece=-PieceType.Knight)

        scene.board.set_piece(6, 7, piece=PieceType.Pawn)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(8, 7, piece=PieceType.Pawn)
        scene.board.set_piece(9, 7, piece=PieceType.Pawn)
        scene.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = (5, 11)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startW1 = (3, 8)
        scene.board.set_piece(*startW1, piece=-PieceType.Wave)

        startW2 = (4, 12)
        scene.board.set_piece(*startW2, piece=PieceType.Wave)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(scene, start, rel, count=24)

        # for i in range(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return scene

    def scn_cot_22_dark_dark_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_22_dark_dark_shaman_interaction_start', bt)

        start = (4, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.LowerRight, mark_type=MarkType.Action)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(*startP, piece=-PieceType.Pawn)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW1 = (1, 13)
        scene.board.set_piece(*startW1, piece=-PieceType.Wave)

        startH = (4, 15)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + startW1), mark_type=MarkType.Action )
        scene.append_arrow( *(startW1 + start), mark_type=MarkType.Action )

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        # for i in range(16):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Legal)

        scene.replace_arrow( *(startP + (8.5, 23.5)), mark_type=MarkType.Illegal, end_pointer=False )
        scene.replace_arrow( *((8.5, 23.5) + startT), mark_type=MarkType.Illegal )

        scene.replace_arrow( *((-0.5, 3.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("6", *startT, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)
        scene.replace_text("8", *startK, corner=Corner.LowerRight, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_23_dark_dark_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_23_dark_dark_shaman_interaction_end', bt)

        start = (4, 11)

        startH0 = (2, 17)
        scene.board.set_piece(*startH0, piece=-PieceType.Shaman)
        scene.append_text("T", *startH0, corner=Corner.LowerRight, mark_type=MarkType.Action)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(5, 3, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW1 = start
        scene.board.set_piece(*startW1, piece=-PieceType.Wave)

        startH = (1, 13)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        # for i in range(16):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        return scene

    def scn_cot_24_dark_dark_shaman_double_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_24_dark_dark_shaman_double_interaction_start', bt)

        start = (4, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.LowerRight, mark_type=MarkType.Action)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(*startP, piece=-PieceType.Pawn)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW1 = (1, 13)
        scene.board.set_piece(*startW1, piece=-PieceType.Wave)

        startH = (4, 15)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + startW1), mark_type=MarkType.Action )
        scene.append_arrow( *(startW1 + start), mark_type=MarkType.Action )

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Legal)

        scene.replace_arrow( *(startP + (8.5, 23.5)), mark_type=MarkType.Illegal, end_pointer=False )
        scene.replace_arrow( *((8.5, 23.5) + startT), mark_type=MarkType.Illegal )

        scene.replace_arrow( *((-0.5, 3.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("6", *startT, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)
        scene.replace_text("8", *startK, corner=Corner.LowerRight, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_25_dark_dark_shaman_double_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_25_dark_dark_shaman_double_interaction_end', bt)

        start = (4, 11)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW1 = start
        scene.board.set_piece(*startW1, piece=-PieceType.Wave)

        startH = (1, 13)
        scene.board.set_piece(*startH, piece=-PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(10):
            aba(str(10 - i), mark_type=MarkType.Blocked)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        return scene

    def scn_cot_26_light_dark_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_26_light_dark_shaman_interaction_start', bt)

        start = (4, 11)
        scene.board.set_piece(*start, piece=-PieceType.Shaman)
        scene.append_text("T", *start, corner=Corner.LowerRight, mark_type=MarkType.Action)

        startT = (0, 23)
        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(*startT, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        scene.board.set_piece(*startK, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(*startP, piece=-PieceType.Pawn)
        scene.board.set_piece(10, 13, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW2 = (2, 12)
        scene.board.set_piece(*startW2, piece=-PieceType.Wave)

        startW1 = (3, 14)
        scene.board.set_piece(*startW1, piece=PieceType.Wave)

        startH = (5, 15)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startH + startW1), mark_type=MarkType.Action )
        scene.append_arrow( *(startW1 + startW2), mark_type=MarkType.Action )
        scene.append_arrow( *(startW2 + start), mark_type=MarkType.Action )

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        # for i in range(10):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Legal)

        scene.replace_arrow( *(startP + (8.5, 23.5)), mark_type=MarkType.Illegal, end_pointer=False )
        scene.replace_arrow( *((8.5, 23.5) + startT), mark_type=MarkType.Illegal )

        scene.replace_arrow( *((-0.5, 3.5) + startK), mark_type=MarkType.Illegal )

        scene.replace_text("6", *startT, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)
        scene.replace_text("8", *startK, corner=Corner.LowerRight, mark_type=MarkType.Illegal)

        return scene

    def scn_cot_27_light_dark_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_27_light_dark_shaman_interaction_end', bt)

        start = (4, 11)

        startH0 = (2, 17)
        scene.board.set_piece(*startH0, piece=-PieceType.Shaman)
        scene.append_text("T", *startH0, corner=Corner.LowerRight, mark_type=MarkType.Action)

        scene.board.set_piece(0, 0, piece=PieceType.Star)
        scene.board.set_piece(0, 23, piece=-PieceType.Star)
        scene.board.set_piece(23, 0, piece=-PieceType.Star)
        scene.board.set_piece(23, 23, piece=PieceType.Star)

        scene.board.set_piece(5, 3, piece=PieceType.King)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(13, 14, piece=-PieceType.Pawn)
        scene.board.set_piece(12, 11, piece=PieceType.Knight)

        scene.board.set_piece(15, 13, piece=-PieceType.Bishop)
        scene.board.set_piece(9, 11, piece=PieceType.Bishop)
        scene.board.set_piece(6, 5, piece=PieceType.Rook)
        scene.board.set_piece(3, 19, piece=-PieceType.Rook)

        scene.board.set_piece(4, 17, piece=PieceType.Pawn)
        scene.board.set_piece(5, 17, piece=PieceType.Pawn)
        scene.board.set_piece(6, 17, piece=PieceType.Pawn)
        scene.board.set_piece(7, 17, piece=PieceType.Pawn)
        scene.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW2 = start
        scene.board.set_piece(*startW2, piece=-PieceType.Wave)

        startW1 = (2, 12)
        scene.board.set_piece(*startW1, piece=PieceType.Wave)

        startH = (3, 14)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        # for i in range(10):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(scene, start, rel, outward_arrows=False, count=24)

        for i in range(12):
            aba(str(12 - i), mark_type=MarkType.Blocked)

        # down arm Knight's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(10, 13), include_prev=False, count=1)

        # i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            # scene.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Action)
            # i += 1

        # down arm Pawn's displacement fields
        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=(16, 15), include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            # scene.append_field_marker(*pos, mark_type=MarkType.Legal)
            scene.append_text(str(i), *pos, mark_type=MarkType.Legal, corner=Corner.LowerLeftFieldMarker)
            i += 1

        return scene

    def scn_cot_28_backward_displacement_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_28_backward_displacement_start', bt)

        start = (15, 12)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperLeft)

        startW = (14, 10)
        scene.board.set_piece(*startW, piece=PieceType.Wave)

        startH = (16, 9)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRight)

        scene.append_arrow( *(startH + startW), mark_type=MarkType.Action )
        scene.append_arrow( *(startW + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # backward displacement

        startB = (15, 17)
        scene.board.set_piece(*startB, piece=-PieceType.Bishop)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startB, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i), *pos, mark_type=MarkType.Action, corner=Corner.UpperLeftFieldMarker)
            i += 1

        return scene

    def scn_cot_29_backward_displacement_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_29_backward_displacement_end', bt)

        startH2 = (15, 17)
        scene.board.set_piece(*startH2, piece=PieceType.Shaman)
        scene.append_text("T", *startH2, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        start = (15, 12)
        scene.board.set_piece(*start, piece=PieceType.Wave)

        startH = (14, 10)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Blocked if i < 2 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        #
        # backward displacement

        startB = (17, 13)
        scene.board.set_piece(*startB, piece=-PieceType.Bishop)

        return scene

    def scn_cot_30_forward_displacement_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_30_forward_displacement_start', bt)

        start = (20, 7)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        startW = (19, 5)
        scene.board.set_piece(*startW, piece=PieceType.Wave)

        startH = (21, 4)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        scene.append_arrow( *(startH + startW), mark_type=MarkType.Action )
        scene.append_arrow( *(startW + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, start, rel, count=24)

        for i in range(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # forward displacement

        startR = (14, 9)
        scene.board.set_piece(*startR, piece=-PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startR, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i), *pos, mark_type=MarkType.Action, corner=Corner.LowerRightFieldMarker)
            i += 1

        return scene

    def scn_cot_31_forward_displacement_step_2(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_31_forward_displacement_step_2', bt)

        start = (14, 9)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        startW = (20, 7)
        scene.board.set_piece(*startW, piece=PieceType.Wave)

        startH = (19, 5)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, startW, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Blocked if i < 3 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        #
        # forward displacement

        startR = (8, 11)
        scene.board.set_piece(*startR, piece=-PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startR, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            scene.append_field_marker(*pos, mark_type=MarkType.Action)
            scene.append_text(str(i), *pos, mark_type=MarkType.Action, corner=Corner.LowerRightFieldMarker)
            i += 1

        return scene

    def scn_cot_32_forward_displacement_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_32_forward_displacement_end', bt, x=-5, y=1)

        start = (8, 11)
        scene.board.set_piece(*start, piece=PieceType.Shaman)
        scene.append_text("T", *start, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        startW = (20, 7)
        # scene.board.set_piece(*startW, piece=PieceType.Wave)

        startH = (19, 5)
        # scene.board.set_piece(*startH, piece=PieceType.Shaman)
        # scene.append_text("S", *startH, mark_type=MarkType.Action, corner=Corner.UpperRightFieldMarker)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, startW, rel, count=24, is_with_field_marker=True)

        for i in range(16):
            mark_type = MarkType.Blocked if i < 7 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        #
        # forward displacement

        startR = (2, 13)
        scene.board.set_piece(*startR, piece=-PieceType.Rook)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=startR, include_prev=False, count=1)

        i = 1
        for pos in gen_abs_pos():
            x, y = pos
            mark_type = MarkType.Action if x >= 0 else MarkType.Illegal
            scene.append_field_marker(*pos, mark_type=mark_type)
            scene.append_text(str(i), *pos, mark_type=mark_type, corner=Corner.LowerRightFieldMarker)
            i += 1

        return scene

    def scn_cot_33_push_pull_entrancement_start(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_33_push_pull_entrancement_start', bt, width=5, height=8)

        startW1 = (3, 5)
        scene.board.set_piece(*startW1, piece=PieceType.Wave)
        scene.append_text("1", *startW1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startW2 = (1, 6)
        scene.board.set_piece(*startW2, piece=PieceType.Wave)
        scene.append_text("2", *startW2, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startH = (1, 1)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("H", *startH, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        scene.append_arrow( *(startW1 + startW2), mark_type=MarkType.Legal )

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=startH, include_prev=True) )
        for i in range(2):
            scene.append_arrow( *coords(), mark_type=MarkType.Legal )

        return scene

    def scn_cot_34_push_pull_entrancement_2(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_34_push_pull_entrancement_2', bt, width=5, height=8)

        startW1 = (1, 6)
        scene.board.set_piece(*startW1, piece=PieceType.Wave)
        scene.append_text("1", *startW1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        startH = (3, 5)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("H", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action)

        scene.append_arrow( *(startW1 + startH), mark_type=MarkType.Action )

        return scene

    def scn_cot_35_push_pull_entrancement_end(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_35_push_pull_entrancement_end', bt)

        startW1 = (1, 6)
        scene.board.set_piece(*startW1, piece=PieceType.Wave)
        scene.append_text("1", *startW1, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        startW2 = (3, 5)
        scene.board.set_piece(*startW2, piece=PieceType.Wave)
        scene.append_text("2", *startW2, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        startH = (5, 16)
        scene.board.set_piece(*startH, piece=PieceType.Shaman)
        scene.append_text("H", *startH, corner=Corner.UpperLeft, mark_type=MarkType.Action)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(scene, startW2, rel, count=24)

        for i in range(16):
            mark_type = MarkType.Blocked if i < 6 else MarkType.Legal
            aba(str(i + 1), mark_type=mark_type)

        return scene

    def place_scout_pawns_around_pieces(self, scene, piece_type):

        pt = PieceType(piece_type)
        b = scene.board
        row, dy, ddy, p = (0, 3, 1, PieceType.Pawn) if pt.is_light() else (b.get_height() - 1, -3, -1, -PieceType.Pawn)

        for i in range( b.get_width() ):
            if b.get_piece(i, row) == pt:
                adder = GS.adder((i, row), include_prev=False)
                b.set_piece_safe(*adder(-2, dy), piece=p)
                b.set_piece_safe(*adder(1, ddy), piece=p)
                b.set_piece_safe(*adder(2, 0), piece=p)
                b.set_piece_safe(*adder(1, -ddy), piece=p)

        return scene

    def scn_cot_36_scout_pawns(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('scn_cot_36_scout_pawns', bt)

        scene.board.set_piece(5, 0, piece=PieceType.Centaur)
        scene.board.set_piece(18, 0, piece=PieceType.Centaur)

        scene.board.set_piece(5, 23, piece=-PieceType.Centaur)
        scene.board.set_piece(18, 23, piece=-PieceType.Centaur)

        self.place_scout_pawns_around_pieces(scene, PieceType.Centaur)
        self.place_scout_pawns_around_pieces(scene, -PieceType.Centaur)


        scene.board.set_piece(10, 0, piece=PieceType.Shaman)
        scene.board.set_piece(13, 0, piece=PieceType.Shaman)

        scene.board.set_piece(10, 23, piece=-PieceType.Shaman)
        scene.board.set_piece(13, 23, piece=-PieceType.Shaman)

        self.place_scout_pawns_around_pieces(scene, PieceType.Shaman)
        self.place_scout_pawns_around_pieces(scene, -PieceType.Shaman)

        return scene


    #
    # test methods

    def test_cot_09_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        scene = Scene('test_cot_09_stop_sign_pattern_full', bt, x=-50, y=-50, width=124, height=124)

        start = (6, 11) # (11, 11)


        rel = (2, 1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal ) # right-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action ) # left-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked ) # left-down

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal ) # right-down


        rel = (-2, -1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in range(11):
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal ) # right-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action ) # left-up

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked ) # left-down

            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
            scene.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal ) # right-down


        return scene



def test_big_pattern():
    from scene_mix import SceneMix
    from save_scene import SaveScene
    from def_render import RenderingSizeEnum

    scene = SceneMix()
    ss = SaveScene(RenderingSizeEnum.Final)
    ss.render_example(scene, scene.test_cot_09_stop_sign_pattern_full, path_prefix='temp/') # , enforce_cot_in_bw=True)


if __name__ == '__main__':
    test_big_pattern()
