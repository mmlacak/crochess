#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2019 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_desc import BoardDesc
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneConquestOfTlalocanMixin(Scene):

    def scn_cot_01_shaman_movement(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        #
        # light Shaman

        start_lH = (6, 6)
        self.board.set_piece(*start_lH, piece=PieceType.Shaman)

        # light Shaman, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_lH, include_prev=False, bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # light Shaman, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_lH, include_prev=False, bounds=((4, 4), (8, 8)))

        i = 1
        for pos in gen_abs_pos_2():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        #
        # dark Shaman

        start_dH = (17, 17)
        self.board.set_piece(*start_dH, piece=-PieceType.Shaman)

        # dark Shaman, long jump

        gen_abs_pos_3 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_dH, include_prev=False, bounds=((13, 13), (21, 21)))

        i = 1
        for pos in gen_abs_pos_3():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # dark Shaman, short jump

        gen_abs_pos_4 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_dH, include_prev=False, bounds=((15, 15), (19, 19)))

        i = 1
        for pos in gen_abs_pos_4():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        #
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

        #
        # light Shaman 2

        start_lH2 = (17, 6)
        self.board.set_piece(*start_lH2, piece=PieceType.Shaman)

        # light Shaman 2, long jump

        gen_abs_pos_5 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_lH2, include_prev=False, bounds=((13, 2), (21, 10)))

        i = 1
        for pos in gen_abs_pos_5():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # light Shaman 2, short jump

        gen_abs_pos_6 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_lH2, include_prev=False, bounds=((15, 4), (19, 8)))

        i = 1
        for pos in gen_abs_pos_6():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        #
        # dark Shaman 2

        start_dH2 = (6, 17)
        self.board.set_piece(*start_dH2, piece=-PieceType.Shaman)

        # dark Shaman 2, long jump

        gen_abs_pos_7 = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start_dH2, include_prev=False, bounds=((2, 13), (10, 21)))

        i = 1
        for pos in gen_abs_pos_7():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # dark Shaman 2, short jump

        gen_abs_pos_8 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start_dH2, include_prev=False, bounds=((4, 15), (8, 19)))

        i = 1
        for pos in gen_abs_pos_8():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_cot_01_shaman_movement'

    #
    # light Shaman's plies

    def scn_cot_02_light_shaman_step_ply(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (3, 9)
        self.board.set_piece(*start, piece=PieceType.Shaman)
        self.board.set_piece(9, 6, piece=PieceType.Pyramid)
        self.board.set_piece(5, 5, piece=PieceType.Wave)
        self.board.set_piece(7, 17, piece=-PieceType.Bishop)
        self.board.set_piece(15, 19, piece=-PieceType.Knight)

        # (2, 1) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(2, 1), ], start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        # (-1, 2) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-1, 2), ], start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        # (-2, 1) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-2, 1), ], start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        # (-2, -1) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-2, -1), ], start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        # (-1, -2) ------------------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_steps([(-1, -2), ], start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        # (1, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, -1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, -1), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (1, -2) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, -2), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, -1), (2, 1)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=(9, 12), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=(17, 16), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        return 'scn_cot_02_light_shaman_step_ply'

    def scn_cot_03_light_shaman_capture_ply(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (3, 9)
        self.board.set_piece(*start, piece=PieceType.Shaman)

        start_W1 = (2, 5)
        self.board.set_piece(*start_W1, piece=PieceType.Wave)
        self.append_text("1", *start_W1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_A1 = (4, 5)
        self.board.set_piece(*start_A1, piece=PieceType.Pyramid)
        self.append_text("1", *start_A1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_W2 = (7, 3)
        self.board.set_piece(*start_W2, piece=PieceType.Wave)
        self.append_text("2", *start_W2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_A2 = (9, 5)
        self.board.set_piece(*start_A2, piece=PieceType.Pyramid)
        self.append_text("2", *start_A2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # (4, 1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=False, count=5) )
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=PieceType.Wave)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (-1, -4) ----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(-1, -4), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (1, -4) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, -4), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(3, 2), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(3, 2), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        self.board.set_piece(*GS.add(start, (13, 13)), piece=-PieceType.Knight)

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=GS.add(start, (9, 6)), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(-3, -2), (3, 2)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=GS.add(start, (15, 10)), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (2, 3) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        coords() # leave empty
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # empty -------------------------------------------------------------------------------------------------------------------

        self.board.set_piece(*GS.add(start, (8, -2)), piece=-PieceType.Pawn)
        self.board.set_piece(*GS.add(start, (12, -3)), piece=-PieceType.Pawn)

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(-1, -4), (1, -4), (3, 2), (4, 1), (2, 3), ] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Blocked )

        return 'scn_cot_03_light_shaman_capture_ply'

    #
    # dark Shaman's plies

    def scn_cot_04_dark_shaman_step_ply(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (3, 9)
        self.board.set_piece(*start, piece=-PieceType.Shaman)

        start_W = (7, 3)
        self.board.set_piece(*start_W, piece=-PieceType.Wave)

        start_A = (9, 5)
        self.board.set_piece(*start_A, piece=-PieceType.Pyramid)

        start_B = (7, 15)
        self.board.set_piece(*start_B, piece=PieceType.Bishop)

        start_N = (5, 17)
        self.board.set_piece(*start_N, piece=-PieceType.Knight)

        # (1, 4) ----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, 4), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, 3) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, -3) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, -3), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, -2) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(3, -2), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        self.board.set_piece(*GS.add(start, (8, 8)), piece=PieceType.Knight)

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=GS.add(start, (9, 6)), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (3, 2) change direction -------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(-3, -2), (3, 2)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=GS.add(start, (15, 10)), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # empty -------------------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(1, 4), (2, 3), (2, -3), (3, -2)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        return 'scn_cot_04_dark_shaman_step_ply'

    def scn_cot_05_dark_shaman_capture_ply(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (3, 9)
        self.board.set_piece(*start, piece=-PieceType.Shaman)
        self.board.set_piece(1, 13, piece=-PieceType.Pyramid)
        self.board.set_piece(1, 5, piece=-PieceType.Wave)

        # (2, 1) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 1), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        for i in xrange( 8 ):
            self.board.set_piece(*coords(), piece=PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pyramid)

        coords = GS.gen_next( GS.gen_steps([(2, 1), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        for i in xrange( 8 ):
            self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        self.board.set_piece(*GS.add(start, (5, 5)), piece=PieceType.Knight)

        # (-2, 1) ------------------------------------------------------------------------------------------------------------------

        self.board.set_piece(1, 10, piece=-PieceType.Wave)

        self.append_arrow( 3, 9, 1, 10, mark_type=MarkType.Action )

        # (-2, -1) ------------------------------------------------------------------------------------------------------------------

        self.board.set_piece(1, 8, piece=-PieceType.Pyramid)

        self.append_arrow( 3, 9, 1, 8, mark_type=MarkType.Action )

        # (1, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        self.board.set_piece(*coords(), piece=PieceType.Pawn)
        self.board.set_piece(*coords(), piece=PieceType.Pawn)
        coords() # keep empty
        self.board.set_piece(*coords(), piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(1, 2), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        for i in xrange(5):
            self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (2, -1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, -1), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        self.board.set_piece(*coords(), piece=PieceType.Pawn)
        self.board.set_piece(*coords(), piece=PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pyramid)
        self.board.set_piece(*coords(), piece=PieceType.Pawn)
        coords() # keep empty
        self.board.set_piece(*coords(), piece=PieceType.Pawn)

        coords = GS.gen_next( GS.gen_steps([(2, -1), ], start=start, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        for i in xrange(6):
            self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (1, -2) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(1, -2), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        coords() # keep empty
        self.board.set_piece(*coords(), piece=PieceType.Pawn)
        self.board.set_piece(*coords(), piece=PieceType.Pawn)

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, -1), (2, 1)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=(9, 12), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # (2, 1) change dir -------------------------------------------------------------------------------------------------------

        gen_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=(17, 16), include_prev=True, count=1)

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Illegal )

        # empty -------------------------------------------------------------------------------------------------------------------

        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_KNIGHT_REL_MOVES, [(-2, 1), (-2, -1), (2, 1), (1, 2), (2, -1)] ) )
        gen_pos = GS.gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos, mark_type=MarkType.Blocked )

        return 'scn_cot_05_dark_shaman_capture_ply'

    #
    # Wave activation

    def scn_cot_06_wave_activated(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (3, 9)

        start_W1 = (2, 5)
        self.board.set_piece(*start_W1, piece=PieceType.Wave)
        self.append_text("1", *start_W1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_A1 = (4, 5)
        self.board.set_piece(*start_A1, piece=PieceType.Pyramid)
        self.append_text("1", *start_A1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        start_W2 = (7, 3)
        self.board.set_piece(*start_W2, piece=PieceType.Wave)
        self.append_text("2", *start_W2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_A2 = (9, 5)
        self.board.set_piece(*start_A2, piece=PieceType.Pyramid)
        self.append_text("2", *start_A2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # (4, 1) -----------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=False, count=5) )
        self.append_text("1", *coords(), mark_type=MarkType.Blocked) # (4, 10), captured Pawn
        self.append_text("2", *coords(), mark_type=MarkType.Blocked) # captured Pawn
        self.board.set_piece(*coords(), piece=PieceType.Shaman) # (15, 12)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=PieceType.Pawn)

        start_W = (15, 12)
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(4, 1), (2, 3)] ) ) # (-4, -1),
        gen_pos = GS.gen_multi_steps(multi_rels, start=start_W, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start_W, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start_W, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # (3, 2) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(3, 2), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)

        self.board.set_piece(*GS.add(start, (13, 13)), piece=-PieceType.Knight)

        # (2, 3) ------------------------------------------------------------------------------------------------------------------

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start, include_prev=False, bounds=self.board.get_position_limits()) )
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        coords() # leave empty
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)

        # empty -------------------------------------------------------------------------------------------------------------------

        self.board.set_piece(*GS.add(start, (8, -2)), piece=-PieceType.Pawn)
        self.board.set_piece(*GS.add(start, (12, -3)), piece=-PieceType.Pawn)

        return 'scn_cot_06_wave_activated'

    #
    # trance-journey

    def scn_cot_07_trance_journey_init(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=12)

        start_H1 = (4, 9)
        self.board.set_piece(*start_H1, piece=PieceType.Shaman)
        self.append_text("1", *start_H1, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_W2 = (2, 5)
        self.board.set_piece(*start_W2, piece=PieceType.Wave)

        start_H2 = (6, 3)
        self.board.set_piece(*start_H2, piece=PieceType.Shaman)
        self.append_text("2", *start_H2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        start_W3 = (4, 7)
        self.board.set_piece(*start_W3, piece=PieceType.Wave)

        start_w1 = (6, 6)
        self.board.set_piece(*start_w1, piece=-PieceType.Wave)

        start_h1 = (7, 8)
        self.board.set_piece(*start_h1, piece=-PieceType.Shaman)
        self.append_text("3", *start_h1, corner=Corner.UpperRight, mark_type=MarkType.Action)

        coords = GS.gen_next( GS.gen_steps(start=start_H1, rels=[(-1, -2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_W2, rels=[(2, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_H2, rels=[(-1, 2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_W3, rels=[(2, -1), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start_w1, rels=[(1, 2), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_cot_07_trance_journey_init'

    def scn_cot_08_knight_directions(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=12)

        self.append_text("S", 6, 6, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=(0.05, 1.0, 0.6, 0.45))

        # left
        self.board.set_piece(5, 9, piece=PieceType.Knight)

        self.append_arrow( 5, 9, 4+0.5, 9+0.5, mark_type=MarkType.Blocked, start_pointer=False, end_pointer=False ) # left
        self.append_arrow( *GS.add_to_all( (4, 9, 3, 8), 0.5 ), mark_type=MarkType.Blocked, start_pointer=False, end_pointer=True ) # left down
        self.append_arrow( *GS.add_to_all( (4, 9, 3, 10), 0.5 ), mark_type=MarkType.Blocked, start_pointer=False, end_pointer=True ) # left up

        self.append_text("L", 3, 8, corner=Corner.LowerLeft, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))
        self.append_text("R", 3, 10, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # down
        self.board.set_piece(2, 7, piece=-PieceType.Knight)

        self.append_arrow( 2, 7, 2+0.5, 6+0.5, mark_type=MarkType.Illegal, start_pointer=False, end_pointer=False ) # down
        self.append_arrow( *GS.add_to_all( (2, 6, 1, 5), 0.5 ), mark_type=MarkType.Illegal, start_pointer=False, end_pointer=True ) # down left
        self.append_arrow( *GS.add_to_all( (2, 6, 3, 5), 0.5 ), mark_type=MarkType.Illegal, start_pointer=False, end_pointer=True ) # down right

        self.append_text("L", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Illegal, rect=(0.05, 1.0, 0.6, 0.45))
        self.append_text("R", 1, 5, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=(0.05, 1.0, 0.6, 0.45))

        # right
        self.board.set_piece(4, 4, piece=PieceType.Knight)

        self.append_arrow( 4, 4, 5+0.5, 4+0.5, mark_type=MarkType.Legal, start_pointer=False, end_pointer=False ) # right
        self.append_arrow( *GS.add_to_all( (5, 4, 6, 3), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # right down
        self.append_arrow( *GS.add_to_all( (5, 4, 6, 5), 0.5 ), mark_type=MarkType.Legal, start_pointer=False, end_pointer=True ) # right up

        self.append_text("L", 6, 5, corner=Corner.UpperRight, mark_type=MarkType.Legal, rect=(0.05, 1.0, 0.6, 0.45))
        self.append_text("R", 6, 3, corner=Corner.LowerRight, mark_type=MarkType.Legal, rect=(0.05, 1.0, 0.6, 0.45))

        # up
        self.board.set_piece(7, 6, piece=-PieceType.Knight)

        self.append_arrow( 7, 6, 7+0.5, 7+0.5, mark_type=MarkType.Action, start_pointer=False, end_pointer=False ) # up
        self.append_arrow( *GS.add_to_all( (7, 7, 6, 8), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up left
        self.append_arrow( *GS.add_to_all( (7, 7, 8, 8), 0.5 ), mark_type=MarkType.Action, start_pointer=False, end_pointer=True ) # up right

        self.append_text("L", 6, 8, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.05, 1.0, 0.6, 0.45))
        self.append_text("R", 8, 8, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.05, 1.0, 0.6, 0.45))

        return 'scn_cot_08_knight_directions'


    def scn_cot_09_stop_sign_pattern(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=12)

        start = (6, 6)
        self.append_text("S", *start, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=(0.05, 1.0, 0.6, 0.45))

        self.append_arrow( *GS.add_to_all( (6, 6, 7, 6), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
        self.append_arrow( *GS.add_to_all( (7, 6, 8, 7), 0.5 ), mark_type=MarkType.Legal ) # right-up

        self.append_text("1", 8, 7, corner=Corner.LowerRight, mark_type=MarkType.Legal, rect=(0.05, 1.0, 0.6, 0.45))

        self.append_arrow( *GS.add_to_all( (8, 7, 8, 8), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
        self.append_arrow( *GS.add_to_all( (8, 8, 7, 9), 0.5 ), mark_type=MarkType.Action ) # left-up

        self.append_text("2", 7, 9, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.05, 1.0, 0.6, 0.45))

        self.append_arrow( *GS.add_to_all( (7, 9, 6, 9), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
        self.append_arrow( *GS.add_to_all( (6, 9, 5, 8), 0.5 ), mark_type=MarkType.Blocked ) # left-down

        self.append_text("3", 5, 8, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        self.append_arrow( *GS.add_to_all( (5, 8, 5, 7), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
        self.append_arrow( *GS.add_to_all( (5, 7, 6, 6), 0.5 ), mark_type=MarkType.Illegal ) # right-down

        return 'scn_cot_09_stop_sign_pattern'

    def append_broken_arrow(self, start, rel, outward_arrows=True, bounds=None, count=None, rect=(0.05, 1.0, 0.6, 0.45)):

        rels = GS.gen_shaman_rel_legs(rel) # , count=count)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True, bounds=bounds) )
        corners = GS.gen_next( GS.gen_shaman_corners(rel) )

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
                self.append_arrow( *GS.add_to_all( leg_1, 0.5 ), mark_type=mark_type, start_pointer=sp1, end_pointer=ep1 ) # start
                self.append_arrow( *GS.add_to_all( leg_2, 0.5 ), mark_type=mark_type, start_pointer=sp2, end_pointer=ep2 ) # end, diagonal
            else:
                self.append_arrow( x0, y0, x1+0.5, y1+0.5, mark_type=mark_type, start_pointer=sp1, end_pointer=ep1 ) # start
                self.append_arrow( x2+0.5, y2+0.5, x3, y3, mark_type=mark_type, start_pointer=sp2, end_pointer=ep2 ) # end, diagonal

            if text is not None:
                self.append_text(text, x3, y3, corner=corners(), mark_type=mark_type, rect=rect)

        return _append_broken_arrow

    def scn_cot_10_stop_sign_pattern_unwind(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=12)

        start = (6, 6)
        rel = (2, 1)

        self.append_text("S", *start, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=(0.05, 1.0, 0.6, 0.45))

        aba = self.append_broken_arrow(start, rel, bounds=self.board.get_position_limits(), count=8)

        aba("1", mark_type=MarkType.Legal)
        aba("2", mark_type=MarkType.Action)
        aba("3", mark_type=MarkType.Blocked)
        aba("4", mark_type=MarkType.Illegal)

        return 'scn_cot_10_stop_sign_pattern_unwind'

    def scn_cot_11_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (5, 11)
        # self.board.set_piece(*start, piece=PieceType.Shaman)

        self.append_field_marker(*start, mark_type=MarkType.Illegal) # , mark_type=MarkType.Blocked
        self.append_text("S", *start, mark_type=MarkType.Illegal, rect=(0.15, 1.0, 0.7, 0.45)) # , mark_type=MarkType.Blocked

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(start, rel, count=24)

        for i in xrange(4):
            aba(str(4 * i + 1), mark_type=MarkType.Legal)
            aba(str(4 * i + 2), mark_type=MarkType.Action)
            aba(str(4 * i + 3), mark_type=MarkType.Blocked)
            aba(str(4 * i + 4), mark_type=MarkType.Illegal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(start, rel, count=24)

        for i in xrange(4):
            aba(str(4 * i + 1), mark_type=MarkType.Legal)
            aba(str(4 * i + 2), mark_type=MarkType.Action)
            aba(str(4 * i + 3), mark_type=MarkType.Blocked)
            aba(str(4 * i + 4), mark_type=MarkType.Illegal)

        return 'scn_cot_11_stop_sign_pattern_full'

    def scn_cot_12_light_shaman_trance_journey(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (5, 11)
        self.board.set_piece(*start, piece=PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(start, rel, count=24)

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        rel = (-2, -1)
        aba = self.append_broken_arrow(start, rel, count=24)

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Action)

        return 'scn_cot_12_light_shaman_trance_journey'

    def scn_cot_13_light_shaman_trance_journey_offset(self, bt=BoardType.ConquestOfTlalocan):

        bd = BoardDesc(reverse_field_colors=True, off_board_left=7, off_board_bottom=1, reverse_off_board_field_colors=True)
        self.init_scene(bt, width=17, height=23, board_desc=bd)

        start = (5, 11)
        self.board.set_piece(*start, piece=PieceType.Shaman)

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(start, rel, count=24)

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(start, rel, count=24)

        # for i in xrange(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return 'scn_cot_13_light_shaman_trance_journey_offset'

    def scn_cot_14_dark_shaman_trance_journey(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (5, 11)
        self.board.set_piece(*start, piece=-PieceType.Shaman)

        #
        # up arm

        rel = (1, 2)
        aba = self.append_broken_arrow(start, rel, outward_arrows=False, count=24)

        for i in xrange(16):
            aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rel = (-1, -2)
        aba = self.append_broken_arrow(start, rel, outward_arrows=False, count=24)

        for i in xrange(16):
            aba(str(12 - i), mark_type=MarkType.Action)

        return 'scn_cot_14_dark_shaman_trance_journey'

    def scn_cot_15_displacement_fields(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=9)

        start = (4, 4)
        self.board.set_piece(*start, piece=PieceType.Shaman)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_DISPLACEMENT_MULTI_REL_MOVES, start=start, include_prev=False, bounds=((1, 1), (7, 7)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=False, bounds=((2, 2), (6, 6)))

        i = 1
        for pos in gen_abs_pos_2():
            # self.append_field_marker(*pos, mark_type=MarkType.Blocked)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_cot_15_displacement_fields'

    def scn_cot_16_light_light_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (4, 12)
        self.board.set_piece(*start, piece=PieceType.Shaman)
        self.append_text("T", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        startT = (0, 0)
        self.board.set_piece(*startT, piece=PieceType.Star)
        self.board.set_piece(0, 23, piece=-PieceType.Star)
        self.board.set_piece(23, 0, piece=-PieceType.Star)
        self.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        self.board.set_piece(*startK, piece=PieceType.King)
        self.board.set_piece(4, 17, piece=PieceType.Knight)
        self.board.set_piece(12, 11, piece=-PieceType.Pawn)
        self.board.set_piece(18, 9, piece=-PieceType.Knight)

        self.board.set_piece(6, 7, piece=PieceType.Pawn)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(8, 7, piece=PieceType.Pawn)
        self.board.set_piece(9, 7, piece=PieceType.Pawn)
        self.board.set_piece(10, 7, piece=PieceType.Pawn)

        startW = (3, 10)
        self.board.set_piece(*startW, piece=PieceType.Wave)

        startH = (5, 9)
        self.board.set_piece(*startH, piece=PieceType.Shaman)
        self.append_text("S", *startH, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( *(startH + startW), mark_type=MarkType.Action )
        self.append_arrow( *(startW + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        rect = (0.05, 1.0, 0.8, 0.3)
        aba = self.append_broken_arrow(start, rel, count=24, rect=rect)

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(start, rel, count=24)

        # for i in xrange(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        # self.append_arrow( *((-1, 1) + startT), mark_type=MarkType.Illegal )
        # self.append_arrow( *((-1, 9) + startK), mark_type=MarkType.Illegal )
        self.replace_arrow( *((-7.5, 8.5) + startT), mark_type=MarkType.Illegal )
        self.replace_arrow( *((-1.5, 10.5) + startK), mark_type=MarkType.Illegal )

        self.replace_text("4", *startK, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=rect)
        self.replace_text("9", *startT, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=rect)

        return 'scn_cot_16_light_light_shaman_interaction_start'

    def scn_cot_17_light_light_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (4, 12)
        self.board.set_piece(*start, piece=PieceType.Wave)

        startT = (0, 0)
        self.board.set_piece(*startT, piece=PieceType.Star)
        self.board.set_piece(0, 23, piece=-PieceType.Star)
        self.board.set_piece(23, 0, piece=-PieceType.Star)
        self.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        self.board.set_piece(*startK, piece=PieceType.King)
        self.board.set_piece(7, 16, piece=PieceType.Knight)
        self.board.set_piece(9, 12, piece=-PieceType.Pawn)
        self.board.set_piece(18, 9, piece=-PieceType.Knight)

        self.board.set_piece(6, 7, piece=PieceType.Pawn)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(8, 7, piece=PieceType.Pawn)
        self.board.set_piece(9, 7, piece=PieceType.Pawn)
        self.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = (3, 10)
        self.board.set_piece(*startH, piece=PieceType.Shaman)
        self.append_text("S", *startH, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        startH1 = (6, 23)
        self.board.set_piece(*startH1, piece=PieceType.Shaman)
        self.append_text("T", *startH1, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        #
        # right arm

        rel = (2, 1)
        aba = self.append_broken_arrow(start, rel, count=24, rect=(0.05, 1.0, 0.8, 0.3))

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(start, rel, count=24)

        # for i in xrange(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return 'scn_cot_17_light_light_shaman_interaction_end'

    def scn_cot_18_dark_light_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (4, 12)
        self.board.set_piece(*start, piece=PieceType.Shaman)
        self.append_text("T", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        startT = (0, 0)
        self.board.set_piece(*startT, piece=PieceType.Star)
        self.board.set_piece(0, 23, piece=-PieceType.Star)
        self.board.set_piece(23, 0, piece=-PieceType.Star)
        self.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        self.board.set_piece(*startK, piece=PieceType.King)
        self.board.set_piece(4, 17, piece=PieceType.Knight)
        self.board.set_piece(12, 11, piece=-PieceType.Pawn)
        self.board.set_piece(18, 9, piece=-PieceType.Knight)

        self.board.set_piece(6, 7, piece=PieceType.Pawn)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(8, 7, piece=PieceType.Pawn)
        self.board.set_piece(9, 7, piece=PieceType.Pawn)
        self.board.set_piece(10, 7, piece=PieceType.Pawn)

        startW1 = (5, 11)
        self.board.set_piece(*startW1, piece=-PieceType.Wave)

        startW2 = (3, 8)
        self.board.set_piece(*startW2, piece=PieceType.Wave)

        startH = (8, 9)
        self.board.set_piece(*startH, piece=-PieceType.Shaman)
        self.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( *(startH + startW1), mark_type=MarkType.Action )
        self.append_arrow( *(startW1 + startW2), mark_type=MarkType.Action )
        self.append_arrow( *(startW2 + start), mark_type=MarkType.Action )

        #
        # right arm

        rel = (2, 1)
        rect = (0.05, 1.0, 0.8, 0.3)
        aba = self.append_broken_arrow(start, rel, count=24, rect=rect)

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Legal)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(start, rel, count=24)

        # for i in xrange(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        # self.append_arrow( *((-1, 1) + startT), mark_type=MarkType.Illegal )
        # self.append_arrow( *((-1, 9) + startK), mark_type=MarkType.Illegal )
        self.replace_arrow( *((-7.5, 8.5) + startT), mark_type=MarkType.Illegal )
        self.replace_arrow( *((-1.5, 10.5) + startK), mark_type=MarkType.Illegal )

        self.replace_text("4", *startK, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=rect)
        self.replace_text("9", *startT, corner=Corner.LowerLeft, mark_type=MarkType.Illegal, rect=rect)

        return 'scn_cot_18_dark_light_shaman_interaction_start'

    def scn_cot_19_dark_light_shaman_interaction_end(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (4, 12)

        startH0 = (6, 23)
        self.board.set_piece(*startH0, piece=PieceType.Shaman)
        self.append_text("T", *startH0, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        startT = (0, 0)
        self.board.set_piece(*startT, piece=PieceType.Star)
        self.board.set_piece(0, 23, piece=-PieceType.Star)
        self.board.set_piece(23, 0, piece=-PieceType.Star)
        self.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (2, 6)
        self.board.set_piece(*startK, piece=PieceType.King)
        # self.board.set_piece(4, 17, piece=PieceType.Knight)
        # self.board.set_piece(12, 11, piece=-PieceType.Pawn)
        self.board.set_piece(18, 9, piece=-PieceType.Knight)

        self.board.set_piece(6, 7, piece=PieceType.Pawn)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(8, 7, piece=PieceType.Pawn)
        self.board.set_piece(9, 7, piece=PieceType.Pawn)
        self.board.set_piece(10, 7, piece=PieceType.Pawn)

        startH = (5, 11)
        self.board.set_piece(*startH, piece=-PieceType.Shaman)
        self.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        startW1 = (3, 8)
        self.board.set_piece(*startW1, piece=-PieceType.Wave)

        startW2 = (4, 12)
        self.board.set_piece(*startW2, piece=PieceType.Wave)

        #
        # right arm

        rel = (2, 1)
        rect = (0.05, 1.0, 0.8, 0.3)
        aba = self.append_broken_arrow(start, rel, count=24, rect=rect)

        for i in xrange(16):
            aba(str(i + 1), mark_type=MarkType.Blocked)

        #
        # left arm

        # rel = (-2, -1)
        # aba = self.append_broken_arrow(start, rel, count=24)

        # for i in xrange(16):
            # aba(str(i + 1), mark_type=MarkType.Action)

        return 'scn_cot_19_dark_light_shaman_interaction_end'

    def scn_cot_20_dark_dark_shaman_interaction_start(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt)

        start = (4, 11)
        self.board.set_piece(*start, piece=-PieceType.Shaman)
        self.append_text("T", *start, corner=Corner.UpperLeft, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        startT = (0, 23)
        self.board.set_piece(0, 0, piece=PieceType.Star)
        self.board.set_piece(*startT, piece=-PieceType.Star)
        self.board.set_piece(23, 0, piece=-PieceType.Star)
        self.board.set_piece(23, 23, piece=PieceType.Star)

        startK = (5, 3)
        startP = (16, 15)
        self.board.set_piece(*startK, piece=PieceType.King)
        self.board.set_piece(3, 9, piece=-PieceType.Knight)
        self.board.set_piece(*startP, piece=-PieceType.Pawn)
        self.board.set_piece(10, 13, piece=PieceType.Knight)

        self.board.set_piece(4, 17, piece=PieceType.Pawn)
        self.board.set_piece(5, 17, piece=PieceType.Pawn)
        self.board.set_piece(6, 17, piece=PieceType.Pawn)
        self.board.set_piece(7, 17, piece=PieceType.Pawn)
        self.board.set_piece(8, 17, piece=PieceType.Pawn)

        startW1 = (6, 14)
        self.board.set_piece(*startW1, piece=-PieceType.Wave)

        startH = (7, 10)
        self.board.set_piece(*startH, piece=-PieceType.Shaman)
        self.append_text("S", *startH, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( *(startH + startW1), mark_type=MarkType.Action )
        self.append_arrow( *(startW1 + start), mark_type=MarkType.Action )

        #
        # up arm

        # rel = (1, 2)
        # aba = self.append_broken_arrow(start, rel, outward_arrows=False, count=24)

        # for i in xrange(16):
            # aba(str(10 - i), mark_type=MarkType.Legal)

        #
        # down arm

        rect = (0.05, 1.0, 0.6, 0.45)
        rel = (-1, -2)
        aba = self.append_broken_arrow(start, rel, outward_arrows=False, count=24, rect=rect)

        for i in xrange(16):
            aba(str(12 - i), mark_type=MarkType.Legal)

        self.replace_arrow( *(startP + (8.5, 23.5)), mark_type=MarkType.Illegal, end_pointer=False )
        self.replace_arrow( *((8.5, 23.5) + startT), mark_type=MarkType.Illegal )

        self.replace_arrow( *((-0.5, 3.5) + startK), mark_type=MarkType.Illegal )

        self.replace_text("6", *startT, corner=Corner.UpperLeft, mark_type=MarkType.Illegal, rect=rect)
        self.replace_text("8", *startK, corner=Corner.LowerRight, mark_type=MarkType.Illegal, rect=rect)

        return 'scn_cot_20_dark_dark_shaman_interaction_start'


    #
    # test methods

    def test_cot_09_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        bd = BoardDesc(off_board_left=50, off_board_top=50, off_board_right=50, off_board_bottom=50)
        self.init_scene(bt, board_desc=bd)

        start = (6, 11) # (11, 11)


        rel = (2, 1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in xrange(11):
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal ) # right-up

            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action ) # left-up

            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked ) # left-down

            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal ) # right-down


        rel = (-2, -1)
        # bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

        rels = GS.gen_shaman_rel_legs(rel)
        coords = GS.gen_next( GS.gen_steps(rels, start=start, include_prev=True) ) # , bounds=bounds

        for i in xrange(11):
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal, end_pointer=False ) # right
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Legal ) # right-up

            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action, end_pointer=False ) # up
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Action ) # left-up

            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked, end_pointer=False ) # left
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Blocked ) # left-down

            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal, end_pointer=False ) # down
            self.append_arrow( *GS.add_to_all( coords(), 0.5 ), mark_type=MarkType.Illegal ) # right-down


        return 'test_cot_09_stop_sign_pattern_full'



def test_big_pattern():
    from scene_mix import SceneMix
    from save_scene import SaveScene
    from def_render import RenderingSizeEnum

    scene = SceneMix()
    ss = SaveScene(RenderingSizeEnum.Final)
    ss.render_example(scene, scene.test_cot_09_stop_sign_pattern_full, path_prefix='temp/') # , enforce_cot_in_bw=True)


if __name__ == '__main__':
    test_big_pattern()

