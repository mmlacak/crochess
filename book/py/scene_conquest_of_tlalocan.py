#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_desc import BoardDesc
from mark import MarkType
from scene import Corner, Scene


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


    def scn_cot_02_shaman_step_ply(self, bt=BoardType.ConquestOfTlalocan):

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

        return 'scn_cot_02_shaman_step_ply'

    def scn_cot_03_shaman_capture_ply(self, bt=BoardType.ConquestOfTlalocan):

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

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=False, count=4) )
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)
        self.board.set_piece(*coords(), piece=-PieceType.Wave)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)

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

        return 'scn_cot_03_shaman_capture_ply'

    def scn_cot_04_wave_activated(self, bt=BoardType.ConquestOfTlalocan):

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

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start, include_prev=False, count=4) )
        self.append_text("1", *coords(), mark_type=MarkType.Blocked) # (4, 10), captured Pawn
        self.append_text("2", *coords(), mark_type=MarkType.Blocked) # captured Pawn
        self.board.set_piece(*coords(), piece=PieceType.Shaman) # (15, 12)
        self.board.set_piece(*coords(), piece=-PieceType.Pawn)

        start_W = (15, 12)
        multi_rels = GS.convert_single_step_into_multi_rels( GS.remove( GS.DEFAULT_UNICORN_REL_LONG_MOVES, [(4, 1), (2, 3)] ) ) # (-4, -1),
        gen_pos = GS.gen_multi_steps(multi_rels, start=start_W, include_prev=True, bounds=self.board.get_position_limits())

        for pos in gen_pos():
            self.append_arrow( *pos )

        coords = GS.gen_next( GS.gen_steps([(4, 1), ], start=start_W, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps([(2, 3), ], start=start_W, include_prev=True, bounds=self.board.get_position_limits()) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

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

        return 'scn_cot_04_wave_activated'

    def scn_cot_05_stop_sign_pattern(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=12)

        self.append_arrow( *GS.add_to_all((6, 6, 7, 6)), mark_type=MarkType.Legal ) # right
        self.append_arrow( *GS.add_to_all((7, 6, 8, 7)), mark_type=MarkType.Legal ) # right-up

        self.append_arrow( *GS.add_to_all((8, 7, 8, 8)), mark_type=MarkType.Action ) # up
        self.append_arrow( *GS.add_to_all((8, 8, 7, 9)), mark_type=MarkType.Action ) # left-up

        self.append_arrow( *GS.add_to_all((7, 9, 6, 9)), mark_type=MarkType.Blocked ) # left
        self.append_arrow( *GS.add_to_all((6, 9, 5, 8)), mark_type=MarkType.Blocked ) # left-down

        self.append_arrow( *GS.add_to_all((5, 8, 5, 7)), mark_type=MarkType.Illegal ) # down
        self.append_arrow( *GS.add_to_all((5, 7, 6, 6)), mark_type=MarkType.Illegal ) # right-down

        return 'scn_cot_05_stop_sign_pattern'

    def scn_cot_06_stop_sign_pattern_unwind(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=12)

        self.append_arrow( *GS.add_to_all((6, 6, 7, 6)), mark_type=MarkType.Legal ) # right
        self.append_arrow( *GS.add_to_all((7, 6, 8, 7)), mark_type=MarkType.Legal ) # right-up

        self.append_arrow( *GS.add_to_all((8, 7, 8, 9)), mark_type=MarkType.Action ) # 2 up
        self.append_arrow( *GS.add_to_all((8, 9, 6, 11)), mark_type=MarkType.Action ) # 2 left-up

        self.append_arrow( *GS.add_to_all((6, 11, 3, 11)), mark_type=MarkType.Blocked ) # 3 left
        self.append_arrow( *GS.add_to_all((3, 11, 0, 8)), mark_type=MarkType.Blocked ) # 3 left-down

        self.append_arrow( *GS.add_to_all((0, 8, 0, 4)), mark_type=MarkType.Illegal ) # 4 down
        self.append_arrow( *GS.add_to_all((0, 4, 4, 0)), mark_type=MarkType.Illegal ) # 4 right-down

        return 'scn_cot_06_stop_sign_pattern_unwind'

    def scn_cot_07_stop_sign_pattern_full(self, bt=BoardType.ConquestOfTlalocan):

        bd = BoardDesc(reverse_field_colors=True, off_board_left=3, off_board_bottom=1, reverse_off_board_field_colors=True)
        self.init_scene(bt, width=21, height=23, board_desc=bd)


        self.append_arrow( *GS.add_to_all((11, 11, 12, 11)), mark_type=MarkType.Legal ) # right
        self.append_arrow( *GS.add_to_all((12, 11, 13, 12)), mark_type=MarkType.Legal ) # right-up

        self.append_arrow( *GS.add_to_all((13, 12, 13, 14)), mark_type=MarkType.Action ) # 2 up
        self.append_arrow( *GS.add_to_all((13, 14, 11, 16)), mark_type=MarkType.Action ) # 2 left-up

        self.append_arrow( *GS.add_to_all((11, 16, 8, 16)), mark_type=MarkType.Blocked ) # 3 left
        self.append_arrow( *GS.add_to_all((8, 16, 5, 13)), mark_type=MarkType.Blocked ) # 3 left-down

        self.append_arrow( *GS.add_to_all((5, 13, 5, 9)), mark_type=MarkType.Illegal ) # 4 down
        self.append_arrow( *GS.add_to_all((5, 9, 9, 5)), mark_type=MarkType.Illegal ) # 4 right-down


        self.append_arrow( *GS.add_to_all((9, 5, 14, 5)), mark_type=MarkType.Legal ) # 5 right
        self.append_arrow( *GS.add_to_all((14, 5, 19, 10)), mark_type=MarkType.Legal ) # 5 right-up

        self.append_arrow( *GS.add_to_all((19, 10, 19, 16)), mark_type=MarkType.Action ) # 6 up
        self.append_arrow( *GS.add_to_all((19, 16, 13, 22)), mark_type=MarkType.Action ) # 6 left-up

        self.append_arrow( *GS.add_to_all((13, 22, 6, 22)), mark_type=MarkType.Blocked ) # 7 left
        self.append_arrow( *GS.add_to_all((6, 22, -1, 15)), mark_type=MarkType.Blocked ) # 7 left-down

        self.append_arrow( *GS.add_to_all((-1, 15, -1, 7)), mark_type=MarkType.Illegal ) # 8 down
        self.append_arrow( *GS.add_to_all((-1, 7, 7, -1)), mark_type=MarkType.Illegal ) # 8 right-down


        self.append_arrow( *GS.add_to_all((7, -1, 16, -1)), mark_type=MarkType.Legal ) # 9 right
        self.append_arrow( *GS.add_to_all((16, -1, 25, 8)), mark_type=MarkType.Legal ) # 9 right-up

        self.append_arrow( *GS.add_to_all((25, 8, 25, 18)), mark_type=MarkType.Action ) # 10 up
        self.append_arrow( *GS.add_to_all((25, 18, 15, 28)), mark_type=MarkType.Action ) # 10 left-up

        self.append_arrow( *GS.add_to_all((15, 28, 4, 28)), mark_type=MarkType.Blocked ) # 11 left
        self.append_arrow( *GS.add_to_all((4, 28, -7, 17)), mark_type=MarkType.Blocked ) # 11 left-down

        self.append_arrow( *GS.add_to_all((-7, 17, -7, 5)), mark_type=MarkType.Illegal ) # 12 down
        self.append_arrow( *GS.add_to_all((-7, 5, 5, -7)), mark_type=MarkType.Illegal ) # 12 right-down


        return 'scn_cot_07_stop_sign_pattern_full'
