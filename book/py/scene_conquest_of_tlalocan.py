#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
# from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, DEFAULT_NEIGHBOURING_REL_MOVES, DEFAULT_KING_REL_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
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

        gen_abs_pos = GS.get_gen_multi_steps(start=start_lH, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # light Shaman, short jump

        gen_abs_pos_2 = GS.get_gen_multi_steps(start=start_lH, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((4, 4), (8, 8)))

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

        gen_abs_pos_3 = GS.get_gen_multi_steps(start=start_dH, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((13, 13), (21, 21)))

        i = 1
        for pos in gen_abs_pos_3():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # dark Shaman, short jump

        gen_abs_pos_4 = GS.get_gen_multi_steps(start=start_dH, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((15, 15), (19, 19)))

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

        gen_abs_pos_5 = GS.get_gen_multi_steps(start=start_lH2, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((13, 2), (21, 10)))

        i = 1
        for pos in gen_abs_pos_5():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # light Shaman 2, short jump

        gen_abs_pos_6 = GS.get_gen_multi_steps(start=start_lH2, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((15, 4), (19, 8)))

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

        gen_abs_pos_7 = GS.get_gen_multi_steps(start=start_dH2, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((2, 13), (10, 21)))

        i = 1
        for pos in gen_abs_pos_7():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # dark Shaman 2, short jump

        gen_abs_pos_8 = GS.get_gen_multi_steps(start=start_dH2, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((4, 15), (8, 19)))

        i = 1
        for pos in gen_abs_pos_8():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_cot_01_shaman_movement'

    def scn_cot_02_activating_passives(self, bt=BoardType.ConquestOfTlalocan):

        self.init_scene(bt, width=9, height=9)

        #
        # light Shaman

        start_lH = (4, 4)
        self.board.set_piece(*start_lH, piece=PieceType.Shaman)

        pos_P1 = (3, 2)
        self.board.set_piece(*pos_P1, piece=PieceType.Pyramid)
        self.board.set_piece(7, 6, piece=PieceType.Pyramid)
        self.board.set_piece(6, 3, piece=PieceType.Wave)
        self.board.set_piece(1, 6, piece=PieceType.Wave)

        self.append_text("1", *pos_P1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", 7, 6, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("1", 6, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", 1, 6, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        # light Shaman, long jump

        gen_abs_pos = GS.get_gen_multi_steps(start=start_lH, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((0, 0), (8, 8)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos, mark_type=MarkType.Action)
            # self.append_text(str(i), *pos, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # light Shaman, short jump

        gen_abs_pos_2 = GS.get_gen_multi_steps(start=start_lH, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((2, 2), (6, 6)))

        i = 1
        for pos in gen_abs_pos_2():
            if pos == pos_P1:
                self.append_field_marker(*pos_P1, mark_type=MarkType.Illegal)
            else:
                self.append_field_marker(*pos, mark_type=MarkType.Legal)
                # self.append_text(str(i), *pos, mark_type=MarkType.Legal, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_cot_02_activating_passives'
