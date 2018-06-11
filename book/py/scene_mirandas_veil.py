#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017, 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneMirandasVeilMixin(Scene):

    def scn_mv_01_move_wave_init(self, bt=BoardType.MirandasVeil):
        # move_wave_init

        self.init_scene(bt)

        self.board.set_piece(1, 1, piece=PieceType.Wave)
        self.board.set_piece(0, 3, piece=PieceType.Knight)

        self.append_arrow(0, 3, 1, 1, mark_type=MarkType.Action)

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        self.board.set_piece(5, 9, piece=-PieceType.Queen)
        self.board.set_piece(7, 13, piece=PieceType.Bishop)

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType.King)
        self.board.set_piece(5, 3, piece=PieceType.Rook)
        self.board.set_piece(7, 4, piece=PieceType.Pyramid)
        self.board.set_piece(9, 5, piece=-PieceType.King)
        self.board.set_piece(13, 7, piece=-PieceType.Wave)

        return 'scn_mv_01_move_wave_init'

    def scn_mv_02_move_wave_activated(self, bt=BoardType.MirandasVeil):
        # move_wave_activated

        self.init_scene(bt)

        start = (1, 1)
        self.board.set_piece(*start, piece=PieceType.Knight)

        self.append_arrow( *(start + (0, 3)) ) # 1, 1, 0, 3
        self.append_arrow( *(start + (3, 0)) ) # 1, 1, 3, 0

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        self.board.set_piece(5, 9, piece=-PieceType.Queen)
        self.board.set_piece(7, 13, piece=PieceType.Bishop)

        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 2)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Ilegal )
        self.append_arrow( *coords(), mark_type=MarkType.Ilegal )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType.King)
        self.board.set_piece(5, 3, piece=PieceType.Rook)
        self.board.set_piece(7, 4, piece=PieceType.Pyramid)
        self.board.set_piece(9, 5, piece=-PieceType.King)
        self.board.set_piece(13, 7, piece=-PieceType.Wave)

        coords = call_gen( get_gen_steps_prev(start=start, rel=(2, 1)) )
        self.append_arrow( *coords(), mark_type=MarkType.Ilegal )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Ilegal )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )

        return 'scn_mv_02_move_wave_activated'

    def scn_mv_03_move_wave_finished(self, bt=BoardType.MirandasVeil):
        # move_wave_finished

        self.init_scene(bt)

        self.board.set_piece(1, 1, piece=PieceType.Knight)

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(4, 7, piece=-PieceType.Pyramid)
        self.board.set_piece(5, 9, piece=-PieceType.Queen)
        self.board.set_piece(7, 13, piece=PieceType.Wave)

        self.append_arrow(7, 13, 6, 12)
        self.append_arrow(7, 13, 6, 14)
        self.append_arrow(7, 13, 8, 12)
        self.append_arrow(7, 13, 8, 14)

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType.King)
        self.board.set_piece(5, 3, piece=PieceType.Rook)
        self.board.set_piece(7, 4, piece=PieceType.Pyramid)
        self.board.set_piece(9, 5, piece=-PieceType.King)
        self.board.set_piece(13, 7, piece=-PieceType.Wave)

        return 'scn_mv_03_move_wave_finished'

    #
    # cascading

    def scn_mv_04_cascading_rook(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_rook

        self.init_scene(bt, width=9, height=9)

        start = (5, 7)
        self.board.set_piece(*start, piece=PieceType.Rook)
        self.board.set_piece(5, 3, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=PieceType.Wave)
        self.board.set_piece(3, 5, piece=PieceType.Queen)

        self.append_text("1", 5, 3, mark_type=MarkType.Blocked)
        self.append_text("2", 3, 3, mark_type=MarkType.Blocked)

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_mv_04_cascading_rook'

    def scn_mv_05_cascading_wave_1(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_1

        self.init_scene(bt, width=9, height=9)

        start = (5, 3)
        self.board.set_piece(*start, piece=PieceType.Rook)
        self.board.set_piece(3, 3, piece=PieceType.Wave)
        self.board.set_piece(3, 5, piece=PieceType.Queen)

        self.append_text("2", 3, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_05_cascading_wave_1'

    def scn_mv_06_cascading_wave_2(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_2

        self.init_scene(bt, width=9, height=9)

        start = (3, 3)
        self.board.set_piece(5, 3, piece=PieceType.Rook)
        self.board.set_piece(*start, piece=PieceType.Wave)
        self.board.set_piece(3, 5, piece=PieceType.Queen)

        self.append_text("1", 3, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_06_cascading_wave_2'

    def scn_mv_07_cascading_rook_2nd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_rook_b

        self.init_scene(bt, width=9, height=9)

        start = (5, 3)
        self.board.set_piece(*start, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=PieceType.Wave)
        self.board.set_piece(3, 5, piece=PieceType.Queen)

        self.append_text("1", 3, 3, mark_type=MarkType.Blocked)
        self.append_text("2", 5, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_07_cascading_rook_2nd_time'

    def scn_mv_08_cascading_wave_1_2nd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_1_b

        self.init_scene(bt, width=9, height=9)

        start = (3, 3)
        self.board.set_piece(3, 5, piece=PieceType.Queen)
        self.board.set_piece(*start, piece=PieceType.Rook)
        self.board.set_piece(5, 3, piece=PieceType.Wave)

        self.append_text("2", 5, 3, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_08_cascading_wave_1_2nd_time'

    def scn_mv_09_cascading_queen(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_queen

        self.init_scene(bt, width=9, height=9)

        start = (3, 5)
        self.board.set_piece(*start, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=PieceType.Rook)
        self.board.set_piece(5, 3, piece=PieceType.Wave)

        self.append_text("1", 3, 5, mark_type=MarkType.Blocked )
        self.append_text("2", 5, 3, mark_type=MarkType.Blocked )

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <1, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <1, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_09_cascading_queen'

    def scn_mv_10_cascading_wave_2_3rd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_2_b

        self.init_scene(bt, width=9, height=9)

        start = (5, 3)
        self.board.set_piece(3, 5, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=PieceType.Rook)
        self.board.set_piece(*start, piece=PieceType.Queen)

        self.append_text("1", 3, 5, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <1, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <1, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_10_cascading_wave_2_3rd_time'

    def scn_mv_11_cascading_wave_1_3rd_time(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_wave_1_c

        self.init_scene(bt, width=9, height=9)

        start = (3, 5)
        self.board.set_piece(*start, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=PieceType.Rook)
        self.board.set_piece(5, 3, piece=PieceType.Queen)

        self.append_text("2", 3, 5, mark_type=MarkType.Blocked )

        # direction <1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <1, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <1, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, 1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        # direction <-1, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(-1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        return 'scn_mv_11_cascading_wave_1_3rd_time'

    def scn_mv_12_cascading_end(self, bt=BoardType.MirandasVeil):
        # move_wave_cascading_end

        self.init_scene(bt, width=9, height=9)

        self.board.set_piece(3, 5, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=PieceType.Rook)
        self.board.set_piece(3, 1, piece=PieceType.Wave)
        self.board.set_piece(5, 3, piece=PieceType.Queen)

        self.append_text("1", 3, 1, mark_type=MarkType.Blocked)
        self.append_text("2", 3, 5, mark_type=MarkType.Blocked)

        return 'scn_mv_12_cascading_end'

    #
    # cascading opponent

    def scn_mv_13_casc_oppo_light_queen(self, bt=BoardType.MirandasVeil):
        # move_wave_opponent_light_queen

        self.init_scene(bt, width=9, height=9)

        start = (5, 6)
        self.board.set_piece(*start, piece=PieceType.Queen)
        self.board.set_piece(5, 3, piece=PieceType.Wave)
        self.board.set_piece(3, 3, piece=-PieceType.Wave)
        self.board.set_piece(3, 5, piece=-PieceType.Queen)

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_mv_13_casc_oppo_light_queen'
