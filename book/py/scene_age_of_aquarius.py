#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from gen_steps import DEFAULT_KNIGHT_REL_MOVES, DEFAULT_UNICORN_REL_LONG_MOVES, add, call_gen, get_gen_steps, get_gen_steps_prev, get_gen_multi_steps

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneAgeOfAquariusMixin(Scene):

    def scn_aoa_01_unicorn_same_color(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_same_color

        self.init_scene(bt, width=5, height=5)

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType.Unicorn)

        gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_aoa_01_unicorn_same_color'

    def scn_aoa_02_unicorn_opposite_color(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_opposite_color

        self.init_scene(bt)

        start = (6, 6)
        self.board.set_piece(*start, piece=PieceType.Unicorn)

        # Unicorn, long jump

        gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Knight, short jump

        gen_abs_pos_2 = get_gen_multi_steps(start=start, rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((4, 4), (8, 8)))

        i = 1
        for pos in gen_abs_pos_2():
            # self.append_field_marker(*pos)
            self.append_text(str(i), *pos, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_aoa_02_unicorn_opposite_color'

    #
    # Delayed promotion

    def scn_aoa_03_delayed_promo_init(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_promo_init

        self.init_scene(bt)

        startB = (12, 5)
        startA = (7, 10)
        startP1 = (11, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType.Pawn)
        self.board.set_piece(*startP2, piece=PieceType.Pawn)
        self.board.set_piece(*startP3, piece=PieceType.Pawn)
        self.board.set_piece(*startA, piece=PieceType.Pyramid)
        self.board.set_piece(*startB, piece=PieceType.Bishop)
        self.board.set_piece(4, 4, piece=-PieceType.Unicorn)

        self.append_text("1", *startP1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", *startP2, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("3", *startP3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <-1, 1>
        coords = call_gen( get_gen_steps_prev(start=startB, rel=(-1, 1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=startA, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_aoa_03_delayed_promo_init'

    def scn_aoa_04_delayed_promo_pawn_2_tagged(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_pawn_2_tagged

        self.init_scene(bt)

        endU = (3, 6)
        startP1 = (11, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType.Pawn)
        self.board.set_piece(*startP2, piece=PieceType.Pawn)
        self.board.set_piece(*startP3, piece=PieceType.Pawn)
        self.board.set_piece(7, 10, piece=PieceType.Bishop)
        self.board.set_piece(*endU, piece=-PieceType.Unicorn)

        self.append_text("1", *startP1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", *startP2, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("3", *startP3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_field_marker( *startP2, mark_type=MarkType.Legal ) # Action

        # direction <-1, 2>
        coords = call_gen( get_gen_steps_prev(end=endU, rel=(-1, 2)) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=startP2, rel=(0, 1)) )
        self.append_arrow( *coords() )

        return 'scn_aoa_04_delayed_promo_pawn_2_tagged'

    def scn_aoa_05_delayed_promo_pawn_2_moved(self, bt=BoardType.AgeOfAquarius):
        # scn_aoa_05_delayed_promo_pawn_1_to_promo
        # move_unicorn_pawn_1_to_promo

        self.init_scene(bt)

        endU = (5, 9)
        startP1 = (11, 12)
        startP2 = (4, 11)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType.Pawn)
        self.board.set_piece(*startP2, piece=PieceType.Pawn)
        self.board.set_piece(*startP3, piece=PieceType.Pawn)
        self.board.set_piece(7, 10, piece=PieceType.Bishop)
        self.board.set_piece(*endU, piece=-PieceType.Unicorn)

        self.append_text("1", *startP1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", *startP2, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("3", *startP3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("P", 4, 10, mark_type=MarkType.Illegal, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <2, 3>
        coords = call_gen( get_gen_steps_prev(end=endU, rel=(2, 3)) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=startP1, rel=(0, 1)) )
        self.append_arrow( *coords() )

        return 'scn_aoa_05_delayed_promo_pawn_2_moved'

    def scn_aoa_06_delayed_promo_pawn_1_tagged(self, bt=BoardType.AgeOfAquarius):
        # scn_aoa_06_delayed_promo_pawn_1_tagged
        # move_unicorn_pawn_1_tagged

        self.init_scene(bt)

        endU = (7, 10)
        startP1 = (11, 13)
        startP2 = (4, 11)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType.Pawn)
        self.board.set_piece(*startP2, piece=PieceType.Pawn)
        self.board.set_piece(*startP3, piece=PieceType.Pawn)
        self.board.set_piece(*endU, piece=-PieceType.Unicorn)

        self.append_text("1", *startP1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("2", *startP2, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("3", *startP3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("P", 4, 10, mark_type=MarkType.Illegal, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <2, 1>
        coords = call_gen( get_gen_steps_prev(end=endU, rel=(2, 1)) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = call_gen( get_gen_steps_prev(start=startP2, rel=(0, 1)) )
        self.append_arrow( *coords() )

        self.append_field_marker( *startP1, mark_type=MarkType.Legal )

        return 'scn_aoa_06_delayed_promo_pawn_1_tagged'

    def scn_aoa_07_delayed_promo_pawn_1_promoted(self, bt=BoardType.AgeOfAquarius):
        # scn_aoa_07_delayed_promo_pawn_2_attacked
        # move_unicorn_pawn_2_attacked

        self.init_scene(bt)

        endU = (4, 12)
        startQ = (11, 13)
        startP3 = (4, 6)

        self.board.set_piece(*startQ, piece=PieceType.Queen)
        self.board.set_piece(*startP3, piece=PieceType.Pawn)
        self.board.set_piece(*endU, piece=-PieceType.Unicorn)

        self.append_text("3", *startP3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_text("P", 4, 10, mark_type=MarkType.Illegal, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <3, 2>
        coords = call_gen( get_gen_steps_prev(end=endU, rel=(-3, 2)) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        return 'scn_aoa_07_delayed_promo_pawn_1_promoted'

    #
    # Converting tagged Pawn

    def scn_aoa_11_tagged_pawn_conv_init(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_tagged_pawn_conv_init

        self.init_scene(bt)

        startP = (3, 1)
        startPd = (8, 1)
        startA = (11, 0)
        startB = (7, 4)

        self.board.set_piece(*startP, piece=PieceType.Pawn)
        self.board.set_piece(*startPd, piece=-PieceType.Pawn)
        self.board.set_piece(*startA, piece=PieceType.Pyramid)
        self.board.set_piece(*startB, piece=PieceType.Bishop)

        # direction <0, -1>
        coords = call_gen( get_gen_steps_prev(start=startPd, rel=(0, -1)) )
        self.append_arrow( *coords() )

        self.append_text("1", *startP, mark_type=MarkType.Blocked )

        return 'scn_aoa_11_tagged_pawn_conv_init'

    def scn_aoa_12_tagged_pawn_conv_tagged(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_tagged_pawn_conv_tag

        self.init_scene(bt)

        startP = (3, 1)
        startPd = (8, 0)
        startA = (11, 0)
        startB = (7, 4)

        self.board.set_piece(*startP, piece=PieceType.Pawn)
        self.board.set_piece(*startPd, piece=-PieceType.Pawn)
        self.board.set_piece(*startA, piece=PieceType.Pyramid)
        self.board.set_piece(*startB, piece=PieceType.Bishop)

        # direction <1, -1>
        coords = call_gen( get_gen_steps_prev(start=startB, rel=(1, -1)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = call_gen( get_gen_steps_prev(start=startA, rel=(-1, 0)) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        self.append_text("1", *startP, mark_type=MarkType.Blocked)

        self.append_field_marker( *startPd, mark_type=MarkType.Action )

        return 'scn_aoa_12_tagged_pawn_conv_tagged'

    def scn_aoa_13_tagged_pawn_converted(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_tagged_pawn_conv_ed

        bt = BoardType(bt)
        self.init_scene(bt)

        startP = (3, 1)
        startPc = (8, 0)
        startB = (11, 0)

        self.board.set_piece(*startP, piece=PieceType.Pawn)
        self.board.set_piece(*startPc, piece=PieceType.Pawn)
        self.board.set_piece(*startB, piece=PieceType.Bishop)

        self.append_text("1", *startP, mark_type=MarkType.Blocked)
        self.append_text("2", *startPc, mark_type=MarkType.Blocked)

        size = (bt.get_size() + 1) // 2

        for i in xrange(3, size):
            self.append_text(str(i-2), 3, i)

        for i in xrange(2, size):
            self.append_text(str(i-1), 8, i)

        return 'scn_aoa_13_tagged_pawn_converted'
