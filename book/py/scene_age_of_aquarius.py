#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2019 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

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

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, bounds=self.board.get_position_limits())

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

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start, bounds=((2, 2), (10, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, bounds=((4, 4), (8, 8)))

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
        coords = GS.gen_next( GS.gen_steps(start=startB, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startA, rels=[(-1, 0), ], include_prev=True) )
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
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(-1, 2), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(0, 1), ], include_prev=True) )
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
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(2, 3), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP1, rels=[(0, 1), ], include_prev=True) )
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
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(2, 1), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(0, 1), ], include_prev=True) )
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
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(-3, 2), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        return 'scn_aoa_07_delayed_promo_pawn_1_promoted'

    #
    # Converting tagged Pawn

    def scn_aoa_11_tagged_pawn_conv_init(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_tagged_pawn_conv_init

        self.init_scene(bt)

        #
        # 1, conversion in Pawn row

        startR1 = (0, 7)
        startA1 = (0, 1)
        startPd1 = (1, 1)
        startAd1 = (2, 1)
        startRd1 = (2, 11)

        self.board.set_piece(*startR1, piece=PieceType.Rook)
        self.board.set_piece(*startA1, piece=PieceType.Pyramid)
        self.board.set_piece(*startPd1, piece=-PieceType.Pawn)
        self.board.set_piece(*startAd1, piece=-PieceType.Pyramid)
        self.board.set_piece(*startRd1, piece=-PieceType.Rook)

        self.append_text("1", *startPd1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startRd1, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startAd1, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # 2, conversion outside piece rows

        startR2 = (5, 7)
        startA2 = (5, 2)
        startPd2 = (6, 2)
        startAd2 = (7, 2)
        startRd2 = (7, 11)

        self.board.set_piece(*startR2, piece=PieceType.Rook)
        self.board.set_piece(*startA2, piece=PieceType.Pyramid)
        self.board.set_piece(*startPd2, piece=-PieceType.Pawn)
        self.board.set_piece(*startAd2, piece=-PieceType.Pyramid)
        self.board.set_piece(*startRd2, piece=-PieceType.Rook)

        self.append_text("2", *startPd2, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startRd2, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startAd2, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # 3, conversion in figure row

        startR3 = (10, 7)
        startA3 = (10, 0)
        startPd3 = (11, 1)

        self.board.set_piece(*startR3, piece=PieceType.Rook)
        self.board.set_piece(*startA3, piece=PieceType.Pyramid)
        self.board.set_piece(*startPd3, piece=-PieceType.Pawn)

        self.append_text("3", *startPd3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startPd3, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )

        #
        # 4, normal rush

        startP = (13, 1)

        self.board.set_piece(*startP, piece=PieceType.Pawn)

        self.append_text("4", *startP, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_aoa_11_tagged_pawn_conv_init'

    def scn_aoa_12_tagged_pawn_conv_tagged(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_tagged_pawn_conv_tag

        self.init_scene(bt)

        #
        # 1, conversion in Pawn row

        startR1 = (0, 7)
        startA1 = (0, 1)
        startPd1 = (1, 1)
        startRd1 = (2, 1)

        self.board.set_piece(*startR1, piece=PieceType.Rook)
        self.board.set_piece(*startA1, piece=PieceType.Pyramid)
        self.board.set_piece(*startPd1, piece=-PieceType.Pawn)
        self.board.set_piece(*startRd1, piece=-PieceType.Rook)

        self.append_text("1", *startPd1, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_field_marker(*startPd1, mark_type=MarkType.Action)

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startR1, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startA1, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # 2, conversion outside piece rows

        startR2 = (5, 7)
        startA2 = (5, 2)
        startPd2 = (6, 2)
        startRd2 = (7, 2)

        self.board.set_piece(*startR2, piece=PieceType.Rook)
        self.board.set_piece(*startA2, piece=PieceType.Pyramid)
        self.board.set_piece(*startPd2, piece=-PieceType.Pawn)
        self.board.set_piece(*startRd2, piece=-PieceType.Rook)

        self.append_text("2", *startPd2, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_field_marker(*startPd2, mark_type=MarkType.Action)

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startR2, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startA2, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # 3, conversion in figure row

        startR3 = (10, 7)
        startA3 = (10, 0)
        startPd3 = (11, 0)

        self.board.set_piece(*startR3, piece=PieceType.Rook)
        self.board.set_piece(*startA3, piece=PieceType.Pyramid)
        self.board.set_piece(*startPd3, piece=-PieceType.Pawn)

        self.append_text("3", *startPd3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_field_marker(*startPd3, mark_type=MarkType.Action)

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startR3, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startA3, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        #
        # 4, normal rush

        startP = (13, 1)

        self.board.set_piece(*startP, piece=PieceType.Pawn)

        self.append_text("4", *startP, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_aoa_12_tagged_pawn_conv_tagged'

    def scn_aoa_13_tagged_pawn_converted(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_tagged_pawn_conv_ed

        self.init_scene(bt)

        bt = BoardType(bt)
        size = (bt.get_size() + 1) // 2

        #
        # 1, conversion in Pawn row

        startR1 = (0, 1)
        startP1c = (1, 1)
        startRd1 = (2, 1)

        self.board.set_piece(*startR1, piece=PieceType.Rook)
        self.board.set_piece(*startP1c, piece=PieceType.Pawn)
        self.board.set_piece(*startRd1, piece=-PieceType.Rook)

        self.append_text("1", *startP1c, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        for i in xrange(3, size):
            self.append_text(str(i-2), 1, i, corner=Corner.UpperRight)

        #
        # 2, conversion outside piece rows

        startR2 = (5, 2)
        startP2c = (6, 2)
        startRd2 = (7, 2)

        self.board.set_piece(*startR2, piece=PieceType.Rook)
        self.board.set_piece(*startP2c, piece=PieceType.Pawn)
        self.board.set_piece(*startRd2, piece=-PieceType.Rook)

        self.append_text("2", *startP2c, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        for i in xrange(4, size):
            self.append_text(str(i-3), 6, i, corner=Corner.UpperRight, mark_type=MarkType.Illegal)

        #
        # 3, conversion in figure row

        startR3 = (10, 0)
        startP3c = (11, 0)

        self.board.set_piece(*startR3, piece=PieceType.Rook)
        self.board.set_piece(*startP3c, piece=PieceType.Pawn)

        self.append_text("3", *startP3c, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        for i in xrange(2, size):
            self.append_text(str(i-1), 11, i, corner=Corner.UpperRight)

        #
        # 4, normal rush

        startP = (13, 1)

        self.board.set_piece(*startP, piece=PieceType.Pawn)

        self.append_text("4", *startP, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        for i in xrange(3, size):
            self.append_text(str(i-2), 13, i, corner=Corner.UpperRight)

        return 'scn_aoa_13_tagged_pawn_converted'

    def scn_aoa_14_pawn_figure_piece_rush_rows(self, bt=BoardType.AgeOfAquarius):

        self.init_scene(bt)

        self.append_arrow(0.7, 13.5, 13.3, 13.5, mark_type=MarkType.Blocked, start_pointer=True, end_pointer=True)
        self.append_arrow(0.7, 12.5, 13.3, 12.5, mark_type=MarkType.Illegal, start_pointer=True, end_pointer=True)

        self.append_arrow(0.7, 1.5, 13.3, 1.5, mark_type=MarkType.Action, start_pointer=True, end_pointer=True)
        self.append_arrow(0.7, 0.5, 13.3, 0.5, mark_type=MarkType.Legal, start_pointer=True, end_pointer=True)

        for i in xrange(0, 14):
            self.append_text(str(i + 1), 0, i, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_aoa_14_pawn_figure_piece_rush_rows'
