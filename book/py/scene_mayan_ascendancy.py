#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneMayanAscendancyMixin(Scene):

    def scn_ma_01_pyramid_activation_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activation_init

        self.init_scene(bt)

        start = (11, 3)
        self.board.set_piece(3, 7, piece=PieceType.Pyramid)
        self.board.set_piece(6, 7, piece=PieceType.Bishop)
        self.board.set_piece(*start, piece=PieceType.Pegasus)
        self.board.set_piece(3, 9, piece=-PieceType.Knight)
        self.board.set_piece(3, 3, piece=-PieceType.Bishop)

        # direction <-2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ]) )
        self.append_text("1", *coords(), corner=Corner.UpperRight)
        self.append_text("2", *coords(), corner=Corner.UpperRight)
        self.append_text("3", *coords(), corner=Corner.UpperRight)
        self.append_text("4", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        return 'scn_ma_01_pyramid_activation_init'

    def scn_ma_02_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activated

        self.init_scene(bt)

        start = (3, 7)
        self.board.set_piece(*start, piece=PieceType.Pegasus)
        self.board.set_piece(6, 7, piece=PieceType.Bishop)
        self.board.set_piece(3, 9, piece=-PieceType.Knight)
        self.board.set_piece(3, 3, piece=-PieceType.Bishop)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)
        self.append_text("4", *coords(), mark_type=MarkType.Blocked)

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords(), mark_type=MarkType.Action)
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)
        self.append_text("4", *coords(), mark_type=MarkType.Blocked)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords(), mark_type=MarkType.Action)

        return 'scn_ma_02_pyramid_activated'

    def scn_ma_03_pyramid_activation_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activation_end

        self.init_scene(bt)

        self.board.set_piece(3, 7, PieceType.Pegasus)
        self.board.set_piece(6, 7, PieceType.Bishop)
        self.board.set_piece(3, 9, PieceType.Pyramid)
        self.board.set_piece(3, 3, -PieceType.Bishop)

        return 'scn_ma_03_pyramid_activation_end'

    #
    # Pawn activating Pyramid

    def scn_ma_04_pyramid_activation_by_pawn(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activation_by_pawn

        self.init_scene(bt)

        self.board.set_piece(4, 2, piece=PieceType.Pawn)
        self.board.set_piece(3, 3, piece=PieceType.Pyramid)

        self.board.set_piece(5, 6, piece=PieceType.Pawn)
        self.board.set_piece(5, 7, piece=PieceType.Pyramid)

        start = (8, 1)
        self.board.set_piece(*start, piece=PieceType.Pawn)
        self.board.set_piece(8, 4, piece=PieceType.Pyramid)

        # capture-fields
        self.append_arrow(4, 2, 3, 3, mark_type=MarkType.Action)
        self.append_arrow(4, 2, 5, 3, mark_type=MarkType.Blocked)

        self.append_text("1", 4, 2, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # step-fields 1
        self.append_arrow(5, 6, 5, 7, mark_type=MarkType.Blocked)

        self.append_text("2", 5, 6, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # step-fields 2
        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        self.append_text("3", *start, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return 'scn_ma_04_pyramid_activation_by_pawn'

    #
    # Promotion

    def scn_ma_05_promo_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_promo_init

        self.init_scene(bt)

        start = (11, 3)
        self.board.set_piece(3, 7, piece=PieceType.Pyramid)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(*start, piece=PieceType.Pegasus)
        self.board.set_piece(5, 0, piece=PieceType.Queen)

        self.append_text("1", 7, 7, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        self.append_text("2", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        # direction <-2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ]) )
        self.append_text("1", *coords(), corner=Corner.UpperRight)
        self.append_text("2", *coords(), corner=Corner.UpperRight)
        self.append_text("3", *coords(), corner=Corner.UpperRight)
        self.append_text("4", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        return 'scn_ma_05_promo_init'

    def scn_ma_06_promo_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_promo_activate

        self.init_scene(bt)

        start = (3, 7)
        self.board.set_piece(*start, piece=PieceType.Pegasus)
        self.board.set_piece(7, 7, piece=PieceType.Pawn)
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(5, 0, piece=PieceType.Queen)

        self.append_text("1", 7, 7, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        self.append_text("2", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords(), mark_type=MarkType.Action)

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords())

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        self.append_text("1", *coords() )
        self.append_text("2", *coords(), mark_type=MarkType.Blocked)
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)
        self.append_text("4", *coords(), mark_type=MarkType.Blocked)

        return 'scn_ma_06_promo_pyramid_activated'

    def scn_ma_07_promo_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_promo_end

        self.init_scene(bt)

        self.board.set_piece(3, 7, piece=PieceType.Pegasus)
        self.board.set_piece(7, 7, piece=PieceType.Queen)
        self.board.set_piece(3, 5, piece=PieceType.Pawn)
        self.board.set_piece(5, 0, piece=PieceType.Queen)

        self.append_text("2", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        return 'scn_ma_07_promo_end'

    #
    # Conversion

    def scn_ma_08_conversion_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_init

        self.init_scene(bt)

        start = (7, 8)
        self.board.set_piece(3, 4, piece=PieceType.Pyramid)
        self.board.set_piece(7, 4, piece=-PieceType.Rook)
        self.board.set_piece(3, 7, piece=-PieceType.Bishop)
        self.board.set_piece(*start, piece=PieceType.Bishop)
        self.board.set_piece(0, 0, piece=PieceType.Rook)
        self.board.set_piece(11, 0, piece=PieceType.Rook)

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords(), mark_type=MarkType.Action )

        return 'scn_ma_08_conversion_init'

    def scn_ma_09_conversion_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_activated

        self.init_scene(bt)

        start = (3, 4)
        self.board.set_piece(*start, piece=PieceType.Bishop)
        self.board.set_piece(7, 4, piece=-PieceType.Rook)
        self.board.set_piece(3, 7, piece=-PieceType.Bishop)
        self.board.set_piece(0, 0, piece=PieceType.Rook)
        self.board.set_piece(11, 0, piece=PieceType.Rook)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords(), mark_type=MarkType.Action )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords(), mark_type=MarkType.Blocked )
        self.append_text("4", *coords(), mark_type=MarkType.Blocked )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords())

        return 'scn_ma_09_conversion_pyramid_activated'

    def scn_ma_10_conversion_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_end

        self.init_scene(bt)

        self.board.set_piece(3, 4, piece=PieceType.Bishop)
        self.board.set_piece(7, 4, piece=PieceType.Rook)
        self.board.set_piece(3, 7, piece=-PieceType.Bishop)
        self.board.set_piece(0, 0, piece=PieceType.Rook)
        self.board.set_piece(11, 0, piece=PieceType.Rook)

        return 'scn_ma_10_conversion_end'

    #
    # Converting Rook with castling ability

    def scn_ma_11_convert_rook_castling_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_rook_init

        self.init_scene(bt, width=12, height=2)

        startQ = (4, 1)
        startA = (0, 1)
        self.board.set_piece(0, 0, piece=-PieceType.Rook)
        self.board.set_piece(*startA, piece=PieceType.Pyramid)
        self.board.set_piece(*startQ, piece=PieceType.Queen)
        self.board.set_piece(6, 0, piece=PieceType.King)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startQ, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startA, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_ma_11_convert_rook_castling_init'

    def scn_ma_12_convert_rook_castling_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_rook_end

        self.init_scene(bt, width=12, height=2)

        self.board.set_piece(0, 0, piece=PieceType(PieceType.Rook))
        self.board.set_piece(0, 1, piece=PieceType(PieceType.Queen))
        self.board.set_piece(6, 0, piece=PieceType(PieceType.King))

        # direction <-1, 0>
        start = (5, 0)
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        return 'scn_ma_12_convert_rook_castling_end'

    def scn_ma_13_convert_rook_castling(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_rook_castling

        self.init_scene(bt, width=12, height=2)

        self.board.set_piece(4, 0, piece=PieceType.Rook)
        self.board.set_piece(0, 1, piece=PieceType.Queen)
        self.board.set_piece(3, 0, piece=PieceType.King)

        # direction <-1, 0>
        start = (5, 0)
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords(), mark_type=MarkType.Blocked)
        self.append_text("2", *coords(), mark_type=MarkType.Blocked)
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)

        self.append_text("K", 6, 0, mark_type=MarkType.Blocked)

        return 'scn_ma_13_convert_rook_castling'

    #
    # Converting Pawn with rush ability

    def scn_ma_14_convert_pawn_rush_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_pawn_init

        self.init_scene(bt, width=3, height=6)

        startR = (0, 4)
        startP = (0, 1)
        self.board.set_piece(*startR, piece=PieceType.Rook)
        self.board.set_piece(*startP, piece=PieceType.Pyramid)
        self.board.set_piece(1, 1, piece=-PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.King)
        self.board.set_piece(2, 3, piece=-PieceType.Pawn)

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=startR, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startP, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        return 'scn_ma_14_convert_pawn_rush_init'

    def scn_ma_15_convert_pawn_rush_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_pawn_end

        self.init_scene(bt, width=3, height=6)

        self.board.set_piece(0, 1, piece=PieceType.Rook)
        self.board.set_piece(1, 1, piece=PieceType.Pawn)
        self.board.set_piece(1, 0, piece=PieceType.King)
        self.board.set_piece(2, 3, piece=-PieceType.Pawn)

        # direction <0, 1>
        start = (1, 2)
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        # direction <-1, -1>
        self.append_arrow(2, 3, 1, 2)

        return 'scn_ma_15_convert_pawn_rush_end'

    #
    # Pyramid cascading

    def scn_ma_16_cascading_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_init

        self.init_scene(bt)

        start = (10, 1)
        self.board.set_piece(*start, piece=PieceType.Queen)
        self.board.set_piece(2, 6, piece=PieceType.Bishop)

        pyramid_1 = (5, 6)
        self.board.set_piece(*pyramid_1, piece=PieceType.Pyramid)

        pyramid_2 = (8, 6)
        self.board.set_piece(*pyramid_2, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.4, 0.4)

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ]) )
        self.append_text("1", *coords(), corner=Corner.UpperRight)
        self.append_text("2", *coords(), corner=Corner.UpperRight)
        self.append_text("3", *coords(), corner=Corner.UpperRight)
        self.append_text("4", *coords(), corner=Corner.UpperRight)
        self.append_text("5", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action )

        # pyramids
        self.append_text("1", *GS.add(pyramid_1, offset), corner=Corner.Position, mark_type=MarkType.Blocked )
        self.append_text("2", *GS.add(pyramid_2, offset), corner=Corner.Position, mark_type=MarkType.Blocked )
        self.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked )

        return 'scn_ma_16_cascading_init'

    def scn_ma_17_cascading_pyramid_1_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_activated_1

        self.init_scene(bt)

        start = (5, 6)
        self.board.set_piece(*start, piece=PieceType.Queen)
        self.board.set_piece(2, 6, piece=PieceType.Bishop)

        pyramid_2 = (8, 6)
        self.board.set_piece(*pyramid_2, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.4, 0.4)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords(), mark_type=MarkType.Blocked)
        self.append_text("4", *coords(), mark_type=MarkType.Blocked)
        self.append_text("5", *coords(), mark_type=MarkType.Blocked)

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords())
        self.append_text("5", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords())
        self.append_text("5", *coords(), mark_type=MarkType.Blocked )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Action )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords(), mark_type=MarkType.Action )
        self.append_text("4", *coords(), mark_type=MarkType.Blocked )
        self.append_text("5", *coords(), mark_type=MarkType.Blocked )

        # pyramids
        self.append_text("2", *GS.add(pyramid_2, offset), corner=Corner.Position, mark_type=MarkType.Blocked )
        self.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked )

        return 'scn_ma_17_cascading_pyramid_1_activated'

    def scn_ma_18_cascading_pyramid_2_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_activated_2

        self.init_scene(bt)

        self.board.set_piece(5, 6, piece=PieceType.Queen)
        self.board.set_piece(2, 6, piece=PieceType.Bishop)

        start = (8, 6)
        self.board.set_piece(*start, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.4, 0.4)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())

        # pyramids
        self.append_text("1", *GS.add(start, offset), corner=Corner.Position, mark_type=MarkType.Blocked)
        self.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked)

        return 'scn_ma_18_cascading_pyramid_2_activated'

    def scn_ma_19_cascading_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_end

        self.init_scene(bt)

        self.board.set_piece(5, 6, piece=PieceType.Queen)
        self.board.set_piece(2, 6, piece=PieceType.Bishop)

        pyramid_1 = (8, 6)
        self.board.set_piece(*pyramid_1, piece=PieceType.Pyramid)

        pyramid_2 = (8, 8)
        self.board.set_piece(*pyramid_2, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.4, 0.4)

        self.append_text("1", *GS.add(pyramid_1, offset), corner=Corner.Position, mark_type=MarkType.Blocked)
        self.append_text("2", *GS.add(pyramid_2, offset), corner=Corner.Position, mark_type=MarkType.Blocked)
        self.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked)

        return 'scn_ma_19_cascading_end'

    #
    # Pyramid against royal powers (King free from actions, effects of passive pieces)

    def scn_ma_20_pyramid_vs_king(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_vs_king

        self.init_scene(bt, width=12, height=3)

        self.board.set_piece(4, 0, -PieceType.King)
        self.board.set_piece(3, 0, PieceType.Pyramid)
        self.board.set_piece(2, 1, PieceType.Queen)

        self.append_arrow(2, 1, 3, 0)
        self.append_arrow(3, 0, 4, 0, mark_type=MarkType.Illegal )

        return 'scn_ma_20_pyramid_vs_king'

    def scn_ma_21_pyramid_vs_bishop(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_vs_bishop

        self.init_scene(bt, width=12, height=3)

        self.board.set_piece(4, 0, -PieceType.Bishop)
        self.board.set_piece(3, 0, PieceType.Pyramid)
        self.board.set_piece(2, 1, PieceType.Queen)

        self.append_arrow(2, 1, 3, 0)
        self.append_arrow(3, 0, 4, 0, mark_type=MarkType.Action)

        return 'scn_ma_21_pyramid_vs_bishop'
