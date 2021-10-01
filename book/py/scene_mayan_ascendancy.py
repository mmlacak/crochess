#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneMayanAscendancyMixin:

    def scn_ma_01_pyramid_activation_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activation_init

        scene = Scene('scn_ma_01_pyramid_activation_init', bt)

        start = (11, 3)
        scene.board.set_piece(3, 7, piece=PieceType.Pyramid)
        scene.board.set_piece(6, 7, piece=PieceType.Bishop)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(3, 3, piece=-PieceType.Bishop)

        # direction <-2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ]) )
        scene.append_text("1", *coords(), corner=Corner.UpperRight)
        scene.append_text("2", *coords(), corner=Corner.UpperRight)
        scene.append_text("3", *coords(), corner=Corner.UpperRight)
        scene.append_text("4", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_ma_02_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activated

        scene = Scene('scn_ma_02_pyramid_activated', bt)

        start = (3, 7)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)
        scene.board.set_piece(6, 7, piece=PieceType.Bishop)
        scene.board.set_piece(3, 9, piece=-PieceType.Knight)
        scene.board.set_piece(3, 3, piece=-PieceType.Bishop)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("4", *coords(), mark_type=MarkType.Blocked)

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords(), mark_type=MarkType.Action)
        scene.append_text("3", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("4", *coords(), mark_type=MarkType.Blocked)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords(), mark_type=MarkType.Action)

        return scene

    def scn_ma_03_pyramid_activation_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activation_end

        scene = Scene('scn_ma_03_pyramid_activation_end', bt)

        scene.board.set_piece(3, 7, PieceType.Pegasus)
        scene.board.set_piece(6, 7, PieceType.Bishop)
        scene.board.set_piece(3, 9, PieceType.Pyramid)
        scene.board.set_piece(3, 3, -PieceType.Bishop)

        return scene

    #
    # Pawn activating Pyramid

    def scn_ma_04_pyramid_activation_by_pawn(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_activation_by_pawn

        scene = Scene('scn_ma_04_pyramid_activation_by_pawn', bt)

        scene.board.set_piece(4, 2, piece=PieceType.Pawn)
        scene.board.set_piece(3, 3, piece=PieceType.Pyramid)

        scene.board.set_piece(5, 6, piece=PieceType.Pawn)
        scene.board.set_piece(5, 7, piece=PieceType.Pyramid)

        start = (8, 1)
        scene.board.set_piece(*start, piece=PieceType.Pawn)
        scene.board.set_piece(8, 4, piece=PieceType.Pyramid)

        # capture-fields
        scene.append_arrow(4, 2, 3, 3, mark_type=MarkType.Action)
        scene.append_arrow(4, 2, 5, 3, mark_type=MarkType.Blocked)

        scene.append_text("1", 4, 2, corner=Corner.UpperRightFieldMarker, mark_type=MarkType.Blocked)

        # step-fields 1
        scene.append_arrow(5, 6, 5, 7, mark_type=MarkType.Blocked)

        scene.append_text("2", 5, 6, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        # step-fields 2
        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        scene.append_text("3", *start, corner=Corner.UpperRight, mark_type=MarkType.Blocked)

        return scene

    #
    # Promotion

    def scn_ma_05_promo_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_promo_init

        scene = Scene('scn_ma_05_promo_init', bt)

        start = (11, 3)
        scene.board.set_piece(3, 7, piece=PieceType.Pyramid)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)
        scene.board.set_piece(5, 0, piece=PieceType.Queen)

        scene.append_text("1", 7, 7, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("2", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        # direction <-2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-2, 1), ]) )
        scene.append_text("1", *coords(), corner=Corner.UpperRight)
        scene.append_text("2", *coords(), corner=Corner.UpperRight)
        scene.append_text("3", *coords(), corner=Corner.UpperRight)
        scene.append_text("4", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action)

        return scene

    def scn_ma_06_promo_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_promo_activate

        scene = Scene('scn_ma_06_promo_pyramid_activated', bt)

        start = (3, 7)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)
        scene.board.set_piece(7, 7, piece=PieceType.Pawn)
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)
        scene.board.set_piece(5, 0, piece=PieceType.Queen)

        scene.append_text("1", 7, 7, corner=Corner.LowerRight, mark_type=MarkType.Blocked)
        scene.append_text("2", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords(), mark_type=MarkType.Action)

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords())

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        scene.append_text("1", *coords() )
        scene.append_text("2", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("3", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("4", *coords(), mark_type=MarkType.Blocked)

        return scene

    def scn_ma_07_promo_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_promo_end

        scene = Scene('scn_ma_07_promo_end', bt)

        scene.board.set_piece(3, 7, piece=PieceType.Pegasus)
        scene.board.set_piece(7, 7, piece=PieceType.Queen)
        scene.board.set_piece(3, 5, piece=PieceType.Pawn)
        scene.board.set_piece(5, 0, piece=PieceType.Queen)

        scene.append_text("2", 3, 5, corner=Corner.LowerRight, mark_type=MarkType.Blocked)

        return scene

    #
    # Conversion

    def scn_ma_08_conversion_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_init

        scene = Scene('scn_ma_08_conversion_init', bt)

        start = (7, 8)
        scene.board.set_piece(3, 4, piece=PieceType.Pyramid)
        scene.board.set_piece(7, 4, piece=-PieceType.Rook)
        scene.board.set_piece(3, 7, piece=-PieceType.Bishop)
        scene.board.set_piece(*start, piece=PieceType.Bishop)
        scene.board.set_piece(0, 0, piece=PieceType.Rook)
        scene.board.set_piece(11, 0, piece=PieceType.Rook)

        # direction <-1, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, -1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords(), mark_type=MarkType.Action )

        return scene

    def scn_ma_09_conversion_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_activated

        scene = Scene('scn_ma_09_conversion_pyramid_activated', bt)

        start = (3, 4)
        scene.board.set_piece(*start, piece=PieceType.Bishop)
        scene.board.set_piece(7, 4, piece=-PieceType.Rook)
        scene.board.set_piece(3, 7, piece=-PieceType.Bishop)
        scene.board.set_piece(0, 0, piece=PieceType.Rook)
        scene.board.set_piece(11, 0, piece=PieceType.Rook)

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords(), mark_type=MarkType.Action )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords(), mark_type=MarkType.Blocked )
        scene.append_text("4", *coords(), mark_type=MarkType.Blocked )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords())

        return scene

    def scn_ma_10_conversion_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_conversion_end

        scene = Scene('scn_ma_10_conversion_end', bt)

        scene.board.set_piece(3, 4, piece=PieceType.Bishop)
        scene.board.set_piece(7, 4, piece=PieceType.Rook)
        scene.board.set_piece(3, 7, piece=-PieceType.Bishop)
        scene.board.set_piece(0, 0, piece=PieceType.Rook)
        scene.board.set_piece(11, 0, piece=PieceType.Rook)

        return scene

    #
    # Pyramid cascading

    def scn_ma_11_cascading_init(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_init

        scene = Scene('scn_ma_11_cascading_init', bt)

        start = (10, 1)
        scene.board.set_piece(*start, piece=PieceType.Queen)
        scene.board.set_piece(2, 6, piece=PieceType.Bishop)

        pyramid_1 = (5, 6)
        scene.board.set_piece(*pyramid_1, piece=PieceType.Pyramid)

        pyramid_2 = (8, 6)
        scene.board.set_piece(*pyramid_2, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        scene.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.45, 0.15)

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 1), ]) )
        scene.append_text("1", *coords(), corner=Corner.UpperRight)
        scene.append_text("2", *coords(), corner=Corner.UpperRight)
        scene.append_text("3", *coords(), corner=Corner.UpperRight)
        scene.append_text("4", *coords(), corner=Corner.UpperRight)
        scene.append_text("5", *coords(), corner=Corner.UpperRight, mark_type=MarkType.Action )

        # pyramids
        scene.append_text("1", *GS.add(pyramid_1, offset), corner=Corner.Position, mark_type=MarkType.Blocked )
        scene.append_text("2", *GS.add(pyramid_2, offset), corner=Corner.Position, mark_type=MarkType.Blocked )
        scene.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked )

        return scene

    def scn_ma_12_cascading_pyramid_1_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_activated_1

        scene = Scene('scn_ma_12_cascading_pyramid_1_activated', bt)

        start = (5, 6)
        scene.board.set_piece(*start, piece=PieceType.Queen)
        scene.board.set_piece(2, 6, piece=PieceType.Bishop)

        pyramid_2 = (8, 6)
        scene.board.set_piece(*pyramid_2, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        scene.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.45, 0.15)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("4", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("5", *coords(), mark_type=MarkType.Blocked)

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords())
        scene.append_text("5", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords())
        scene.append_text("5", *coords(), mark_type=MarkType.Blocked )

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords(), mark_type=MarkType.Action )
        scene.append_text("4", *coords(), mark_type=MarkType.Blocked )
        scene.append_text("5", *coords(), mark_type=MarkType.Blocked )

        # pyramids
        scene.append_text("2", *GS.add(pyramid_2, offset), corner=Corner.Position, mark_type=MarkType.Blocked )
        scene.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked )

        return scene

    def scn_ma_13_cascading_pyramid_2_activated(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_activated_2

        scene = Scene('scn_ma_13_cascading_pyramid_2_activated', bt)

        scene.board.set_piece(5, 6, piece=PieceType.Queen)
        scene.board.set_piece(2, 6, piece=PieceType.Bishop)

        start = (8, 6)
        scene.board.set_piece(*start, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        scene.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.45, 0.15)

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())

        # direction <1, 0>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 0), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())

        # direction <0, -1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, -1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(0, 1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())

        # pyramids
        scene.append_text("1", *GS.add(start, offset), corner=Corner.Position, mark_type=MarkType.Blocked)
        scene.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked)

        return scene

    def scn_ma_14_cascading_end(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_cascading_end

        scene = Scene('scn_ma_14_cascading_end', bt)

        scene.board.set_piece(5, 6, piece=PieceType.Queen)
        scene.board.set_piece(2, 6, piece=PieceType.Bishop)

        pyramid_1 = (8, 6)
        scene.board.set_piece(*pyramid_1, piece=PieceType.Pyramid)

        pyramid_2 = (8, 8)
        scene.board.set_piece(*pyramid_2, piece=PieceType.Pyramid)

        pyramid_3 = (5, 1)
        scene.board.set_piece(*pyramid_3, piece=PieceType.Pyramid)

        offset = (0.45, 0.15)

        scene.append_text("1", *GS.add(pyramid_1, offset), corner=Corner.Position, mark_type=MarkType.Blocked)
        scene.append_text("2", *GS.add(pyramid_2, offset), corner=Corner.Position, mark_type=MarkType.Blocked)
        scene.append_text("3", *GS.add(pyramid_3, offset), corner=Corner.Position, mark_type=MarkType.Blocked)

        return scene

    #
    # Pyramid against royal powers (King free from actions, effects of passive pieces)

    def scn_ma_15_pyramid_vs_king(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_vs_king

        scene = Scene('scn_ma_15_pyramid_vs_king', bt, width=12, height=3)

        scene.board.set_piece(4, 0, -PieceType.King)
        scene.board.set_piece(3, 0, PieceType.Pyramid)
        scene.board.set_piece(2, 1, PieceType.Queen)

        scene.append_arrow(2, 1, 3, 0)
        scene.append_arrow(3, 0, 4, 0, mark_type=MarkType.Illegal )

        return scene

    def scn_ma_16_pyramid_vs_bishop(self, bt=BoardType.MayanAscendancy):
        # move_pyramid_vs_bishop

        scene = Scene('scn_ma_16_pyramid_vs_bishop', bt, width=12, height=3)

        scene.board.set_piece(4, 0, -PieceType.Bishop)
        scene.board.set_piece(3, 0, PieceType.Pyramid)
        scene.board.set_piece(2, 1, PieceType.Queen)

        scene.append_arrow(2, 1, 3, 0)
        scene.append_arrow(3, 0, 4, 0, mark_type=MarkType.Action)

        return scene
