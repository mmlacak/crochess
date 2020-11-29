#!/usr/bin/env python3
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


class SceneAgeOfAquariusMixin:

    def scn_aoa_01_unicorn_same_color(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_same_color

        scene = Scene('scn_aoa_01_unicorn_same_color', bt, x=1.0, y=0.0, width=5, height=5)

        start = (3, 2)
        scene.board.set_piece(*start, piece=PieceType.Unicorn)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, bounds=scene.board_view.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_aoa_02_unicorn_opposite_color(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_opposite_color

        scene = Scene('scn_aoa_02_unicorn_opposite_color', bt)

        start = (6, 6)
        scene.board.set_piece(*start, piece=PieceType.Unicorn)

        # Unicorn, long jump

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_UNICORN_MULTI_REL_LONG_MOVES, start=start, bounds=((2, 2), (10, 10)))

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        # Knight, short jump

        gen_abs_pos_2 = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, bounds=((4, 4), (8, 8)))

        for i, pos in enumerate( gen_abs_pos_2() ):
            # scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, mark_type=MarkType.Blocked, corner=Corner.UpperRightFieldMarker)

        return scene

    #
    # Delayed promotion

    def scn_aoa_03_delayed_promo_init(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_promo_init

        scene = Scene('scn_aoa_03_delayed_promo_init', bt)

        startB = (12, 5)
        startA = (7, 10)
        startP1 = (11, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        scene.board.set_piece(*startP1, piece=PieceType.Pawn)
        scene.board.set_piece(*startP2, piece=PieceType.Pawn)
        scene.board.set_piece(*startP3, piece=PieceType.Pawn)
        scene.board.set_piece(*startA, piece=PieceType.Pyramid)
        scene.board.set_piece(*startB, piece=PieceType.Bishop)
        scene.board.set_piece(4, 4, piece=-PieceType.Unicorn)

        scene.append_text("1", *startP1, mark_type=MarkType.Blocked)
        scene.append_text("2", *startP2, mark_type=MarkType.Blocked)
        scene.append_text("3", *startP3, mark_type=MarkType.Blocked)

        # direction <-1, 1>
        coords = GS.gen_next( GS.gen_steps(start=startB, rels=[(-1, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        # direction <-1, 0>
        coords = GS.gen_next( GS.gen_steps(start=startA, rels=[(-1, 0), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )

        return scene

    def scn_aoa_04_delayed_promo_pawn_2_tagged(self, bt=BoardType.AgeOfAquarius):
        # move_unicorn_pawn_2_tagged

        scene = Scene('scn_aoa_04_delayed_promo_pawn_2_tagged', bt)

        endU = (3, 6)
        startP1 = (11, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        scene.board.set_piece(*startP1, piece=PieceType.Pawn)
        scene.board.set_piece(*startP2, piece=PieceType.Pawn)
        scene.board.set_piece(*startP3, piece=PieceType.Pawn)
        scene.board.set_piece(7, 10, piece=PieceType.Bishop)
        scene.board.set_piece(*endU, piece=-PieceType.Unicorn)

        scene.append_text("1", *startP1, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_text("2", *startP2, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_text("3", *startP3, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)

        scene.append_field_marker( *startP2, mark_type=MarkType.Legal ) # Action

        # direction <-1, 2>
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(-1, 2), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )

        return scene

    def scn_aoa_05_delayed_promo_pawn_2_moved(self, bt=BoardType.AgeOfAquarius):
        # scn_aoa_05_delayed_promo_pawn_1_to_promo
        # move_unicorn_pawn_1_to_promo

        scene = Scene('scn_aoa_05_delayed_promo_pawn_2_moved', bt)

        endU = (5, 9)
        startP1 = (11, 12)
        startP2 = (4, 11)
        startP3 = (4, 6)

        scene.board.set_piece(*startP1, piece=PieceType.Pawn)
        scene.board.set_piece(*startP2, piece=PieceType.Pawn)
        scene.board.set_piece(*startP3, piece=PieceType.Pawn)
        scene.board.set_piece(7, 10, piece=PieceType.Bishop)
        scene.board.set_piece(*endU, piece=-PieceType.Unicorn)

        scene.append_text("1", *startP1, mark_type=MarkType.Blocked)
        scene.append_text("2", *startP2, mark_type=MarkType.Blocked)
        scene.append_text("3", *startP3, mark_type=MarkType.Blocked)
        scene.append_text("P", 4, 10, mark_type=MarkType.Illegal)

        # direction <2, 3>
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(2, 3), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP1, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )

        return scene

    def scn_aoa_06_delayed_promo_pawn_1_tagged(self, bt=BoardType.AgeOfAquarius):
        # scn_aoa_06_delayed_promo_pawn_1_tagged
        # move_unicorn_pawn_1_tagged

        scene = Scene('scn_aoa_06_delayed_promo_pawn_1_tagged', bt)

        endU = (7, 10)
        startP1 = (11, 13)
        startP2 = (4, 11)
        startP3 = (4, 6)

        scene.board.set_piece(*startP1, piece=PieceType.Pawn)
        scene.board.set_piece(*startP2, piece=PieceType.Pawn)
        scene.board.set_piece(*startP3, piece=PieceType.Pawn)
        scene.board.set_piece(*endU, piece=-PieceType.Unicorn)

        scene.append_text("1", *startP1, mark_type=MarkType.Blocked, corner=Corner.UpperLeftFieldMarker)
        scene.append_text("2", *startP2, mark_type=MarkType.Blocked)
        scene.append_text("3", *startP3, mark_type=MarkType.Blocked)
        scene.append_text("P", 4, 10, mark_type=MarkType.Illegal)

        # direction <2, 1>
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(2, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction <0, 1>
        coords = GS.gen_next( GS.gen_steps(start=startP2, rels=[(0, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )

        scene.append_field_marker( *startP1, mark_type=MarkType.Legal )

        return scene

    def scn_aoa_07_delayed_promo_pawn_1_promoted(self, bt=BoardType.AgeOfAquarius):
        # scn_aoa_07_delayed_promo_pawn_2_attacked
        # move_unicorn_pawn_2_attacked

        scene = Scene('scn_aoa_07_delayed_promo_pawn_1_promoted', bt)

        endU = (4, 12)
        startQ = (11, 13)
        startP3 = (4, 6)

        scene.board.set_piece(*startQ, piece=PieceType.Queen)
        scene.board.set_piece(*startP3, piece=PieceType.Pawn)
        scene.board.set_piece(*endU, piece=-PieceType.Unicorn)

        scene.append_text("3", *startP3, mark_type=MarkType.Blocked)
        scene.append_text("P", 4, 10, mark_type=MarkType.Illegal)

        # direction <3, 2>
        coords = GS.gen_next( GS.gen_steps(end=endU, rels=[(-3, 2), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        return scene
