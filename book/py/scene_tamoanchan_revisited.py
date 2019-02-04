#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2019 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneTamoanchanRevisitedMixin(Scene):

    def scn_tr_01_serpent_diagonals(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        # self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        self.board.set_piece(*start, piece=PieceType.Serpent)

        self.append_arrow( 2, 2, 1, 3 )
        self.append_text('A', 1, 3, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 2, 2, 3, 1 )
        self.append_text('A', 3, 1, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Action )
        self.append_text('B', 3, 3, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 2, 2, 1, 1, mark_type=MarkType.Action )
        self.append_text('B', 1, 1, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_tr_01_serpent_diagonals'

    def scn_tr_02_serpent_1(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        self.board.set_piece(3, 3, piece=PieceType.Serpent)

        self.append_arrow( 3, 3, 2, 4 )
        self.append_text('A', 2, 4, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 3, 3, 4, 2 )
        self.append_text('A', 4, 2, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_tr_02_serpent_1'

    def scn_tr_03_serpent_2(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        self.append_text('1', 3, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        self.board.set_piece(2, 4, piece=PieceType.Serpent)

        self.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Action )
        self.append_text('B', 3, 5, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 2, 4, 1, 3, mark_type=MarkType.Action )
        self.append_text('B', 1, 3, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_tr_03_serpent_2'

    def scn_tr_04_serpent_3(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        self.append_text('1', 3, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        self.append_text('2', 2, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        self.board.set_piece(3, 5, piece=PieceType.Serpent)

        self.append_arrow( 3, 5, 2, 6 )
        self.append_text('A', 2, 6, rect=(0.15, 1.0, 0.7, 0.45))

        self.append_arrow( 3, 5, 4, 4 )
        self.append_text('A', 4, 4, rect=(0.15, 1.0, 0.7, 0.45))

        return 'scn_tr_04_serpent_3'

    def scn_tr_05_serpent_end(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        self.append_text('1', 3, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        self.append_text('2', 2, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        self.append_text('3', 3, 5, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 5, 4, 4, mark_type=MarkType.Blocked )

        self.append_text('4', 4, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 4, 4, 5, 5, mark_type=MarkType.Blocked )

        self.append_text('5', 5, 5, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 5, 5, 6, 4, mark_type=MarkType.Blocked )

        self.append_text('6', 6, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 6, 4, 5, 3, mark_type=MarkType.Blocked )

        self.append_text('7', 5, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 5, 3, 6, 2, mark_type=MarkType.Blocked )

        self.board.set_piece(6, 2, piece=PieceType.Serpent)

        return 'scn_tr_05_serpent_end'

    def scn_tr_06_serpent_neighbors(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType.Serpent)
        self.board.set_piece(1, 2, piece=PieceType.Wave)

        self.append_arrow( 2, 2, 3, 2 )
        self.append_arrow( 2, 2, 2, 3 )
        self.append_arrow( 2, 2, 1, 2, mark_type=MarkType.Blocked )
        self.append_arrow( 2, 2, 2, 1 )

        self.append_field_marker(3, 2)
        self.append_field_marker(2, 3)
        self.append_field_marker(1, 2, mark_type=MarkType.Blocked)
        self.append_field_marker(2, 1)

        return 'scn_tr_06_serpent_neighbors'

    def scn_tr_07_serpent_loop_1(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        self.append_text('1', 3, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        self.append_text('2', 2, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        self.board.set_piece(2, 6, piece=PieceType.Pyramid)
        self.board.set_piece(3, 5, piece=PieceType.Serpent)

        self.append_arrow( 3, 5, 2, 6, mark_type=MarkType.Action )
        self.append_arrow( 3, 5, 4, 4 )

        return 'scn_tr_07_serpent_loop_1'

    def scn_tr_08_serpent_loop_end(self, bt=BoardType.TamoanchanRevisited):

        self.init_scene(bt, width=8, height=8)

        start = (2, 2)
        self.append_text('S', *start, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 2, 3, 3, mark_type=MarkType.Blocked )

        self.append_text('1', 3, 3, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        # self.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Blocked )

        self.append_text('2', 2, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        # self.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Blocked )

        self.append_text('3', 3, 5, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 5, 4, 4, mark_type=MarkType.Blocked )

        self.append_text('4', 4, 4, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 4, 4, 3, 3, mark_type=MarkType.Action )

        self.append_text('5', 3, 3, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 3, 2, 4, mark_type=MarkType.Action )

        self.append_text('6', 2, 4, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 2, 4, 3, 5, mark_type=MarkType.Action )

        self.append_text('7', 3, 5, corner=Corner.UpperRight, mark_type=MarkType.Action, rect=(0.15, 1.0, 0.7, 0.45))
        self.append_arrow( 3, 5, 2, 6, mark_type=MarkType.Blocked )

        self.append_text('8', 2, 6, mark_type=MarkType.Blocked, rect=(0.15, 1.0, 0.7, 0.45))

        self.board.set_piece(2, 6, piece=PieceType.Serpent)

        return 'scn_tr_08_serpent_loop_end'
