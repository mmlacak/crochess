#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from piece import PieceType
from board import BoardType
from board import Board, BoardHints
from mark import Arrow, Text, FieldMarker

import gen_steps as GS
import scene_helper as SH


class SceneUnicornMixin(object):

    def move_unicorn_same_color(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt, 5, 5, BoardHints(reverse_field_colors=True))
        self.board.clear()
        self.delete_all_marks()

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType(-PieceType.Unicorn))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = GS.get_func_multi_steps(start=start, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_unicorn_same_color")

    def move_unicorn_opposite_color(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        start = (7, 6)
        self.board.set_piece(*start, piece=PieceType(-PieceType.Unicorn))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = GS.get_func_multi_steps(start=start, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((3, 11), (2, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_unicorn_opposite_color")
