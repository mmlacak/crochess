#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from piece import PieceType
from board import BoardType
from board import Board, BoardHints
from mark import Arrow, Text, FieldMarker

import move_gen as MG
import scene_helper as SH


class ScenePegasusMixin(object):

    def move_pegasus_initial(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt, 5, 5)
        self.board.clear()
        self.delete_all_marks()

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.20, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = MG.get_gen_abs_pos(MG.gen_knight_rel_moves, start=start, pos_limits=self.board.get_position_limits())

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_pegasus_initial")

    def move_pegasus_direction(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt, 10, 10)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_text_position_2 = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.45, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        # main direction, i.e. <1, 2>

        self.arrows.append( SH.get_new_arrow(2, 1, 3, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 4, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 5, 5, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 6, 9, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(4, 5, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(5, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(6, 9, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction 2a, i.e. <2, 1>

        self.arrows.append( SH.get_new_arrow(4, 5, 6, 6, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2a", *get_text_position_2(6, 6, SH.Corner.UpperRight), **get_text_colors(False)) )

        # direction 2b, i.e. <2, -1>

        self.arrows.append( SH.get_new_arrow(4, 5, 6, 4, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2b", *get_text_position_2(6, 4, SH.Corner.UpperRight), **get_text_colors(False)) )

        # direction 2c, i.e. <1, -2>

        self.arrows.append( SH.get_new_arrow(4, 5, 5, 3, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2c", *get_text_position_2(5, 3, SH.Corner.UpperRight), **get_text_colors(False)) )

        # direction 2d, i.e. <-2, -1>

        self.arrows.append( SH.get_new_arrow(4, 5, 2, 4, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2d", *get_text_position(2, 4, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction 2e, i.e. <-2, 1>

        self.arrows.append( SH.get_new_arrow(4, 5, 2, 6, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2e", *get_text_position(2, 6, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction 2f, i.e. <-1, 2>

        self.arrows.append( SH.get_new_arrow(4, 5, 3, 7, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2f", *get_text_position_2(3, 7, SH.Corner.UpperRight), **get_text_colors(False)) )

        return self.format_return_values("move_pegasus_direction")

    def move_pegasus(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        self.board.set_piece(5, 7, PieceType(PieceType.Pawn))
        self.board.set_piece(6, 3, PieceType(-PieceType.Pawn))

        self.board.set_piece(3, 4, PieceType(-PieceType.Rook))
        self.board.set_piece(4, 4, PieceType(-PieceType.Rook))

        self.board.set_piece(5, 2, PieceType(PieceType.Rook))
        self.board.set_piece(5, 3, PieceType(PieceType.Rook))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        # direction 1, i.e. <2, 1>

        self.arrows.append( SH.get_new_arrow(2, 1, 4, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 2, 6, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 3, 8, 4, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 2, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(6, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(8, 4, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction 2, i.e. <1, 2>

        self.arrows.append( SH.get_new_arrow(2, 1, 3, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 4, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 5, 5, 7, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 6, 9, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(4, 5, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 7, SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(6, 9, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction 3, i.e. <-1, 2>

        self.arrows.append( SH.get_new_arrow(2, 1, 1, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 3, 0, 5, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("3", *get_text_position(1, 3, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(0, 5, SH.Corner.UpperRight), **get_text_colors(True)) )

        # direction 4, i.e. <-2, 1>

        self.arrows.append( SH.get_new_arrow(2, 1, 0, 2, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("4", *get_text_position(0, 2, SH.Corner.UpperRight), **get_text_colors(True)) )

        # direction 5, i.e. <-2, -1>

        self.arrows.append( SH.get_new_arrow(2, 1, 0, 0, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("5", *get_text_position(0, 0, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction 6, i.e. <2, -1>

        self.arrows.append( SH.get_new_arrow(2, 1, 4, 0, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("6", *get_text_position(4, 0, SH.Corner.UpperRight), **get_text_colors(True)) )

        return self.format_return_values("move_pegasus")
