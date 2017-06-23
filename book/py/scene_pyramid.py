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


class ScenePyramidMixin(object):

    def move_pyramid_activation_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 7, PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 7, PieceType(PieceType.Bishop))
        self.board.set_piece(11, 3, PieceType(PieceType.Pegasus))
        self.board.set_piece(3, 9, PieceType(-PieceType.Knight))

        self.arrows.append( SH.get_new_arrow(11, 3, 9, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(9, 4, 7, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 5, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 3, 7, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(9, 4, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(7, 5, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(5, 6, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 7, SH.Corner.UpperRight), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_activation_init")

    def move_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 7, PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, PieceType(PieceType.Bishop))
        self.board.set_piece(3, 9, PieceType(-PieceType.Knight))

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 7, 4, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 7, 5, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 6, 7, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(6, 7, 7, 7, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(6, 7, SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(7, 7, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 8, 3, 9, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 9, 3, 10, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(3, 10, 3, 11, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 8, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 9, SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(3, 10, SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 11, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 7, 2, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 7, 1, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 7, 0, 7, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(2, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(1, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(0, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 6, 3, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 6, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(3, 4, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_activated")

    def move_pyramid_activation_end(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(3, 7, PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, PieceType(PieceType.Bishop))
        self.board.set_piece(3, 9, PieceType(PieceType.Pyramid))

        return self.format_return_values("move_pyramid_activation_end")

    def move_pyramid_promo_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 7, PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 7, PieceType(PieceType.Pawn))
        self.board.set_piece(11, 3, PieceType(PieceType.Pegasus))

        self.arrows.append( SH.get_new_arrow(11, 3, 9, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(9, 4, 7, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 5, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 3, 7, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(9, 4, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(7, 5, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(5, 6, SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 7, SH.Corner.UpperRight), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_promo_init")

    def move_pyramid_promo_activate(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(3, 7, PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, PieceType(PieceType.Pawn))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 7, 4, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 7, 5, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 6, 7, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(6, 7, 7, 7, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(6, 7, SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(7, 7, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 8, 3, 9, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 9, 3, 10, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 10, 3, 11, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 8, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 9, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(3, 10, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 11, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 7, 2, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 7, 1, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 7, 0, 7, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(2, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(1, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(0, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 6, 3, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 6, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(3, 4, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_promo_activate")

    def move_pyramid_promo_end(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(3, 7, PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, PieceType(PieceType.Queen))

        return self.format_return_values("move_pyramid_promo_end")

    def move_pyramid_conversion_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 1, PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 1, PieceType(-PieceType.Rook))
        self.board.set_piece(7, 5, PieceType(PieceType.Bishop))

        self.arrows.append( SH.get_new_arrow(4, 2, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 3, 4, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 4, 5, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 5, 6, 4, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(6, 4, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(4, 2, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_conversion_init")

    def move_pyramid_conversion_activated(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 1, PieceType(PieceType.Bishop))
        self.board.set_piece(6, 1, PieceType(-PieceType.Rook))

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 1, 4, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 1, 5, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 1, 6, 1, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(6, 1, 7, 1, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(6, 1, SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(7, 1, SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 1, 3, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 2, 3, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 5, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 2, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(3, 4, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 1, 2, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 1, 1, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 1, 0, 1, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(2, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(1, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(0, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 1, 3, 0, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 0, SH.Corner.UpperLeft), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_conversion_activated")

    def move_pyramid_conversion_end(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(3, 1, PieceType(PieceType.Bishop))
        self.board.set_piece(6, 1, PieceType(PieceType.Rook))

        return self.format_return_values("move_pyramid_conversion_end")

    def move_pyramid_cascading_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt, 12, 3)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(1, 1, PieceType(PieceType.Rook))
        self.board.set_piece(5, 1, PieceType(PieceType.Pyramid))
        self.board.set_piece(7, 1, PieceType(PieceType.Pyramid))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        self.arrows.append( SH.get_new_arrow(1, 1, 2, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 1, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 1, 4, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 1, 5, 1, **get_arrow_colors_alt(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(2, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(4, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(5, 1, SH.Corner.UpperLeft), **get_text_colors_alt(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(5.4, 1.45, SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(7.4, 1.45, SH.Corner.Position), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_cascading_init")

    def move_pyramid_cascading_activated_1(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt, 12, 3)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(5, 1, PieceType(PieceType.Rook))
        self.board.set_piece(7, 1, PieceType(PieceType.Pyramid))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        self.arrows.append( SH.get_new_arrow(5, 1, 4, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 1, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 1, 2, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 1, 1, 1, **get_arrow_colors(True)) )

        self.arrows.append( SH.get_new_arrow(5, 1, 5, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 2, 5, 3, **get_arrow_colors(True)) )

        self.arrows.append( SH.get_new_arrow(5, 1, 5, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 0, 5, -1, **get_arrow_colors(True)) )

        self.arrows.append( SH.get_new_arrow(5, 1, 6, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 1, 7, 1, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(7, 1, 8, 1, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(8, 1, 9, 1, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(2, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(1, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(5, 2, SH.Corner.UpperLeft), **get_text_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(5, 0, SH.Corner.UpperLeft), **get_text_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(6, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(7, 1, SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(8, 1, SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(9, 1, SH.Corner.UpperLeft), **get_text_colors(False)) )

        self.texts.append( SH.get_new_text("2", *get_text_position(7.4, 1.45, SH.Corner.Position), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_cascading_activated_1")

    def move_pyramid_vs_king(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt, 12, 3)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(4, 0, PieceType(-PieceType.King))

        self.board.set_piece(3, 0, PieceType(PieceType.Pyramid))
        self.board.set_piece(2, 1, PieceType(PieceType.Queen))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))

        self.arrows.append( SH.get_new_arrow(2, 1, 3, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 0, 4, 0, **get_arrow_colors(False)) )

        return self.format_return_values("move_pyramid_vs_king")

    def move_pyramid_vs_bishop(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt, 12, 3)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(4, 0, PieceType(-PieceType.Bishop))

        self.board.set_piece(3, 0, PieceType(PieceType.Pyramid))
        self.board.set_piece(2, 1, PieceType(PieceType.Queen))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")

        self.arrows.append( SH.get_new_arrow(2, 1, 3, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 0, 4, 0, **get_arrow_colors_alt(True)) )

        return self.format_return_values("move_pyramid_vs_bishop")

    def get_example_mixin_methods(self):
        return  [ \
                self.move_pyramid_activation_init, \
                self.move_pyramid_activated, \
                self.move_pyramid_activation_end, \
                self.move_pyramid_promo_init, \
                self.move_pyramid_promo_activate, \
                self.move_pyramid_promo_end, \
                self.move_pyramid_conversion_init, \
                self.move_pyramid_conversion_activated, \
                self.move_pyramid_conversion_end, \
                self.move_pyramid_cascading_init, \
                self.move_pyramid_cascading_activated_1, \
                self.move_pyramid_vs_king, \
                self.move_pyramid_vs_bishop, \
                ]
