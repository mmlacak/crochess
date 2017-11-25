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


class ScenePyramidMixin(object):

    def move_pyramid_activation_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        start = (11, 3)
        self.board.set_piece(3, 7, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 7, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))
        self.board.set_piece(3, 9, piece=PieceType(-PieceType.Knight))

        # direction <-2, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-2, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-2, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors_alt(True)) )

        return self.format_return_values("move_pyramid_activation_init")

    def move_pyramid_activated(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        start = (3, 7)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(3, 9, piece=PieceType(-PieceType.Knight))

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, -1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

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

    def move_pyramid_activation_by_pawn(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.65, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(4, 2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Pyramid))

        self.board.set_piece(5, 6, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(5, 7, piece=PieceType(PieceType.Pyramid))

        start = (8, 1)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(8, 4, piece=PieceType(PieceType.Pyramid))

        # capture-fields
        self.arrows.append( SH.get_new_arrow(4, 2, 3, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(4, 2, 5, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 2, SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        # step-fields 1
        self.arrows.append( SH.get_new_arrow(5, 6, 5, 7, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("2", *get_text_position(5, 6, SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        # step-fields 2
        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("3", *get_text_position(*start, corner=SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_activation_by_pawn")

    def move_pyramid_promo_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        start = (11, 3)
        self.board.set_piece(3, 7, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 7, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))

        # direction <-2, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-2, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-2, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors_alt(True)) )

        return self.format_return_values("move_pyramid_promo_init")

    def move_pyramid_promo_activate(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        start = (3, 7)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, piece=PieceType(PieceType.Pawn))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, -1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_promo_activate")

    def move_pyramid_promo_end(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(3, 7, piece=PieceType(PieceType.Pegasus))
        self.board.set_piece(6, 7, piece=PieceType(PieceType.Queen))

        return self.format_return_values("move_pyramid_promo_end")

    def move_pyramid_conversion_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        start = (7, 5)
        self.board.set_piece(3, 1, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 1, piece=PieceType(-PieceType.Rook))
        self.board.set_piece(*start, piece=PieceType(PieceType.Bishop))

        # direction <-1, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, -1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )

        return self.format_return_values("move_pyramid_conversion_init")

    def move_pyramid_conversion_activated(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        start = (3, 1)
        self.board.set_piece(*start, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(6, 1, piece=PieceType(-PieceType.Rook))

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, -1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        return self.format_return_values("move_pyramid_conversion_activated")

    def move_pyramid_conversion_end(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(3, 1, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(6, 1, piece=PieceType(PieceType.Rook))

        return self.format_return_values("move_pyramid_conversion_end")

    def move_pyramid_cascading_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        start = (10, 1)
        self.board.set_piece(*start, piece=PieceType(PieceType.Queen))

        pyramid_1 = (5, 6)
        self.board.set_piece(*pyramid_1, piece=PieceType(PieceType.Pyramid))

        pyramid_2 = (8, 6)
        self.board.set_piece(*pyramid_2, piece=PieceType(PieceType.Pyramid))

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType(PieceType.Pyramid))

        offset = (0.4, 0.45)

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        # direction <-1, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("5", *get_text_position(*coords(), corner=SH.Corner.UpperRight), **get_text_colors_alt(True)) )

        # pyramids
        self.texts.append( SH.get_new_text("1", *get_text_position(*GS.add(pyramid_1, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*GS.add(pyramid_2, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*GS.add(pyramid_3, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_cascading_init")

    def move_pyramid_cascading_activated_1(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        start = (5, 6)
        self.board.set_piece(*start, piece=PieceType(PieceType.Queen))

        pyramid_2 = (8, 6)
        self.board.set_piece(*pyramid_2, piece=PieceType(PieceType.Pyramid))

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType(PieceType.Pyramid))

        offset = (0.4, 0.45)

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("5", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("5", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, -1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("5", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )
        self.texts.append( SH.get_new_text("5", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(False)) )

        # pyramids
        self.texts.append( SH.get_new_text("2", *get_text_position(*GS.add(pyramid_2, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*GS.add(pyramid_3, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_cascading_activated_1")

    def move_pyramid_cascading_activated_2(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(5, 6, piece=PieceType(PieceType.Queen))

        start = (8, 6)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pyramid))

        pyramid_3 = (5, 1)
        self.board.set_piece(*pyramid_3, piece=PieceType(PieceType.Pyramid))

        offset = (0.4, 0.45)

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(-1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(1, 0)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, -1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        coords = GS.call_gen( GS.get_gen_steps(start=start, rel=(0, 1)) )
        self.texts.append( SH.get_new_text("1", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*coords(), corner=SH.Corner.UpperLeft), **get_text_colors(True)) )

        # pyramids
        self.texts.append( SH.get_new_text("1", *get_text_position(*GS.add(start, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*GS.add(pyramid_3, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_cascading_activated_2")

    def move_pyramid_cascading_end(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(5, 6, piece=PieceType(PieceType.Queen))

        pyramid_1 = (8, 6)
        self.board.set_piece(*pyramid_1, piece=PieceType(PieceType.Pyramid))

        pyramid_2 = (5, 1)
        self.board.set_piece(*pyramid_2, piece=PieceType(PieceType.Pyramid))

        pyramid_3 = (8, 8)
        self.board.set_piece(*pyramid_3, piece=PieceType(PieceType.Pyramid))

        offset = (0.4, 0.45)

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        self.texts.append( SH.get_new_text("1", *get_text_position(*GS.add(pyramid_1, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*GS.add(pyramid_2, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*GS.add(pyramid_3, offset), corner=SH.Corner.Position), **get_text_colors_alt(False)) )

        return self.format_return_values("move_pyramid_cascading_end")

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
