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


class SceneWaveMixin(object):

    def move_wave_init(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#00C0C0", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(1, 1, piece=PieceType(PieceType.Wave))
        self.board.set_piece(0, 3, piece=PieceType(PieceType.Knight))

        self.arrows.append( SH.get_new_arrow(0, 3, 1, 1, **get_arrow_colors_alt(True)) )

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(4, 7, piece=PieceType(-PieceType.Pyramid))
        self.board.set_piece(5, 9, piece=PieceType(-PieceType.Queen))
        self.board.set_piece(7, 13, piece=PieceType(PieceType.Bishop))

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType(PieceType.King))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(7, 4, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(9, 5, piece=PieceType(-PieceType.King))
        self.board.set_piece(13, 7, piece=PieceType(-PieceType.Wave))

        return self.format_return_values("move_wave_init")

    def move_wave_activated(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#00C0C0", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(1, 1, piece=PieceType(PieceType.Knight))

        self.arrows.append( SH.get_new_arrow(1, 1, 0, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 1, 3, 0, **get_arrow_colors(True)) )

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(4, 7, piece=PieceType(-PieceType.Pyramid))
        self.board.set_piece(5, 9, piece=PieceType(-PieceType.Queen))
        self.board.set_piece(7, 13, piece=PieceType(PieceType.Bishop))

        self.arrows.append( SH.get_new_arrow(1, 1, 2, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 3, 3, 5, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 7, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(4, 7, 5, 9, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(5, 9, 6, 11, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 11, 7, 13, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(7, 13, 8, 15, **get_arrow_colors(True)) )

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType(PieceType.King))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(7, 4, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(9, 5, piece=PieceType(-PieceType.King))
        self.board.set_piece(13, 7, piece=PieceType(-PieceType.Wave))

        self.arrows.append( SH.get_new_arrow(1, 1, 3, 2, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(3, 2, 5, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(5, 3, 7, 4, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(7, 4, 9, 5, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(9, 5, 11, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(11, 6, 13, 7, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(13, 7, 15, 8, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_activated")

    def move_wave_finished(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#00C0C0", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(1, 1, piece=PieceType(PieceType.Knight))

        # direction <1, 2>
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(4, 7, piece=PieceType(-PieceType.Pyramid))
        self.board.set_piece(5, 9, piece=PieceType(-PieceType.Queen))
        self.board.set_piece(7, 13, piece=PieceType(PieceType.Wave))

        self.arrows.append( SH.get_new_arrow(7, 13, 6, 12, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 13, 6, 14, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 13, 8, 12, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 13, 8, 14, **get_arrow_colors(True)) )

        # direction <2, 1>
        self.board.set_piece(3, 2, piece=PieceType(PieceType.King))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(7, 4, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(9, 5, piece=PieceType(-PieceType.King))
        self.board.set_piece(13, 7, piece=PieceType(-PieceType.Wave))

        return self.format_return_values("move_wave_finished")


    # --- cascading ---------------------------------------------------------------------------------------------------------------

    def move_wave_cascading_rook(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(5, 7, piece=PieceType(PieceType.Rook))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("1", *get_text_position(5, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(5, 7, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 5, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 5, 5, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 4, 5, 3, **get_arrow_colors_alt(True)) )

        return self.format_return_values("move_wave_cascading_rook")

    def move_wave_cascading_wave_1(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(5, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("2", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 3, 7, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 3, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 9, 3, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(5, 3, 4, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 3, 3, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 2, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 3, 1, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 3, 0, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 3, -1, 3, **get_arrow_colors(True)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(5, 3, 5, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 4, 5, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 5, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 5, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 5, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 8, 5, 9, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(5, 3, 5, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 2, 5, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 1, 5, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 0, 5, -1, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_wave_1")

    def move_wave_cascading_wave_2(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(5, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 3, 4, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 3, 5, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 3, 7, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 3, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 9, 3, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 3, 2, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 3, 1, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 3, 0, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 3, -1, 3, **get_arrow_colors(True)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 3, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 5, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 6, 3, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 8, 3, 9, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 3, 3, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 2, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 1, 3, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 0, 3, -1, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_wave_2")

    def move_wave_cascading_rook_b(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(5, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 5, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 3, 7, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 3, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 9, 3, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(5, 3, 4, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 3, 3, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 2, 3, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(2, 3, 1, 3, **get_arrow_colors(False)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(5, 3, 5, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 4, 5, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 5, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 5, 7, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(5, 3, 5, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 2, 5, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 1, 5, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 0, 5, -1, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_rook_b")

    def move_wave_cascading_wave_1_b(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 5, piece=PieceType(PieceType.Queen))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Wave))

        self.texts.append( SH.get_new_text("2", *get_text_position(5, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 3, 4, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 3, 5, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 3, 7, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 3, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 9, 3, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 3, 2, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 3, 1, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 3, 0, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 3, -1, 3, **get_arrow_colors(True)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 3, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 5, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 6, 3, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 8, 3, 9, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 3, 3, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 2, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 1, 3, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 0, 3, -1, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_wave_1_b")

    def move_wave_cascading_queen(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 5, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Wave))

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(5, 3, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 5, 5, 5, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 5, 1, 5, **get_arrow_colors(True)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 6, 3, 7, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 3, **get_arrow_colors(False)) )

        # direction <1, 1>
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 6, 5, 7, **get_arrow_colors(True)) )

        # direction <1, -1>
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 4, 5, 3, **get_arrow_colors_alt(True)) )

        # direction <-1, 1>
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 6, 1, 7, **get_arrow_colors(True)) )

        # direction <-1, -1>
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 4, 1, 3, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_queen")

    def move_wave_cascading_wave_2_b(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 5, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 3, 7, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 3, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 9, 3, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(5, 3, 4, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 3, 3, 3, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 2, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 3, 1, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 3, 0, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 3, -1, 3, **get_arrow_colors(True)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(5, 3, 5, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 4, 5, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 5, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 5, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 5, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 8, 5, 9, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(5, 3, 5, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 2, 5, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 1, 5, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 0, 5, -1, **get_arrow_colors(True)) )

        # direction <1, 1>
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 4, 7, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 5, 8, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 6, 9, 7, **get_arrow_colors(True)) )

        # direction <1, -1>
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 2, 7, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 1, 8, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 0, 9, -1, **get_arrow_colors(True)) )

        # direction <-1, 1>
        self.arrows.append( SH.get_new_arrow(5, 3, 4, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 4, 3, 5, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 6, 1, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 7, 0, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 8, -1, 9, **get_arrow_colors(True)) )

        # direction <-1, -1>
        self.arrows.append( SH.get_new_arrow(5, 3, 4, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 2, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 1, 2, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 0, 1, -1, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_wave_2_b")

    def move_wave_cascading_wave_1_c(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 5, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("2", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <1, 0>
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 5, 5, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 5, 6, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 5, 7, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 5, 8, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 5, 9, 5, **get_arrow_colors(True)) )

        # direction <-1, 0>
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 5, 1, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 5, 0, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 5, -1, 5, **get_arrow_colors(True)) )

        # direction <0, 1>
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 6, 3, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 7, 3, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 8, 3, 9, **get_arrow_colors(True)) )

        # direction <0, -1>
        self.arrows.append( SH.get_new_arrow(3, 5, 3, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 4, 3, 3, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(3, 3, 3, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 2, 3, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 1, 3, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(3, 0, 3, -1, **get_arrow_colors(True)) )

        # direction <1, 1>
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 6, 5, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 7, 6, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 8, 7, 9, **get_arrow_colors(True)) )

        # direction <1, -1>
        self.arrows.append( SH.get_new_arrow(3, 5, 4, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(4, 4, 5, 3, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(5, 3, 6, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(6, 2, 7, 1, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 1, 8, 0, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 0, 9, -1, **get_arrow_colors(True)) )

        # direction <-1, 1>
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 6, 1, 7, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 7, 0, 8, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 8, -1, 9, **get_arrow_colors(True)) )

        # direction <-1, -1>
        self.arrows.append( SH.get_new_arrow(3, 5, 2, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(2, 4, 1, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(1, 3, 0, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(0, 2, -1, 1, **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_cascading_wave_1_c")

    def move_wave_cascading_end(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 5, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(PieceType.Rook))
        self.board.set_piece(3, 1, piece=PieceType(PieceType.Wave))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Queen))

        self.texts.append( SH.get_new_text("1", *get_text_position(3, 1, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(3, 5, SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        return self.format_return_values("move_wave_cascading_end")

    # --- cascading ---------------------------------------------------------------------------------------------------------------

    # --- cascading opponent ------------------------------------------------------------------------------------------------------

    def move_wave_opponent_light_queen(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        start = (5, 6)
        self.board.set_piece(*start, piece=PieceType(PieceType.Queen))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Wave))
        self.board.set_piece(3, 3, piece=PieceType(-PieceType.Wave))
        self.board.set_piece(3, 5, piece=PieceType(-PieceType.Queen))

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        return self.format_return_values("move_wave_opponent_light_queen")

    def move_wave_opponent_light_wave(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        start = (5, 3)
        self.board.set_piece(3, 5, piece=PieceType(-PieceType.Queen))
        self.board.set_piece(3, 3, piece=PieceType(-PieceType.Wave))
        self.board.set_piece(*start, piece=PieceType(PieceType.Queen))

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <1, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <1, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <-1, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <-1, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_opponent_light_wave")

    def move_wave_opponent_dark_wave(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.7, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        start = (3, 3)
        self.board.set_piece(3, 5, piece=PieceType(-PieceType.Queen))
        self.board.set_piece(*start, piece=PieceType(PieceType.Wave))
        self.board.set_piece(5, 3, piece=PieceType(PieceType.Queen))

        # direction <1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <0, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(0, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <1, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <1, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(1, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <-1, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        # direction <-1, -1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=start, rel=(-1, -1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_wave_opponent_dark_wave")


    # --- cascading opponent ------------------------------------------------------------------------------------------------------

    def move_wave_activation_by_pawn(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.65, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#00C0C0", "#808080", "#006060", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(4, 2, PieceType(PieceType.Pawn))
        self.board.set_piece(3, 3, PieceType(PieceType.Wave))

        self.board.set_piece(5, 11, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 12, PieceType(PieceType.Wave))

        self.board.set_piece(8, 1, PieceType(PieceType.Pawn))
        self.board.set_piece(8, 4, PieceType(PieceType.Wave))

        # capture-fields
        self.arrows.append( SH.get_new_arrow(4, 2, 3, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(4, 2, 5, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 2, SH.Corner.UpperRight), **get_text_colors_alt(True)) )

        # step-fields 1
        self.arrows.append( SH.get_new_arrow(5, 11, 5, 12, **get_arrow_colors_alt(True)) )

        self.texts.append( SH.get_new_text("2", *get_text_position(5, 11, SH.Corner.UpperRight), **get_text_colors_alt(True)) )

        # step-fields 2
        self.arrows.append( SH.get_new_arrow(8, 1, 8, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 2, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 8, 4, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(8, 4, 8, 5, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(8, 5, 8, 6, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(8, 6, 8, 7, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("3", *get_text_position(8, 1, SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        return self.format_return_values("move_wave_activation_by_pawn")
