#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017, 2018 Mario Mlačak, mmlacak@gmail.com
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
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = GS.get_gen_multi_steps(start=start, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())

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
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = GS.get_gen_multi_steps(start=start, rel_lst=GS.DEFAULT_UNICORN_REL_LONG_MOVES, pos_bounds=((3, 2), (11, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_unicorn_opposite_color")

    def move_unicorn_promo_init(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startB = (12, 5)
        startA = (7, 10)
        startP1 = (8, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startA, piece=PieceType(PieceType.Pyramid))
        self.board.set_piece(*startB, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(4, 1, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        # direction <-1, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startB, rel=(-1, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        # direction <-1, 0>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startA, rel=(-1, 0)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors_alt(True)) )

        return self.format_return_values("move_unicorn_promo_init")

    def move_unicorn_pawn_2_tagged(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (4, 1)
        startP1 = (8, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        self.field_markers.append( SH.get_new_field_marker(*startP2, **get_field_marker_colors(True)) )

        # direction <-1, 4>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startU, rel=(-1, 4)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_unicorn_pawn_2_tagged")

    def move_unicorn_pawn_1_to_promo(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (3, 5)
        startP1 = (8, 12)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        self.field_markers.append( SH.get_new_field_marker(*startP2, **get_field_marker_colors(True)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startP1, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_unicorn_pawn_1_to_promo")

    def move_unicorn_pawn_1_tagged(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (3, 5)
        startP1 = (8, 13)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        self.field_markers.append( SH.get_new_field_marker(*startP1, **get_field_marker_colors(True)) )
        self.field_markers.append( SH.get_new_field_marker(*startP2, **get_field_marker_colors(True)) )

        # direction <-1, 2>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startU, rel=(-1, 2)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_unicorn_pawn_1_tagged")

    def move_unicorn_pawn_2_attacked(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (2, 7)
        startP1 = (8, 13)
        startP2 = (4, 10)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )

        self.field_markers.append( SH.get_new_field_marker(*startP1, **get_field_marker_colors(True)) )
        self.field_markers.append( SH.get_new_field_marker(*startP2, **get_field_marker_colors(True)) )

        # direction <0, 1>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startP2, rel=(0, 1)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_unicorn_pawn_2_attacked")

    def move_unicorn_pawn_2_moved(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (2, 7)
        startP1 = (8, 13)
        startP2 = (4, 11)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("P", *get_text_position(4, 10, corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )

        self.field_markers.append( SH.get_new_field_marker(*startP1, **get_field_marker_colors(True)) )

        # direction <3, 2>
        coords = GS.call_gen( GS.get_gen_steps_prev(start=startU, rel=(3, 2)) )
        self.arrows.append( SH.get_new_arrow(*coords(), **get_arrow_colors(True)) )

        return self.format_return_values("move_unicorn_pawn_2_moved")

    def move_unicorn_pawn_2_and_bishop_attacked(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (5, 9)
        startP1 = (8, 13)
        startP2 = (4, 11)
        startP3 = (4, 6)

        self.board.set_piece(*startP1, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("1", *get_text_position(*startP1, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("P", *get_text_position(4, 10, corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )

        self.field_markers.append( SH.get_new_field_marker(*startP1, **get_field_marker_colors(True)) )

        return self.format_return_values("move_unicorn_pawn_2_and_bishop_attacked")

    def move_unicorn_pawn_1_promoted(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.45)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#00C0C0")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#101010", "#303030", font=get_font_definition(bt.get_size()))

        startU = (5, 9)
        startQ = (8, 13)
        startP2 = (4, 11)
        startP3 = (4, 6)

        self.board.set_piece(*startQ, piece=PieceType(PieceType.Queen))
        self.board.set_piece(*startP2, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(*startP3, piece=PieceType(PieceType.Pawn))
        self.board.set_piece(7, 10, piece=PieceType(PieceType.Bishop))
        self.board.set_piece(*startU, piece=PieceType(-PieceType.Unicorn))

        self.texts.append( SH.get_new_text("2", *get_text_position(*startP2, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(*startP3, corner=SH.Corner.UpperLeft), **get_text_colors_alt(False)) )
        self.texts.append( SH.get_new_text("P", *get_text_position(4, 10, corner=SH.Corner.UpperLeft), **get_text_colors_alt(True)) )

        return self.format_return_values("move_unicorn_pawn_1_promoted")
