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
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(3, 4, piece=PieceType(PieceType.Wave))
        self.board.set_piece(5, 5, piece=PieceType(PieceType.Knight))



        return self.format_return_values("move_wave_init")

    def move_wave_activated(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        start_pegasus = (13, 9)
        pos_pegasus = (3, 4)
        self.board.set_piece(*pos_pegasus, piece=PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        # get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        # get_arrow_action_colors = SH.get_func_get_colors(*self.get_arrow_action_colors(bt))
        get_text_action_colors = SH.get_func_get_colors(*self.get_text_action_colors(bt), font=get_font_definition(bt.get_size()))

        gen_rel = MG.gen_knight_rel_moves() # MG.get_gen_move((-2, -1))
        gen_abs_pos = MG.get_gen_abs_pos(gen_rel, start=pos_pegasus, pos_limits=self.board.get_position_limits())

        i = 1
        start_arrow = pos_pegasus
        for pos in gen_abs_pos():
            arrow = (start_arrow[0], start_arrow[1], pos[0], pos[1])

            self.arrows.append( SH.get_new_arrow(*arrow, **get_arrow_colors(True)) )
            if pos != start_pegasus:
                self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            else:
                self.texts.append( SH.get_new_text('G', *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_action_colors(True)) )
            # self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )

            i += 1
            start_arrow = pos

        return self.format_return_values("move_wave_activated")

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
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#00C0C0", "#808080", font=get_font_definition(bt.get_size()))

        self.board.set_piece(4, 2, PieceType(PieceType.Pawn))
        self.board.set_piece(3, 3, PieceType(PieceType.Wave))

        self.board.set_piece(5, 11, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 12, PieceType(PieceType.Wave))

        self.board.set_piece(8, 1, PieceType(PieceType.Pawn))
        self.board.set_piece(8, 4, PieceType(PieceType.Wave))

        # capture-fields
        self.arrows.append( SH.get_new_arrow(4, 2, 3, 3, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(4, 2, 5, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 2, SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        # step-fields 1
        self.arrows.append( SH.get_new_arrow(5, 11, 5, 12, **get_arrow_colors_alt(True)) )

        self.texts.append( SH.get_new_text("2", *get_text_position(5, 11, SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        # step-fields 2
        self.arrows.append( SH.get_new_arrow(8, 1, 8, 2, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 2, 8, 3, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(8, 3, 8, 4, **get_arrow_colors_alt(True)) )
        self.arrows.append( SH.get_new_arrow(8, 4, 8, 5, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(8, 5, 8, 6, **get_arrow_colors(False)) )
        self.arrows.append( SH.get_new_arrow(8, 6, 8, 7, **get_arrow_colors(False)) )

        self.texts.append( SH.get_new_text("3", *get_text_position(8, 1, SH.Corner.UpperRight), **get_text_colors_alt(False)) )

        return self.format_return_values("move_wave_activation_by_pawn")
