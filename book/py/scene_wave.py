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

        start_wave = (3, 4)
        self.board.set_piece(*start_wave, piece=PieceType(PieceType.Wave))

        start_pegasus = (13, 9)
        self.board.set_piece(*start_pegasus, piece=PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))
        # get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        get_arrow_action_colors = SH.get_func_get_colors(*self.get_arrow_action_colors(bt))
        get_text_action_colors = SH.get_func_get_colors(*self.get_text_action_colors(bt), font=get_font_definition(bt.get_size()))

        gen_rel = MG.get_gen_move((-2, -1))
        gen_abs_pos = MG.get_gen_abs_pos(gen_rel, start=start_pegasus, pos_limits=self.board.get_position_limits())

        i = 1
        start_arrow = start_pegasus
        steps_to_wave = 5
        for pos in gen_abs_pos():
            arrow = (start_arrow[0], start_arrow[1], pos[0], pos[1])
            before_wave = bool(i < (steps_to_wave+1))

            if i <> steps_to_wave:
                self.arrows.append( SH.get_new_arrow(*arrow, **get_arrow_colors(before_wave)) )
                self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(before_wave)) )
                # self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(before_wave)) )
            else:
                self.arrows.append( SH.get_new_arrow(*arrow, **get_arrow_action_colors(before_wave)) )
                self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_action_colors(before_wave)) )
                # self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(before_wave)) )

            i += 1
            start_arrow = pos

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

    def get_example_mixin_methods(self):
        return [ self.move_wave_init, \
                 self.move_wave_activated, \
               ]
