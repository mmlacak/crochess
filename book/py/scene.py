#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

from piece import PieceType
from board import BoardType
from board import Board, BoardHints
from mark import Arrow, Text, FieldMarker

from gfx_def import GD
import move_gen as MG
import scene_helper as SH

from scene_pegasus import ScenePegasusMixin
from scene_pyramid import ScenePyramidMixin
from scene_unicorn import SceneUnicornMixin
from scene_wave import SceneWaveMixin


class Scene(ScenePegasusMixin, \
            ScenePyramidMixin, \
            SceneUnicornMixin, \
            SceneWaveMixin):

    ARROW_COLORS_DICT = { BoardType.none:                    ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddClassical:            ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.Classical:               ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddCroatianTies:         ("#303030", "#00FF00", "#303030", "#808080"),
                          BoardType.CroatianTies:            ("#303030", "#00FF00", "#303030", "#808080"),
                          BoardType.OddMayanAscendancy:      ("#303030", "#00FF00", "#101010", "#303030"),
                          BoardType.MayanAscendancy:         ("#303030", "#00FF00", "#101010", "#303030"),
                          BoardType.OddAgeOfAquarius:        ("#303030", "#0000FF", "#303030", "#FF0000"),
                          BoardType.AgeOfAquarius:           ("#303030", "#0000FF", "#303030", "#FF0000"),
                          BoardType.OddMirandasVeil:         ("#303030", "#00FF00", "#303030", "#808080"), # ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.MirandasVeil:            ("#303030", "#00FF00", "#303030", "#808080"), # ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddNineteen:             ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.Nineteen:                ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddHemerasDawn:          ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.HemerasDawn:             ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddTamoanchanRevisited:  ("#303030", "#FFFF00", "#303030", "#FF0000"),
                          BoardType.TamoanchanRevisited:     ("#303030", "#FFFF00", "#303030", "#FF0000"),
                          BoardType.OddConquestOfTlalocan:   ("#303030", "#00FF00", "#303030", "#808080"),
                          BoardType.ConquestOfTlalocan:      ("#303030", "#00FF00", "#303030", "#808080"),
                          BoardType.OddDiscovery:            ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.Discovery:               ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddOne:                  ("#303030", "#00FF00", "#303030", "#808080"), # ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.One:                     ("#303030", "#00FF00", "#303030", "#808080"), # ("#303030", "#00FF00", "#303030", "#FF0000"),
                        }

    ARROW_ACTION_COLORS = ("#303030", "#FF0000", "#303030", "#FF0000")

    TEXT_COLORS_DICT =  { BoardType.none:                    ("#FF00FF", "#303030", "#303030", "#808080"),
                          BoardType.OddClassical:            ("#0080FF", "#303030", "#101010", "#808080"),
                          BoardType.Classical:               ("#0080FF", "#303030", "#101010", "#808080"),
                          BoardType.OddCroatianTies:         ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.CroatianTies:            ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.OddMayanAscendancy:      ("#FF00FF", "#303030", "#303030", "#808080"),
                          BoardType.MayanAscendancy:         ("#FF00FF", "#303030", "#303030", "#808080"),
                          BoardType.OddAgeOfAquarius:        ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.AgeOfAquarius:           ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.OddMirandasVeil:         ("#00D000", "#303030", "#808080", "#303030"), # ("#FF00FF", "#303030", "#808080", "#303030"),
                          BoardType.MirandasVeil:            ("#00D000", "#303030", "#808080", "#303030"), # ("#FF00FF", "#303030", "#808080", "#303030"),
                          BoardType.OddNineteen:             ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.Nineteen:                ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.OddHemerasDawn:          ("#0000FF", "#303030", "#505050", "#808080"),
                          BoardType.HemerasDawn:             ("#0000FF", "#303030", "#505050", "#808080"),
                          BoardType.OddTamoanchanRevisited:  ("#FF00FF", "#303030", "#808080", "#303030"),
                          BoardType.TamoanchanRevisited:     ("#FF00FF", "#303030", "#808080", "#303030"),
                          BoardType.OddConquestOfTlalocan:   ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.ConquestOfTlalocan:      ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.OddDiscovery:            ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.Discovery:               ("#0000FF", "#303030", "#303030", "#808080"),
                          BoardType.OddOne:                  ("#00D000", "#303030", "#808080", "#303030"), # ("#FF00FF", "#303030", "#808080", "#303030"),
                          BoardType.One:                     ("#00D000", "#303030", "#808080", "#303030"), # ("#FF00FF", "#303030", "#808080", "#303030"),
                        }

    TEXT_ACTION_COLORS = ("#FF0000", "#303030", "#FF0000", "#303030")

    FIELD_MARKER_COLORS_DICT = None

    @staticmethod
    def switch_active_inactive_colors(colors_dict):
        d = {}
        for k, v in colors_dict.iteritems():
            fg_a, bg_a, fg_i, bg_i = v
            d[k] = (fg_i, bg_i, fg_a, bg_a)
        return d

    @staticmethod
    def switch_fg_bg_colors(colors_dict):
        d = {}
        for k, v in colors_dict.iteritems():
            fg_a, bg_a, fg_i, bg_i = v
            d[k] = (bg_a, fg_a, bg_i, fg_i)
        return d

    def __init__(self, board=None, *args, **kwargs):
        super(Scene, self).__init__(*args, **kwargs)

        self.board = board
        self.arrows = [] # :: [ mark.Arrow, ... ]
        self.texts = [] # :: [ mark.Text, ... ]
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

        Scene.FIELD_MARKER_COLORS_DICT = Scene.switch_fg_bg_colors(Scene.TEXT_COLORS_DICT)

    def delete_all_arrows(self):
        self.arrows = [] # :: [ mark.Arrow, ... ]

    def delete_all_texts(self):
        self.texts = [] # :: [ mark.Text, ... ]

    def delete_all_field_markers(self):
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

    def delete_all_marks(self):
        self.delete_all_arrows()
        self.delete_all_texts()
        self.delete_all_field_markers()

    def get_arrow_colors(self, bt):
        return Scene.ARROW_COLORS_DICT[bt]

    def get_arrow_action_colors(self, bt):
        return Scene.ARROW_ACTION_COLORS

    def get_text_colors(self, bt):
        return Scene.TEXT_COLORS_DICT[bt]

    def get_text_action_colors(self, bt):
        return Scene.TEXT_ACTION_COLORS

    def get_field_marker_colors(self, bt):
        return Scene.FIELD_MARKER_COLORS_DICT[bt]

    def format_return_values(self, filename, size_x=None, size_y=None):
        # size_x, size_y :: int | float | None

        width = size_x
        height = size_y

        b = self.board

        if width is None:
            if b.get_width() != b.type.get_size():
                width = float(b.get_width()) / float(b.type.get_size())

        if height is None:
            if b.get_height() != b.type.get_size():
                height = float(b.get_height()) / float(b.type.get_size())

        if isinstance(width, float):
            width = int( width * GD.DEFAULT_BOARD_RENDERING_SIZE )

        if isinstance(height, float):
            height = int( height * GD.DEFAULT_BOARD_RENDERING_SIZE )

        return filename, width, height, b.type

# --- Scenes --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

    def intro_piece(self, bt, piece_type=None):
        bt = BoardType(bt)
        self.board = Board(bt, 2, 2)
        self.board.clear()
        self.delete_all_marks()

        piece_type = piece_type or bt.get_newly_introduced_piece()
        if piece_type is None:
            return None

        self.board.set_pieces([(0, 0, PieceType(piece_type)),
                               (1, 0, PieceType(piece_type)),
                               (0, 1, PieceType(-piece_type)),
                               (1, 1, PieceType(-piece_type))])

        return piece_type

    def intro_castling(self, bt):
        bt = BoardType(bt)
        self.board = Board(bt, bt.get_size(), 1)
        self.board.clear()
        self.delete_all_marks()

        pos_king_h = bt.get_size() // 2
        offset = 1 if bt.does_contain(PieceType.Star) else 0
        pos_rook_r = bt.get_size() - 1 - offset

        self.board.set_piece(pos_king_h, 0, PieceType.King)
        self.board.set_piece(offset, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        diff = pos_rook_r - pos_king_h
        for i in xrange(2, diff):
            pos_l = pos_king_h - i
            pos_r = pos_king_h + i

            self.texts.append( SH.get_new_text(str(i-1), *get_text_position(pos_l, 0, SH.Corner.UpperLeft), **get_text_colors(True)) )
            self.texts.append( SH.get_new_text(str(i-1), *get_text_position(pos_r, 0, SH.Corner.UpperLeft), **get_text_colors(True)) )

        return bt

    def castling_long_left(self, bt):
        bt = BoardType(bt)
        self.board = Board(bt, bt.get_size(), 1)
        self.board.clear()
        self.delete_all_marks()

        pos_king_init = bt.get_size() // 2
        offset = 1 if bt.does_contain(PieceType.Star) else 0
        pos_rook_r = bt.get_size() - 1 - offset
        # diff = pos_rook_r - pos_king_init
        pos_king_h = offset + 2

        self.board.set_piece(pos_king_h, 0, PieceType.King)
        self.board.set_piece(pos_king_h + 1, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        self.texts.append( SH.get_new_text("K", *get_text_position(pos_king_init, 0, SH.Corner.UpperLeft), **get_text_colors(False)) )

        return bt

    def castling_short_right(self, bt):
        bt = BoardType(bt)
        self.board = Board(bt, bt.get_size(), 1)
        self.board.clear()
        self.delete_all_marks()

        pos_king_init = bt.get_size() // 2
        offset = 1 if bt.does_contain(PieceType.Star) else 0
        # pos_init_rook_r = bt.get_size() - 1 - offset
        # diff = pos_rook_r - pos_king_init
        pos_king_h = pos_king_init + 2
        pos_rook_r = pos_king_h - 1

        self.board.set_piece(pos_king_h, 0, PieceType.King)
        self.board.set_piece(offset, 0, PieceType.Rook)
        self.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            self.board.set_piece(0, 0, PieceType.Star)
            self.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        self.texts.append( SH.get_new_text("K", *get_text_position(pos_king_init, 0, SH.Corner.UpperLeft), **get_text_colors(False)) )

        return bt

    def get_en_passant_font_size(self, bt, vertical_board_size):
        return { BoardType.none: 0,
                 BoardType.OddClassical: 192,
                 BoardType.Classical: 192,
                 BoardType.OddCroatianTies: 192,
                 BoardType.CroatianTies: 192,
                 BoardType.OddMayanAscendancy: 192,
                 BoardType.MayanAscendancy: 192,
                 BoardType.OddAgeOfAquarius: 192,
                 BoardType.AgeOfAquarius: 192,
                 BoardType.OddMirandasVeil: 192,
                 BoardType.MirandasVeil: 192,
                 BoardType.OddNineteen: 192,
                 BoardType.Nineteen: 192,
                 BoardType.OddHemerasDawn: 192,
                 BoardType.HemerasDawn: 192,
                 BoardType.OddTamoanchanRevisited: 192,
                 BoardType.TamoanchanRevisited: 192,
                 BoardType.OddConquestOfTlalocan: 192,
                 BoardType.ConquestOfTlalocan: 192,
                 BoardType.OddDiscovery: 192,
                 BoardType.Discovery: 192,
                 BoardType.OddOne: 192,
                 BoardType.One: 192 }[bt]

    def intro_en_passant(self, bt):
        bt = BoardType(bt)
        size = (bt.get_size() + 1) // 2
        self.board = Board(bt, 3, size)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.5, bottom=0.05)
        get_font_definition = SH.get_func_get_font_definition()
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=get_font_definition(bt.get_size()))

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in xrange(3, size):
            loc = 0 if i % 2 == 0 else 2
            self.board.set_piece(loc, i, PieceType(-PieceType.Pawn))

            self.arrows.append( SH.get_new_arrow(loc, i, 1, i-1, **get_arrow_colors(True)) )

            self.texts.append( SH.get_new_text(str(i-2), *get_text_position(1, i, SH.Corner.UpperRight), **get_text_colors(True)) )

        return bt

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

    def set_mirandas_veil_1(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 6, 6)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_pieces([(0, 0, PieceType(PieceType.Wave)),
                               (0, 2, PieceType(PieceType.King)),
                               (2, 0, PieceType(PieceType.Queen)),
                               (2, 2, PieceType(PieceType.Bishop)),
                               (2, 4, PieceType(PieceType.Rook)),
                               (4, 2, PieceType(PieceType.Rook)),
                               (4, 4, PieceType(PieceType.Wave)),
                               (0, 4, PieceType(-PieceType.Wave)),
                               (4, 0, PieceType(-PieceType.Wave))])

        return self.format_return_values("set_mirandas_veil_1")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

    def get_all_example_scene_methods(self):
        funcs = []

        names = [ n for n in dir(self) if n.startswith('move_') ] # or n.startswith('scene_')
        fncs = [ getattr(self, a) for a in names ]
        funcs.extend( fncs )

        return funcs

    def get_recent_example_scene_methods(self):
        return  [
                # self.move_wave_init, \
                # self.move_wave_activated, \
                # self.move_wave_finished, \
                # self.move_pegasus_step_ply, \
                self.move_wave_cascading_rook, \
                self.move_wave_cascading_wave_1, \
                self.move_wave_cascading_wave_2, \
                self.move_wave_cascading_rook_b, \
                self.move_wave_cascading_wave_1_b, \
                self.move_wave_cascading_queen, \
                self.move_wave_cascading_wave_2_b, \
                self.move_wave_cascading_wave_1_c, \
                self.move_wave_cascading_end, \
#                 self.move_wave_activation_by_pawn, \
                ]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

    def test_move_pegasus(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        self.arrows.append( Arrow(600, 500, 4300, 2700, fg_color="#44FF44", bg_color="#440044"), )
        self.arrows.append( Arrow(6.3, 5.1, 4.3, 2.7, fg_color="#202020", bg_color="#7F00FF"), )

        self.texts.append( Text("howdy!", 999, 333, "sans bold 144", fg_color="#003080", bg_color="#000000") )
        self.texts.append( Text("hola!", 7.1, 3.33, "serif italic 277", fg_color="#008030", bg_color="#000000") )
        self.texts.append( Text("here! 0123456798", 3.1, 2.78, "DejaVu Sans Mono 96", fg_color="#300080", bg_color="#000000") )

        self.field_markers.append( FieldMarker(3, 1, fg_color="#7F00FF", bg_color="#5F00AF", inv_width_ratio=5.0) )
        self.field_markers.append( FieldMarker(5, 7, fg_color="#7F00FF", bg_color="#5F00AF", inv_width_ratio=7.0) )
        self.field_markers.append( FieldMarker(7, 0, fg_color="#7F00FF", bg_color="#5F00AF", inv_width_ratio=10.0) )

        return "test_move_pegasus"

    def test_move_pegasus_2(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        self.arrows.append( Arrow(600, 500, 4300, 2700, fg_color="#44FF44", bg_color="#440044"), )
        self.arrows.append( Arrow(6.3, 5.1, 14.3, 22.7, fg_color="#202020", bg_color="#7F00FF"), )

        self.texts.append( Text("howdy!", 999, 333, "sans bold 144", fg_color="#FFFF00", bg_color="#000000") )
        self.texts.append( Text("hola!", 7.1, 3.33, "serif italic 277", fg_color="#FFFF00", bg_color="#000000") )
        self.texts.append( Text("here! 0123456798", 3.1, 2.78, "DejaVu Sans Mono 96", fg_color="#300080", bg_color="#000000") )

        self.field_markers.append( FieldMarker(3, 1, fg_color="#FFFF00", bg_color="#FF7F7F", inv_width_ratio=5.0) )
        self.field_markers.append( FieldMarker(5, 7, fg_color="#FFFF00", bg_color="#FF7F7F", inv_width_ratio=7.0) )
        self.field_markers.append( FieldMarker(7, 0, fg_color="#FFFF00", bg_color="#FF7F7F", inv_width_ratio=10.0) )

        return "test_move_pegasus_2"
