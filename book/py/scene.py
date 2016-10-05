#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

from piece import PieceType
from board import BoardType
from board import Board, BoardHints
from mark import Arrow, Text, FieldMarker

import move_gen as MG
import scene_helper as SH


class Scene(object):

    ARROW_COLORS_DICT = { BoardType.none:                    ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddClassical:            ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.Classical:               ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.OddCroatianTies:         ("#303030", "#00FF00", "#303030", "#808080"),
                          BoardType.CroatianTies:            ("#303030", "#00FF00", "#303030", "#808080"),
                          BoardType.OddMayanAscendancy:      ("#303030", "#00FF00", "#101010", "#303030"),
                          BoardType.MayanAscendancy:         ("#303030", "#00FF00", "#101010", "#303030"),
                          BoardType.OddAgeOfAquarius:        ("#303030", "#0000FF", "#303030", "#FF0000"),
                          BoardType.AgeOfAquarius:           ("#303030", "#0000FF", "#303030", "#FF0000"),
                          BoardType.OddMirandasVeil:         ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.MirandasVeil:            ("#303030", "#00FF00", "#303030", "#FF0000"),
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
                          BoardType.OddOne:                  ("#303030", "#00FF00", "#303030", "#FF0000"),
                          BoardType.One:                     ("#303030", "#00FF00", "#303030", "#FF0000") }

    TEXT_COLORS_DICT = { BoardType.none:                    ("#FF00FF", "#303030", "#303030", "#808080"),
                         BoardType.OddClassical:            ("#0080FF", "#303030", "#101010", "#808080"),
                         BoardType.Classical:               ("#0080FF", "#303030", "#101010", "#808080"),
                         BoardType.OddCroatianTies:         ("#0000FF", "#303030", "#303030", "#808080"),
                         BoardType.CroatianTies:            ("#0000FF", "#303030", "#303030", "#808080"),
                         BoardType.OddMayanAscendancy:      ("#FF00FF", "#303030", "#303030", "#808080"),
                         BoardType.MayanAscendancy:         ("#FF00FF", "#303030", "#303030", "#808080"),
                         BoardType.OddAgeOfAquarius:        ("#0000FF", "#303030", "#303030", "#808080"),
                         BoardType.AgeOfAquarius:           ("#0000FF", "#303030", "#303030", "#808080"),
                         BoardType.OddMirandasVeil:         ("#FF00FF", "#303030", "#808080", "#303030"),
                         BoardType.MirandasVeil:            ("#FF00FF", "#303030", "#808080", "#303030"),
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
                         BoardType.OddOne:                  ("#FF00FF", "#303030", "#808080", "#303030"),
                         BoardType.One:                     ("#FF00FF", "#303030", "#808080", "#303030") }

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

    FIELD_MARKER_COLORS_DICT = None

    def __init__(self, board=None):
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

    def get_text_colors(self, bt):
        return Scene.TEXT_COLORS_DICT[bt]

    def get_field_marker_colors(self, bt):
        return Scene.FIELD_MARKER_COLORS_DICT[bt]

    def format_return_values(self, filename, size_x=None, size_y=None):
        return filename, size_x, size_y

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
        # font = "sans bold %d" % SH.get_log_font_size(bt.get_size())
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

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
        # font = "sans bold %d" % SH.get_log_font_size(bt.get_size())
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

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
        # font = "sans bold %d" % SH.get_log_font_size(bt.get_size())
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

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
        font = "sans bold %d" % self.get_en_passant_font_size(bt, size)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font=font)

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in xrange(3, size):
            loc = 0 if i % 2 == 0 else 2
            self.board.set_piece(loc, i, PieceType(-PieceType.Pawn))

            self.arrows.append( SH.get_new_arrow(loc, i, 1, i-1, **get_arrow_colors(True)) )

            self.texts.append( SH.get_new_text(str(i-2), *get_text_position(1, i, SH.Corner.UpperRight), **get_text_colors(True)) )

        return bt

    def move_pegasus_initial(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt, 5, 5)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.20, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.20, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = MG.get_gen_abs_pos(MG.gen_knight_rel_moves, start=start, pos_limits=((0, 4), (0, 4)))

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_pegasus_initial", size_x=4000, size_y=4000)

    def move_pegasus_direction(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt, 10, 10)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_text_position_2 = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.5, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

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
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

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

    def move_pyramid_promo_init(self, bt=BoardType.MayanAscendancy):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

        self.board.set_piece(3, 7, PieceType(PieceType.Pyramid))
        self.board.set_piece(6, 7, PieceType(PieceType.Pawn))
        self.board.set_piece(11, 3, PieceType(PieceType.Pegasus))

        self.arrows.append( SH.get_new_arrow(11, 3, 9, 4, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(9, 4, 7, 5, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(7, 5, 5, 6, **get_arrow_colors(True)) )
        self.arrows.append( SH.get_new_arrow(5, 6, 3, 7, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(9, 4, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("2", *get_text_position(7, 5, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("3", *get_text_position(5, 6, SH.Corner.UpperLeft), **get_text_colors(True)) )
        self.texts.append( SH.get_new_text("4", *get_text_position(3, 7, SH.Corner.UpperLeft), **get_text_colors(True)) )

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
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font="sans bold 192")

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
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")

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
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_arrow_colors_alt = SH.get_func_get_colors("#303030", "#FF0000", "#101010", "#303030")
        get_text_colors_alt = SH.get_func_get_colors("#FF0000", "#303030", "#303030", "#808080", font="sans bold 192")

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

    def move_unicorn_same_color(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt, 5, 5, BoardHints(reverse_field_colors=True))
        self.board.clear()
        self.delete_all_marks()

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.20, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType(-PieceType.Unicorn))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = MG.get_gen_abs_pos(MG.gen_knight_rel_moves, start=start, pos_limits=((0, 4), (0, 4)))

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_unicorn_same_color", size_x=4000, size_y=4000)

    def move_unicorn_opposite_color(self, bt=BoardType.AgeOfAquarius):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_marks()

        start = (7, 6)
        self.board.set_piece(*start, piece=PieceType(-PieceType.Unicorn))

        get_arrow_colors = SH.get_func_get_colors(*self.get_arrow_colors(bt))
        get_text_position = SH.get_func_get_text_position(left=0.15, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors(*self.get_text_colors(bt), font="sans bold 192")
        get_field_marker_colors = SH.get_func_get_colors(*self.get_field_marker_colors(bt))

        gen_abs_pos = MG.get_gen_abs_pos(MG.gen_unicorn_rel_long_moves, start=start, pos_limits=((3, 11), (2, 10)))

        i = 1
        for pos in gen_abs_pos():
            self.field_markers.append( SH.get_new_field_marker(*pos, **get_field_marker_colors(True)) )
            self.texts.append( SH.get_new_text(str(i), *get_text_position(*pos, corner=SH.Corner.UpperLeft), **get_text_colors(True)) )
            i += 1

        return self.format_return_values("move_unicorn_opposite_color")

    def move_shaman(self, bt=BoardType.ConquestOfTlalocan):
        bt = BoardType(bt)
        self.board = Board(bt, 11, 11)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(5, 5, PieceType(-PieceType.Shaman))

        return self.format_return_values("move_shaman")

    def move_shaman_2(self, bt=BoardType.ConquestOfTlalocan):
        bt = BoardType(bt)
        self.board = Board(bt, 15, 15)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(7, 7, PieceType(PieceType.Shaman))

        return self.format_return_values("move_shaman_2")

    def move_monolith(self, bt=BoardType.Discovery):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(4, 4, PieceType(PieceType.Monolith))

        return self.format_return_values("move_monolith")

    def move_monolith_2(self, bt=BoardType.Discovery):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(4, 4, PieceType(PieceType.Monolith))
        self.board.set_piece(3, 4, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 5, PieceType(-PieceType.Rook))

        return self.format_return_values("move_monolith_2")

    def move_starchild(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(4, 4, PieceType(PieceType.Starchild))

        return self.format_return_values("move_starchild")

    def move_starchild_2(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(4, 4, PieceType(PieceType.Starchild))
        self.board.set_piece(3, 4, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 5, PieceType(-PieceType.Rook))

        return self.format_return_values("move_starchild_2")

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

    def set_example_1(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 16, 5)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_pieces([(0, 0, PieceType(PieceType.Wave)),
                               (3, 3, PieceType(PieceType.Queen)),
                               (12, 0, PieceType(PieceType.Bishop)),
                               (14, 1, PieceType(PieceType.King)),
                               (15, 2, PieceType(-PieceType.Rook)),
                               (15, 0, PieceType(-PieceType.Queen)),
                               (9, 0, PieceType(-PieceType.King))])

        return self.format_return_values("set_example_1")

    def set_star_journey(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 8, 8)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_pieces([(0, 0, PieceType(-PieceType.Star)),
                               (1, 2, PieceType(PieceType.Monolith)),
                               (5, 3, PieceType(PieceType.Knight)),
                               (6, 4, PieceType(PieceType.Monolith)),
                               (3, 5, PieceType(PieceType.Starchild))])

        return self.format_return_values("set_star_journey")

    def get_example_scene_functions(self):
        return [ self.move_pegasus_initial, \
                 self.move_pegasus_direction, \
                 self.move_pegasus, \
                 self.move_pyramid_promo_init, \
                 self.move_pyramid_promo_activate, \
                 self.move_pyramid_promo_end, \
                 self.move_pyramid_conversion_init, \
                 self.move_pyramid_conversion_activated, \
                 self.move_pyramid_conversion_end, \
                 self.move_unicorn_same_color, \
                 self.move_unicorn_opposite_color, \
                 self.move_shaman, \
                 self.move_shaman_2, \
                 self.move_monolith, \
                 self.move_monolith_2, \
                 self.move_starchild, \
                 self.move_starchild_2, \
                 self.set_mirandas_veil_1, \
                 self.set_example_1, \
                 self.set_star_journey ]

    def get_example_scene_function_indexes(self):
        return [9, ] # None

    # ~

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
