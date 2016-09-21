#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from piece import PieceType
from board import BoardType
from board import Board
from mark import Arrow, Text, FieldMarker

import scene_helper as SH


class Scene(object):
    def __init__(self, board=None):
        self.board = board
        self.arrows = [] # :: [ mark.Arrow, ... ]
        self.texts = [] # :: [ mark.Text, ... ]
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

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

        self.board.set_pieces([(pos_king_h, 0, PieceType(PieceType.King)),
                               (offset, 0, PieceType(PieceType.Rook)),
                               (bt.get_size() - 1 - offset, 0, PieceType(PieceType.Rook))])

        return bt

    def intro_en_passant(self, bt):
        bt = BoardType(bt)
        size = (bt.get_size() + 1) // 2
        self.board = Board(bt, 3, size)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in xrange(3, size):
            loc = 0 if i % 2 == 0 else 2
            self.board.set_piece(loc, i, PieceType(-PieceType.Pawn))

        return bt

    def move_pegasus_initial(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt, 5, 5)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 2, PieceType(PieceType.Pegasus))

        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))
        self.board.set_piece(1, 2, PieceType(PieceType.Pawn))
        self.board.set_piece(1, 3, PieceType(PieceType.Pawn))
        self.board.set_piece(2, 3, PieceType(PieceType.Pawn))

        self.board.set_piece(3, 3, PieceType(PieceType.Pawn))
        self.board.set_piece(3, 2, PieceType(PieceType.Pawn))
        self.board.set_piece(3, 1, PieceType(PieceType.Pawn))
        self.board.set_piece(2, 1, PieceType(PieceType.Pawn))

        get_arrow_colors = SH.get_func_get_colors("#303030", "#00FF00", "#303030", "#808080")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
#         get_text_colors = SH.get_func_get_colors("#0000FF", "#303030", "#303030", "#808080", font="sans bold 384")
        get_text_colors = SH.get_func_get_colors("#0000FF", "#303030", "#303030", "#808080", font="sans bold 192")

        # direction 1, i.e. <2, 1>

        self.arrows.append( SH.get_new_arrow(2, 2, 4, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("1", *get_text_position(4, 3, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction 2, i.e. <1, 2>

        self.arrows.append( SH.get_new_arrow(2, 2, 3, 4, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("2", *get_text_position(3, 4, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction 3, i.e. <-1, 2>

        self.arrows.append( SH.get_new_arrow(2, 2, 1, 4, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("3", *get_text_position(1, 4, SH.Corner.UpperRight), **get_text_colors(True)) )

        # direction 4, i.e. <-2, 1>

        self.arrows.append( SH.get_new_arrow(2, 2, 0, 3, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("4", *get_text_position(0, 3, SH.Corner.UpperRight), **get_text_colors(True)) )

        # direction 5, i.e. <-2, -1>

        self.arrows.append( SH.get_new_arrow(2, 2, 0, 1, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("5", *get_text_position(0, 1, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction 6, i.e. <-1, -2>

        self.arrows.append( SH.get_new_arrow(2, 2, 1, 0, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("6", *get_text_position(1, 0, SH.Corner.UpperLeft), **get_text_colors(True)) )

        # direction 7, i.e. <1, -2>

        self.arrows.append( SH.get_new_arrow(2, 2, 3, 0, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("7", *get_text_position(3, 0, SH.Corner.UpperRight), **get_text_colors(True)) )

        # direction 8, i.e. <2, -1>

        self.arrows.append( SH.get_new_arrow(2, 2, 4, 1, **get_arrow_colors(True)) )

        self.texts.append( SH.get_new_text("8", *get_text_position(4, 1, SH.Corner.UpperRight), **get_text_colors(True)) )

        return self.format_return_values("move_pegasus_initial", size_x=4000, size_y=4000)

    def move_pegasus_direction(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt, 10, 10)
        self.board.clear()
        self.delete_all_marks()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        get_arrow_colors = SH.get_func_get_colors("#303030", "#00FF00", "#303030", "#808080")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_text_position_2 = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.5, bottom=0.05)
        get_text_colors = SH.get_func_get_colors("#0000FF", "#303030", "#303030", "#808080", font="sans bold 192")

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

        get_arrow_colors = SH.get_func_get_colors("#303030", "#00FF00", "#303030", "#808080")
        get_text_position = SH.get_func_get_text_position(left=0.05, top=1.0, right=0.75, bottom=0.05)
        get_text_colors = SH.get_func_get_colors("#0000FF", "#303030", "#303030", "#808080", font="sans bold 192")

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
        return [2, ] # None

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
