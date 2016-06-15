#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from piece import PieceType
from board import BoardType
from board import Board
from mark import Arrow


class Scene(object):
    def __init__(self, board=None):
        self.board = board
        self.arrows = [] # :: [((x0, y0), (x1, y1)), ...] # list of start-end point pairs

    def delete_all_arrows(self):
        self.arrows = [] # :: [((x0, y0), (x1, y1)), ...] # list of start-end point pairs

    def intro_piece(self, bt, piece_type=None):
        bt = BoardType(bt)
        self.board = Board(bt, 2, 2)
        self.board.clear()
        self.delete_all_arrows()

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
        self.delete_all_arrows()

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
        self.delete_all_arrows()

        self.board.set_piece(1, 0, PieceType(PieceType.Knight))
        self.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in xrange(3, size):
            loc = 0 if i % 2 == 0 else 2
            self.board.set_piece(loc, i, PieceType(-PieceType.Pawn))

        return bt

    def move_pegasus(self, bt=BoardType.CroatianTies):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        self.arrows.append( Arrow(600, 500, 4300, 2700, fg_color="#44FF44", bg_color="#440044"), )

        self.arrows.append( Arrow(6.3, 5.1, 4.3, 2.7, fg_color="#202020", bg_color="#7F00FF"), )

        return "move_pegasus"

    def move_pegasus_2(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(2, 1, PieceType(PieceType.Pegasus))

        self.arrows.append( Arrow(600, 500, 4300, 2700, fg_color="#44FF44", bg_color="#440044"), )

        self.arrows.append( Arrow(6.3, 5.1, 14.3, 22.7, fg_color="#202020", bg_color="#7F00FF"), )

        return "move_pegasus_2"

    def move_shaman(self, bt=BoardType.ConquestOfTlalocan):
        bt = BoardType(bt)
        self.board = Board(bt, 11, 11)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(5, 5, PieceType(-PieceType.Shaman))

        return "move_shaman"

    def move_shaman_2(self, bt=BoardType.ConquestOfTlalocan):
        bt = BoardType(bt)
        self.board = Board(bt, 15, 15)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(7, 7, PieceType(PieceType.Shaman))

        return "move_shaman_2"

    def move_monolith(self, bt=BoardType.Discovery):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(4, 4, PieceType(PieceType.Monolith))

        return "move_monolith"

    def move_monolith_2(self, bt=BoardType.Discovery):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(4, 4, PieceType(PieceType.Monolith))
        self.board.set_piece(3, 4, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 5, PieceType(-PieceType.Rook))

        return "move_monolith_2"

    def move_starchild(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(4, 4, PieceType(PieceType.Starchild))

        return "move_starchild"

    def move_starchild_2(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_piece(4, 4, PieceType(PieceType.Starchild))
        self.board.set_piece(3, 4, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 5, PieceType(-PieceType.Rook))

        return "move_starchild_2"

    def set_mirandas_veil_1(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 6, 6)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_pieces([(0, 0, PieceType(PieceType.Wave)),
                               (0, 2, PieceType(PieceType.King)),
                               (2, 0, PieceType(PieceType.Queen)),
                               (2, 2, PieceType(PieceType.Bishop)),
                               (2, 4, PieceType(PieceType.Rook)),
                               (4, 2, PieceType(PieceType.Rook)),
                               (4, 4, PieceType(PieceType.Wave)),
                               (0, 4, PieceType(-PieceType.Wave)),
                               (4, 0, PieceType(-PieceType.Wave))])

        return "set_mirandas_veil_1"

    def set_example_1(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 16, 5)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_pieces([(0, 0, PieceType(PieceType.Wave)),
                               (3, 3, PieceType(PieceType.Queen)),
                               (12, 0, PieceType(PieceType.Bishop)),
                               (14, 1, PieceType(PieceType.King)),
                               (15, 2, PieceType(-PieceType.Rook)),
                               (15, 0, PieceType(-PieceType.Queen)),
                               (9, 0, PieceType(-PieceType.King))])

        return "set_example_1"

    def set_star_journey(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 8, 8)
        self.board.clear()
        self.delete_all_arrows()

        self.board.set_pieces([(0, 0, PieceType(-PieceType.Star)),
                               (1, 2, PieceType(PieceType.Monolith)),
                               (5, 3, PieceType(PieceType.Knight)),
                               (6, 4, PieceType(PieceType.Monolith)),
                               (3, 5, PieceType(PieceType.Starchild))])

        return "set_star_journey"

    def get_example_scene_functions(self):
        """return [ self.move_pegasus, \
                 self.move_pegasus_2, \
                 self.move_shaman, \
                 self.move_shaman_2, \
                 self.move_monolith, \
                 self.move_monolith_2, \
                 self.move_starchild, \
                 self.move_starchild_2, \
                 self.set_mirandas_veil_1, \
                 self.set_example_1, \
                 self.set_star_journey ] """
        return [ self.move_pegasus, \
                 self.move_pegasus_2 ]
