#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from piece import PieceType
from board import BoardType
from board import Board

class Scene(object):
    def __init__(self, board=None):
        self.board = board

    def intro_piece(self, bt, piece_type=None):
        self.board = Board(bt, 2, 2)
        self.board.clear()

        piece_type = piece_type or bt.get_newly_introduced_piece()
        if piece_type is None:
            return None

        self.board.set_pieces([(0, 0, PieceType(piece_type)),
                               (1, 0, PieceType(piece_type)),
                               (0, 1, PieceType(-piece_type)),
                               (1, 1, PieceType(-piece_type))])

        return piece_type

    def move_shaman(self, bt=BoardType.ConquestOfTlalocan):
        bt = BoardType(bt)
        self.board = Board(bt, 11, 11)
        self.board.clear()

        self.board.set_piece(5, 5, PieceType(-PieceType.Shaman))

        return "move_shaman"

    def move_shaman_2(self, bt=BoardType.ConquestOfTlalocan):
        bt = BoardType(bt)
        self.board = Board(bt, 15, 15)
        self.board.clear()

        self.board.set_piece(7, 7, PieceType(PieceType.Shaman))

        return "move_shaman_2"

    def move_monolith(self, bt=BoardType.Discovery):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()

        self.board.set_piece(4, 4, PieceType(PieceType.Monolith))

        return "move_monolith"

    def move_monolith_2(self, bt=BoardType.Discovery):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()

        self.board.set_piece(4, 4, PieceType(PieceType.Monolith))
        self.board.set_piece(3, 4, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 5, PieceType(-PieceType.Rook))

        return "move_monolith_2"

    def move_starchild(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()

        self.board.set_piece(4, 4, PieceType(PieceType.Starchild))

        return "move_starchild"

    def move_starchild_2(self, bt=BoardType.One):
        bt = BoardType(bt)
        self.board = Board(bt, 9, 9)
        self.board.clear()

        self.board.set_piece(4, 4, PieceType(PieceType.Starchild))
        self.board.set_piece(3, 4, PieceType(PieceType.Pawn))
        self.board.set_piece(5, 5, PieceType(-PieceType.Rook))

        return "move_starchild_2"

    def set_mirandas_veil_1(self, bt=BoardType.MirandasVeil):
        bt = BoardType(bt)
        self.board = Board(bt, 6, 6)
        self.board.clear()

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

        self.board.set_pieces([(0, 0, PieceType(-PieceType.Star)),
                               (1, 2, PieceType(PieceType.Monolith)),
                               (5, 3, PieceType(PieceType.Knight)),
                               (6, 4, PieceType(PieceType.Monolith)),
                               (3, 5, PieceType(PieceType.Starchild))])

        return "set_star_journey"

    def get_example_scene_functions(self):
        return [ self.move_shaman, \
                 self.move_shaman_2, \
                 self.move_monolith, \
                 self.move_monolith_2, \
                 self.move_starchild, \
                 self.move_starchild_2, \
                 self.set_mirandas_veil_1, \
                 self.set_example_1, \
                 self.set_star_journey ]
