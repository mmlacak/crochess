#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from piece import PieceType
from board import BoardType
from board import Board

import math

class Rules(object):
    def __init__(self, board):
        self.board = board

    def init_none(self, offset_width, offset_height, size):
        pass

    def _init_pawns(self, offset_width, offset_height, size):
        w = size # self.board.get_width()
        h = size # self.board.get_height()

        h_light = offset_height + 1
        h_dark = offset_height + h - 2

        for i in xrange(w):
            self.board.set_piece(offset_width + i, h_light, PieceType(PieceType.Pawn))
            self.board.set_piece(offset_width + i, h_dark, PieceType(-PieceType.Pawn))

    def init_classic(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 0, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 1, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 2, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 3, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 3, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 3 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 3 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 4 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 4 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 5 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 5 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 6 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 6 + offset, h_1, PieceType(-PieceType.Rook))])

    def init_croatian_ties(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 0, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 1, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 2, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 3, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 4, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 4, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 4 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 4 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 5 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 5 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 6 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 6 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 7 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 7 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 8 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 8 + offset, h_1, PieceType(-PieceType.Rook))])

    def init_mayan_ascendancy(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 0, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 1, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 3, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 4, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 5, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 5, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 5 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 5 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 6 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 6 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 7 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 7 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 8 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 8 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 9 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 9 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 10 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 10 + offset, h_1, PieceType(-PieceType.Rook))])

    def init_age_of_aquarius(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 0, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 1, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 3, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 4, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 5, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 6, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 6, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 6 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 6 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 7 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 7 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 8 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 8 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 9 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 9 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 10 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 10 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 11 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Rook))])

    def init_mirandas_veil(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 0, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 1, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 3, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 4, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 5, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 6, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 7, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 7, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 7 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 7 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 8 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 8 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 9 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 9 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 10 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 10 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 11 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Rook))])

    def init_nineteen(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(-PieceType.Star)),
                               (offset_width + 0, h_1, PieceType(PieceType.Star)),
                               (offset_width + 1, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 3, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 4, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 5, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 6, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 7, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 7, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 8, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 8, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 8 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 8 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 9 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 9 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 10 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 10 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 11 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 15 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 15 + offset, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 16 + offset, h_0, PieceType(PieceType.Star)),
                               (offset_width + 16 + offset, h_1, PieceType(-PieceType.Star))])

    def init_hemeras_dawn(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(-PieceType.Star)),
                               (offset_width + 0, h_1, PieceType(PieceType.Star)),
                               (offset_width + 1, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 3, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 4, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 5, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 6, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 7, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 7, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 8, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 8, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 9, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 9, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 9 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 9 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 10 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 10 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 11 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 15 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 15 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 16 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 16 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 17 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 17 + offset, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 18 + offset, h_0, PieceType(PieceType.Star)),
                               (offset_width + 18 + offset, h_1, PieceType(-PieceType.Star))])

    def init_tamoanchan_revisited(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(-PieceType.Star)),
                               (offset_width + 0, h_1, PieceType(PieceType.Star)),
                               (offset_width + 1, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 3, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 4, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 5, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 6, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 7, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 7, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 8, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 8, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 9, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 9, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 10, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 10, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 10 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 10 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 11 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 15 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 15 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 16 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 16 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 17 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 17 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 18 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 18 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 19 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 19 + offset, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 20 + offset, h_0, PieceType(PieceType.Star)),
                               (offset_width + 20 + offset, h_1, PieceType(-PieceType.Star))])

    def init_conquest_of_tlalocan(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(-PieceType.Star)),
                               (offset_width + 0, h_1, PieceType(PieceType.Star)),
                               (offset_width + 1, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 3, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 4, h_0, PieceType(PieceType.Shaman)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Shaman)),
                               (offset_width + 5, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 6, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 7, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 7, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 8, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 8, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 9, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 9, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 10, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 10, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 11, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 11, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 11 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 15 + offset, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 15 + offset, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 16 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 16 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 17 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 17 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 18 + offset, h_0, PieceType(PieceType.Shaman)),
                               (offset_width + 18 + offset, h_1, PieceType(-PieceType.Shaman)),
                               (offset_width + 19 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 19 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 20 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 20 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 21 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 21 + offset, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 22 + offset, h_0, PieceType(PieceType.Star)),
                               (offset_width + 22 + offset, h_1, PieceType(-PieceType.Star))])

    def _calc_monolith_init_pos(self, pt, offset_width, offset_height):
        w = self.board.get_width()
        dx = int(math.floor(w / 11.0))
        h = self.board.get_height()
        dy = int(math.ceil(7.0 * h / 22.0))
        if pt.is_light_or_dark():
            return (offset_width + dx - 1, offset_height + dy - 1)
        else:
#            return (offset_width + w - dx, offset_height + h - dy)
#            return (w - dx, h - dy)
            return (w - dx - offset_width, h - dy - offset_width)

    def _init_monolith(self, pt, offset_width, offset_height):
        pt = PieceType(pt)
        i, j = self._calc_monolith_init_pos(pt, offset_width, offset_height)
        self.board.set_piece(i, j, pt)

    def init_discovery(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(-PieceType.Star)),
                               (offset_width + 0, h_1, PieceType(PieceType.Star)),
                               (offset_width + 1, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 3, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 4, h_0, PieceType(PieceType.Shaman)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Shaman)),
                               (offset_width + 5, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 6, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 7, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 7, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 8, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 8, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 9, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 9, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 10, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 10, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 11, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 11, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 11 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 11 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 12 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 15 + offset, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 15 + offset, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 16 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 16 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 17 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 17 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 18 + offset, h_0, PieceType(PieceType.Shaman)),
                               (offset_width + 18 + offset, h_1, PieceType(-PieceType.Shaman)),
                               (offset_width + 19 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 19 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 20 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 20 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 21 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 21 + offset, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 22 + offset, h_0, PieceType(PieceType.Star)),
                               (offset_width + 22 + offset, h_1, PieceType(-PieceType.Star))])

        self._init_monolith(PieceType.Monolith, offset_width, offset_height)
        self._init_monolith(-PieceType.Monolith, offset_width, offset_height)

    def init_one(self, offset_width, offset_height, size):
        self.board.clear()
        self._init_pawns(offset_width, offset_height, size)

        h_dark = size - 1 # self.board.get_height() - 1
        h_0 = offset_height + 0
        h_1 = offset_height + h_dark
        self.board.set_pieces([(offset_width + 0, h_0, PieceType(-PieceType.Star)),
                               (offset_width + 0, h_1, PieceType(PieceType.Star)),
                               (offset_width + 1, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 1, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 2, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 2, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 3, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 3, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 4, h_0, PieceType(PieceType.Shaman)),
                               (offset_width + 4, h_1, PieceType(-PieceType.Shaman)),
                               (offset_width + 5, h_0, PieceType(PieceType.Starchild)),
                               (offset_width + 5, h_1, PieceType(-PieceType.Starchild)),
                               (offset_width + 6, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 6, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 7, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 7, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 8, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 8, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 9, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 9, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 10, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 10, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 11, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 11, h_1, PieceType(-PieceType.Bishop))])

        offset = 0
        if self.board.type.is_even_or_odd():
            self.board.set_piece(offset_width + 12, h_0, PieceType(PieceType.Queen))
            self.board.set_piece(offset_width + 12, h_1, PieceType(-PieceType.Queen))
            offset = 1

        self.board.set_pieces([(offset_width + 12 + offset, h_0, PieceType(PieceType.King)),
                               (offset_width + 12 + offset, h_1, PieceType(-PieceType.King)),
                               (offset_width + 13 + offset, h_0, PieceType(PieceType.Bishop)),
                               (offset_width + 13 + offset, h_1, PieceType(-PieceType.Bishop)),
                               (offset_width + 14 + offset, h_0, PieceType(PieceType.Knight)),
                               (offset_width + 14 + offset, h_1, PieceType(-PieceType.Knight)),
                               (offset_width + 15 + offset, h_0, PieceType(PieceType.Serpent)),
                               (offset_width + 15 + offset, h_1, PieceType(-PieceType.Serpent)),
                               (offset_width + 16 + offset, h_0, PieceType(PieceType.Centaur)),
                               (offset_width + 16 + offset, h_1, PieceType(-PieceType.Centaur)),
                               (offset_width + 17 + offset, h_0, PieceType(PieceType.Wave)),
                               (offset_width + 17 + offset, h_1, PieceType(-PieceType.Wave)),
                               (offset_width + 18 + offset, h_0, PieceType(PieceType.Unicorn)),
                               (offset_width + 18 + offset, h_1, PieceType(-PieceType.Unicorn)),
                               (offset_width + 19 + offset, h_0, PieceType(PieceType.Starchild)),
                               (offset_width + 19 + offset, h_1, PieceType(-PieceType.Starchild)),
                               (offset_width + 20 + offset, h_0, PieceType(PieceType.Shaman)),
                               (offset_width + 20 + offset, h_1, PieceType(-PieceType.Shaman)),
                               (offset_width + 21 + offset, h_0, PieceType(PieceType.Pyramid)),
                               (offset_width + 21 + offset, h_1, PieceType(-PieceType.Pyramid)),
                               (offset_width + 22 + offset, h_0, PieceType(PieceType.Pegasus)),
                               (offset_width + 22 + offset, h_1, PieceType(-PieceType.Pegasus)),
                               (offset_width + 23 + offset, h_0, PieceType(PieceType.Rook)),
                               (offset_width + 23 + offset, h_1, PieceType(-PieceType.Rook)),
                               (offset_width + 24 + offset, h_0, PieceType(PieceType.Star)),
                               (offset_width + 24 + offset, h_1, PieceType(-PieceType.Star))])

        self._init_monolith(PieceType.Monolith, offset_width, offset_height)
        self._init_monolith(-PieceType.Monolith, offset_width, offset_height)

    def init_board(self, board_type=None):
        bt = board_type or self.board.type
        f = { BoardType.none: self.init_none,
              BoardType.OddClassical: self.init_classic,
              BoardType.Classical: self.init_classic,
              BoardType.OddCroatianTies: self.init_croatian_ties,
              BoardType.CroatianTies: self.init_croatian_ties,
              BoardType.OddMayanAscendancy: self.init_mayan_ascendancy,
              BoardType.MayanAscendancy: self.init_mayan_ascendancy,
              BoardType.OddAgeOfAquarius: self.init_age_of_aquarius,
              BoardType.AgeOfAquarius: self.init_age_of_aquarius,
              BoardType.OddMirandasVeil: self.init_mirandas_veil,
              BoardType.MirandasVeil: self.init_mirandas_veil,
              BoardType.OddNineteen: self.init_nineteen,
              BoardType.Nineteen: self.init_nineteen,
              BoardType.OddHemerasDawn: self.init_hemeras_dawn,
              BoardType.HemerasDawn: self.init_hemeras_dawn,
              BoardType.OddTamoanchanRevisited: self.init_tamoanchan_revisited,
              BoardType.TamoanchanRevisited: self.init_tamoanchan_revisited,
              BoardType.OddConquestOfTlalocan: self.init_conquest_of_tlalocan,
              BoardType.ConquestOfTlalocan: self.init_conquest_of_tlalocan,
              BoardType.OddDiscovery: self.init_discovery,
              BoardType.Discovery: self.init_discovery,
              BoardType.OddOne: self.init_one,
              BoardType.One: self.init_one }[bt]
        offset_width = (self.board.get_width() - bt.get_size()) / 2
        offset_height = (self.board.get_height() - bt.get_size()) / 2
        size = bt.get_size()
        f(offset_width, offset_height, size)
