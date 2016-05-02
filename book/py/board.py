#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010, .. 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from piece import PieceType

class BoardType(int):
    none = 0
    OddClassical = 1
    Classical = 2
    OddCroatianTies = 3
    CroatianTies = 4
    OddMayanAscendancy = 5
    MayanAscendancy = 6
    OddAgeOfAquarius = 7
    AgeOfAquarius = 8
    OddMirandasVeil = 9
    MirandasVeil = 10
    OddNineteen = 11
    Nineteen = 12
    OddHemerasDawn = 13
    HemerasDawn = 14
    OddTamoanchanRevisited = 15
    TamoanchanRevisited = 16
    OddConquestOfTlalocan = 17
    ConquestOfTlalocan = 18
    OddDiscovery = 19
    Discovery = 20
    OddOne = 21
    One = 22

    def __new__(cls, value):
        if BoardType._is_valid(value):
            return super(BoardType, cls).__new__(cls, value)
        else:
            raise ValueError("No such a board type, received '%s'." % (str(value), ))

    def is_even_or_odd(self):
        return (self % 2) == 0

    @staticmethod
    def _is_valid(board_type):
        return BoardType.none <= board_type <= BoardType.One

    def is_valid(self):
        return BoardType._is_valid(self)

    def get_name(self):
        return { BoardType.none: 'none',
                 BoardType.OddClassical: 'Odd Classical',
                 BoardType.Classical: 'Classical',
                 BoardType.OddCroatianTies: 'Odd Croatian Ties',
                 BoardType.CroatianTies: 'Croatian Ties',
                 BoardType.OddMayanAscendancy: 'Odd Mayan Ascendancy',
                 BoardType.MayanAscendancy: 'Mayan Ascendancy',
                 BoardType.OddAgeOfAquarius: 'Odd Age Of Aquarius',
                 BoardType.AgeOfAquarius: 'Age Of Aquarius',
                 BoardType.OddMirandasVeil: 'Odd Miranda\'s Veil',
                 BoardType.MirandasVeil: 'Miranda\'s Veil',
                 BoardType.OddNineteen: 'Odd Nineteen',
                 BoardType.Nineteen: 'Nineteen',
                 BoardType.OddHemerasDawn: 'Odd Hemera\'s Dawn',
                 BoardType.HemerasDawn: 'Hemera\'s Dawn',
                 BoardType.OddTamoanchanRevisited: 'Odd Tamoanchan Revisited',
                 BoardType.TamoanchanRevisited: 'Tamoanchan Revisited',
                 BoardType.OddConquestOfTlalocan: 'Odd Conquest Of Tlalocan',
                 BoardType.ConquestOfTlalocan: 'Conquest Of Tlalocan',
                 BoardType.OddDiscovery: 'Odd Discovery',
                 BoardType.Discovery: 'Discovery',
                 BoardType.OddOne: 'Odd One',
                 BoardType.One: 'One' }[self]

    def get_size(self):
        return { BoardType.none: 0,
                 BoardType.OddClassical: 7,
                 BoardType.Classical: 8,
                 BoardType.OddCroatianTies: 9,
                 BoardType.CroatianTies: 10,
                 BoardType.OddMayanAscendancy: 11,
                 BoardType.MayanAscendancy: 12,
                 BoardType.OddAgeOfAquarius: 13,
                 BoardType.AgeOfAquarius: 14,
                 BoardType.OddMirandasVeil: 15,
                 BoardType.MirandasVeil: 16,
                 BoardType.OddNineteen: 17,
                 BoardType.Nineteen: 18,
                 BoardType.OddHemerasDawn: 19,
                 BoardType.HemerasDawn: 20,
                 BoardType.OddTamoanchanRevisited: 21,
                 BoardType.TamoanchanRevisited: 22,
                 BoardType.OddConquestOfTlalocan: 23,
                 BoardType.ConquestOfTlalocan: 24,
                 BoardType.OddDiscovery: 23,
                 BoardType.Discovery: 24,
                 BoardType.OddOne: 25,
                 BoardType.One: 26 }[self]

class Board(object):
    def __init__(self, type, width=None, height=None):
        self.type = type

        if width is None:
            width = self.type.get_size()
        self._width = width

        if height is None:
            height = self.type.get_size()
        self._height = height

        self._board = [ [ PieceType(PieceType.none) for i in xrange(self._width) ] for j in xrange(self._height) ]

    def _is_file(self, i):
        return 0 <= i < self._width

    def _is_rank(self, j):
        return 0 <= j < self._height

    def is_on_board(self, i, j):
        return self._is_file(i) and self._is_rank(j)

    def is_square(self):
        return self._height == self._width

    def is_by_the_book(self):
        return (self._height == self.type.get_size()) and (self._width == self.type.get_size())

    def clear(self):
        for j in xrange(self._height):
            for i in xrange(self._width):
                self.set_piece(i, j, PieceType(PieceType.none))

    def get_width(self):
        return self._width

    def get_height(self):
        return self._height

    def set_piece(self, i, j, piece):
        self[j][i] = piece

    def set_pieces(self, l):
        for t in l:
            self.set_piece(*t)

    def get_piece(self, i, j):
        return self[j][i]

    # Pretending to be a list.

    def __len__(self):
        return self._width * self._height

    def __getitem__(self, index):
        return self._board[index]

    def __setitem__(self, index, piece):
        self._board[index] = piece

    def __delitem__(self, index):
        self._board[index] = PieceType(PieceType.none)

    def __iter__(self):
        return iter(self._board)

    def __reversed__(self):
        raise NotImplementedError("Can't reverse board. All pieces would fall off.")

    def __contains__(self, piece):
        for l in self._board:
            for p in l:
                if p == piece:
                    return p
        return None
