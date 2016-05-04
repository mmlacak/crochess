#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

class PieceType(int):
    none = 0
    Pawn = 1
    Bishop = 2
    Knight = 3
    Rook = 4
    Queen = 5
    King = 6
    Pegasus = 7
    Pyramid = 8
    Unicorn = 9
    Wave = 10
    Star = 11
    Centaur = 12
    Serpent = 13
    Shaman = 14
    Monolith = 15
    Starchild = 16

    def __new__(cls, value):
        if PieceType._is_valid(value):
            return super(PieceType, cls).__new__(cls, value)
        else:
            raise ValueError("No such a piece type, received '%s'." % (str(value), ))

    @staticmethod
    def foreach(start=None, end=None, step=1):
        start = start or PieceType.none
        end = end or PieceType.Starchild

        for pt in xrange(start, end+1, step):
            # Added +1 because upper limit is not included in loop.
            yield PieceType(pt)

    def get_symbol(self):
        return { PieceType.none: ' ',
                 PieceType.Pawn: 'P',
                 PieceType.Bishop: 'B',
                 PieceType.Knight: 'N',
                 PieceType.Rook: 'R',
                 PieceType.Queen: 'Q',
                 PieceType.King: 'K',
                 PieceType.Pegasus: 'G',
                 PieceType.Pyramid: 'A',
                 PieceType.Unicorn: 'U',
                 PieceType.Wave: 'W',
                 PieceType.Star: 'T',
                 PieceType.Centaur: 'C',
                 PieceType.Serpent: 'S',
                 PieceType.Shaman: 'H',
                 PieceType.Monolith: 'M',
                 PieceType.Starchild: 'I' }[self.get_enumerated()]

    def get_name(self):
        return { PieceType.none: 'none',
                 PieceType.Pawn: 'Pawn',
                 PieceType.Bishop: 'Bishop',
                 PieceType.Knight: 'Knight',
                 PieceType.Rook: 'Rook',
                 PieceType.Queen: 'Queen',
                 PieceType.King: 'King',
                 PieceType.Pegasus: 'Pegasus',
                 PieceType.Pyramid: 'Pyramid',
                 PieceType.Unicorn: 'Unicorn',
                 PieceType.Wave: 'Wave',
                 PieceType.Star: 'Star',
                 PieceType.Centaur: 'Centaur',
                 PieceType.Serpent: 'Serpent',
                 PieceType.Shaman: 'Shaman',
                 PieceType.Monolith: 'Monolith',
                 PieceType.Starchild: 'Starchild' }[self.get_enumerated()]

    def is_light(self):
        return self > PieceType.none

    def is_dark(self):
        return self < PieceType.none

    def is_light_or_dark(self):
        return self > PieceType.none

    def get_enumerated(self):
        return PieceType(abs(self))

    def get_opposite(self):
        return PieceType(-self)

    @staticmethod
    def _is_valid(piece_type):
        return (-PieceType.Starchild) <= piece_type <= PieceType.Starchild

    def is_valid(self):
        return PieceType._is_valid(self)
