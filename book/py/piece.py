#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.


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
    def iter(include_none=False, include_light_pieces=True, include_dark_pieces=False, do_construct=True):
        lst = []

        p_lst = [ PieceType.Pawn, \
                  PieceType.Bishop, \
                  PieceType.Knight, \
                  PieceType.Rook, \
                  PieceType.Queen, \
                  PieceType.King, \
                  PieceType.Pegasus, \
                  PieceType.Pyramid, \
                  PieceType.Unicorn, \
                  PieceType.Wave, \
                  PieceType.Star, \
                  PieceType.Centaur, \
                  PieceType.Serpent, \
                  PieceType.Shaman, \
                  PieceType.Monolith, \
                  PieceType.Starchild ]

        if include_light_pieces:
            lst.extend(p_lst)

        if include_dark_pieces:
            l = [ -pt for pt in p_lst ]
            lst.extend(l)

        if include_none:
            lst.insert(0, PieceType.none)

        lst.sort()
        return [ PieceType(pt) if do_construct else pt for pt in lst ]

    @staticmethod
    def _is_valid(piece_type):
        return piece_type in PieceType.iter(include_none=True, include_dark_pieces=True, do_construct=False)

    def get_symbol(self):
        return { PieceType.none: '.',
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
                 PieceType.Starchild: 'I' }[ self.get_enumerated() ]

    def get_label(self):
        sym = self.get_symbol()
        lbl = sym.upper() if self.is_light() else sym.lower()
        return lbl

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
                 PieceType.Starchild: 'Starchild' }[ self.get_enumerated() ]

    def is_light(self):
        return self > PieceType.none

    def is_dark(self):
        return self < PieceType.none

    def get_enumerated(self):
        return PieceType(abs(self))

    def get_opposite(self):
        return PieceType(-self)

    def get_light(self):
        return self if self.is_light() else self.get_opposite()

    def get_dark(self):
        return self if self.is_dark() else self.get_opposite()

    def is_friend(self, other):
        o = PieceType(other)
        return (self.is_light() and o.is_light()) or (self.is_dark() and o.is_dark())

    def is_foe(self, other):
        o = PieceType(other)
        return (self.is_light() and o.is_dark()) or (self.is_dark() and o.is_light())

    def __str__(self):
        return self.get_label()


def test_1():
    print()

    for pt in PieceType.iter(include_none=True, include_light_pieces=True, include_dark_pieces=True):
        print( pt.get_label(), pt.get_name(), pt.get_symbol(), pt.is_light(), pt.is_dark(), pt.get_opposite(), pt.get_light(), pt.get_dark() )

    print()

if __name__ == '__main__':
    test_1()
