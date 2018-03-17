#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

from piece import PieceType as PT


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

#    @staticmethod
#    def foreach(start=None, end=None, step=1):
#        start = start or BoardType.none
#        end = end or BoardType.One

#        for bt in xrange(start, end+1, step):
#            # Added +1 because upper limit is not included in loop.
#            yield BoardType(bt)

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

#    def get_newly_introduced_piece(self):
#        return { BoardType.none: None,
#                 BoardType.OddClassical: None,
#                 BoardType.Classical: None,
#                 BoardType.OddCroatianTies: PT.Pegasus,
#                 BoardType.CroatianTies: PT.Pegasus,
#                 BoardType.OddMayanAscendancy: PT.Pyramid,
#                 BoardType.MayanAscendancy: PT.Pyramid,
#                 BoardType.OddAgeOfAquarius: PT.Unicorn,
#                 BoardType.AgeOfAquarius: PT.Unicorn,
#                 BoardType.OddMirandasVeil: PT.Wave,
#                 BoardType.MirandasVeil: PT.Wave,
#                 BoardType.OddNineteen: PT.Star,
#                 BoardType.Nineteen: PT.Star,
#                 BoardType.OddHemerasDawn: PT.Centaur,
#                 BoardType.HemerasDawn: PT.Centaur,
#                 BoardType.OddTamoanchanRevisited: PT.Serpent,
#                 BoardType.TamoanchanRevisited: PT.Serpent,
#                 BoardType.OddConquestOfTlalocan: PT.Shaman,
#                 BoardType.ConquestOfTlalocan: PT.Shaman,
#                 BoardType.OddDiscovery: PT.Monolith,
#                 BoardType.Discovery: PT.Monolith,
#                 BoardType.OddOne: PT.Starchild,
#                 BoardType.One: PT.Starchild }[self]

#    def get_newly_introducing_board_types(self, piece_type):
#        pt = PT(piece_type)
#        return { PT.none: None,
#                 PT.Pawn: None,
#                 PT.Bishop: None,
#                 PT.Knight: None,
#                 PT.Rook: None,
#                 PT.Queen: None,
#                 PT.King: None,
#                 PT.Pegasus: [BoardType.OddCroatianTies, BoardType.CroatianTies],
#                 PT.Pyramid: [BoardType.OddMayanAscendancy, BoardType.MayanAscendancy],
#                 PT.Unicorn: [BoardType.OddAgeOfAquarius, BoardType.AgeOfAquarius],
#                 PT.Wave: [BoardType.OddMirandasVeil, BoardType.MirandasVeil],
#                 PT.Star: [BoardType.OddNineteen, BoardType.Nineteen],
#                 PT.Centaur: [BoardType.OddHemerasDawn, BoardType.HemerasDawn],
#                 PT.Serpent: [BoardType.OddTamoanchanRevisited, BoardType.TamoanchanRevisited],
#                 PT.Shaman: [BoardType.OddConquestOfTlalocan, BoardType.ConquestOfTlalocan],
#                 PT.Monolith: [BoardType.OddDiscovery, BoardType.Discovery],
#                 PT.Starchild: [BoardType.OddOne, BoardType.One] }[pt.get_enumerated()]

#    def get_all_that_contain(self, piece_type):
#        start = self.get_newly_introducing_board_types(piece_type)
#        start = start[0] if start is not None else BoardType.OddClassical

#        lst = [ BoardType(bt) for bt in BoardType.foreach(start) ]
#        return lst

#    def does_contain(self, piece_type):
#        start = PT.Pawn
#        end = self.get_newly_introduced_piece() or PT.King
#        return piece_type in PT.foreach(start, end)

    def get_position_limits(self):
        limit = self.get_size() - 1
        return ((0, 0), (limit, limit))


class BoardHints(object):
#     def __init__(self, is_universal=False, reverse_field_colors=False):
    def __init__(self, reverse_field_colors=False):
#         self.is_universal = is_universal
        self.reverse_field_colors = reverse_field_colors


def get_opposites(pieces):
    return [ PT(p).get_opposite() for p in pieces ]

def remove_pieces(pieces, to_remove=(PT.Queen, -PT.Queen)):
    return [ p for p in pieces if p not in to_remove ]


class Board(object):
    def __init__(self, board_type, width=None, height=None, hints=None):
        self.type = BoardType(board_type)

        self._width = width or self.type.get_size()
        self._height = height or self.type.get_size()

        self._board = [ [ PT(PT.none) for i in xrange(self.get_width()) ] for j in xrange(self.get_height()) ]

        self.hints = hints or BoardHints()

    def _is_file(self, i):
        return 0 <= i < self.get_width()

    def _is_rank(self, j):
        return 0 <= j < self.get_height()

    def is_on_board(self, i, j):
        return self._is_file(i) and self._is_rank(j)

    def is_by_the_book(self):
        return (self.get_height() == self.type.get_size()) and (self.get_width() == self.type.get_size())

    def clear(self):
        for j in xrange(self.get_height()):
            for i in xrange(self.get_width()):
                self.set_piece(i, j, PT(PT.none))

    def get_width(self):
        return self._width

    def get_height(self):
        return self._height

    def set_piece(self, i, j, piece):
        self._board[j][i] = PT(piece)

    def set_pieces(self, lst):
        # lst :: [ ( i, j, piece ), ... ]
        for tpl in lst:
            self.set_piece(*tpl)

    def set_row(self, j, pieces):
        for i, p in enumerate(pieces):
            self.set_piece(i, j, p)

    def get_piece(self, i, j):
        return self._board[j][i]

    def get_position_limits(self):
        h = self.get_height() - 1
        w = self.get_width() - 1
        return ((0, 0), (w, h))

    # -----------------------------------------------------------------
    # Setting up initial position

    def _setup_pawns(self):
        if not self.is_by_the_book():
            return

        light = [ PT.Pawn for i in xrange(self.get_width()) ]
        self.set_row(1, light)

        dark = get_opposites(light)
        self.set_row(self.get_height() - 2, dark)

    def _setup_board(self, light_pieces):
        if not self.is_by_the_book():
            return

        self.clear()
        self._setup_pawns()

        if not self.type.is_even_or_odd():
            light_pieces = remove_pieces(light_pieces)
        self.set_row(0, light_pieces)

        dark = get_opposites(light_pieces)
        self.set_row(self.get_height() - 1, dark)

    def setup_none(self):
        pass

    def setup_classic(self):
        light = [ PT.Rook, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Rook ]

        self._setup_board(light)

    def setup_croatian_ties(self):
        light = [ PT.Rook, \
                  PT.Pegasus, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Pegasus, \
                  PT.Rook ]

        self._setup_board(light)

    def setup_mayan_ascendancy(self):
        light = [ PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook ]

        self._setup_board(light)

    def setup_age_of_aquarius(self):
        light = [ PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Unicorn, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Unicorn, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook ]

        self._setup_board(light)

    def setup_mirandas_veil(self):
        light = [ PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook ]

        self._setup_board(light)

    def setup_nineteen(self):
        light = [ PT.Star, \
                  PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook, \
                  PT.Star ]

        self._setup_board(light)

    def setup_hemeras_dawn(self):
        light = [ PT.Star, \
                  PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Centaur, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Centaur, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook, \
                  PT.Star ]

        self._setup_board(light)

    def setup_tamoanchan_revisited(self):
        light = [ PT.Star, \
                  PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Centaur, \
                  PT.Serpent, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Serpent, \
                  PT.Centaur, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook, \
                  PT.Star ]

        self._setup_board(light)

    def setup_conquest_of_tlalocan(self):
        light = [ PT.Star, \
                  PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Shaman, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Centaur, \
                  PT.Serpent, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Serpent, \
                  PT.Centaur, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Shaman, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook, \
                  PT.Star ]

        self._setup_board(light)


    def _calc_monolith_init_pos(self, pt):
        pt = PT(pt)

        w = self.get_width()
        dx = int(math.floor(w / 11.0))

        h = self.get_height()
        dy = int(math.ceil(7.0 * h / 22.0))

        if pt.is_light():
            return (dx - 1, dy - 1)
        else:
            return (w - dx, h - dy)

    def _setup_monolith(self, pt):
        pt = PT(pt)
        i, j = self._calc_monolith_init_pos(pt)
        self.set_piece(i, j, pt)

    def setup_discovery(self):
        light = [ PT.Star, \
                  PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Shaman, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Centaur, \
                  PT.Serpent, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Serpent, \
                  PT.Centaur, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Shaman, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook, \
                  PT.Star ]

        self._setup_board(light)

        self._setup_monolith(PT.Monolith)
        self._setup_monolith(-PT.Monolith)

    def setup_one(self):
        light = [ PT.Star, \
                  PT.Rook, \
                  PT.Pegasus, \
                  PT.Pyramid, \
                  PT.Shaman, \
                  PT.Starchild, \
                  PT.Unicorn, \
                  PT.Wave, \
                  PT.Centaur, \
                  PT.Serpent, \
                  PT.Knight, \
                  PT.Bishop, \
                  PT.Queen, \
                  PT.King, \
                  PT.Bishop, \
                  PT.Knight, \
                  PT.Serpent, \
                  PT.Centaur, \
                  PT.Wave, \
                  PT.Unicorn, \
                  PT.Starchild, \
                  PT.Shaman, \
                  PT.Pyramid, \
                  PT.Pegasus, \
                  PT.Rook, \
                  PT.Star ]

        self._setup_board(light)

        self._setup_monolith(PT.Monolith)
        self._setup_monolith(-PT.Monolith)

    def setup(self):
        f = { BoardType.none: self.setup_none,
              BoardType.OddClassical: self.setup_classic,
              BoardType.Classical: self.setup_classic,
              BoardType.OddCroatianTies: self.setup_croatian_ties,
              BoardType.CroatianTies: self.setup_croatian_ties,
              BoardType.OddMayanAscendancy: self.setup_mayan_ascendancy,
              BoardType.MayanAscendancy: self.setup_mayan_ascendancy,
              BoardType.OddAgeOfAquarius: self.setup_age_of_aquarius,
              BoardType.AgeOfAquarius: self.setup_age_of_aquarius,
              BoardType.OddMirandasVeil: self.setup_mirandas_veil,
              BoardType.MirandasVeil: self.setup_mirandas_veil,
              BoardType.OddNineteen: self.setup_nineteen,
              BoardType.Nineteen: self.setup_nineteen,
              BoardType.OddHemerasDawn: self.setup_hemeras_dawn,
              BoardType.HemerasDawn: self.setup_hemeras_dawn,
              BoardType.OddTamoanchanRevisited: self.setup_tamoanchan_revisited,
              BoardType.TamoanchanRevisited: self.setup_tamoanchan_revisited,
              BoardType.OddConquestOfTlalocan: self.setup_conquest_of_tlalocan,
              BoardType.ConquestOfTlalocan: self.setup_conquest_of_tlalocan,
              BoardType.OddDiscovery: self.setup_discovery,
              BoardType.Discovery: self.setup_discovery,
              BoardType.OddOne: self.setup_one,
              BoardType.One: self.setup_one }[self.type]
        f()

    # -----------------------------------------------------------------

    def __str__(self):
        s = ""
        for j in xrange(self.get_height()-1, -1, -1):
            for i in xrange(self.get_width()):
                p = self.get_piece(i, j)
                s += "%c" % p.get_label()
            s += "\n"
        return s


def test_1():
    b = Board(BoardType.Classical, width=3, height=2)

    print
    print b.get_position_limits()
    print
    print str(b)
    print

    b.set_piece(2, 1, PT.Bishop)
    b.set_piece(1, 0, PT.Pawn)
    b.set_piece(0, 1, PT.Knight)
    b.set_piece(1, 1, -PT.Pawn)

    print
    print b.get_position_limits()
    print
    print str(b)
    print

def test_2():
    b = Board(BoardType.One)
    b.setup()

    print
    print b.get_position_limits()
    print
    print str(b)
    print

if __name__ == '__main__':
    # test_1()

    test_2()
