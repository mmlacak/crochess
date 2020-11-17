#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

from util import just_count
from pixel_math import assert_floor, assert_floor_2
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

    @staticmethod
    def iter(include_none=False, include_even=True, include_odd=False, do_construct=True):
        l_even =  [ BoardType.Classical, \
                    BoardType.CroatianTies, \
                    BoardType.MayanAscendancy, \
                    BoardType.AgeOfAquarius, \
                    BoardType.MirandasVeil, \
                    BoardType.Nineteen, \
                    BoardType.HemerasDawn, \
                    BoardType.TamoanchanRevisited, \
                    BoardType.ConquestOfTlalocan, \
                    BoardType.Discovery, \
                    BoardType.One ]

        l_odd =   [ BoardType.OddClassical, \
                    BoardType.OddCroatianTies, \
                    BoardType.OddMayanAscendancy, \
                    BoardType.OddAgeOfAquarius, \
                    BoardType.OddMirandasVeil, \
                    BoardType.OddNineteen, \
                    BoardType.OddHemerasDawn, \
                    BoardType.OddTamoanchanRevisited, \
                    BoardType.OddConquestOfTlalocan, \
                    BoardType.OddDiscovery, \
                    BoardType.OddOne ]

        lst = []

        if include_odd:
            lst.extend(l_odd)

        if include_even:
            lst.extend(l_even)

        if include_none:
            lst.insert(0, BoardType.none)

        lst.sort()
        return [ BoardType(bt) if do_construct else bt for bt in lst ]

    @staticmethod
    def _is_valid(board_type):
        return board_type in BoardType.iter(include_none=True, include_even=True, include_odd=True, do_construct=False)


    def is_even(self):
        return (self % 2) == 0

    def is_odd(self):
        return (self % 2) != 0

    def get_even(self):
        if self == BoardType.none:
            return self
        return self if self.is_even() else BoardType(self + 1)

    def get_odd(self):
        if self == BoardType.none:
            return self
        return BoardType(self - 1) if self.is_even() else self

    def get_pair(self):
        if self == BoardType.none:
            return [ self ]
        return [ self.get_odd(), self.get_even() ]

    def is_variants(self, bt):
        bt = BoardType(bt)
        return self in bt.get_pair()


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

    def get_symbol(self):
        return { BoardType.none: '',
                 BoardType.OddClassical: 'OC',
                 BoardType.Classical: 'C',
                 BoardType.OddCroatianTies: 'OCT',
                 BoardType.CroatianTies: 'CT',
                 BoardType.OddMayanAscendancy: 'OMA',
                 BoardType.MayanAscendancy: 'MA',
                 BoardType.OddAgeOfAquarius: 'OAOA',
                 BoardType.AgeOfAquarius: 'AOA',
                 BoardType.OddMirandasVeil: 'OMV',
                 BoardType.MirandasVeil: 'MV',
                 BoardType.OddNineteen: 'ON',
                 BoardType.Nineteen: 'N',
                 BoardType.OddHemerasDawn: 'OHD',
                 BoardType.HemerasDawn: 'HD',
                 BoardType.OddTamoanchanRevisited: 'OTR',
                 BoardType.TamoanchanRevisited: 'TR',
                 BoardType.OddConquestOfTlalocan: 'OCOT',
                 BoardType.ConquestOfTlalocan: 'COT',
                 BoardType.OddDiscovery: 'OD',
                 BoardType.Discovery: 'D',
                 BoardType.OddOne: 'OO',
                 BoardType.One: 'O' }[self]

    def get_label(self):
        return self.get_symbol().lower()

    @staticmethod
    def get(label, case_insensitive=True):
        dct  = { # '':     BoardType.none,
                 'OC':   BoardType.OddClassical,
                 'C':    BoardType.Classical,
                 'OCT':  BoardType.OddCroatianTies,
                 'CT':   BoardType.CroatianTies,
                 'OMA':  BoardType.OddMayanAscendancy,
                 'MA':   BoardType.MayanAscendancy,
                 'OAOA': BoardType.OddAgeOfAquarius,
                 'AOA':  BoardType.AgeOfAquarius,
                 'OMV':  BoardType.OddMirandasVeil,
                 'MV':   BoardType.MirandasVeil,
                 'ON':   BoardType.OddNineteen,
                 'N':    BoardType.Nineteen,
                 'OHD':  BoardType.OddHemerasDawn,
                 'HD':   BoardType.HemerasDawn,
                 'OTR':  BoardType.OddTamoanchanRevisited,
                 'TR':   BoardType.TamoanchanRevisited,
                 'OCOT': BoardType.OddConquestOfTlalocan,
                 'COT':  BoardType.ConquestOfTlalocan,
                 'OD':   BoardType.OddDiscovery,
                 'D':    BoardType.Discovery,
                 'OO':   BoardType.OddOne,
                 'O':    BoardType.One }

        lbl = label.upper() if case_insensitive else label

        if lbl in dct:
            return BoardType( dct[ lbl ] )
        else:
            return BoardType( BoardType.none )

    @staticmethod
    def get_even(do_construct=True):
        return list( BoardType.iter(include_none=False, include_even=True, include_odd=False, do_construct=do_construct) )

    @staticmethod
    def get_odd(do_construct=True):
        return list( BoardType.iter(include_none=False, include_even=False, include_odd=True, do_construct=do_construct) )

    @staticmethod
    def get_all(do_construct=True, include_none=False):
        return list( BoardType.iter(include_none=include_none, include_even=True, include_odd=True, do_construct=do_construct) )

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

    def get_newly_introduced_piece(self):
        pt = { BoardType.none: None,
               BoardType.OddClassical: None,
               BoardType.Classical: None,
               BoardType.OddCroatianTies: PT.Pegasus,
               BoardType.CroatianTies: PT.Pegasus,
               BoardType.OddMayanAscendancy: PT.Pyramid,
               BoardType.MayanAscendancy: PT.Pyramid,
               BoardType.OddAgeOfAquarius: PT.Unicorn,
               BoardType.AgeOfAquarius: PT.Unicorn,
               BoardType.OddMirandasVeil: PT.Wave,
               BoardType.MirandasVeil: PT.Wave,
               BoardType.OddNineteen: PT.Star,
               BoardType.Nineteen: PT.Star,
               BoardType.OddHemerasDawn: PT.Centaur,
               BoardType.HemerasDawn: PT.Centaur,
               BoardType.OddTamoanchanRevisited: PT.Serpent,
               BoardType.TamoanchanRevisited: PT.Serpent,
               BoardType.OddConquestOfTlalocan: PT.Shaman,
               BoardType.ConquestOfTlalocan: PT.Shaman,
               BoardType.OddDiscovery: PT.Monolith,
               BoardType.Discovery: PT.Monolith,
               BoardType.OddOne: PT.Starchild,
               BoardType.One: PT.Starchild }[ self ]
        return PT(pt) if pt is not None else None

    def get_newly_introducing_board_types(self, piece_type):
        pt = PT(piece_type)
        return { PT.none: None,
                 PT.Pawn: None,
                 PT.Bishop: None,
                 PT.Knight: None,
                 PT.Rook: None,
                 PT.Queen: None,
                 PT.King: None,
                 PT.Pegasus: [ BoardType.OddCroatianTies, BoardType.CroatianTies ],
                 PT.Pyramid: [ BoardType.OddMayanAscendancy, BoardType.MayanAscendancy ],
                 PT.Unicorn: [ BoardType.OddAgeOfAquarius, BoardType.AgeOfAquarius ],
                 PT.Wave: [ BoardType.OddMirandasVeil, BoardType.MirandasVeil ],
                 PT.Star: [ BoardType.OddNineteen, BoardType.Nineteen ],
                 PT.Centaur: [ BoardType.OddHemerasDawn, BoardType.HemerasDawn ],
                 PT.Serpent: [ BoardType.OddTamoanchanRevisited, BoardType.TamoanchanRevisited ],
                 PT.Shaman: [ BoardType.OddConquestOfTlalocan, BoardType.ConquestOfTlalocan ],
                 PT.Monolith: [ BoardType.OddDiscovery, BoardType.Discovery ],
                 PT.Starchild: [ BoardType.OddOne, BoardType.One ] }[ pt.get_enumerated() ]

    def get_all_that_contain(self, piece_type):
        start = self.get_newly_introducing_board_types(piece_type)
        start = start[0] if start is not None else BoardType.OddClassical
        return [ BoardType(bt) for bt in range(start, BoardType.One+1) ]

    def does_contain(self, piece_type):
        start = PT.Pawn
        end = self.get_newly_introduced_piece() or PT.King
        return piece_type in range(start, end+1)

    def get_position_limits(self):
        limit = self.get_size() - 1
        return ((0, 0), (limit, limit))


def get_opposites(pieces):
    return [ PT(p).get_opposite() for p in pieces ]

def remove_pieces(pieces, to_remove=(PT.Queen, -PT.Queen)):
    return [ PT(p) for p in pieces if p not in to_remove ]

def filter_setup(pieces, to_remain=(PT.King, PT.Rook, PT.Star, -PT.Star)):
    return [ PT(p) if p in to_remain else PT(PT.none) for p in pieces ]

def get_indexes(pieces, piece=PT.King):
    if piece not in pieces:
        return None

    return [ i for i, p in enumerate(pieces) if p == piece ]


class Board:
    def __init__(self, board_type):
        self.type = BoardType(board_type)

        self._board = [ [ PT(PT.none) for i in range(self.get_width()) ] for j in range(self.get_height()) ]

    def _is_file(self, i):
        _i = assert_floor(i)
        return 0 <= _i < self.get_width()

    def _is_rank(self, j):
        _j = assert_floor(j)
        return 0 <= _j < self.get_height()

    def is_on_board(self, i, j):
        return self._is_file(i) and self._is_rank(j)

    def is_light(self, i, j):
        _i, _j = assert_floor_2(i, j)

        b = self.get_width() % 2

        if (b == 0):
            is_light = bool((_i + _j) % 2 != 0)
        else:
            is_light = bool((_i + _j) % 2 == 0)

        return is_light

    def is_dark(self, i, j):
        return not self.is_light(i, j)

    def clear(self):
        for j in range(self.get_height()):
            for i in range(self.get_width()):
                self.set_piece(i, j, PT(PT.none))

    def get_width(self):
        return self.type.get_size()

    def get_height(self):
        return self.type.get_size()

    def get_piece(self, i, j):
        _i, _j = assert_floor_2(i, j)
        return self._board[ _j ][ _i ] if self.is_on_board(_i, _j) else PT.none # TODO :: having pieces outside of a board ?!?!?!

    def set_piece(self, i, j, piece):
        _i, _j = assert_floor_2(i, j)
        self._board[ _j ][ _i ] = PT(piece)

    def set_pieces(self, lst):
        # lst :: [ ( i, j, piece ), ... ]
        for tpl in lst:
            self.set_piece(*tpl)

    def set_row(self, j, pieces):
        _j = assert_floor(j)

        for i, p in enumerate(pieces):
            self.set_piece(i, j, p)

    def get_position_limits(self):
        h = self.get_height() - 1
        w = self.get_width() - 1
        return ((0, 0), (w, h))

    @staticmethod
    def get_castling_limits(board_type):
        bt = BoardType(board_type)

        if bt == BoardType.none:
            return (0, 0)

        light_pieces = Board.get_light_row(bt)

        pos_king = just_count(get_indexes(light_pieces, piece=PT.King))
        pos_rook_l, pos_rook_r = just_count(get_indexes(light_pieces, piece=PT.Rook), count=2)

        diff_l, diff_r = abs(pos_king - pos_rook_l), abs(pos_rook_r - pos_king)
        diff = min(diff_l, diff_r) - 1

        return (2, diff)

    # -----------------------------------------------------------------
    # Defining initial position

    @staticmethod
    def _get_none_row(is_even=True):
        return []

    @staticmethod
    def _get_classic_row(is_even=True):
        if is_even:
            lst =   [ \
                        PT.Rook, \
                        PT.Knight, \
                        PT.Bishop, \
                        PT.Queen, \
                        PT.King, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.Rook, \
                    ]
        else:
            lst =   [ \
                        PT.Rook, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.King, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.Rook, \
                    ]
        return lst

    @staticmethod
    def _get_croatian_ties_row(is_even=True):
        if is_even:
            lst =   [ \
                        PT.Rook, \
                        PT.Pegasus, \
                        PT.Knight, \
                        PT.Bishop, \
                        PT.Queen, \
                        PT.King, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.Pegasus, \
                        PT.Rook, \
                    ]
        else:
            lst =   [ \
                        PT.Rook, \
                        PT.Pegasus, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.King, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.Pegasus, \
                        PT.Rook, \
                    ]
        return lst

    @staticmethod
    def _get_mayan_ascendancy_row(is_even=True):
        if is_even:
            lst =   [ \
                        PT.Rook, \
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
                        PT.Rook, \
                    ]
        else:
            lst =   [ \
                        PT.Rook, \
                        PT.Pegasus, \
                        PT.Pyramid, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.King, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.Pyramid, \
                        PT.Pegasus, \
                        PT.Rook, \
                    ]
        return lst

    @staticmethod
    def _get_age_of_aquarius_row(is_even=True):
        if is_even:
            lst =   [ \
                        PT.Rook, \
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
                        PT.Rook, \
                    ]
        else:
            lst =   [ \
                        PT.Rook, \
                        PT.Pegasus, \
                        PT.Pyramid, \
                        PT.Unicorn, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.King, \
                        PT.Bishop, \
                        PT.Knight, \
                        PT.Unicorn, \
                        PT.Pyramid, \
                        PT.Pegasus, \
                        PT.Rook, \
                    ]
        return lst

    @staticmethod
    def _get_mirandas_veil_row(is_even=True):
        return  [ PT.Rook, \
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

    @staticmethod
    def _get_nineteen_row(is_even=True):
        return  [ PT.Star, \
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
                  -PT.Star ]

    @staticmethod
    def _get_hemeras_dawn_row(is_even=True):
        return  [ PT.Star, \
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
                  -PT.Star ]

    @staticmethod
    def _get_tamoanchan_revisited_row(is_even=True):
        return  [ PT.Star, \
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
                  -PT.Star ]

    @staticmethod
    def _get_conquest_of_tlalocan_row(is_even=True):
        return  [ PT.Star, \
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
                  -PT.Star ]

    @staticmethod
    def _get_discovery_row(is_even=True):
        return  [ PT.Star, \
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
                  -PT.Star ]

    @staticmethod
    def _get_one_row(is_even=True):
        return  [ PT.Star, \
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
                  -PT.Star ]

    @staticmethod
    def get_light_row(board_type):
        bt = BoardType(board_type)

        f = { BoardType.none: Board._get_none_row,
              BoardType.OddClassical: Board._get_classic_row,
              BoardType.Classical: Board._get_classic_row,
              BoardType.OddCroatianTies: Board._get_croatian_ties_row,
              BoardType.CroatianTies: Board._get_croatian_ties_row,
              BoardType.OddMayanAscendancy: Board._get_mayan_ascendancy_row,
              BoardType.MayanAscendancy: Board._get_mayan_ascendancy_row,
              BoardType.OddAgeOfAquarius: Board._get_age_of_aquarius_row,
              BoardType.AgeOfAquarius: Board._get_age_of_aquarius_row,
              BoardType.OddMirandasVeil: Board._get_mirandas_veil_row,
              BoardType.MirandasVeil: Board._get_mirandas_veil_row,
              BoardType.OddNineteen: Board._get_nineteen_row,
              BoardType.Nineteen: Board._get_nineteen_row,
              BoardType.OddHemerasDawn: Board._get_hemeras_dawn_row,
              BoardType.HemerasDawn: Board._get_hemeras_dawn_row,
              BoardType.OddTamoanchanRevisited: Board._get_tamoanchan_revisited_row,
              BoardType.TamoanchanRevisited: Board._get_tamoanchan_revisited_row,
              BoardType.OddConquestOfTlalocan: Board._get_conquest_of_tlalocan_row,
              BoardType.ConquestOfTlalocan: Board._get_conquest_of_tlalocan_row,
              BoardType.OddDiscovery: Board._get_discovery_row,
              BoardType.Discovery: Board._get_discovery_row,
              BoardType.OddOne: Board._get_one_row,
              BoardType.One: Board._get_one_row }[ bt ]

        light_pieces = f(is_even=bt.is_even())

        # if bt.is_odd():
        #     light_pieces = remove_pieces(light_pieces, to_remove=(PT.Queen, -PT.Queen))

        return light_pieces

    # -----------------------------------------------------------------
    # Setting up initial positions

    def _setup_pawns(self):
        light = [ PT.Pawn for i in range(self.get_width()) ]
        self.set_row(1, light)

        dark = get_opposites(light)
        self.set_row(self.get_height() - 2, dark)

    def _setup_board(self, light_pieces):
        self.clear()
        self._setup_pawns()

        # if not self.type.is_even():
        #     light_pieces = remove_pieces(light_pieces, to_remove=(PT.Queen, -PT.Queen))

        #     idx_K = len(light_pieces) // 2 # King is now in the middle of the row.

        #     # Swap left (Queen) side all neighboring pairs of figures, until reaching Rook, e.g. Knight and Bishop, etc.
        #     for i, j in zip( range(idx_K-1, 0, -2), range(idx_K-2, 0, -2) ):
        #         if j > 0 and light_pieces[ j ] != PT.Rook:
        #             light_pieces[ i ], light_pieces[ j ] = light_pieces[ j ], light_pieces[ i ]

        self.set_row(0, light_pieces)

        dark = get_opposites(light_pieces)
        self.set_row(self.get_height() - 1, dark)

    def _setup_none(self):
        pass

    def _setup_classic(self):
        light = Board._get_classic_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_croatian_ties(self):
        light = Board._get_croatian_ties_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_mayan_ascendancy(self):
        light = Board._get_mayan_ascendancy_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_age_of_aquarius(self):
        light = Board._get_age_of_aquarius_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_mirandas_veil(self):
        light = Board._get_mirandas_veil_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_nineteen(self):
        light = Board._get_nineteen_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_hemeras_dawn(self):
        light = Board._get_hemeras_dawn_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_tamoanchan_revisited(self):
        light = Board._get_tamoanchan_revisited_row(is_even=self.type.is_even())
        self._setup_board(light)

    def _setup_conquest_of_tlalocan(self):
        light = Board._get_conquest_of_tlalocan_row(is_even=self.type.is_even())
        self._setup_board(light)


    def _calc_monolith_init_pos(self, pt):
        pt = PT(pt)

        w = self.get_width()
        dx = int(math.floor(w / 11.0))

        h = self.get_height()
        dy = int(math.floor(7.0 * h / 22.0))

        if pt.is_light():
            return (dx - 1, dy - 1)
        else:
            return (w - dx, h - dy)

    def _setup_monolith(self, pt):
        pt = PT(pt)
        i, j = self._calc_monolith_init_pos(pt)
        self.set_piece(i, j, pt)

    def _setup_discovery(self):
        light = Board._get_discovery_row(is_even=self.type.is_even())
        self._setup_board(light)

        self._setup_monolith(PT.Monolith)
        self._setup_monolith(-PT.Monolith)

    def _setup_one(self):
        light = Board._get_one_row(is_even=self.type.is_even())
        self._setup_board(light)

        self._setup_monolith(PT.Monolith)
        self._setup_monolith(-PT.Monolith)

    def setup(self):
        f = { BoardType.none: self._setup_none,
              BoardType.OddClassical: self._setup_classic,
              BoardType.Classical: self._setup_classic,
              BoardType.OddCroatianTies: self._setup_croatian_ties,
              BoardType.CroatianTies: self._setup_croatian_ties,
              BoardType.OddMayanAscendancy: self._setup_mayan_ascendancy,
              BoardType.MayanAscendancy: self._setup_mayan_ascendancy,
              BoardType.OddAgeOfAquarius: self._setup_age_of_aquarius,
              BoardType.AgeOfAquarius: self._setup_age_of_aquarius,
              BoardType.OddMirandasVeil: self._setup_mirandas_veil,
              BoardType.MirandasVeil: self._setup_mirandas_veil,
              BoardType.OddNineteen: self._setup_nineteen,
              BoardType.Nineteen: self._setup_nineteen,
              BoardType.OddHemerasDawn: self._setup_hemeras_dawn,
              BoardType.HemerasDawn: self._setup_hemeras_dawn,
              BoardType.OddTamoanchanRevisited: self._setup_tamoanchan_revisited,
              BoardType.TamoanchanRevisited: self._setup_tamoanchan_revisited,
              BoardType.OddConquestOfTlalocan: self._setup_conquest_of_tlalocan,
              BoardType.ConquestOfTlalocan: self._setup_conquest_of_tlalocan,
              BoardType.OddDiscovery: self._setup_discovery,
              BoardType.Discovery: self._setup_discovery,
              BoardType.OddOne: self._setup_one,
              BoardType.One: self._setup_one }[ self.type ]

        f()

    # -----------------------------------------------------------------

    def __str__(self):
        s = ""
        for j in range(self.get_height()-1, -1, -1):
            for i in range(self.get_width()):
                p = self.get_piece(i, j)
                s += "%c" % p.get_label()
            s += "\n"
        return s


def test_1():
    b = Board(BoardType.Classical)

    print()
    print( b.get_position_limits() )
    print()
    print( str(b) )
    print()

    b.set_piece(2, 1, PT.Bishop)
    b.set_piece(1, 0, PT.Pawn)
    b.set_piece(0, 1, PT.Knight)
    b.set_piece(1, 1, -PT.Pawn)

    print()
    print( b.get_position_limits() )
    print()
    print( str(b) )
    print()

def test_2():
    bt = BoardType.CroatianTies # One
    b = Board(bt)
    b.setup()

    print()
    print( b.get_position_limits() )
    print()
    print( b.get_castling_limits(bt) )
    print()
    print( str(b) )
    print()

def test_3():
    print()

    for bt in BoardType.iter(include_none=True, include_even=True, include_odd=True):
        print( bt.get_name() )

    print()

def test_4():
    print()

    for bt in BoardType.iter(include_none=True, include_even=True, include_odd=True):
        b = Board(bt)
        b.setup()

        print( bt.get_name(), b.get_position_limits(), Board.get_castling_limits(bt) )

    print()

if __name__ == '__main__':
    test_1()
    test_2()
    test_3()
    test_4()
