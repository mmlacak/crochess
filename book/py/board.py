#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

import math
import random

from utils import just_count, iterate
from pixel_math import assert_floor, assert_floor_2
from piece import PieceType as PT


class BoardType( int ):
    none = 0
    Classical = 2
    CroatianTies = 4
    MayanAscendancy = 6
    AgeOfAquarius = 8
    MirandasVeil = 10
    Nineteen = 12
    HemerasDawn = 14
    TamoanchanRevisited = 16
    ConquestOfTlalocan = 18
    Discovery = 20
    One = 22

    Classic_14 = 24
    Classic_20 = 26
    Classic_26 = 28
    Croatian_14 = 30
    Croatian_20 = 32
    Croatian_26 = 34

    Classic_14_Mirrored = 36
    Classic_20_Mirrored = 38
    Classic_26_Mirrored = 40
    Croatian_14_Mirrored = 42
    Croatian_20_Mirrored = 44
    Croatian_26_Mirrored = 46

    Classic_14_Symmetrical = 48
    Classic_20_Symmetrical = 50
    Classic_26_Symmetrical = 52
    Croatian_14_Symmetrical = 54
    Croatian_20_Symmetrical = 56
    Croatian_26_Symmetrical = 58

    def __new__( cls, value ):
        if BoardType._is_valid( value ):
            return super( BoardType, cls ).__new__( cls, value )
        else:
            raise ValueError( "No such a board type, received '%s'." % ( str( value ), ) )

    @staticmethod
    def iter( include_none=False,
              include_even=True,
              include_simple=True,
              include_mirrored=True,
              include_symmetrical=True,
              do_construct=True ):
        l_even =  [ BoardType.Classical,
                    BoardType.CroatianTies,
                    BoardType.MayanAscendancy,
                    BoardType.AgeOfAquarius,
                    BoardType.MirandasVeil,
                    BoardType.Nineteen,
                    BoardType.HemerasDawn,
                    BoardType.TamoanchanRevisited,
                    BoardType.ConquestOfTlalocan,
                    BoardType.Discovery,
                    BoardType.One, ]

        lst = [ ]

        if include_even:
            lst.extend( l_even )

        if include_none:
            lst.insert( 0, BoardType.none )

        if include_simple:
            lst.extend( [ BoardType.Classic_14,
                          BoardType.Classic_20,
                          BoardType.Classic_26,
                          BoardType.Croatian_14,
                          BoardType.Croatian_20,
                          BoardType.Croatian_26, ] )

        if include_mirrored:
            lst.extend( [ BoardType.Classic_14_Mirrored,
                          BoardType.Classic_20_Mirrored,
                          BoardType.Classic_26_Mirrored,
                          BoardType.Croatian_14_Mirrored,
                          BoardType.Croatian_20_Mirrored,
                          BoardType.Croatian_26_Mirrored, ] )

        if include_symmetrical:
            lst.extend( [ BoardType.Classic_14_Symmetrical,
                          BoardType.Classic_20_Symmetrical,
                          BoardType.Classic_26_Symmetrical,
                          BoardType.Croatian_14_Symmetrical,
                          BoardType.Croatian_20_Symmetrical,
                          BoardType.Croatian_26_Symmetrical, ] )

        lst.sort()
        return [ BoardType( bt ) if do_construct else bt for bt in lst ]

    @staticmethod
    def _is_valid( board_type ):
        return board_type in BoardType.iter( include_none=True,
                                             include_even=True,
                                             include_simple=True,
                                             include_mirrored=True,
                                             include_symmetrical=True,
                                             do_construct=False )

    def is_variant( self, bt ):
        return self == BoardType( bt )

    def in_variants( self, bts ):
        bts = [ BoardType( bt ) for bt in bts ]
        return self in bts

    @staticmethod
    def is_simple( board_type,
                   include_old=True,
                   include_new=True,
                   include_mirrored=True,
                   include_symmetrical=True ):
        if include_old:
            if board_type == BoardType.Classical:
                return True

        if include_new:
            if board_type in [ BoardType.Classic_14,
                               BoardType.Classic_20,
                               BoardType.Classic_26,
                               BoardType.Croatian_14,
                               BoardType.Croatian_20,
                               BoardType.Croatian_26, ]:
                return True

        if include_mirrored:
            if board_type in [ BoardType.Classic_14_Mirrored,
                               BoardType.Classic_20_Mirrored,
                               BoardType.Classic_26_Mirrored,
                               BoardType.Croatian_14_Mirrored,
                               BoardType.Croatian_20_Mirrored,
                               BoardType.Croatian_26_Mirrored, ]:
                return True

        if include_symmetrical:
            if board_type in [ BoardType.Classic_14_Symmetrical,
                               BoardType.Classic_20_Symmetrical,
                               BoardType.Classic_26_Symmetrical,
                               BoardType.Croatian_14_Symmetrical,
                               BoardType.Croatian_20_Symmetrical,
                               BoardType.Croatian_26_Symmetrical, ]:
                return True

        return False

    def get_name( self ):
        return { BoardType.none: 'none',
                 BoardType.Classical: 'Classical',
                 BoardType.CroatianTies: 'Croatian Ties',
                 BoardType.MayanAscendancy: 'Mayan Ascendancy',
                 BoardType.AgeOfAquarius: 'Age Of Aquarius',
                 BoardType.MirandasVeil: 'Miranda\'s Veil',
                 BoardType.Nineteen: 'Nineteen',
                 BoardType.HemerasDawn: 'Hemera\'s Dawn',
                 BoardType.TamoanchanRevisited: 'Tamoanchan Revisited',
                 BoardType.ConquestOfTlalocan: 'Conquest Of Tlalocan',
                 BoardType.Discovery: 'Discovery',
                 BoardType.One: 'One',

                 BoardType.Classic_14: 'Classical 14',
                 BoardType.Classic_20: 'Classical 20',
                 BoardType.Classic_26: 'Classical 26',
                 BoardType.Croatian_14: 'Croatian Ties 14',
                 BoardType.Croatian_20: 'Croatian Ties 20',
                 BoardType.Croatian_26: 'Croatian Ties 26',

                 BoardType.Classic_14_Mirrored: 'Classical 14 Mirrored',
                 BoardType.Classic_20_Mirrored: 'Classical 20 Mirrored',
                 BoardType.Classic_26_Mirrored: 'Classical 26 Mirrored',
                 BoardType.Croatian_14_Mirrored: 'Croatian Ties 14 Mirrored',
                 BoardType.Croatian_20_Mirrored: 'Croatian Ties 20 Mirrored',
                 BoardType.Croatian_26_Mirrored: 'Croatian Ties 26 Mirrored',

                 BoardType.Classic_14_Symmetrical: 'Classical 14 Symmetrical',
                 BoardType.Classic_20_Symmetrical: 'Classical 20 Symmetrical',
                 BoardType.Classic_26_Symmetrical: 'Classical 26 Symmetrical',
                 BoardType.Croatian_14_Symmetrical: 'Croatian Ties 14 Symmetrical',
                 BoardType.Croatian_20_Symmetrical: 'Croatian Ties 20 Symmetrical',
                 BoardType.Croatian_26_Symmetrical: 'Croatian Ties 26 Symmetrical', }[ self ]

    def get_symbol( self ):
        return { BoardType.none: '',
                 BoardType.Classical: 'C',
                 BoardType.CroatianTies: 'CT',
                 BoardType.MayanAscendancy: 'MA',
                 BoardType.AgeOfAquarius: 'AOA',
                 BoardType.MirandasVeil: 'MV',
                 BoardType.Nineteen: 'N',
                 BoardType.HemerasDawn: 'HD',
                 BoardType.TamoanchanRevisited: 'TR',
                 BoardType.ConquestOfTlalocan: 'COT',
                 BoardType.Discovery: 'D',
                 BoardType.One: 'O',

                 BoardType.Classic_14: 'C14',
                 BoardType.Classic_20: 'C20',
                 BoardType.Classic_26: 'C26',
                 BoardType.Croatian_14: 'CT14',
                 BoardType.Croatian_20: 'CT20',
                 BoardType.Croatian_26: 'CT26',

                 BoardType.Classic_14_Mirrored: 'C14M',
                 BoardType.Classic_20_Mirrored: 'C20M',
                 BoardType.Classic_26_Mirrored: 'C26M',
                 BoardType.Croatian_14_Mirrored: 'CT14M',
                 BoardType.Croatian_20_Mirrored: 'CT20M',
                 BoardType.Croatian_26_Mirrored: 'CT26M',

                 BoardType.Classic_14_Symmetrical: 'C14S',
                 BoardType.Classic_20_Symmetrical: 'C20S',
                 BoardType.Classic_26_Symmetrical: 'C26S',
                 BoardType.Croatian_14_Symmetrical: 'CT14S',
                 BoardType.Croatian_20_Symmetrical: 'CT20S',
                 BoardType.Croatian_26_Symmetrical: 'CT26S', }[ self ]

    def get_label( self ):
        return self.get_symbol().lower()

    @staticmethod
    def get( label, case_insensitive=True ):
        dct  = { # '':     BoardType.none,
                 'C':    BoardType.Classical,
                 'CT':   BoardType.CroatianTies,
                 'MA':   BoardType.MayanAscendancy,
                 'AOA':  BoardType.AgeOfAquarius,
                 'MV':   BoardType.MirandasVeil,
                 'N':    BoardType.Nineteen,
                 'HD':   BoardType.HemerasDawn,
                 'TR':   BoardType.TamoanchanRevisited,
                 'COT':  BoardType.ConquestOfTlalocan,
                 'D':    BoardType.Discovery,
                 'O':    BoardType.One,

                 'C14':  BoardType.Classic_14,
                 'C20':  BoardType.Classic_20,
                 'C26':  BoardType.Classic_26,
                 'CT14': BoardType.Croatian_14,
                 'CT20': BoardType.Croatian_20,
                 'CT26': BoardType.Croatian_26,

                 'C14M':  BoardType.Classic_14_Mirrored,
                 'C20M':  BoardType.Classic_20_Mirrored,
                 'C26M':  BoardType.Classic_26_Mirrored,
                 'CT14M': BoardType.Croatian_14_Mirrored,
                 'CT20M': BoardType.Croatian_20_Mirrored,
                 'CT26M': BoardType.Croatian_26_Mirrored,

                 'C14S':  BoardType.Classic_14_Symmetrical,
                 'C20S':  BoardType.Classic_20_Symmetrical,
                 'C26S':  BoardType.Classic_26_Symmetrical,
                 'CT14S': BoardType.Croatian_14_Symmetrical,
                 'CT20S': BoardType.Croatian_20_Symmetrical,
                 'CT26S': BoardType.Croatian_26_Symmetrical, }

        lbl = label.upper() if case_insensitive else label

        if lbl in dct:
            return BoardType( dct[ lbl ] )
        else:
            return BoardType( BoardType.none )

    @staticmethod
    def get_list( do_construct=True ):
        return list( BoardType.iter( include_none=False,
                                     include_even=True,
                                     include_simple=True,
                                     include_mirrored=True,
                                     include_symmetrical=True,
                                     do_construct=do_construct ) )

    @staticmethod
    def get_all_list( do_construct=True, include_none=False ):
        return list( BoardType.iter( include_none=include_none,
                                     include_even=True,
                                     include_simple=True,
                                     include_mirrored=True,
                                     include_symmetrical=True,
                                     do_construct=do_construct ) )

    def get_size( self ):
        return { BoardType.none: 0,
                 BoardType.Classical: 8,
                 BoardType.CroatianTies: 10,
                 BoardType.MayanAscendancy: 12,
                 BoardType.AgeOfAquarius: 14,
                 BoardType.MirandasVeil: 16,
                 BoardType.Nineteen: 18,
                 BoardType.HemerasDawn: 20,
                 BoardType.TamoanchanRevisited: 22,
                 BoardType.ConquestOfTlalocan: 24,
                 BoardType.Discovery: 24,
                 BoardType.One: 26,

                 BoardType.Classic_14: 14,
                 BoardType.Classic_20: 20,
                 BoardType.Classic_26: 26,
                 BoardType.Croatian_14: 14,
                 BoardType.Croatian_20: 20,
                 BoardType.Croatian_26: 26,

                 BoardType.Classic_14_Mirrored: 14,
                 BoardType.Classic_20_Mirrored: 20,
                 BoardType.Classic_26_Mirrored: 26,
                 BoardType.Croatian_14_Mirrored: 14,
                 BoardType.Croatian_20_Mirrored: 20,
                 BoardType.Croatian_26_Mirrored: 26,

                 BoardType.Classic_14_Symmetrical: 14,
                 BoardType.Classic_20_Symmetrical: 20,
                 BoardType.Classic_26_Symmetrical: 26,
                 BoardType.Croatian_14_Symmetrical: 14,
                 BoardType.Croatian_20_Symmetrical: 20,
                 BoardType.Croatian_26_Symmetrical: 26, }[ self ]

    def get_newly_introduced_pieces( self, include_classical=False ):
        pts = { BoardType.none: None,
                BoardType.Classical: \
                    [ PT.Pawn, PT.Bishop, PT.Knight, PT.Rook, PT.Queen, PT.King ] if include_classical else \
                    None,
                BoardType.CroatianTies: [ PT.Pegasus, ],
                BoardType.MayanAscendancy: [ PT.Pyramid, ],
                BoardType.AgeOfAquarius: [ PT.Unicorn, ],
                BoardType.MirandasVeil: [ PT.Wave, ],
                BoardType.Nineteen: [ PT.Star, ],
                BoardType.HemerasDawn: [ PT.Centaur, PT.Scout, PT.Grenadier ],
                BoardType.TamoanchanRevisited: [ PT.Serpent, ],
                BoardType.ConquestOfTlalocan: [ PT.Shaman, ],
                BoardType.Discovery: [ PT.Monolith, ],
                BoardType.One: [ PT.Starchild, ],

                BoardType.Classic_14: None,
                BoardType.Classic_20: None,
                BoardType.Classic_26: None,
                BoardType.Croatian_14: None,
                BoardType.Croatian_20: None,
                BoardType.Croatian_26: None,

                BoardType.Classic_14_Mirrored: None,
                BoardType.Classic_20_Mirrored: None,
                BoardType.Classic_26_Mirrored: None,
                BoardType.Croatian_14_Mirrored: None,
                BoardType.Croatian_20_Mirrored: None,
                BoardType.Croatian_26_Mirrored: None,

                BoardType.Classic_14_Symmetrical: None,
                BoardType.Classic_20_Symmetrical: None,
                BoardType.Classic_26_Symmetrical: None,
                BoardType.Croatian_14_Symmetrical: None,
                BoardType.Croatian_20_Symmetrical: None,
                BoardType.Croatian_26_Symmetrical: None, }[ self ]
        return  [ PT(pt) for pt in iterate( pts ) ] if pts is not None else \
                None

    def get_newly_introducing_board_types( self, piece_type ):
        pt = PT( piece_type )
        return { PT.none: None,
                 PT.Pawn: None,
                 PT.Bishop: None,
                 PT.Knight: None,
                 PT.Rook: None,
                 PT.Queen: None,
                 PT.King: None,
                 PT.Pegasus: [ BoardType.CroatianTies, ],
                 PT.Pyramid: [ BoardType.MayanAscendancy, ],
                 PT.Unicorn: [ BoardType.AgeOfAquarius, ],
                 PT.Wave: [ BoardType.MirandasVeil, ],
                 PT.Star: [ BoardType.Nineteen, ],
                 PT.Centaur: [ BoardType.HemerasDawn, ],
                 PT.Scout: [ BoardType.HemerasDawn, ],
                 PT.Grenadier: [ BoardType.HemerasDawn, ],
                 PT.Serpent: [ BoardType.TamoanchanRevisited, ],
                 PT.Shaman: [ BoardType.ConquestOfTlalocan, ],
                 PT.Monolith: [ BoardType.Discovery, ],
                 PT.Starchild: [ BoardType.One, ], }[ pt.get_enumerated() ]

    def get_all_that_contain( self, piece_type ):
        start = self.get_newly_introducing_board_types( piece_type )
        start = start[ 0 ] if start is not None else BoardType.Classical
        return [ BoardType( bt ) for bt in range( start, BoardType.One+1 ) ]

    def does_contain( self, piece_type ):
        start = PT.Pawn
        end = PT.King

        pts = self.get_newly_introduced_pieces( include_classical=False )
        for pt in iterate( pts ):
            end = pt if pt > end else end

        return piece_type in range( start, end+1 )

    def get_position_limits( self ):
        limit = self.get_size() - 1
        return ((0, 0), (limit, limit))


def get_opposites( pieces ):
    return [ PT(p).get_opposite() for p in pieces ]

def remove_pieces( pieces, to_remove=(PT.Queen, -PT.Queen) ):
    return [ PT(p) for p in pieces if p not in to_remove ]

def filter_setup( pieces, to_remain=(PT.King, PT.Rook, PT.Star, -PT.Star) ):
    return [ PT(p) if p in to_remain else PT(PT.none) for p in pieces ]

def get_indexes( pieces, piece=PT.King ):
    if piece not in pieces:
        return None

    return [ i for i, p in enumerate( pieces ) if p == piece ]


class Board:
    def __init__( self, board_type ):
        self.type = BoardType( board_type )

        self._board = [ [ PT( PT.none ) for i in range( self.get_width() ) ] for j in range( self.get_height() ) ]

    def _is_file( self, i ):
        _i = assert_floor( i )
        return 0 <= _i < self.get_width()

    def _is_rank( self, j ):
        _j = assert_floor( j )
        return 0 <= _j < self.get_height()

    def is_on_board( self, i, j ):
        return self._is_file( i ) and self._is_rank( j )

    def is_light( self, i, j ):
        _i, _j = assert_floor_2( i, j )

        b = self.get_width() % 2

        if ( b == 0 ):
            is_light = bool( (_i + _j) % 2 != 0 )
        else:
            is_light = bool( (_i + _j) % 2 == 0 )

        return is_light

    def is_dark( self, i, j ):
        return not self.is_light( i, j )

    def is_simple( self,
                  include_old=True,
                  include_new=True,
                  include_mirrored=True,
                  include_symmetrical=True ):
        return BoardType.is_simple( self.type,
                                    include_old=include_old,
                                    include_new=include_new,
                                    include_mirrored=include_mirrored,
                                    include_symmetrical=include_symmetrical )

    def clear( self ):
        for j in range( self.get_height() ):
            for i in range( self.get_width() ):
                self.set_piece( i, j, PT( PT.none ) )

    def get_width( self ):
        return self.type.get_size()

    def get_height( self ):
        return self.type.get_size()

    def get_piece( self, i, j ):
        _i, _j = assert_floor_2( i, j )
        return self._board[ _j ][ _i ] if self.is_on_board( _i, _j ) \
               else PT.none # TODO :: having pieces outside of a board ?!?!?!

    def set_piece( self, i, j, piece ):
        _i, _j = assert_floor_2( i, j )
        self._board[ _j ][ _i ] = PT( piece )

    def set_piece_safe( self, i, j, piece ):
        if self.is_on_board( i, j ):
            self.set_piece( i, j, piece )

    def set_pieces( self, lst ):
        # lst :: [ ( i, j, piece ), ... ]
        for tpl in lst:
            self.set_piece( *tpl )

    def set_row( self, j, pieces ):
        _j = assert_floor( j )

        for i, p in enumerate( pieces ):
            self.set_piece( i, j, p )

    def get_position_limits( self ):
        h = self.get_height() - 1
        w = self.get_width() - 1
        return ( (0, 0), (w, h) )

    @staticmethod
    def get_castling_files( board_type ):
        bt = BoardType( board_type )

        if bt == BoardType.none:
            return (0, [], [])

        light_pieces = Board.get_light_row( bt )

        pos_king = just_count( get_indexes( light_pieces, piece=PT.King ) )
        pos_rook = get_indexes( light_pieces, piece=PT.Rook )
        pos_rook_l = [ pr for pr in pos_rook if pr < pos_king ]
        pos_rook_r = [ pr for pr in pos_rook if pr > pos_king ]

        return (pos_king, pos_rook_l, pos_rook_r)

    @staticmethod
    def get_castling_limits( board_type, file_rook_init ):
        bt = BoardType( board_type )
        assert isinstance( file_rook_init, int )

        if bt == BoardType.none:
            return (0, 0)

        file_king, files_rooks_l, files_rooks_r = Board.get_castling_files( bt )
        assert ( file_rook_init in files_rooks_l ) or ( file_rook_init in files_rooks_r )

        file_rook_l = max( [ fr for fr in files_rooks_l if fr <= file_rook_init ] ) \
                      if file_rook_init < file_king \
                      else files_rooks_l[ 0 ]
        file_rook_l += 1

        file_rook_r = min( [ fr for fr in files_rooks_r if file_rook_init <= fr ] ) \
                      if file_king < file_rook_init \
                      else files_rooks_r[ -1 ]

        diff_l, diff_r = abs( file_king - file_rook_l ), abs( file_rook_r - file_king )
        diff = min( diff_l, diff_r ) - 1

        return (2, diff)

    # -----------------------------------------------------------------
    # Defining initial position

    @staticmethod
    def _get_none_row( board_type=BoardType.none ):
        return []

    @staticmethod
    def _get_classic_row( board_type=BoardType.Classical ):
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
        return lst

    @staticmethod
    def _get_croatian_ties_row( board_type=BoardType.CroatianTies ):
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
        return lst

    @staticmethod
    def _get_mayan_ascendancy_row( board_type=BoardType.MayanAscendancy ):
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
        return lst

    @staticmethod
    def _get_age_of_aquarius_row( board_type=BoardType.AgeOfAquarius ):
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
        return lst

    @staticmethod
    def _get_mirandas_veil_row( board_type=BoardType.MirandasVeil ):
        lst =   [
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
                    PT.Rook
                ]
        return lst

    @staticmethod
    def _get_nineteen_row( board_type=BoardType.Nineteen ):
        lst =   [
                    PT.Star, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Wave, \
                    PT.Pegasus, \
                    PT.Unicorn, \
                    PT.Pyramid, \
                    PT.Queen, \
                    PT.King, \
                    PT.Pyramid, \
                    PT.Unicorn, \
                    PT.Pegasus, \
                    PT.Wave, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    -PT.Star
                ]
        return lst

    @staticmethod
    def _get_hemeras_dawn_row( board_type=BoardType.HemerasDawn ):
        lst =   [
                    PT.Star, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Centaur, \
                    PT.Wave, \
                    PT.Pegasus, \
                    PT.Unicorn, \
                    PT.Pyramid, \
                    PT.Queen, \
                    PT.King, \
                    PT.Pyramid, \
                    PT.Unicorn, \
                    PT.Pegasus, \
                    PT.Wave, \
                    PT.Centaur, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    -PT.Star
                ]
        return lst

    @staticmethod
    def _get_tamoanchan_revisited_row( board_type=BoardType.TamoanchanRevisited ):
        lst =   [
                    PT.Star, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Serpent, \
                    PT.Wave, \
                    PT.Unicorn, \
                    PT.Pegasus, \
                    PT.Centaur, \
                    PT.Pyramid, \
                    PT.Queen, \
                    PT.King, \
                    PT.Pyramid, \
                    PT.Centaur, \
                    PT.Pegasus, \
                    PT.Unicorn, \
                    PT.Wave, \
                    PT.Serpent, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    -PT.Star
                ]
        return lst

    @staticmethod
    def _get_conquest_of_tlalocan_row( board_type=BoardType.ConquestOfTlalocan ):
        lst =   [
                    PT.Star, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Serpent, \
                    PT.Centaur, \
                    PT.Unicorn, \
                    PT.Wave, \
                    PT.Pegasus, \
                    PT.Pyramid, \
                    PT.Shaman, \
                    PT.Queen, \
                    PT.King, \
                    PT.Shaman, \
                    PT.Pyramid, \
                    PT.Pegasus, \
                    PT.Wave, \
                    PT.Unicorn, \
                    PT.Centaur, \
                    PT.Serpent, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    -PT.Star
                ]
        return lst

    @staticmethod
    def _get_discovery_row( board_type=BoardType.Discovery ):
        lst =   [
                    PT.Star, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Serpent, \
                    PT.Centaur, \
                    PT.Unicorn, \
                    PT.Wave, \
                    PT.Pegasus, \
                    PT.Pyramid, \
                    PT.Shaman, \
                    PT.Queen, \
                    PT.King, \
                    PT.Shaman, \
                    PT.Pyramid, \
                    PT.Pegasus, \
                    PT.Wave, \
                    PT.Unicorn, \
                    PT.Centaur, \
                    PT.Serpent, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    -PT.Star
                ]
        return lst

    @staticmethod
    def _get_one_row( board_type=BoardType.One ):
        lst =   [
                    PT.Star, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Serpent, \
                    PT.Starchild, \
                    PT.Centaur, \
                    PT.Unicorn, \
                    PT.Pegasus, \
                    PT.Wave, \
                    PT.Pyramid, \
                    PT.Shaman, \
                    PT.Queen, \
                    PT.King, \
                    PT.Shaman, \
                    PT.Pyramid, \
                    PT.Wave, \
                    PT.Pegasus, \
                    PT.Unicorn, \
                    PT.Centaur, \
                    PT.Starchild, \
                    PT.Serpent, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    -PT.Star
                ]
        return lst

    @staticmethod
    def _get_classic_14_row( board_type=BoardType.Classic_14 ):
        lst =   [ \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Queen, \
                    PT.King, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                ]
        return lst

    @staticmethod
    def _get_classic_20_row( board_type=BoardType.Classic_20 ):
        lst =   [ \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Queen, \
                    PT.King, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                ]
        return lst

    @staticmethod
    def _get_classic_26_row( board_type=BoardType.Classic_26 ):
        lst =   [ \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Knight, \
                    PT.Bishop, \
                    PT.Queen, \
                    PT.King, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Knight, \
                    PT.Rook, \
                ]
        return lst

    @staticmethod
    def _get_croatian_14_row( board_type=BoardType.Croatian_14 ):
        lst =   [ \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Queen, \
                    PT.King, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                ]
        return lst

    @staticmethod
    def _get_croatian_20_row( board_type=BoardType.Croatian_20 ):
        lst =   [ \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Queen, \
                    PT.King, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                ]
        return lst

    @staticmethod
    def _get_croatian_26_row( board_type=BoardType.Croatian_26 ):
        lst =   [ \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Rook, \
                    PT.Pegasus, \
                    PT.Bishop, \
                    PT.Queen, \
                    PT.King, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                    PT.Bishop, \
                    PT.Pegasus, \
                    PT.Rook, \
                ]
        return lst

    @staticmethod
    def _get_random_row( board_type ):
        bt = BoardType( board_type )
        if not bt.in_variants( BoardType.iter( include_none=False,
                                               include_even=False,
                                               include_simple=False,
                                               include_mirrored=True,
                                               include_symmetrical=True ) ):
            return None

        size = bt.get_size()
        half = size // 2
        ixs = [ i for i in range( 0, size ) ]
        lst = [ PT.none for i in range( 0, size ) ]

        count = int( ( size - 2 ) // 3 ) # Number of pieces, e.g. Rooks
        # - 2 == King + Queen
        #\ // 3 == Bishop + Knight + Rook #\ Because without it VS Code renders // non-readable (striked- and blacked-out --> C code renderer?).

        # Bishops on light fields
        bls = [ 2 * i + 1 for i in range( 0, size // 2 ) ]
        for b in range( count // 2 ): #\ // 2 == Bishops, but only on light fields
            i = random.choice( bls )
            lst[ i ] = PT.Bishop
            # print( "Removing: '%d'." % i )
            bls.remove( i )
            ixs.remove( i )

        # Bishops on dark fields
        bds = [ 2 * i for i in range( 0, size // 2 ) ]
        for b in range( count // 2 ): #\ // 2 == Bishops, but only on dark fields
            i = random.choice( bds )
            lst[ i ] = PT.Bishop
            # print( "Removing: '%d'." % i )
            bds.remove( i )
            ixs.remove( i )

        jumper = PT.Pegasus if bt in [ BoardType.Croatian_14_Mirrored,
                                       BoardType.Croatian_20_Mirrored,
                                       BoardType.Croatian_26_Mirrored,
                                       BoardType.Croatian_14_Symmetrical,
                                       BoardType.Croatian_20_Symmetrical,
                                       BoardType.Croatian_26_Symmetrical, ] else \
                 PT.Knight

        # Rooks, Knights or Pegasuses
        for p in [ PT.Rook, jumper ]:
            for c in range( count ):
                i = random.choice( ixs )
                lst[ i ] = p
                # print( "Removing: '%d'." % i )
                ixs.remove( i )

        # Queen
        i = random.choice( ixs )
        lst[ i ] = PT.Queen
        # print( "Removing: '%d'." % i )
        ixs.remove( i )

        # King
        i = ixs[ 0 ]
        lst[ i ] = PT.King

        return lst

    @staticmethod
    def _get_classic_14_random_row( board_type=BoardType.Classic_14_Mirrored ):
        lst = Board._get_random_row( board_type )
        return lst

    @staticmethod
    def _get_classic_20_random_row( board_type=BoardType.Classic_20_Mirrored ):
        lst = Board._get_random_row( board_type )
        return lst

    @staticmethod
    def _get_classic_26_random_row( board_type=BoardType.Classic_26_Mirrored ):
        lst = Board._get_random_row( board_type )
        return lst

    @staticmethod
    def _get_croatian_14_random_row( board_type=BoardType.Croatian_14_Mirrored ):
        lst = Board._get_random_row( board_type )
        return lst

    @staticmethod
    def _get_croatian_20_random_row( board_type=BoardType.Croatian_20_Mirrored ):
        lst = Board._get_random_row( board_type )
        return lst

    @staticmethod
    def _get_croatian_26_random_row( board_type=BoardType.Croatian_26_Mirrored ):
        lst = Board._get_random_row( board_type )
        return lst

    @staticmethod
    def get_light_row( board_type ):
        bt = BoardType( board_type )

        f = { BoardType.none: Board._get_none_row,
              BoardType.Classical: Board._get_classic_row,
              BoardType.CroatianTies: Board._get_croatian_ties_row,
              BoardType.MayanAscendancy: Board._get_mayan_ascendancy_row,
              BoardType.AgeOfAquarius: Board._get_age_of_aquarius_row,
              BoardType.MirandasVeil: Board._get_mirandas_veil_row,
              BoardType.Nineteen: Board._get_nineteen_row,
              BoardType.HemerasDawn: Board._get_hemeras_dawn_row,
              BoardType.TamoanchanRevisited: Board._get_tamoanchan_revisited_row,
              BoardType.ConquestOfTlalocan: Board._get_conquest_of_tlalocan_row,
              BoardType.Discovery: Board._get_discovery_row,
              BoardType.One: Board._get_one_row,

              BoardType.Classic_14: Board._get_classic_14_row,
              BoardType.Classic_20: Board._get_classic_20_row,
              BoardType.Classic_26: Board._get_classic_26_row,
              BoardType.Croatian_14: Board._get_croatian_14_row,
              BoardType.Croatian_20: Board._get_croatian_20_row,
              BoardType.Croatian_26: Board._get_croatian_26_row,

              BoardType.Classic_14_Mirrored: Board._get_classic_14_random_row,
              BoardType.Classic_20_Mirrored: Board._get_classic_20_random_row,
              BoardType.Classic_26_Mirrored: Board._get_classic_26_random_row,
              BoardType.Croatian_14_Mirrored: Board._get_croatian_14_random_row,
              BoardType.Croatian_20_Mirrored: Board._get_croatian_20_random_row,
              BoardType.Croatian_26_Mirrored: Board._get_croatian_26_random_row,

              BoardType.Classic_14_Symmetrical: Board._get_classic_14_random_row,
              BoardType.Classic_20_Symmetrical: Board._get_classic_20_random_row,
              BoardType.Classic_26_Symmetrical: Board._get_classic_26_random_row,
              BoardType.Croatian_14_Symmetrical: Board._get_croatian_14_random_row,
              BoardType.Croatian_20_Symmetrical: Board._get_croatian_20_random_row,
              BoardType.Croatian_26_Symmetrical: Board._get_croatian_26_random_row, }[ bt ]

        light_pieces = f( bt )
        return light_pieces

    # -----------------------------------------------------------------
    # Setting up initial positions

    def _setup_pawn_rows( self, is_light ):
        assert isinstance( is_light, bool )

        pt = PT.Pawn if is_light else -PT.Pawn
        # gt = PT.Grenadier if is_light else -PT.Grenadier

        plst = [ pt for i in range( self.get_width() ) ]
        # glst = [ gt for i in range( self.get_width() ) ]

        # lst = glst if self.type >= BoardType.OddNineteen else plst
        row = 1 if is_light else self.get_height() - 2
        self.set_row( row, plst ) # lst)

        if self.is_simple( include_old=False, include_new=True, include_mirrored=True, include_symmetrical=True ):
            return
        elif self.type > BoardType.MirandasVeil:
            row_2 = 2 if is_light else self.get_height() - 3
            self.set_row( row_2, plst )

    def _setup_pawns( self ):
        self._setup_pawn_rows( True )
        self._setup_pawn_rows( False )

    def _setup_scouts( self, is_light ):
        assert isinstance( is_light, bool )

        if self.is_simple( include_old=False, include_new=True, include_mirrored=True, include_symmetrical=True ):
            return
        elif self.type > BoardType.Nineteen:
            # pt = PT.Pawn if is_light else -PT.Pawn
            st = PT.Scout if is_light else -PT.Scout
            figure_row = 0 if is_light else self.get_height() - 1
            figures = [ PT.Centaur, PT.Shaman ] if is_light else [ -PT.Centaur, -PT.Shaman ]
            row_3 = 3 if is_light else self.get_height() - 4
            row_4 = 4 if is_light else self.get_height() - 5
            # half = self.get_width() // 2

            for i in range( self.get_width() ):
                p = self.get_piece( i, figure_row )
                if p in figures:
                    # self.set_pieces( [ (i-2, row_3, pt), (i+2, row_3, pt), (i-1, row_4, pt), (i+1, row_4, pt) ] )
                    self.set_pieces( [ (i-2, row_3, st), (i+2, row_3, st), (i-1, row_4, st), (i+1, row_4, st) ] )

    def _setup_grenadiers( self, is_light ):
        assert isinstance( is_light, bool )

        if self.is_simple( include_old=False, include_new=True, include_mirrored=True, include_symmetrical=True ):
            return
        elif self.type > BoardType.Nineteen:
            pt = PT.Grenadier if is_light else -PT.Grenadier
            figure_row = 0 if is_light else self.get_height() - 1
            figures = [ PT.Centaur, PT.Shaman ] if is_light else [ -PT.Centaur, -PT.Shaman ]
            row_2 = 2 if is_light else self.get_height() - 3
            row_1 = 1 if is_light else self.get_height() - 2
            # half = self.get_width() // 2

            for i in range( self.get_width() ):
                p = self.get_piece( i, figure_row )
                if p in figures:
                    # self.set_pieces( [ (i-2, row_3, pt), (i+2, row_3, pt), (i-1, row_4, pt), (i+1, row_4, pt) ] )
                    self.set_pieces( [ (i-2, row_2, pt), (i+2, row_2, pt), (i-1, row_1, pt), (i+1, row_1, pt) ] )

    def _setup_all_scouts( self ):
        self._setup_scouts( True )
        self._setup_scouts( False )

    def _setup_all_grenadiers( self ):
        self._setup_grenadiers( True )
        self._setup_grenadiers( False )

    def _setup_board( self, light_pieces ):
        self.clear()
        self._setup_pawns()

        self.set_row( 0, light_pieces )

        dark = get_opposites( light_pieces )
        if self.type in [ BoardType.Classic_14_Symmetrical,
                          BoardType.Classic_20_Symmetrical,
                          BoardType.Classic_26_Symmetrical,
                          BoardType.Croatian_14_Symmetrical,
                          BoardType.Croatian_20_Symmetrical,
                          BoardType.Croatian_26_Symmetrical, ]:
            dark.reverse()
        self.set_row( self.get_height() - 1, dark )

        # Keep order! # --> Scouts --> Grenadiers
        self._setup_all_scouts() # This *must* be 2nd to last item!
        self._setup_all_grenadiers() # This *must* be last item!

    def _setup_none( self ):
        pass

    def _setup_classic( self ):
        light = Board._get_classic_row()
        self._setup_board( light )

    def _setup_croatian_ties( self ):
        light = Board._get_croatian_ties_row()
        self._setup_board( light )

    def _setup_mayan_ascendancy( self ):
        light = Board._get_mayan_ascendancy_row()
        self._setup_board( light )

    def _setup_age_of_aquarius( self ):
        light = Board._get_age_of_aquarius_row()
        self._setup_board( light )

    def _setup_mirandas_veil( self ):
        light = Board._get_mirandas_veil_row()
        self._setup_board( light )

    def _setup_nineteen( self ):
        light = Board._get_nineteen_row()
        self._setup_board( light )

    def _setup_hemeras_dawn( self ):
        light = Board._get_hemeras_dawn_row()
        self._setup_board( light )

    def _setup_tamoanchan_revisited( self ):
        light = Board._get_tamoanchan_revisited_row()
        self._setup_board( light )

    def _setup_conquest_of_tlalocan( self ):
        light = Board._get_conquest_of_tlalocan_row()
        self._setup_board( light )


    def _calc_monolith_init_pos( self, pt ):
        pt = PT( pt )

        w = self.get_width()
        dx = int( math.floor( w / 11.0 ) )

        h = self.get_height()
        dy = int( math.floor( 7.0 * h / 22.0 ) )

        if pt.is_light():
            return (dx - 1, dy - 1)
        else:
            return (w - dx, h - dy)

    def _setup_monolith( self, pt ):
        pt = PT( pt )
        i, j = self._calc_monolith_init_pos( pt )
        self.set_piece( i, j, pt )

    def _setup_discovery( self ):
        light = Board._get_discovery_row()
        self._setup_board( light )

        self._setup_monolith( PT.Monolith )
        self._setup_monolith( -PT.Monolith )

    def _setup_one( self ):
        light = Board._get_one_row()
        self._setup_board( light )

        self._setup_monolith( PT.Monolith )
        self._setup_monolith( -PT.Monolith )

    def _setup_classic_14( self ):
        light = Board._get_classic_14_row()
        self._setup_board( light )

    def _setup_classic_20( self ):
        light = Board._get_classic_20_row()
        self._setup_board( light )

    def _setup_classic_26( self ):
        light = Board._get_classic_26_row()
        self._setup_board( light )

    def _setup_croatian_14( self ):
        light = Board._get_croatian_14_row()
        self._setup_board( light )

    def _setup_croatian_20( self ):
        light = Board._get_croatian_20_row()
        self._setup_board( light )

    def _setup_croatian_26( self ):
        light = Board._get_croatian_26_row()
        self._setup_board( light )

    def _setup_classic_14_random( self ):
        light = Board._get_classic_14_random_row()
        self._setup_board( light )

    def _setup_classic_20_random( self ):
        light = Board._get_classic_20_random_row()
        self._setup_board( light )

    def _setup_classic_26_random( self ):
        light = Board._get_classic_26_random_row()
        self._setup_board( light )

    def _setup_croatian_14_random( self ):
        light = Board._get_croatian_14_random_row()
        self._setup_board( light )

    def _setup_croatian_20_random( self ):
        light = Board._get_croatian_20_random_row()
        self._setup_board( light )

    def _setup_croatian_26_random( self ):
        light = Board._get_croatian_26_random_row()
        self._setup_board( light )

    def setup( self ):
        f = { BoardType.none: self._setup_none,
              BoardType.Classical: self._setup_classic,
              BoardType.CroatianTies: self._setup_croatian_ties,
              BoardType.MayanAscendancy: self._setup_mayan_ascendancy,
              BoardType.AgeOfAquarius: self._setup_age_of_aquarius,
              BoardType.MirandasVeil: self._setup_mirandas_veil,
              BoardType.Nineteen: self._setup_nineteen,
              BoardType.HemerasDawn: self._setup_hemeras_dawn,
              BoardType.TamoanchanRevisited: self._setup_tamoanchan_revisited,
              BoardType.ConquestOfTlalocan: self._setup_conquest_of_tlalocan,
              BoardType.Discovery: self._setup_discovery,
              BoardType.One: self._setup_one,

              BoardType.Classic_14: self._setup_classic_14,
              BoardType.Classic_20: self._setup_classic_20,
              BoardType.Classic_26: self._setup_classic_26,
              BoardType.Croatian_14: self._setup_croatian_14,
              BoardType.Croatian_20: self._setup_croatian_20,
              BoardType.Croatian_26: self._setup_croatian_26,

              BoardType.Classic_14_Mirrored: self._setup_classic_14_random,
              BoardType.Classic_20_Mirrored: self._setup_classic_20_random,
              BoardType.Classic_26_Mirrored: self._setup_classic_26_random,
              BoardType.Croatian_14_Mirrored: self._setup_croatian_14_random,
              BoardType.Croatian_20_Mirrored: self._setup_croatian_20_random,
              BoardType.Croatian_26_Mirrored: self._setup_croatian_26_random,

              BoardType.Classic_14_Symmetrical: self._setup_classic_14_random,
              BoardType.Classic_20_Symmetrical: self._setup_classic_20_random,
              BoardType.Classic_26_Symmetrical: self._setup_classic_26_random,
              BoardType.Croatian_14_Symmetrical: self._setup_croatian_14_random,
              BoardType.Croatian_20_Symmetrical: self._setup_croatian_20_random,
              BoardType.Croatian_26_Symmetrical: self._setup_croatian_26_random, }[ self.type ]

        f()

    # -----------------------------------------------------------------

    def __str__( self ):
        s = ""
        for j in range( self.get_height()-1, -1, -1 ):
            for i in range( self.get_width() ):
                p = self.get_piece( i, j )
                s += "%c" % p.get_label()
            s += "\n"
        return s


def test_1():
    b = Board( BoardType.Classical )

    print()
    print( b.get_position_limits() )
    print()
    print( str( b ) )
    print()

    b.set_piece( 2, 1, PT.Bishop )
    b.set_piece( 1, 0, PT.Pawn )
    b.set_piece( 0, 1, PT.Knight )
    b.set_piece( 1, 1, -PT.Pawn )

    print()
    print( b.get_position_limits() )
    print()
    print( str( b ) )
    print()

def test_2():
    bt = BoardType.CroatianTies # One
    b = Board( bt )
    b.setup()

    print()
    print( b.get_position_limits() )
    print()
    print( b.get_castling_files( bt ) )
    print()
    print( str( b ) )
    print()

def test_3():
    print()

    for bt in BoardType.iter( include_none=True, include_even=True, include_simple=False ):
        print( bt.get_name() )

    print()

def test_4():
    print()

    for bt in BoardType.iter( include_none=True, include_even=True, include_simple=False ):
        b = Board( bt )
        b.setup()

        print( bt.get_name(), b.get_position_limits(), Board.get_castling_files( bt ) )

    print()

def test_5():
    print()

    # % Discovery
    # % (1, 6)
    # % (22, 17)

    # % One
    # % (1, 7)
    # % (24, 18)

    for board_type in [ BoardType.Discovery, BoardType.One ]:
        bt = BoardType( board_type )
        b = Board( bt )
        # b.setup()

        print()
        print( bt.get_name() )
        print( b._calc_monolith_init_pos( PT.Monolith ) )
        print( b._calc_monolith_init_pos( -PT.Monolith ) )

    print()


if __name__ == '__main__':
    # test_1()
    # test_2()
    # test_3()
    # test_4()
    test_5()
