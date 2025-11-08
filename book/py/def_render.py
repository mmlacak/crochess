#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


class RenderingSizeEnum( int ):
    none = 0
    Info = 1
    Draft = 2
    Normal = 3
    Good = 4
    Final = 5

    def __new__( cls, value ):
        if RenderingSizeEnum._is_valid( value ):
            return super(RenderingSizeEnum, cls).__new__(cls, value)
        else:
            raise ValueError("No such a rendering type, received '%s'." % (str(value), ))

    @staticmethod
    def iter( include_none=False, include_info=True, do_construct=True ):
        lst = [ RenderingSizeEnum.Draft, \
                RenderingSizeEnum.Normal, \
                RenderingSizeEnum.Good, \
                RenderingSizeEnum.Final ]

        if include_info:
            lst.insert(0, RenderingSizeEnum.Info)

        if include_none:
            lst.insert(0, RenderingSizeEnum.none)

        lst.sort()
        return [ RenderingSizeEnum(rs) if do_construct else rs for rs in lst ]

    @staticmethod
    def _is_valid( rendering_size ):
        return rendering_size in RenderingSizeEnum.iter(include_none=True, include_info=True, do_construct=False)

    def needs_rendering( self ):
        return self in RenderingSizeEnum.iter(include_none=False, include_info=False)


class RenderingSizeItem( object ):
    def __init__( self, \
                  board_width_pix, \
                  board_max_height_pix, \
                  piece_2_by_2_pix ):
        assert isinstance(board_width_pix, int)
        assert isinstance(board_max_height_pix, int)
        assert isinstance(piece_2_by_2_pix, int)

        self.board_width_pix = board_width_pix
        self.board_max_height_pix = board_max_height_pix
        self.piece_2_by_2_pix = piece_2_by_2_pix

    def as_tuple( self ):
        return ( self.board_width_pix, \
                 self.board_max_height_pix, \
                 self.piece_2_by_2_pix )

    @staticmethod
    def from_tuple( tpl ):
        return RenderingSizeItem( *tpl[ 0 : 3 ] )


class RenderingSize( dict ):

    def __init__( self ):

        # All rendering is done for A5 format book, with
        # borders: top=15.0mm, bottom=20.0mm, left=15.0mm, right=20.0mm.
        # This yields approx. 10 cm horizontal text size, i.e. approx. 4 inches,
        # and approx. 15 cm vertical text size, i.e. approx. 6 inches.

        # board_width_pix == 9600 --> cca. 2400 dpi
        # board_width_pix == 4800 --> cca. 1200 dpi
        # board_width_pix == 2400 --> cca.  600 dpi
        # board_width_pix == 1200 --> cca.  300 dpi

        # board_max_height_pix == 50% added to rendering size

        # piece_2_by_2_pix == 40% of rendering size

        # line_width_pix >= 1 + (6 * rendering size / 5) // 1000

        RSE = RenderingSizeEnum
        RSI = RenderingSizeItem

        self[ RSE.Final ] = RSI( 9600, 14400, 3840 )
        self[ RSE.Good ] = RSI( 4800, 7200, 1920 )
        self[ RSE.Normal ] = RSI( 2400, 3600, 960 )
        self[ RSE.Draft ] = RSI( 1200, 1800, 480 )
        self[ RSE.Info ] = None
        self[ RSE.none ] = None

RenderingSize = RenderingSize()


def get_rendering_size_item( rs_enum ):
    assert isinstance(rs_enum, RenderingSizeEnum)

    return RenderingSize[rs_enum]
