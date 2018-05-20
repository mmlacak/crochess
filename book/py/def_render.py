#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class RenderingSizeEnum(int):
    none = 0
    Info = 1
    Draft = 2
    Normal = 3
    Good = 4
    Final = 5

    def __new__(cls, value):
        if RenderingSizeEnum._is_valid(value):
            return super(RenderingSizeEnum, cls).__new__(cls, value)
        else:
            raise ValueError("No such a rendering type, received '%s'." % (str(value), ))

    def __iter__(self):
        for rs in [ RenderingSizeEnum.none, \
                    RenderingSizeEnum.Info, \
                    RenderingSizeEnum.Draft, \
                    RenderingSizeEnum.Normal, \
                    RenderingSizeEnum.Good, \
                    RenderingSizeEnum.Final ]:
            yield RenderingSizeEnum(rs)

    @staticmethod
    def _is_valid(rendering_size):
        return RenderingSizeEnum.none <= rendering_size <= RenderingSizeEnum.Final

    def need_rendering(self):
        return RenderingSizeEnum.Draft <= self <= RenderingSizeEnum.Final


class RenderingSizeItem(object):
    def __init__( self, \
                  board_width_pix, \
                  board_max_height_pix, \
                  piece_2_by_2_pix, \
                  line_width_pix ):
        assert isinstance(board_width_pix, int)
        assert isinstance(board_max_height_pix, int)
        assert isinstance(piece_2_by_2_pix, int)
        assert isinstance(line_width_pix, int)

        self.board_width_pix = board_width_pix
        self.board_max_height_pix = board_max_height_pix
        self.piece_2_by_2_pix = piece_2_by_2_pix
        self.line_width_pix = line_width_pix

    def as_tuple(self):
        return ( self.board_width_pix, \
                 self.board_max_height_pix, \
                 self.piece_2_by_2_pix, \
                 self.line_width_pix )

    @staticmethod
    def from_tuple(tpl):
        return RenderingSizeItem( *tpl[ 0 : 4 ] )


class RenderingSize(dict):

    def __init__(self):

        # All rendering is done for A5 format book, with
        # borders: top=15.0mm, bottom=20.0mm, left=15.0mm, right=20.0mm.
        # This yields approx. 10 cm horizontal text size, i.e. approx. 4 inches,
        # and approx. 15 cm vertical size, i.e. approx. 6 inches.

        # board_width_pix == 9600 --> cca. 2400 dpi
        # board_width_pix == 4800 --> cca. 1200 dpi
        # board_width_pix == 2400 --> cca.  600 dpi
        # board_width_pix == 1200 --> cca.  300 dpi

        # board_max_height_pix == 50% added to rendering size

        # piece_2_by_2_pix == 40% of rendering size

        # line_width_pix >= 1 + (6 * rendering size / 5) // 1000

        RSE = RenderingSizeEnum
        RSI = RenderingSizeItem

        self[ RSE.Final ] = RSI( 9600, 14400, 3840, 13 )
        self[ RSE.Good ] = RSI( 4800, 7200, 1920, 7 )
        self[ RSE.Normal ] = RSI( 2400, 3600, 960, 4 )
        self[ RSE.Draft ] = RSI( 1200, 1800, 480, 2 )
        self[ RSE.Info ] = None
        self[ RSE.none ] = None

RenderingSize = RenderingSize()
