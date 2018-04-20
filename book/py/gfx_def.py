#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


DEFAULT_PATH = '../gfx/' # '../tmp/'
# DEFAULT_FILE_EXT = '.png'
# DEFAULT_FILE_TYPE = 'png'

DEFAULT_ARROW_INVERSE_WIDTH_RATIO = 12.0 # Compared to field size.
DEFAULT_ARROW_POINTY_BIT_RATIO = 1.5 # Compared to arrow width. # 80.0

DEFAULT_FIELD_MARKER_INVERSE_WIDTH_RATIO = 5.0 # Compared to field size.


class RenderingSize(int):
    none = 0
    Info = 1
    Draft = 2
    Normal = 3
    Good = 4
    Final = 5

    def __new__(cls, value):
        if RenderingSize._is_valid(value):
            return super(RenderingSize, cls).__new__(cls, value)
        else:
            raise ValueError("No such a rendering type, received '%s'." % (str(value), ))

    def __iter__(self):
        for rs in [ RenderingSize.none, \
                    RenderingSize.Info, \
                    RenderingSize.Draft, \
                    RenderingSize.Normal, \
                    RenderingSize.Good, \
                    RenderingSize.Final ]:
            yield RenderingSize(rs)

    @staticmethod
    def _is_valid(rendering_size):
        return RenderingSize.none <= rendering_size <= RenderingSize.Final

    def need_rendering(self):
        return RenderingSize.Draft <= self <= RenderingSize.Final


class GfxDefSizes(object):
    def __init__( self, \
                  board_rendering_size, \
                  board_max_vertical_rendering_size, \
                  piece_2_by_2_rendering_size, \
                  board_line_width ):
        assert isinstance(board_rendering_size, int)
        assert isinstance(board_max_vertical_rendering_size, int)
        assert isinstance(piece_2_by_2_rendering_size, int)
        assert isinstance(board_line_width, int)

        self.board_rendering_size = board_rendering_size
        self.board_max_vertical_rendering_size = board_max_vertical_rendering_size
        self.piece_2_by_2_rendering_size = piece_2_by_2_rendering_size
        self.board_line_width = board_line_width

    def as_tuple(self):
        return ( self.board_rendering_size, \
                 self.board_max_vertical_rendering_size, \
                 self.piece_2_by_2_rendering_size, \
                 self.board_line_width )

    @staticmethod
    def from_tuple(tpl):
        return GfxDefSizes( board_rendering_size=tpl[0], \
                            board_max_vertical_rendering_size=tpl[1], \
                            piece_2_by_2_rendering_size=tpl[2], \
                            board_line_width=tpl[3] )


GD = None


class GfxDef(object):

    def __init__(self, rendering_size):
        self.rendering_size = RenderingSize(rendering_size)

    @property
    def defaults(self):

        # All rendering is done for A5 format book, with
        # borders: top=15.0mm, bottom=20.0mm, left=15.0mm, right=20.0mm.
        # This yields approx. 10 cm horizontal text size, i.e. approx. 4 inches,
        # and approx. 15 cm vertical size, i.e. approx. 6 inches.

        # board_rendering_size == 9600 --> cca. 2400 dpi
        # board_rendering_size == 4800 --> cca. 1200 dpi
        # board_rendering_size == 2400 --> cca.  600 dpi
        # board_rendering_size == 1200 --> cca.  300 dpi

        # board_max_vertical_rendering_size == 50% added to rendering size

        # piece_2_by_2_rendering_size == 40% of rendering size

        # board_line_width >= 1 + (6 * rendering size / 5) // 1000

        GDS = GfxDefSizes.from_tuple

        dct = { RenderingSize.Final : GDS( (9600, 14400, 3840, 13) ), \
                RenderingSize.Good : GDS( (4800, 7200, 1920, 7) ), \
                RenderingSize.Normal : GDS( (2400, 3600, 960, 4) ), \
                RenderingSize.Draft : GDS( (1200, 1800, 480, 2) ), \
                RenderingSize.Info : None, \
                RenderingSize.none : None }

        return dct[ self.rendering_size ]

    @staticmethod
    def instantiate(rendering_size):
        global GD

        GD = GfxDef(rendering_size=rendering_size)
        return GD
