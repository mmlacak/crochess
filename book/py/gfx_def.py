#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


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

    def is_valid(self):
        return RenderingSize._is_valid(self)

#     def do_render(self):
    def need_rendering(self):
        return RenderingSize.Draft <= self <= RenderingSize.Final


GD = None


class GfxDef(object):
    # Default is draft quality.
    #
    # These "constants" should have valid values. Otherwise importing scene helper crashes.
    #
    # All rendering is done for A5 format book, with
    # borders: top=15.0mm, bottom=20.0mm, left=15.0mm, right=20.0mm.
    # This yields approx. 10 cm horizontal text size, i.e. approx. 4 inches.
    #
    DEFAULT_BOARD_RENDERING_SIZE = 2400 # cca. 600 dpi
    DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 3600 # 50% added to rendering size
    DEFAULT_PIECE_2x2_RENDERING_SIZE = 960 # 40% of rendering size
    DEFAULT_BOARD_LINE_WIDTH = 4 # >= 1 + (6 * rendering size / 5) // 1000

    DEFAULT_PATH = '../gfx/' # '../tmp/'
#    DEFAULT_FILE_EXT = '.png'
#    DEFAULT_FILE_TYPE = 'png'

    def __init__(self, rendering_size=RenderingSize.Normal):
        self.rendering_size = RenderingSize(rendering_size)

        if self.rendering_size == RenderingSize.Final:
            GfxDef.DEFAULT_BOARD_RENDERING_SIZE = 9600 # cca. 2400 dpi
            GfxDef.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 14400 # 50% added to rendering size
            GfxDef.DEFAULT_PIECE_2x2_RENDERING_SIZE = 3840 # 40% of rendering size
            GfxDef.DEFAULT_BOARD_LINE_WIDTH = 13 # >= 1 + (6 * rendering size / 5) // 1000
        elif self.rendering_size == RenderingSize.Good:
            GfxDef.DEFAULT_BOARD_RENDERING_SIZE = 4800 # cca. 1200 dpi
            GfxDef.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 7200 # 50% added to rendering size
            GfxDef.DEFAULT_PIECE_2x2_RENDERING_SIZE = 1920 # 40% of rendering size
            GfxDef.DEFAULT_BOARD_LINE_WIDTH = 7 # >= 1 + (6 * rendering size / 5) // 1000
        elif self.rendering_size == RenderingSize.Normal:
            GfxDef.DEFAULT_BOARD_RENDERING_SIZE = 2400 # cca. 600 dpi
            GfxDef.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 3600 # 50% added to rendering size
            GfxDef.DEFAULT_PIECE_2x2_RENDERING_SIZE = 960 # 40% of rendering size
            GfxDef.DEFAULT_BOARD_LINE_WIDTH = 4 # >= 1 + (6 * rendering size / 5) // 1000
        elif self.rendering_size == RenderingSize.Draft:
            GfxDef.DEFAULT_BOARD_RENDERING_SIZE = 1200 # cca. 300 dpi
            GfxDef.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 1800 # 50% added to rendering size
            GfxDef.DEFAULT_PIECE_2x2_RENDERING_SIZE = 480 # 40% of rendering size
            GfxDef.DEFAULT_BOARD_LINE_WIDTH = 2 # >= 1 + (6 * rendering size / 5) // 1000

    @staticmethod
    def instantiate(rendering_size=RenderingSize.Normal):
        global GD

        GD = GfxDef(rendering_size=rendering_size)
        return GD
