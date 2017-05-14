#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016, 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# Final version graphics size.
#
# DEFAULT_BOARD_RENDERING_SIZE = 8000
# DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 12000 # 50% added to rendering size
# DEFAULT_PIECE_2x2_RENDERING_SIZE = 3200 # 40% of rendering size
# DEFAULT_BOARD_LINE_WIDTH = 11 # >= 1 + (6 * rendering size / 5) // 1000

# Draft version graphics size.
#
# DEFAULT_BOARD_RENDERING_SIZE = 2000
# DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 3000 # 50% added to rendering size
# DEFAULT_PIECE_2x2_RENDERING_SIZE = 800 # 40% of rendering size
# DEFAULT_BOARD_LINE_WIDTH = 4 # >= 1 + (6 * rendering size / 5) // 1000

# DEFAULT_PATH = '../gfx/' # '../tmp/'
# DEFAULT_FILE_EXT = '.png'
# DEFAULT_FILE_TYPE = 'png'


GD = None


class GfxDef(object):
    DEFAULT_BOARD_RENDERING_SIZE = None
    DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = None
    DEFAULT_PIECE_2x2_RENDERING_SIZE = None
    DEFAULT_BOARD_LINE_WIDTH = None

    DEFAULT_PATH = '../gfx/' # '../tmp/'
    DEFAULT_FILE_EXT = '.png'
    DEFAULT_FILE_TYPE = 'png'

    def __init__(self, is_final_or_draft=False):
        # default is draft

        if is_final_or_draft:
            GfxDef.DEFAULT_BOARD_RENDERING_SIZE = 8000
            GfxDef.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 12000 # 50% added to rendering size
            GfxDef.DEFAULT_PIECE_2x2_RENDERING_SIZE = 3200 # 40% of rendering size
            GfxDef.DEFAULT_BOARD_LINE_WIDTH = 11 # >= 1 + (6 * rendering size / 5) // 1000
        else:
            GfxDef.DEFAULT_BOARD_RENDERING_SIZE = 2000
            GfxDef.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 3000 # 50% added to rendering size
            GfxDef.DEFAULT_PIECE_2x2_RENDERING_SIZE = 800 # 40% of rendering size
            GfxDef.DEFAULT_BOARD_LINE_WIDTH = 4 # >= 1 + (6 * rendering size / 5) // 1000

    @staticmethod
    def instantiate(is_final_or_draft=False):
        global GD

        if GD is None:
            GD = GfxDef(is_final_or_draft=is_final_or_draft)

        return GD
