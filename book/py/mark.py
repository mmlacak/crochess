#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


import pixel_math as pm


DEFAULT_FOREGROUND_COLOR = "#FFFFFF"
DEFAULT_BACKGROUND_COLOR = "#000000"
DEFAULT_INVERSE_ARROW_WIDTH_RATIO = 12.0 # Compared to field size.
DEFAULT_POINTY_ARROW_BIT_RATIO = 80.0 # 1.5 # Compared to arrow width.


class Arrow(object):
    def __init__(self, start_x, start_y, end_x, end_y, \
                 fg_color=DEFAULT_FOREGROUND_COLOR, \
                 bg_color=DEFAULT_BACKGROUND_COLOR, \
                 inv_width_ratio=DEFAULT_INVERSE_ARROW_WIDTH_RATIO, \
                 pointy_bit_ratio=DEFAULT_POINTY_ARROW_BIT_RATIO):
        # self.start_x = start_x
        # self.start_y = start_y
        self.start = (start_x, start_y)

        # self.end_x = end_x
        # self.end_y = end_y
        self.end = (end_x, end_y)

        self.fg_color = fg_color
        self.bg_color = bg_color

        self.inv_width_ratio = inv_width_ratio
        self.pointy_bit_ratio = pointy_bit_ratio

    def reverse(self):
        # self.start_x, self.end_x = self.end_x, self.start_x
        # self.start_y, self.end_y = self.end_y, self.start_y
        self.start, self.end = self.end, self.start
