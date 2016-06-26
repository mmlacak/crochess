#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pixel_math as pm

class Arrow(object):

    DEFAULT_FOREGROUND_COLOR = "#FFFFFF"
    DEFAULT_BACKGROUND_COLOR = "#000000"
    DEFAULT_INVERSE_ARROW_WIDTH_RATIO = 12.0 # Compared to field size.
    DEFAULT_POINTY_ARROW_BIT_RATIO = 1.5 # Compared to arrow width. # 80.0

    def __init__(self, start_x, start_y, end_x, end_y, \
                 fg_color=None, \
                 bg_color=None, \
                 inv_width_ratio=None, \
                 pointy_bit_ratio=None):
        # self.start_x = start_x
        # self.start_y = start_y
        self.start = (start_x, start_y)

        # self.end_x = end_x
        # self.end_y = end_y
        self.end = (end_x, end_y)

        self.fg_color = fg_color or Arrow.DEFAULT_FOREGROUND_COLOR
        self.bg_color = bg_color or Arrow.DEFAULT_BACKGROUND_COLOR

        self.inv_width_ratio = inv_width_ratio or Arrow.DEFAULT_INVERSE_ARROW_WIDTH_RATIO
        self.pointy_bit_ratio = pointy_bit_ratio or Arrow.DEFAULT_POINTY_ARROW_BIT_RATIO

    def reverse(self):
        # self.start_x, self.end_x = self.end_x, self.start_x
        # self.start_y, self.end_y = self.end_y, self.start_y
        self.start, self.end = self.end, self.start

class Text(object):

    DEFAULT_FOREGROUND_COLOR = "#FFFFFF"
    DEFAULT_BACKGROUND_COLOR = "#000000"
    DEFAULT_FONT = "normal 10"

    def __init__(self, text, pos_x, pos_y, font=None, \
                 fg_color=None, \
                 bg_color=None):
        self.text = text

        # self.pos_x = pos_x
        # self.pos_y = pos_y
        self.pos = (pos_x, pos_x)

        self.font = font or Text.DEFAULT_FONT

        self.fg_color = fg_color or Text.DEFAULT_FOREGROUND_COLOR
        self.bg_color = bg_color or Text.DEFAULT_BACKGROUND_COLOR

class FieldMarker(object):

    DEFAULT_FOREGROUND_COLOR = "#FFFFFF"
    DEFAULT_BACKGROUND_COLOR = "#000000"
    DEFAULT_INVERSE_WIDTH_RATIO = 5.0 # Compared to field size.

    def __init__(self, field_i, field_j, \
                 fg_color=None, \
                 bg_color=None, \
                 inv_width_ratio=None):
        # self.field_i = field_i
        # self.field_j = field_j
        self.field = (field_i, field_j)

        self.fg_color = fg_color or FieldMarker.DEFAULT_FOREGROUND_COLOR
        self.bg_color = bg_color or FieldMarker.DEFAULT_BACKGROUND_COLOR

        self.inv_width_ratio = inv_width_ratio or FieldMarker.DEFAULT_INVERSE_WIDTH_RATIO
