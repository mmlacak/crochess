#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pixel_math as pm

class CoordinateType(int):
    Pixel = 1
    FieldIndex = 2

    def __new__(cls, value):
        if CoordinateType._is_valid(value):
            return super(CoordinateType, cls).__new__(cls, value)
        else:
            raise ValueError("No such a coordinate type, received '%s'." % (str(value), ))

    @staticmethod
    def _is_valid(coord_type):
        return CoordinateType.Pixel <= coord_type <= CoordinateType.FieldIndex

    def is_valid(self):
        return CoordinateType._is_valid(self)

class Arrow(object):

    DEFAULT_FOREGROUND_COLOR = "#FFFFFF"
    DEFAULT_BACKGROUND_COLOR = "#000000"
    DEFAULT_INVERSE_ARROW_WIDTH_RATIO = 12.0 # Compared to field size.
    DEFAULT_POINTY_ARROW_BIT_RATIO = 1.5 # Compared to arrow width. # 80.0

    def __init__(self, start_x, start_y, end_x, end_y, \
                 coord_type=CoordinateType.FieldIndex, \
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

        self.coord_type = coord_type

        self.fg_color = fg_color or Arrow.DEFAULT_FOREGROUND_COLOR
        self.bg_color = bg_color or Arrow.DEFAULT_BACKGROUND_COLOR

        self.inv_width_ratio = inv_width_ratio or Arrow.DEFAULT_INVERSE_ARROW_WIDTH_RATIO
        self.pointy_bit_ratio = pointy_bit_ratio or Arrow.DEFAULT_POINTY_ARROW_BIT_RATIO

    def reverse(self):
        # self.start_x, self.end_x = self.end_x, self.start_x
        # self.start_y, self.end_y = self.end_y, self.start_y
        self.start, self.end = self.end, self.start
