#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


import pixel_math as pm


class Arrow(object):
    def __init__(self, start_x, start_y, end_x, end_y, \
                 fg_color="#FFFFFF", \
                 bg_color="#000000"):
        # self.start_x = start_x
        # self.start_y = start_y
        self.start = (start_x, start_y)

        # self.end_x = end_x
        # self.end_y = end_y
        self.end = (end_x, end_y)

        self.fg_color = fg_color
        self.bg_color = bg_color

    def reverse(self):
        # self.start_x, self.end_x = self.end_x, self.start_x
        # self.start_y, self.end_y = self.end_y, self.start_y
        self.start, self.end = self.end, self.start
