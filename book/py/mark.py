#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class Arrow(object):

    def __init__(self, start_x, start_y, end_x, end_y, \
                 start_pointer=False, \
                 end_pointer=True):
        self.start = (start_x, start_y)
        self.end = (end_x, end_y)
        self.start_pointer = start_pointer
        self.end_pointer = end_pointer

    def reverse(self):
        self.start, self.end = self.end, self.start
        self.start_pointer, self.end_pointer = self.end_pointer, self.start_pointer


class Text(object):

    def __init__(self, text, pos_x, pos_y):
        self.text = text
        self.pos = (pos_x, pos_y)


class FieldMarker(object):

    def __init__(self, field_i, field_j):
        self.field = (field_i, field_j)
