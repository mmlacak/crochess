#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class BoardDesc(object):
    def __init__(self, \
                 reverse_field_colors=False, \
                 border_left_pix=0, \
                 border_top_pix=0, \
                 border_right_pix=0, \
                 border_bottom_pix=0):

        assert isinstance(reverse_field_colors, bool)

        # TODO :: allow floats, as size relative to field
        assert isinstance(border_left_pix, int)
        assert isinstance(border_top_pix, int)
        assert isinstance(border_right_pix, int)
        assert isinstance(border_bottom_pix, int)

        self.reverse_field_colors = reverse_field_colors

        self.border_left_pix = border_left_pix
        self.border_top_pix = border_top_pix
        self.border_right_pix = border_right_pix
        self.border_bottom_pix = border_bottom_pix

    def as_tuple(self):
        return ( self.reverse_field_colors, \
                 self.border_left_pix, \
                 self.border_top_pix, \
                 self.border_right_pix, \
                 self.border_bottom_pix )

    @staticmethod
    def from_tuple(tpl):
        return BoardDesc( reverse_field_colors=tpl[0], \
                          border_left_pix=tpl[1], \
                          border_top_pix=tpl[2], \
                          border_right_pix=tpl[3], \
                          border_bottom_pix=tpl[4] )
