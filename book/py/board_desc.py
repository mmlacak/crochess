#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# class BoardDesc(object):
#     def __init__(self, \
#                  reverse_field_colors=False, \
#                  off_board_left=0, \
#                  off_board_top=0, \
#                  off_board_right=0, \
#                  off_board_bottom=0, \
#                  reverse_off_board_field_colors=False, \
#                  border_left_pix=0, \
#                  border_top_pix=0, \
#                  border_right_pix=0, \
#                  border_bottom_pix=0):

#         assert isinstance(reverse_field_colors, bool)

#         assert isinstance(off_board_left, int)
#         assert isinstance(off_board_top, int)
#         assert isinstance(off_board_right, int)
#         assert isinstance(off_board_bottom, int)

#         assert off_board_left >= 0
#         assert off_board_top >= 0
#         assert off_board_right >= 0
#         assert off_board_bottom >= 0

#         assert isinstance(reverse_off_board_field_colors, bool)

#         # TODO :: allow floats, as size relative to field
#         assert isinstance(border_left_pix, int)
#         assert isinstance(border_top_pix, int)
#         assert isinstance(border_right_pix, int)
#         assert isinstance(border_bottom_pix, int)

#         assert border_left_pix >= 0
#         assert border_top_pix >= 0
#         assert border_right_pix >= 0
#         assert border_bottom_pix >= 0

#         self.reverse_field_colors = reverse_field_colors

#         self.off_board_left = off_board_left
#         self.off_board_top = off_board_top
#         self.off_board_right = off_board_right
#         self.off_board_bottom = off_board_bottom

#         self.reverse_off_board_field_colors = reverse_off_board_field_colors

#         self.border_left_pix = border_left_pix
#         self.border_top_pix = border_top_pix
#         self.border_right_pix = border_right_pix
#         self.border_bottom_pix = border_bottom_pix

#     def as_tuple(self):
#         return ( self.reverse_field_colors, \
#                  self.off_board_left, \
#                  self.off_board_top, \
#                  self.off_board_right, \
#                  self.off_board_bottom, \
#                  self.reverse_off_board_field_colors, \
#                  self.border_left_pix, \
#                  self.border_top_pix, \
#                  self.border_right_pix, \
#                  self.border_bottom_pix )

#     @staticmethod
#     def from_tuple(tpl):
#         return BoardDesc( reverse_field_colors=tpl[0], \
#                           off_board_left=tpl[1], \
#                           off_board_top=tpl[2], \
#                           off_board_right=tpl[3], \
#                           off_board_bottom=tpl[4], \
#                           reverse_off_board_field_colors=tpl[5], \
#                           border_left_pix=tpl[6], \
#                           border_top_pix=tpl[7], \
#                           border_right_pix=tpl[8], \
#                           border_bottom_pix=tpl[9] )
