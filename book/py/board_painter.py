#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk
import math

from piece import PieceType
from board import BoardType
from painter_context import FieldPosition
from painter_context import DrawablePosition
from painter_context import DrawableRectangle
from painter_context import ColorPair
from painter_context import ColorContext
from painter_context import PainterContext
from piece_painter import PiecePainter

import pixel_math as pm

import debug_

class BoardPainter(PiecePainter):
    BorderWidth_pix = 0 # 3
    BorderHeight_pix = 0 # 3
    BorderTop_pix = 0 # 1
    BorderLeft_pix = 0 # 1
    BorderThickness_pix = 0 # 1

    def __init__(self, drawable, board):
        super(BoardPainter, self).__init__(drawable)

        self.init(board)

    def init(self, board):
        self.board = board

        if self.board.is_square():
            w_pix, h_pix = self.get_square_size()
        else:
            # w_pix, h_pix = self.drawable.get_size()
            w_pix, h_pix = self.get_aspect_ratio_size()

        # is_by_the_book = self.board.is_by_the_book()
        # if is_by_the_book:
        #     w_pix = w_pix - 2 * BoardPainter.BorderWidth_pix
        self.field_width_pix = int(math.floor(w_pix / self.board.get_width()))
        # if is_by_the_book:
        #     h_pix = h_pix - 2 * BoardPainter.BorderHeight_pix
        self.field_height_pix = int(math.floor(h_pix / self.board.get_height()))

        self.board_width_pix = self.field_width_pix * self.board.get_width()
        self.board_height_pix = self.field_height_pix * self.board.get_height()
        # if is_by_the_book:
        #     self.board_top_pix = BoardPainter.BorderHeight_pix
        #     self.board_left_pix = BoardPainter.BorderWidth_pix
        # else:
        #     self.board_top_pix = 0
        #     self.board_left_pix = 0
        self.board_top_pix = 0
        self.board_left_pix = 0

        # if is_by_the_book:
        #     self.border_top_pix = BoardPainter.BorderTop_pix
        #     self.border_left_pix = BoardPainter.BorderLeft_pix
        #     self.border_width_pix = self.board_width_pix + 2 * (BoardPainter.BorderWidth_pix - BoardPainter.BorderThickness_pix) - 1
        #     self.border_height_pix = self.board_height_pix + 2 * (BoardPainter.BorderHeight_pix - BoardPainter.BorderThickness_pix) - 1
        # else:
        #     self.border_top_pix = 0
        #     self.border_left_pix = 0
        #     self.border_width_pix = self.board_width_pix
        #     self.border_height_pix = self.board_height_pix
        self.border_top_pix = 0
        self.border_left_pix = 0
        self.border_width_pix = self.board_width_pix
        self.border_height_pix = self.board_height_pix

    def get_aspect_ratio_size(self):
        w, h = self.drawable.get_size()
        i = self.board.get_width()
        j = self.board.get_height()
        x = w / i
        y = h / j
        m = min(x, y)
        return (m * i, m * j)

    def get_field_start_pix(self, i, j):
        left_pix = self.board_left_pix + i * self.field_width_pix
        j_reverse = self.board.get_height() - j - 1
        top_pix = self.board_top_pix + j_reverse * self.field_height_pix
        return DrawablePosition(left_pix, top_pix)

    def get_field_position(self, x_pix, y_pix):
        i = (x_pix - self.board_left_pix) / self.field_size_pix
        i = int(round(i))
        j = (y_pix - self.board_top_pix) / self.field_size_pix
        j_reverse = self.board.get_height() - j - 1
        j_reverse = int(round(j_reverse))
        return FieldPosition(i, j_reverse)

    def is_field_light_or_dark(self, i, j):
        if len(self.board) % 2 == 0:
            is_light_or_dark = bool((i + j) % 2 != 0)
        else:
            is_light_or_dark = bool((i + j) % 2 == 0)
        return is_light_or_dark

    def get_gc_field_color(self, i, j, pc):
        is_light_or_dark = self.is_field_light_or_dark(i, j)
        return pc.get_gc_field(i, j, is_light_or_dark)

    def get_field_colors(self, i, j, pc):
        is_light_or_dark = self.is_field_light_or_dark(i, j)
        cp = pc.cc.get_pair_field(is_light_or_dark)
        return cp.foreground, cp.background

    def draw_field(self, i, j, pc):
        dp = self.get_field_start_pix(i, j)
        gc = self.get_gc_field_color(i, j, pc)
        self.drawable.draw_rectangle(gc, True, dp.x_pix, dp.y_pix, self.field_width_pix, self.field_height_pix)

    def draw_all_fields(self, pc):
        for i in xrange(self.board.get_width()):
            for j in xrange(self.board.get_height()):
                self.draw_field(i, j, pc)

    def draw_border(self, pc, is_light_or_dark):
        gc = pc.get_gc_field(0, 0, is_light_or_dark)
        self.drawable.draw_rectangle(gc, False, self.border_top_pix, self.border_left_pix, self.border_width_pix, self.border_height_pix)

    def draw_piece_at_field(self, i, j, pc):
        dp = self.get_field_start_pix(i, j)
        pc.set_rect(dp.x_pix, dp.y_pix, self.field_width_pix, self.field_height_pix)
        p = self.board[j][i]
        self.draw_piece(pc, p)

    def draw_all_pieces(self, pc):
        for i in xrange(self.board.get_width()):
            for j in xrange(self.board.get_height()):
                self.draw_piece_at_field(i, j, pc)

    def draw_board(self, pc):
        # if self.board.is_by_the_book():
        #     self.draw_border(pc, False)
        self.draw_all_fields(pc)
        self.draw_all_pieces(pc)

#     def draw_arrow(self, start, end, pc):
#         width = self.field_width_pix / 7.0
#         distance = width / 2.0
#         length = pm.calc_line_length(start, end)
#
#         start_lst = pm.calc_distant_points_on_inverse_line(start, end, distance)
#
#         ratio = abs((length - width) / width) # Shouldn't be negative, i.e. outside line segment.
#         mid_point = pm.calc_division_point(start, end, ratio)
#
#         mid_lst = pm.calc_distant_points_on_inverse_line(mid_point, start, distance)
#         mid_lst_2 = pm.calc_distant_points_on_inverse_line(mid_point, start, 2.0 * distance)
#
#         arrow_lst = [ end, mid_lst_2[0], mid_lst[0], start_lst[0], start_lst[1], mid_lst[1], mid_lst_2[1] ]
#         arrow_lst_2 = [ (int(tpl[0]), int(tpl[1])) for tpl in arrow_lst ]
#         self.draw_polygon_with_background(pc.get_gc_monolith(), arrow_lst_2)
#
#     def draw_all_arrows(self, arrows, pc):
#         for arrow in arrows:
#             start, end = arrow
#             self.draw_arrow(start, end, pc )
