#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

from util import xor
from board import BoardType, Board
from colors import ColorsPair, ColorsShade, ColorsPiece, ColorsItem
from draw import get_new_drawable, get_new_gc, DrawableRectangle, Draw


class BoardDesc(object):
    def __init__(self, margin_top_pix=0, margin_left_pix=0, margin_right_pix=0, margin_bottom_pix=0, reverse_field_colors=False):
        self.margin_top_pix = margin_top_pix
        self.margin_left_pix = margin_left_pix
        self.margin_right_pix = margin_right_pix
        self.margin_bottom_pix = margin_bottom_pix

        self.reverse_field_colors = reverse_field_colors

    def as_tuple(self):
        return (self.margin_top_pix, self.margin_left_pix, self.margin_right_pix, self.margin_bottom_pix, self.reverse_field_colors)

    @staticmethod
    def from_tuple(tpl):
        return BoardDesc(margin_top_pix=tpl[0], margin_left_pix=tpl[1], margin_right_pix=tpl[2], margin_bottom_pix=tpl[3], reverse_field_colors=tpl[4])


class DrawBoard(Draw):

    def __init__(self, drawable, gc, board, board_desc=None):
        super(DrawBoard, self).__init__(drawable, gc)

        assert isinstance(board, Board)
        self.board = board

        self.board_desc = board_desc or BoardDesc()

        self.calc_board_geometry()

    def calc_board_geometry(self):
        w_pix, h_pix = self.get_field_size_pix()

        self.field_width_pix = w_pix
        self.field_height_pix = h_pix

        self.board_width_pix = self.field_width_pix * self.board.get_width()
        self.board_height_pix = self.field_height_pix * self.board.get_height()
        self.board_top_pix = self.board_desc.margin_top_pix
        self.board_left_pix = self.board_desc.margin_left_pix

    def get_field_size_pix(self):
        w, h = self.drawable.get_size()
        w = w - self.board_desc.margin_left_pix - self.board_desc.margin_right_pix
        h = h - self.board_desc.margin_top_pix - self.board_desc.margin_bottom_pix

        i = self.board.get_width()
        j = self.board.get_height()

        x = w / i
        y = h / j

        m = min(x, y)
        return (m, m)

    def get_field_start_pix(self, i, j):
        left_pix = self.board_left_pix + i * self.field_width_pix
        j_reverse = self.board.get_height() - j - 1
        top_pix = self.board_top_pix + j_reverse * self.field_height_pix
        return (top_pix, left_pix)

    def draw_field(self, i, j, cshade=None, gc=None):
        assert isinstance(cshade, (ColorsShade, NoneType))

        x_pix, y_pix = self.get_field_start_pix(i, j)

        if cshade is not None:
            color = cshade.light \
                    if xor( self.board.is_light(i, j), self.board_desc.reverse_field_colors, fail=False ) \
                    else cshade.dark

            self.draw_rectangle(x_pix, y_pix, self.field_width_pix, self.field_height_pix, filled=True, fg=color.interior, bg=color.outline, gc=gc)
        else:
            self.draw_rectangle(x_pix, y_pix, self.field_width_pix, self.field_height_pix, filled=True, gc=gc)

    def draw_all_fields(self, cshade=None, gc=None):
        for i in xrange(self.board.get_width()):
            for j in xrange(self.board.get_height()):
                self.draw_field(i, j, cshade=cshade, gc=gc)

# TODO :: field coordinates


def test_1(board_desc=None, width=None, height=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 11)
    b = Board(BoardType.CroatianTies, width=width, height=height)
    b.setup()

    d = DrawBoard(drw, gc, b, board_desc=board_desc)
    d.clear_area()

    cs = ColorsShade.from_tuple( ('#B0B0B0', '#B0B0B0', '#404040', '#404040') )
    d.draw_all_fields(cshade=cs)

    file_path = 'temp/board%s.IGNORE.png' % name
    d.save_image(file_path)


if __name__ == '__main__':
    test_1()
    test_1(board_desc=BoardDesc(reverse_field_colors=True), name='_reverse')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=40), name='_margin')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=90, margin_bottom_pix=40), name='_margin_2')

    width = 3
    height = 7

    test_1(width=width, height=height)
    test_1(board_desc=BoardDesc(reverse_field_colors=True), width=width, height=height, name='_vert_reverse')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=40), width=width, height=height, name='_vert_margin')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=90, margin_bottom_pix=40), width=width, height=height, name='_vert_margin_2')

    width = 7
    height = 3

    test_1(width=width, height=height)
    test_1(board_desc=BoardDesc(reverse_field_colors=True), width=width, height=height, name='_hor_reverse')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=40), width=width, height=height, name='_hor_margin')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=90, margin_bottom_pix=40), width=width, height=height, name='_hor_margin_2')
