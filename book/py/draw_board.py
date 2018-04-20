#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

from util import xor
from board import BoardType, Board
from colors import ColorsPair, ColorsShade, ColorsPiece, ColorsItem
from draw import get_new_drawable, get_new_gc, DrawableRectangle, Draw
from draw_piece import DrawableRectangle, DrawPiece


class BoardDesc(object):
    def __init__(self, \
                 margin_top_pix=0, \
                 margin_left_pix=0, \
                 margin_right_pix=0, \
                 margin_bottom_pix=0, \
                 reverse_field_colors=False):

        assert isinstance(margin_top_pix, int)
        assert isinstance(margin_left_pix, int)
        assert isinstance(margin_right_pix, int)
        assert isinstance(margin_bottom_pix, int)
        assert isinstance(reverse_field_colors, bool)

        self.margin_top_pix = margin_top_pix
        self.margin_left_pix = margin_left_pix
        self.margin_right_pix = margin_right_pix
        self.margin_bottom_pix = margin_bottom_pix

        self.reverse_field_colors = reverse_field_colors

    def as_tuple(self):
        return ( self.margin_top_pix, \
                 self.margin_left_pix, \
                 self.margin_right_pix, \
                 self.margin_bottom_pix, \
                 self.reverse_field_colors )

    @staticmethod
    def from_tuple(tpl):
        return BoardDesc( margin_top_pix=tpl[0], \
                          margin_left_pix=tpl[1], \
                          margin_right_pix=tpl[2], \
                          margin_bottom_pix=tpl[3], \
                          reverse_field_colors=tpl[4] )


class DrawBoard(Draw):

    def __init__(self, drawable, gc, board, board_desc=None):
        super(DrawBoard, self).__init__(drawable, gc)

        assert isinstance(board, Board)
        self.board = board

        self.board_desc = board_desc or BoardDesc()

        self.calc_board_geometry()

        self.draw_piece = DrawPiece(self.drawable, self.gc)

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
        return (left_pix, top_pix)

    def draw_field(self, i, j, cshade=None, gc=None):
        assert isinstance(cshade, (ColorsShade, NoneType))

        x_pix, y_pix = self.get_field_start_pix(i, j)

        if cshade is not None:
            color = cshade.light \
                    if xor( self.board.is_light(i, j), self.board_desc.reverse_field_colors, default=False ) \
                    else cshade.dark

            self.draw_rectangle(x_pix, y_pix, self.field_width_pix, self.field_height_pix, filled=True, fg=color.interior, bg=color.outline, gc=gc)
        else:
            self.draw_rectangle(x_pix, y_pix, self.field_width_pix, self.field_height_pix, filled=True, gc=gc)

    def draw_all_fields(self, cshade=None, gc=None):
        for j in xrange(self.board.get_height()):
            for i in xrange(self.board.get_width()):
                self.draw_field(i, j, cshade=cshade, gc=gc)

    def draw_piece_at_field(self, i, j, colors_item, gc=None):
        x_pix, y_pix = self.get_field_start_pix(i, j)
        p = self.board.get_piece(i, j)
        dr = DrawableRectangle(x_pix, y_pix, self.field_width_pix, self.field_height_pix)
        self.draw_piece.draw_piece(p, dr, colors_item, gc=gc)

    def draw_all_pieces(self, colors_item, gc=None):
        for j in xrange(self.board.get_height()):
            for i in xrange(self.board.get_width()):
                self.draw_piece_at_field(i, j, colors_item, gc=gc)

    def draw_board(self, colors_item, gc=None):
        self.draw_all_fields(colors_item.field, gc=gc)
        self.draw_all_pieces(colors_item, gc=gc)

# TODO :: field coordinates (converting pixel coords into field coords)

    def convert_field_width_to_pixel(self, x):
        if isinstance(x, float):
            return x * self.field_width_pix
        return x

    def convert_field_height_to_pixel(self, y):
        if isinstance(y, float):
            y_reverse = self.board.get_height() - y # - 1.0
            return y_reverse * self.field_height_pix
        return y

    def convert_field_x_to_pixel(self, x):
        if isinstance(x, float):
            return self.board_left_pix + x * self.field_width_pix
        return x

    def convert_field_y_to_pixel(self, y):
        if isinstance(y, float):
            y_reverse = self.board.get_height() - y # - 1.0
            return self.board_top_pix + y_reverse * self.field_height_pix
        return y

    def convert_field_coords_to_pixel(self, x, y):
        x_pix = self.convert_field_x_to_pixel(x)
        y_pix = self.convert_field_y_to_pixel(y)
        return (x_pix, y_pix)


def test_1(board_desc=None, width=None, height=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 11)
    b = Board(BoardType.CroatianTies, width=width, height=height)
    b.setup()

    d = DrawBoard(drw, gc, b, board_desc=board_desc)
    d.clear_area()

    cs = ColorsShade.from_tuple( ('#B0B0B0', '#B0B0B0', '#404040', '#404040' ) )
    d.draw_all_fields(cshade=cs)

    file_path = 'temp/board%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2(board_desc=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 3)
    b = Board(BoardType.CroatianTies)
    b.setup()

    d = DrawBoard(drw, gc, b, board_desc=board_desc)
    d.clear_area()

    from colors import Colors
    d.draw_board( Colors[BoardType.Classical] )

    file_path = 'temp/setup%s.IGNORE.png' % name
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

    test_2()
