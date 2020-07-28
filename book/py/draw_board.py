#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import xor, UNDEFINED
from board_desc import BoardDesc
from board import BoardType, Board
from colors import ColorsShade, ColorsItem, Colors
from pixel_math import RectanglePix
from draw import DEFAULT_COLOR_SPACE
from draw_piece import DrawPiece


class DrawBoard(DrawPiece):

    def __init__(self, width_pix, height_pix, board, board_desc=None, bg_color="#FFFFFF", color_space=DEFAULT_COLOR_SPACE):
        super(DrawBoard, self).__init__(width_pix, height_pix, bg_color=bg_color, color_space=color_space)
        self.init_board(board, board_desc=board_desc)

    def init_board(self, board, board_desc=None):
        assert isinstance(board, Board)

        self.board = board
        self.board_desc = board_desc or BoardDesc()

        assert isinstance(self.board_desc, BoardDesc)

        self.store_board_geometry()

    def calc_total_field_counts(self):
        w = self.board_desc.off_board_left + self.board.get_width() + self.board_desc.off_board_right
        h = self.board_desc.off_board_top + self.board.get_height() + self.board_desc.off_board_bottom
        return (w, h)

    def calc_total_field_geometry(self):
        left = -self.board_desc.off_board_left
        top = self.board.get_height() - 1 + self.board_desc.off_board_top
        right = self.board.get_width() - 1 + self.board_desc.off_board_right
        bottom = -self.board_desc.off_board_bottom
        return (left, top, right, bottom)

    def calc_fields_canvas_size(self):
        w, h = self.image.size
        w2 = w - self.board_desc.border_left_pix - self.board_desc.border_right_pix
        h2 = h - self.board_desc.border_top_pix - self.board_desc.border_bottom_pix
        return (w2, h2)

    def store_board_geometry(self):
        w, h = self.calc_total_field_counts()
        self.field_count_hor = w
        self.field_count_vert = h

        left, top, right, bottom = self.calc_total_field_geometry()
        self.fields_left = left
        self.fields_top = top
        self.fields_right = right
        self.fields_bottom = bottom

        w_pix, h_pix = self.calc_fields_canvas_size()
        self.fields_canvas_left_pix = self.board_desc.border_left_pix
        self.fields_canvas_top_pix = self.board_desc.border_top_pix
        self.fields_canvas_width_pix = w_pix
        self.fields_canvas_height_pix = h_pix

        w_pix, h_pix = self.get_field_size_pix()
        self.field_width_pix = w_pix
        self.field_height_pix = h_pix

        self.board_left_pix = self.fields_canvas_left_pix + self.board_desc.off_board_left * self.field_width_pix
        self.board_top_pix = self.fields_canvas_top_pix + self.board_desc.off_board_top * self.field_height_pix
        self.board_width_pix = self.field_width_pix * self.board.get_width()
        self.board_height_pix = self.field_height_pix * self.board.get_height()

    def get_field_size_pix(self):
        w, h = self.calc_fields_canvas_size()
        i, j = self.calc_total_field_counts()

        x = w // i
        y = h // j

        m = min(x, y)
        return (m, m)

    def get_field_start_pix(self, i, j):
        left_pix = self.board_left_pix + i * self.field_width_pix
        j_reverse = self.board.get_height() - j - 1
        top_pix = self.board_top_pix + j_reverse * self.field_height_pix
        return (left_pix, top_pix)

    def is_light(self, i, j):
        rev = self.board_desc.reverse_field_colors if self.board.is_on_board(i, j) else self.board_desc.reverse_off_board_field_colors
        return xor( self.board.is_light(i, j), rev, default=False )

    def draw_field(self, i, j, cshade=None, line_width=UNDEFINED):
        assert isinstance(cshade, (ColorsShade, type(None)))

        x_pix, y_pix = self.get_field_start_pix(i, j)

        cs = cshade if self.board.is_on_board(i, j) else Colors[ BoardType.none ].field
        assert isinstance(cs, ColorsShade)

        color = cs.light if self.is_light(i, j) else cs.dark
        self.draw_rectangle(x_pix, y_pix, self.field_width_pix, self.field_height_pix, interior=color.interior, outline=None, line_width=None)

    def draw_all_fields(self, cshade=None, line_width=UNDEFINED):
        for j in range(self.fields_bottom, self.fields_top+1):
            for i in range(self.fields_left, self.fields_right+1):
                self.draw_field(i, j, cshade=cshade)

    def draw_piece_at_field(self, i, j, colors_item, line_width=UNDEFINED):
        x_pix, y_pix = self.get_field_start_pix(i, j)
        p = self.board.get_piece(i, j)
        dr = RectanglePix(x_pix, y_pix, self.field_width_pix, self.field_height_pix)
        self.draw_piece(p, dr, colors_item, line_width=line_width)

    def draw_all_pieces(self, colors_item, line_width=UNDEFINED):
        for j in range(self.board.get_height()):
            for i in range(self.board.get_width()):
                self.draw_piece_at_field(i, j, colors_item, line_width=line_width)

    def draw_board(self, colors_item, line_width=UNDEFINED):
        assert isinstance(colors_item, ColorsItem)

        self.draw_all_fields(colors_item.field, line_width=line_width)
        self.draw_all_pieces(colors_item, line_width=line_width)

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
    b = Board(BoardType.CroatianTies, width=width, height=height)
    b.setup()

    d = DrawBoard(1200, 1200, b, board_desc=board_desc)

    cs = ColorsShade.from_tuple( ('#B0B0B0', '#B0B0B0', '#404040', '#404040' ) )
    line_width = 11
    d.draw_all_fields(cshade=cs, line_width=line_width)

    file_path = 'temp/board%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2(board_desc=None, name=''):
    b = Board(BoardType.CroatianTies)
    b.setup()

    d = DrawBoard(1200, 1200, b, board_desc=board_desc)

    from colors import Colors
    line_width = 3
    d.draw_board( Colors[BoardType.Classical], line_width=line_width )

    file_path = 'temp/setup%s.IGNORE.png' % name
    d.save_image(file_path)

if __name__ == '__main__':
    test_1()
    test_1(board_desc=BoardDesc(reverse_field_colors=True), name='_reverse')
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), name='_border')
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=90, border_bottom_pix=40), name='_border_2')

    width = 3
    height = 7

    test_1(width=width, height=height)
    test_1(board_desc=BoardDesc(reverse_field_colors=True), width=width, height=height, name='_vert_reverse')
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), width=width, height=height, name='_vert_border')
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=90, border_bottom_pix=40), width=width, height=height, name='_vert_border_2')

    width = 7
    height = 3

    test_1(width=width, height=height)
    test_1(board_desc=BoardDesc(reverse_field_colors=True), width=width, height=height, name='_hor_reverse')
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), width=width, height=height, name='_hor_border')
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=90, border_bottom_pix=40), width=width, height=height, name='_hor_border_2')

    test_2()

    test_1(board_desc=BoardDesc(off_board_left=3, off_board_top=2), name='_off_board')
    test_1(board_desc=BoardDesc(off_board_right=2, off_board_bottom=3, reverse_off_board_field_colors=True), name='_off_board_2')
