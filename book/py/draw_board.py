#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from util import xor
from pixel_math import Rectangle, assert_floor, assert_floor_2
from board_view import BoardView
from board import BoardType, Board
from colors import ColorsShade, ColorsItem, Colors
from draw import DEFAULT_LINE_WIDTH # Draw
from draw_piece import DrawPiece


class DrawBoard(DrawPiece):

    def __init__(self, board_or_type, width_pix, height_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF", board_view=None):
        assert isinstance(board_or_type, (Board, BoardType))
        assert isinstance(board_view, (BoardView, type(None)))

        self.board = board_or_type if isinstance(board_or_type, Board) else Board(board_or_type)
        self.board_view = board_view or BoardView(board_type=self.board.type)

        field_size_in_pix = self.get_field_size_pix(width_pix, height_pix)
# TODO :: recalculate canvas size in pixels
        super(DrawBoard, self).__init__(width_pix, height_pix, field_size_in_pix, line_width=line_width, color_str=color_str)

    def calc_view_field_counts(self):
        w = self.board_view.width
        h = self.board_view.height
        return (w, h)

    def calc_view_canvas_size_ratios(self):
        w = self.board_view.width / (self.board_view.border.left + self.board_view.width + self.board_view.border.right)
        h = self.board_view.height / (self.board_view.border.top + self.board_view.height + self.board_view.border.bottom)
        return (w, h)

    def calc_view_canvas_size_pix(self, width_pix, height_pix):
        w, h = self.calc_view_canvas_size_ratios()
        return ( w * width_pix, h * height_pix )

    def get_field_size_pix(self, width_pix, height_pix):
        w, h = self.calc_view_canvas_size_pix(width_pix, height_pix)
        i, j = self.calc_view_field_counts()

        x = w / i
        y = h / j
        return min(x, y)

    def get_field_start(self, i, j):
        _i, _j = assert_floor_2(i, j)

        left = self.board_view.border.left + _i - self.board_view.x

        j_reverse = self.board.get_height() - 1.0 - _j + self.board_view.y
        top = self.board_view.border.top + j_reverse

        return (left, top)

    def is_light(self, i, j):
        _i, _j = assert_floor_2(i, j)
        rev = False if self.board.is_on_board(_i, _j) else self.board_view.reverse_off_board_field_colors
        return xor( self.board.is_light(_i, _j), rev, default=False )

    def draw_field(self, i, j, cshade=None):
        assert isinstance(cshade, (ColorsShade, type(None)))

        x, y = self.get_field_start(i, j)
        cs = cshade if self.board.is_on_board(i, j) else Colors[ BoardType.none ].field
        color = cs.light if self.is_light(i, j) else cs.dark
        self.draw_rectangle(x, y, 1.0, 1.0, interior_str=color.interior)

    def draw_all_fields(self, cshade=None):
        for j in range(self.board.get_height()):
            for i in range(self.board.get_width()):
                self.draw_field(i, j, cshade=cshade)

    def draw_piece_at_field(self, i, j, colors_item):
        # _i, _j = assert_floor_2(i, j)
        x, y = self.get_field_start(i, j)
        p = self.board.get_piece(i, j)
        dr = Rectangle(x, y, 1.0, 1.0)
        self.draw_piece(p, dr, colors_item)

    def draw_all_pieces(self, colors_item):
        for j in range(self.board.get_height()):
            for i in range(self.board.get_width()):
                self.draw_piece_at_field(i, j, colors_item)

    def draw_board(self, colors_item):
        assert isinstance(colors_item, ColorsItem)

        self.draw_all_fields(colors_item.field)
        self.draw_all_pieces(colors_item)

    # def convert_field_width_to_pixel(self, x):
    #     if isinstance(x, float):
    #         return x * self.field_size_in_pix
    #     return x

    # def convert_field_height_to_pixel(self, y):
    #     if isinstance(y, float):
    #         y_reverse = self.board.get_height() - y # - 1.0
    #         return y_reverse * self.field_size_in_pix
    #     return y

    # def convert_field_x_to_pixel(self, x):
    #     if isinstance(x, float):
    #         return self.board_left_pix + x * self.field_size_in_pix
    #     return x

    # def convert_field_y_to_pixel(self, y):
    #     if isinstance(y, float):
    #         y_reverse = self.board.get_height() - y # - 1.0
    #         return self.board_top_pix + y_reverse * self.field_size_in_pix
    #     return y

    # def convert_field_coords_to_pixel(self, x, y):
    #     x_pix = self.convert_field_x_to_pixel(x)
    #     y_pix = self.convert_field_y_to_pixel(y)
    #     return (x_pix, y_pix)


def test_1(board_view=None, name=''):
    bt = BoardType(BoardType.CroatianTies)
    bv = board_view or BoardView(board_type=bt)

    b = Board(bt)
    b.setup()

    d = DrawBoard(b, 1200, 1200, board_view=bv)

    from colors import Colors

    ci = Colors[ bt ]
    cs = ci.field

    d.draw_all_fields(cshade=cs)
    d.draw_all_pieces(colors_item=ci)

    file_path = 'temp/board%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2(board_view=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 3)
    b = Board(BoardType.CroatianTies)
    b.setup()

    d = DrawBoard(drw, gc, b, board_view=board_view)

    from colors import Colors
    d.draw_board( Colors[BoardType.Classical] )

    file_path = 'temp/setup%s.IGNORE.png' % name
    d.save_image(file_path)

if __name__ == '__main__':

    test_1()
    test_1(board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0), name='_clipped')
    test_1(board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0), name='_clipped_2')
    # test_1(board_view=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), name='_border')
    # test_1(board_view=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=90, border_bottom_pix=40), name='_border_2')

    # width = 3
    # height = 7

    # test_1(width=width, height=height)
    # test_1(board_view=BoardDesc(reverse_field_colors=True), width=width, height=height, name='_vert_reverse')
    # test_1(board_view=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), width=width, height=height, name='_vert_border')
    # test_1(board_view=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=90, border_bottom_pix=40), width=width, height=height, name='_vert_border_2')

    # width = 7
    # height = 3

    # test_1(width=width, height=height)
    # test_1(board_view=BoardDesc(reverse_field_colors=True), width=width, height=height, name='_hor_reverse')
    # test_1(board_view=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), width=width, height=height, name='_hor_border')
    # test_1(board_view=BoardDesc(border_left_pix=20, border_top_pix=10, border_right_pix=90, border_bottom_pix=40), width=width, height=height, name='_hor_border_2')

    # test_2()

    # test_1(board_view=BoardDesc(off_board_left=3, off_board_top=2), name='_off_board')
    # test_1(board_view=BoardDesc(off_board_right=2, off_board_bottom=3, reverse_off_board_field_colors=True), name='_off_board_2')
