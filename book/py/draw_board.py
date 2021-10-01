#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

import math

from consts import DEFAULT_LINE_WIDTH

from util import xor
from pixel_math import Rectangle, assert_floor, assert_floor_2
from board_view import Margin, BoardView
from board import BoardType, Board
from colors import ColorsShade, ColorsItem, Colors
from draw_piece import DrawPiece


class DrawBoard(DrawPiece):

    def __init__(self, board_or_type, max_width_pix, max_height_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF", board_view=None):
        assert isinstance(board_or_type, (Board, BoardType))
        assert isinstance(board_view, (BoardView, type(None)))

        self.board = board_or_type if isinstance(board_or_type, Board) else Board(board_or_type)
        self.board_view = board_view or BoardView(board_type=self.board.type)

        field_size_in_pix = self.get_field_size_pix(max_width_pix, max_height_pix)
        width_pix, height_pix = self.calc_total_size_pix(max_width_pix, max_height_pix, field_size_in_pix)
        super(DrawBoard, self).__init__(width_pix, height_pix, field_size_in_pix, line_width=line_width, color_str=color_str)

    def calc_view_field_counts(self):
        w = self.board_view.width
        h = self.board_view.height
        return (w, h)

    def calc_total_field_counts(self):
        w = self.board_view.margin.left + self.board_view.width + self.board_view.margin.right
        h = self.board_view.margin.top + self.board_view.height + self.board_view.margin.bottom
        return (w, h)

    def calc_view_canvas_size_ratios(self):
        wf, hf = self.calc_view_field_counts()
        wt, ht = self.calc_total_field_counts()
        return (wf / wt, hf / ht)

    def calc_view_canvas_size_pix(self, width_pix, height_pix):
        w, h = self.calc_view_canvas_size_ratios()
        return ( w * width_pix, h * height_pix )

    def calc_total_size_pix(self, width_pix, height_pix, field_size_in_pix):
        w, h = self.calc_total_field_counts()
        w2 = min( w * field_size_in_pix, width_pix )
        h2 = min( h * field_size_in_pix, height_pix )
        return ( math.floor(w2), math.floor(h2) )

    def get_field_size_pix(self, width_pix, height_pix):
        w, h = self.calc_view_canvas_size_pix(width_pix, height_pix)
        i, j = self.calc_view_field_counts()

        x = w / i
        y = h / j
        return min(x, y)

    def get_field_start(self, i, j):
        _i, _j = assert_floor_2(i, j)
        return self.convert_field_to_user_coords(_i, _j + 1.0)

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

    def calc_view_fields_range(self):
        x = math.floor(self.board_view.x)
        y = math.floor(self.board_view.y)
        w = math.ceil(self.board_view.width)
        h = math.ceil(self.board_view.height)
        return (x, y, w, h)

    def draw_all_fields(self, cshade=None, skip_if_rendering_board=None):
        x, y, w, h = self.calc_view_fields_range()

        for j in range(y, y + h + 1):
            for i in range(x, x + w + 1):
                if not skip_if_rendering_board or (i, j) not in skip_if_rendering_board:
                    self.draw_field(i, j, cshade=cshade)

    def draw_piece_at_field(self, i, j, colors_item):
        x, y = self.get_field_start(i, j)
        p = self.board.get_piece(i, j)
        dr = Rectangle(x, y, 1.0, 1.0)
        self.draw_piece(p, dr, colors_item)

    def draw_all_pieces(self, colors_item, skip_if_rendering_board=None):
        x, y, w, h = self.calc_view_fields_range()

        for j in range(y, y + h + 1):
            for i in range(x, x + w + 1):
                if not skip_if_rendering_board or (i, j) not in skip_if_rendering_board:
                    self.draw_piece_at_field(i, j, colors_item)

    def clip_board(self):
        x = self.board_view.margin.left
        y = self.board_view.margin.top
        w = self.board_view.width
        h = self.board_view.height
        self.set_clip(x, y, w, h)

    def draw_board(self, colors_item, skip_if_rendering_board=None):
        assert isinstance(colors_item, ColorsItem)

        sirb = skip_if_rendering_board or self.board_view.skip_if_rendering_board

        self.clip_board()
        self.draw_all_fields(colors_item.field, skip_if_rendering_board=sirb)
        self.draw_all_pieces(colors_item, skip_if_rendering_board=sirb)
        self.reset_clip()

    def convert_field_to_user_coords(self, x, y):
        _x = self.board_view.margin.left + x - self.board_view.x
        _y = self.board_view.margin.top + self.board_view.height - y + self.board_view.y
        return (_x, _y)


def test_1(board_type=BoardType.CroatianTies, board_view=None, name=''):
    bt = BoardType(board_type)
    bv = board_view or BoardView(board_type=bt)

    b = Board(bt)
    b.setup()

    d = DrawBoard(b, 1200, 1200, board_view=bv)

    from colors import Colors

    ci = Colors[ bt ]
    d.draw_board(colors_item=ci)

    file_path = 'temp/board%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2():
    for bt in BoardType.iter():
        name = ".%s" % bt.get_label()

        test_1(board_type=bt, name=name)
        test_1(board_type=bt, board_view=BoardView(margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name=name + '_margin')

        test_1(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0), name=name + '_clipped_2')
        test_1(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name=name + '_margin_2')

        test_1(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0), name=name + '_clipped_3')
        test_1(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name=name + '_margin_3')

if __name__ == '__main__':

    # bt = BoardType.One # BoardType.CroatianTies
    # test_1(board_type=bt)
    # test_1(board_type=bt, board_view=BoardView(margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin')

    # test_1(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0), name='_clipped_2')
    # test_1(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_2')

    # test_1(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0), name='_clipped_3')
    # test_1(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_3')

    test_2()
