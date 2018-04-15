#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

import gtk.gdk
import pango

import pixel_math as pm
from board import BoardType, Board
from mark import MarkType, Arrow, Text, FieldMarker
from colors import ColorsPair, ColorsShade, ColorsPiece, ColorsMark, ColorsItem
from draw import get_new_drawable, get_new_gc, DrawableRectangle, Draw
from draw_board import BoardDesc, DrawBoard


def get_mark_color_pair(cmark=None, mark_type=None):
    if cmark is None:
        return None

    if mark_type is None:
        return None

    assert isinstance(cmark, ColorsMark)
    assert isinstance(mark_type, MarkType)

    _map = { MarkType.none : None, \
             MarkType.Legal : cmark.legal, \
             MarkType.Ilegal : cmark.ilegal, \
             MarkType.Action : cmark.action, \
             MarkType.Forbidden : cmark.forbidden }

    return _map[ mark_type ]


class DrawMark(Draw):

    DEFAULT_INVERSE_ARROW_WIDTH_RATIO = 12.0 # Compared to field size.
    DEFAULT_POINTY_ARROW_BIT_RATIO = 1.5 # Compared to arrow width. # 80.0

    def __init__(self, drawable, gc, board, board_desc=None):
        super(DrawMark, self).__init__(drawable, gc)

        self.draw_board = DrawBoard(self.drawable, self.gc, board, board_desc=board_desc)

    def calc_arrow(self, arrow, inv_width_ratio=None, pointy_bit_ratio=None):
        arrow.start_pix = self.draw_board.convert_field_coords_to_pixel( *arrow.start )
        arrow.end_pix = self.draw_board.convert_field_coords_to_pixel( *arrow.end )

        # inv_width_ratio - compared to field size
        inv_width_ratio = inv_width_ratio or DrawMark.DEFAULT_INVERSE_ARROW_WIDTH_RATIO

        # pointy_bit_ratio - compared to arrow width
        pointy_bit_ratio = pointy_bit_ratio or DrawMark.DEFAULT_POINTY_ARROW_BIT_RATIO

        width = self.draw_board.field_width_pix / inv_width_ratio
        distance = width / 2.0
        length = pm.calc_line_length(arrow.start_pix, arrow.end_pix)

        start_lst = pm.calc_distant_points_on_inverse_line(arrow.start_pix, arrow.end_pix, distance)

        arrow_size = pointy_bit_ratio * width # self.board_width_pix / pointy_bit_ratio
        line_division_ratio = abs((length - arrow_size) / arrow_size) # Shouldn't be negative, i.e. outside line segment.
        mid_point = pm.calc_division_point(arrow.start_pix, arrow.end_pix, line_division_ratio)

        mid_lst = pm.calc_distant_points_on_inverse_line(mid_point, arrow.start_pix, distance)
        mid_lst_2 = pm.calc_distant_points_on_inverse_line(mid_point, arrow.start_pix, arrow_size)

        arrow_lst = [ arrow.end_pix, mid_lst_2[0], mid_lst[0], start_lst[0], start_lst[1], mid_lst[1], mid_lst_2[1] ]
        arrow_lst_2 = [ (int(tpl[0]), int(tpl[1])) for tpl in arrow_lst ]
        return arrow_lst_2

    def draw_arrow(self, arrow, cpair=None, gc=None, \
                   inv_width_ratio=None, \
                   pointy_bit_ratio=None):
        assert isinstance(arrow, Arrow)
        # assert isinstance(cpair, (ColorsPair, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))
        assert isinstance(inv_width_ratio, (float, NoneType))
        assert isinstance(pointy_bit_ratio, (float, NoneType))

        points_pix = self.calc_arrow(arrow, inv_width_ratio=inv_width_ratio, pointy_bit_ratio=pointy_bit_ratio)

        if cpair is not None:
            self.draw_outlined_polygon(points_pix, interior=cpair.interior, outline=cpair.outline, gc=gc)
        else:
            self.draw_outlined_polygon(points_pix, gc=gc)

    def draw_all_arrows(self, arrows, cmark=None, gc=None, \
                        inv_width_ratio=None, \
                        pointy_bit_ratio=None):
        assert isinstance(cmark, (ColorsMark, NoneType))

        for arrow in arrows:
            # assert isinstance(arrow, Arrow)

            cpair = get_mark_color_pair(cmark=cmark, mark_type=arrow.mark_type)

            self.draw_arrow(arrow, cpair=cpair, gc=gc, \
                            inv_width_ratio=inv_width_ratio, \
                            pointy_bit_ratio=pointy_bit_ratio)


def test_1(board_desc=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 3)
    b = Board(BoardType.CroatianTies)
    b.setup()

    d = DrawMark(drw, gc, b, board_desc=board_desc)
    d.draw_board.clear_area()

    from colors import Colors
    d.draw_board.draw_board( Colors[BoardType.Classical] )

    arws = [ Arrow(1.7, 2.3, 3.4, 5.1, mark_type=MarkType(MarkType.Legal)), \
             Arrow(6.7, 4.3, 9.7, 4.3, mark_type=MarkType(MarkType.Ilegal)), \
             Arrow(2.7, 6.3, 2.7, 9.3, mark_type=MarkType(MarkType.Action)), \
             Arrow(4.3, 5.4, 6.7, 0.9, mark_type=MarkType(MarkType.Forbidden)), ]
    d.draw_all_arrows(arws, cmark=Colors[BoardType.Classical].arrow)

    arws2 = [ Arrow(100, 200, 100, 400, mark_type=MarkType(MarkType.Legal)), \
              Arrow(300, 900, 400, 900, mark_type=MarkType(MarkType.Ilegal)), \
              Arrow(600, 500, 800, 600, mark_type=MarkType(MarkType.Action)), \
              Arrow(800, 300, 700, 100, mark_type=MarkType(MarkType.Forbidden)), ]
    d.draw_all_arrows(arws2, cmark=Colors[BoardType.Classical].arrow)

    file_path = 'temp/arrows%s.IGNORE.png' % name
    d.save_image(file_path)

if __name__ == '__main__':
    test_1()
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=130, margin_bottom_pix=40), name='_margin')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=240), name='_margin_2')
