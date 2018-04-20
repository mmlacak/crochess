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
from draw import get_new_drawable, get_new_gc, set_new_colors, DrawableRectangle, Draw
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

    DEFAULT_ARROW_INVERSE_WIDTH_RATIO = 12.0 # Compared to field size.
    DEFAULT_ARROW_POINTY_BIT_RATIO = 1.5 # Compared to arrow width. # 80.0

    DEFAULT_FONT = "sans bold"
    DEFAULT_FONT_SIZE = 192

    DEFAULT_FIELD_MARKER_INVERSE_WIDTH_RATIO = 5.0 # Compared to field size.

    def __init__(self, drawable, gc, board, board_desc=None):
        super(DrawMark, self).__init__(drawable, gc)

        self.draw_board = DrawBoard(self.drawable, self.gc, board, board_desc=board_desc)

    # arrows

    def calc_arrow(self, arrow, inv_width_ratio=None, pointy_bit_ratio=None):
        arrow.start_pix = self.draw_board.convert_field_coords_to_pixel( *arrow.start )
        arrow.end_pix = self.draw_board.convert_field_coords_to_pixel( *arrow.end )

        # inv_width_ratio - compared to field size
        inv_width_ratio = inv_width_ratio or DrawMark.DEFAULT_ARROW_INVERSE_WIDTH_RATIO

        # pointy_bit_ratio - compared to arrow width
        pointy_bit_ratio = pointy_bit_ratio or DrawMark.DEFAULT_ARROW_POINTY_BIT_RATIO

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

    # text

    @staticmethod
    def get_font(font=None):
        if font is None:
            return "%s %d" % (DrawMark.DEFAULT_FONT, DrawMark.DEFAULT_FONT_SIZE)
        else:
            return font

    def draw_text(self, text, cpair=None, gc=None, font=None):
        assert isinstance(text, Text)
        # assert isinstance(cpair, (ColorsPair, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))
        # assert isinstance(font, (str, NoneType))

        x, y = text.pos_pix = self.draw_board.convert_field_coords_to_pixel( *text.pos )

        font = DrawMark.get_font(font)
        gc = set_new_colors(gc or self.gc, fg=cpair.interior, bg=cpair.outline)

        screen = gtk.gdk.screen_get_default()
        pango_ctx = gtk.gdk.pango_context_get_for_screen(screen)

        layout = pango.Layout(pango_ctx)
        layout.set_text(text.text)

        font_desc = pango.FontDescription(font)
        layout.set_font_description(font_desc)

        x, y = pm.round_floats_to_int((x, y))
        self.drawable.draw_layout(gc, x, y, layout)

    def draw_all_texts(self, texts, cmark=None, gc=None, font=None):
        assert isinstance(cmark, (ColorsMark, NoneType))

        for text in texts:
            # assert isinstance(text, Text)

            cpair = get_mark_color_pair(cmark=cmark, mark_type=text.mark_type)

            self.draw_text(text, cpair=cpair, gc=gc, font=font)

    # field marker

    def calc_field_marker(self, field_marker, inv_width_ratio=None):
        assert isinstance(field_marker, FieldMarker)
        # assert isinstance(inv_width_ratio, (float, NoneType))

        # inv_width_ratio - compared to field size
        inv_width_ratio = inv_width_ratio or DrawMark.DEFAULT_FIELD_MARKER_INVERSE_WIDTH_RATIO

        width_ratio = 1.0 / inv_width_ratio
        width_pix = self.draw_board.convert_field_width_to_pixel(width_ratio)

        x_pix, y_pix = self.draw_board.get_field_start_pix(*field_marker.field)
        upper_left_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix + width_pix, y_pix), (x_pix, y_pix + width_pix) ])

        x_pix, y_pix = x_pix + self.draw_board.field_width_pix, y_pix
        upper_right_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix - width_pix, y_pix), (x_pix, y_pix + width_pix) ])

        x_pix, y_pix = x_pix, y_pix + self.draw_board.field_height_pix
        lower_right_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix - width_pix, y_pix), (x_pix, y_pix - width_pix) ])

        x_pix, y_pix = x_pix - self.draw_board.field_width_pix, y_pix
        lower_left_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix + width_pix, y_pix), (x_pix, y_pix - width_pix) ])

        field_markers_pix = [ upper_left_triangle, upper_right_triangle, lower_right_triangle, lower_left_triangle ]

        return field_markers_pix

    def draw_field_marker(self, field_marker, cpair=None, gc=None, draw_outlined=False, inv_width_ratio=None):
        assert isinstance(field_marker, FieldMarker)
        # assert isinstance(cpair, (ColorsPair, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))
        # assert isinstance(inv_width_ratio, (float, NoneType))

        markers_pix = self.calc_field_marker(field_marker, inv_width_ratio=inv_width_ratio)

        for points_pix in markers_pix:
            if cpair is not None:
                if draw_outlined:
                    self.draw_outlined_polygon(points_pix, interior=cpair.interior, outline=cpair.outline, gc=gc)
                else:
                    self.draw_polygon(points_pix, fg=cpair.interior, bg=cpair.outline, gc=gc)
            else:
                if draw_outlined:
                    self.draw_outlined_polygon(points_pix, gc=gc)
                else:
                    self.draw_polygon(points_pix, gc=gc)

    def draw_all_field_markers(self, field_markers, cmark=None, gc=None, draw_outlined=False, inv_width_ratio=None):
        assert isinstance(cmark, (ColorsMark, NoneType))

        for field_marker in field_markers:
            # assert isinstance(field_marker, FieldMarker)

            cpair = get_mark_color_pair(cmark=cmark, mark_type=field_marker.mark_type)

            self.draw_field_marker(field_marker, cpair=cpair, gc=gc, draw_outlined=draw_outlined, inv_width_ratio=inv_width_ratio)


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
             Arrow(6.7, 4.3, 9.7, 4.3, mark_type=MarkType(MarkType.Legal)), \
             Arrow(2.7, 6.3, 2.7, 9.3, mark_type=MarkType(MarkType.Legal)), \
             Arrow(4.3, 5.4, 6.7, 0.9, mark_type=MarkType(MarkType.Legal)), ]
    d.draw_all_arrows(arws, cmark=Colors[BoardType.Classical].arrow)

    arws2 = [ Arrow(100, 200, 100, 400, mark_type=MarkType(MarkType.Action)), \
              Arrow(300, 900, 400, 900, mark_type=MarkType(MarkType.Action)), \
              Arrow(600, 500, 800, 600, mark_type=MarkType(MarkType.Action)), \
              Arrow(800, 300, 700, 100, mark_type=MarkType(MarkType.Action)), ]
    d.draw_all_arrows(arws2, cmark=Colors[BoardType.Classical].arrow)

    file_path = 'temp/arrows%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2(board_desc=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 3)
    b = Board(BoardType.CroatianTies)
    b.setup()

    d = DrawMark(drw, gc, b, board_desc=board_desc)
    d.draw_board.clear_area()

    from colors import Colors
    d.draw_board.draw_board( Colors[BoardType.Classical] )

    txts = [ Text("l1", 1.7, 2.3, mark_type=MarkType(MarkType.Legal)), \
             Text("i1", 6.7, 4.3, mark_type=MarkType(MarkType.Ilegal)), \
             Text("a1", 2.7, 6.3, mark_type=MarkType(MarkType.Action)), \
             Text("f1", 4.3, 5.4, mark_type=MarkType(MarkType.Forbidden)),  ]
    d.draw_all_texts(txts, cmark=Colors[BoardType.Classical].text, font="sans bold 16")

    txts2 = [ Text("l2", 100, 200, mark_type=MarkType(MarkType.Legal)), \
              Text("i2", 300, 900, mark_type=MarkType(MarkType.Ilegal)), \
              Text("a2", 600, 500, mark_type=MarkType(MarkType.Action)), \
              Text("f2", 800, 300, mark_type=MarkType(MarkType.Forbidden)), ]
    d.draw_all_texts(txts2, cmark=Colors[BoardType.Classical].text, font="sans bold 16")

    file_path = 'temp/texts%s.IGNORE.png' % name
    d.save_image(file_path)

def test_3(board_desc=None, name=''):
    drw = get_new_drawable(1200, 1200)
    gc = get_new_gc(drw, 3)
    b = Board(BoardType.CroatianTies)
    b.setup()

    d = DrawMark(drw, gc, b, board_desc=board_desc)
    d.draw_board.clear_area()

    from colors import Colors
    d.draw_board.draw_board( Colors[BoardType.Classical] )

    fms = [ FieldMarker(1, 2, mark_type=MarkType(MarkType.Legal)), \
            FieldMarker(6, 4, mark_type=MarkType(MarkType.Ilegal)), \
            FieldMarker(2, 6, mark_type=MarkType(MarkType.Action)), \
            FieldMarker(4, 5, mark_type=MarkType(MarkType.Forbidden)),  ]
    d.draw_all_field_markers(fms, cmark=Colors[BoardType.Classical].marker)

    fms2 = [ FieldMarker(2, 1, mark_type=MarkType(MarkType.Legal)), \
             FieldMarker(7, 5, mark_type=MarkType(MarkType.Ilegal)), \
             FieldMarker(3, 7, mark_type=MarkType(MarkType.Action)), \
             FieldMarker(5, 4, mark_type=MarkType(MarkType.Forbidden)),  ]
    d.draw_all_field_markers(fms2, cmark=Colors[BoardType.Classical].marker, draw_outlined=True, inv_width_ratio=3.5)

    file_path = 'temp/markers%s.IGNORE.png' % name
    d.save_image(file_path)

if __name__ == '__main__':
    test_1()
    test_1(board_desc=BoardDesc(margin_top_pix=210, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=40), name='_margin')
    test_1(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=120, margin_right_pix=30, margin_bottom_pix=40), name='_margin_2')

    test_2()
    test_2(board_desc=BoardDesc(margin_top_pix=210, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=40), name='_margin')
    test_2(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=120, margin_right_pix=30, margin_bottom_pix=40), name='_margin_2')

    test_3()
    test_3(board_desc=BoardDesc(margin_top_pix=210, margin_left_pix=20, margin_right_pix=30, margin_bottom_pix=40), name='_margin')
    test_3(board_desc=BoardDesc(margin_top_pix=10, margin_left_pix=120, margin_right_pix=30, margin_bottom_pix=40), name='_margin_2')
