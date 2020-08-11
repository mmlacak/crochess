#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

import gtk.gdk
import pango

import pixel_math as pm
from mark import MarkType, Arrow, Text, FieldMarker
import def_mark as dm

from colors import ColorsPair, ColorsShade, ColorsMark, ColorsMarkSimple
from draw import set_new_colors, Draw
from draw_board import DrawBoard


def get_mark_color_pair(cmark=None, mark_type=None, is_light=None):
    if cmark is None:
        return None

    if mark_type is None:
        return None

    if is_light is None and isinstance(cmark, ColorsMark):
        return None

    assert isinstance(cmark, (ColorsMark, ColorsMarkSimple))
    assert isinstance(mark_type, MarkType)
    assert isinstance(is_light, (bool, NoneType))

    _map = { MarkType.none : None, \
             MarkType.Legal : cmark.legal, \
             MarkType.Illegal : cmark.illegal, \
             MarkType.Action : cmark.action, \
             MarkType.Blocked : cmark.blocked }

    cpair = cshade = _map[ mark_type ]

    if isinstance(cshade, ColorsShade):
        cpair = cshade.light if is_light else cshade.dark

    return cpair


class DrawMark(Draw):

    def __init__(self, drawable, gc, board, board_desc=None):
        super(DrawMark, self).__init__(drawable, gc)

        self.draw_board = DrawBoard(self.drawable, self.gc, board, board_desc=board_desc)

    #
    # arrows

    def calc_arrow(self, arrow, adef=None):
        assert isinstance(arrow, Arrow)
        assert isinstance(adef, (dm.ArrowDef, NoneType))

        _adef = adef or dm.MarkDef[ self.draw_board.board.type ].arrow_def

        arrow.start_pix = self.draw_board.convert_field_coords_to_pixel( *arrow.start )
        arrow.end_pix = self.draw_board.convert_field_coords_to_pixel( *arrow.end )

        # inv_width_ratio - compared to field size
        inv_width_ratio = _adef.inv_width_ratio # if _adef is not None else dm.DEFAULT_ARROW_INVERSE_WIDTH_RATIO

        # pointy_bit_ratio - compared to arrow width
        pointy_bit_ratio = _adef.pointy_bit_ratio # if _adef is not None else dm.DEFAULT_ARROW_POINTY_BIT_RATIO

        width = self.draw_board.field_width_pix / inv_width_ratio
        distance = width / 2.0
        length = pm.calc_line_length(arrow.start_pix, arrow.end_pix)

        arrow_size = pointy_bit_ratio * width # self.board_width_pix / pointy_bit_ratio
        line_division_ratio = abs((length - arrow_size) / arrow_size) # Shouldn't be negative, i.e. outside line segment.

        def _calc_end(do_start):
            if do_start:
                _point_pix, _other_pix, _pointer = arrow.start_pix, arrow.end_pix, arrow.start_pointer
            else:
                _point_pix, _other_pix, _pointer = arrow.end_pix, arrow.start_pix, arrow.end_pointer

            if _pointer:
                _mid_point = pm.calc_division_point(_other_pix, _point_pix, line_division_ratio)
                _inner = pm.calc_distant_points_on_inverse_line(_mid_point, _point_pix, distance)
                _outer = pm.calc_distant_points_on_inverse_line(_mid_point, _point_pix, arrow_size)
                _lst = [ _inner[1], _outer[1], _point_pix, _outer[0], _inner[0] ]
            else:
                _lst = pm.calc_distant_points_on_inverse_line(_point_pix, _other_pix, distance)

            _lst_out = [ (int(tpl[0]), int(tpl[1])) for tpl in _lst ]
            return _lst_out

        start_lst = _calc_end(True)
        end_lst =  _calc_end(False)
        return start_lst + end_lst

    def draw_arrow(self, arrow, adef=None, cpair=None, gc=None):
        # assert isinstance(arrow, Arrow)
        # assert isinstance(adef, (ArrowDef, NoneType))
        assert isinstance(cpair, (ColorsPair, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))

        points_pix = self.calc_arrow(arrow, adef=adef)

        if cpair is not None:
            self.draw_polygon(points_pix, interior=cpair.interior, outline=cpair.outline, gc=gc)
        else:
            self.draw_polygon(points_pix, gc=gc)

    def draw_all_arrows(self, arrows, adef=None, cmark=None, gc=None):
        # assert isinstance(adef, (ArrowDef, NoneType))
        # assert isinstance(cmark, (ColorsMarkSimple, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))

        for arrow in arrows:
            # assert isinstance(arrow, Arrow)

            cpair = get_mark_color_pair(cmark=cmark, mark_type=arrow.mark_type)

            self.draw_arrow(arrow, adef=adef, cpair=cpair, gc=gc)

    #
    # text

    def draw_text(self, text, fdef=None, cpair=None, gc=None):
        assert isinstance(text, Text)
        assert isinstance(fdef, (dm.FontDef, NoneType))
        assert isinstance(cpair, (ColorsPair, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))

        fdef = fdef or dm.MarkDef[ self.draw_board.board.type ].font_def

        x, y = text.pos_pix = self.draw_board.convert_field_coords_to_pixel( *text.pos )

        font = fdef.get_font(self.draw_board.field_height_pix)
        if cpair is not None:
            gc = set_new_colors(gc or self.gc, fg=cpair.interior, bg=cpair.outline)
        else:
            gc = set_new_colors(gc or self.gc)

        screen = gtk.gdk.screen_get_default()
        pango_ctx = gtk.gdk.pango_context_get_for_screen(screen)

        layout = pango.Layout(pango_ctx)
        layout.set_text(text.text)

        font_desc = pango.FontDescription(font)
        layout.set_font_description(font_desc)

        x, y = pm.round_floats_to_int((x, y))
        self.drawable.draw_layout(gc, x, y, layout)

    def draw_all_texts(self, texts, fdef=None, cmark=None, gc=None):
        assert isinstance(cmark, (ColorsMark, NoneType))

        for text in texts:
            # assert isinstance(text, Text)

            x = int( text.pos[ 0 ] )
            y = int( text.pos[ 1 ] )

            # Top border is + 1.0 to field y coord --> int() yields coord of first field to the north,
            # i.e. wrong one, because it's exactly of the opposite color.
            x = x if float(x) < float(text.pos[ 0 ]) else x - 1
            y = y if float(y) < float(text.pos[ 1 ]) else y - 1

            is_light = self.draw_board.is_light( x, y )

            cpair = get_mark_color_pair(cmark=cmark, mark_type=text.mark_type, is_light=is_light)

            self.draw_text(text, fdef=fdef, cpair=cpair, gc=gc)

    #
    # field marker

    def calc_field_marker(self, field_marker, fmdef=None):
        assert isinstance(field_marker, FieldMarker)
        assert isinstance(fmdef, (dm.FieldMarkerDef, NoneType))

        fmdef = fmdef or dm.MarkDef[ self.draw_board.board.type ].field_mark_def

        # inv_width_ratio - compared to field size
        inv_width_ratio = fmdef.inv_width_ratio if fmdef is not None else dm.DEFAULT_FIELD_MARKER_INVERSE_WIDTH_RATIO

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

    def draw_field_marker(self, field_marker, fmdef=None, cpair=None, gc=None, draw_outlined=False):
        # assert isinstance(field_marker, FieldMarker)
        # assert isinstance(fmdef, (dm.FieldMarkerDef, NoneType))
        assert isinstance(cpair, (ColorsPair, NoneType))
        # assert isinstance(gc, (gtk.gdk.GC, NoneType))

        markers_pix = self.calc_field_marker(field_marker, fmdef=fmdef)

        for points_pix in markers_pix:
            if cpair is not None:
                if draw_outlined:
                    self.draw_polygon(points_pix, interior=cpair.interior, outline=cpair.outline, gc=gc)
                else:
                    self.draw_polygon(points_pix, fg=cpair.interior, bg=cpair.outline, gc=gc)
            else:
                if draw_outlined:
                    self.draw_polygon(points_pix, gc=gc)
                else:
                    self.draw_polygon(points_pix, gc=gc)

    def draw_all_field_markers(self, field_markers, fmdef=None, cmark=None, gc=None, draw_outlined=False):
        assert isinstance(cmark, (ColorsMark, NoneType))

        for field_marker in field_markers:
            # assert isinstance(field_marker, FieldMarker)

            is_light = self.draw_board.is_light( *field_marker.field )

            cpair = get_mark_color_pair(cmark=cmark, mark_type=field_marker.mark_type, is_light=is_light)

            self.draw_field_marker(field_marker, fmdef=fmdef, cpair=cpair, gc=gc, draw_outlined=draw_outlined)


TEST_BOARD_SIZE_PIX = 2400 # 9600 # 1200
TEST_LINE_WIDTH = 3 # 11 # 3

def test_1(board_desc=None, name=''):
    drw = get_new_drawable(TEST_BOARD_SIZE_PIX, TEST_BOARD_SIZE_PIX)
    gc = get_new_gc(drw, TEST_LINE_WIDTH)
    b = Board(BoardType.One) # CroatianTies)
    b.setup()

    d = DrawMark(drw, gc, b, board_desc=board_desc)

    from colors import Colors
    d.draw_board.draw_board( Colors[BoardType.Classical] )

    arws = [ Arrow(1.7, 2.3, 3.4, 5.1, mark_type=MarkType(MarkType.Legal), start_pointer=True, end_pointer=True), \
             Arrow(6.7, 4.3, 9.7, 4.3, mark_type=MarkType(MarkType.Legal)), \
             Arrow(2.7, 6.3, 2.7, 9.3, mark_type=MarkType(MarkType.Legal)), \
             Arrow(4.3, 5.4, 6.7, 0.9, mark_type=MarkType(MarkType.Legal), start_pointer=False, end_pointer=False), ]
    d.draw_all_arrows(arws, cmark=Colors[BoardType.Classical].arrow)

    arws2 = [ Arrow(100, 200, 100, 400, mark_type=MarkType(MarkType.Action)), \
              Arrow(300, 900, 400, 900, mark_type=MarkType(MarkType.Action)), \
              Arrow(600, 500, 800, 600, mark_type=MarkType(MarkType.Action), start_pointer=True, end_pointer=True), \
              Arrow(800, 300, 700, 100, mark_type=MarkType(MarkType.Action), start_pointer=False, end_pointer=False), ]
    d.draw_all_arrows(arws2, cmark=Colors[BoardType.Classical].arrow)

    file_path = 'temp/arrows%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2(board_desc=None, name=''):
    drw = get_new_drawable(TEST_BOARD_SIZE_PIX, TEST_BOARD_SIZE_PIX)
    gc = get_new_gc(drw, TEST_LINE_WIDTH)
    b = Board(BoardType.One) # CroatianTies)
    b.setup()

    d = DrawMark(drw, gc, b, board_desc=board_desc)

    from colors import Colors
    d.draw_board.draw_board( Colors[BoardType.Classical] )

    fdef = dm.FontDef('sans bold', 3.5)
    txts = [ Text("l1", 1.7, 2.3, mark_type=MarkType(MarkType.Legal)), \
             Text("i1", 6.7, 4.3, mark_type=MarkType(MarkType.Illegal)), \
             Text("a1", 2.7, 6.3, mark_type=MarkType(MarkType.Action)), \
             Text("f1", 4.3, 5.4, mark_type=MarkType(MarkType.Blocked)), \
             Text("WW", 5.3, 5.4, mark_type=MarkType(MarkType.Blocked)), \
             Text("MM", 4.3, 6.4, mark_type=MarkType(MarkType.Blocked)), ]
    d.draw_all_texts(txts, fdef=fdef, cmark=Colors[BoardType.Classical].text)

    fdef2 = dm.FontDef('sans bold', 4.5)
    txts2 = [ Text("l2", 100, 200, mark_type=MarkType(MarkType.Legal)), \
              Text("i2", 300, 900, mark_type=MarkType(MarkType.Illegal)), \
              Text("a2", 600, 500, mark_type=MarkType(MarkType.Action)), \
              Text("f2", 800, 300, mark_type=MarkType(MarkType.Blocked)), \
              Text("WW", 300, 300, mark_type=MarkType(MarkType.Blocked)),\
              Text("MM", 200, 500, mark_type=MarkType(MarkType.Blocked)), ]
    d.draw_all_texts(txts2, fdef=fdef2, cmark=Colors[BoardType.Classical].text)

    file_path = 'temp/texts%s.IGNORE.png' % name
    d.save_image(file_path)

def test_3(board_desc=None, name=''):
    drw = get_new_drawable(TEST_BOARD_SIZE_PIX, TEST_BOARD_SIZE_PIX)
    gc = get_new_gc(drw, TEST_LINE_WIDTH)
    b = Board(BoardType.One) # CroatianTies)
    b.setup()

    d = DrawMark(drw, gc, b, board_desc=board_desc)

    from colors import Colors
    d.draw_board.draw_board( Colors[BoardType.Classical] )

    fms = [ FieldMarker(1, 2, mark_type=MarkType(MarkType.Legal)), \
            FieldMarker(6, 4, mark_type=MarkType(MarkType.Illegal)), \
            FieldMarker(2, 6, mark_type=MarkType(MarkType.Action)), \
            FieldMarker(4, 5, mark_type=MarkType(MarkType.Blocked)),  ]
    d.draw_all_field_markers(fms, cmark=Colors[BoardType.Classical].marker)

    fmdef = dm.FieldMarkerDef(7.0)
    fms2 = [ FieldMarker(2, 1, mark_type=MarkType(MarkType.Legal)), \
             FieldMarker(7, 5, mark_type=MarkType(MarkType.Illegal)), \
             FieldMarker(3, 7, mark_type=MarkType(MarkType.Action)), \
             FieldMarker(5, 4, mark_type=MarkType(MarkType.Blocked)),  ]
    d.draw_all_field_markers(fms2, fmdef=fmdef, cmark=Colors[BoardType.Classical].marker, draw_outlined=True)

    file_path = 'temp/markers%s.IGNORE.png' % name
    d.save_image(file_path)

if __name__ == '__main__':
    from board import BoardType, Board
    from board_desc import BoardDesc
    from draw import get_new_drawable, get_new_gc

    test_1()
    test_1(board_desc=BoardDesc(border_left_pix=20, border_top_pix=210, border_right_pix=30, border_bottom_pix=40), name='_border')
    test_1(board_desc=BoardDesc(border_left_pix=120, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), name='_border_2')

    test_2()
    test_2(board_desc=BoardDesc(border_left_pix=20, border_top_pix=210, border_right_pix=30, border_bottom_pix=40), name='_border')
    test_2(board_desc=BoardDesc(border_left_pix=120, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), name='_border_2')

    test_3()
    test_3(board_desc=BoardDesc(border_left_pix=20, border_top_pix=210, border_right_pix=30, border_bottom_pix=40), name='_border')
    test_3(board_desc=BoardDesc(border_left_pix=120, border_top_pix=10, border_right_pix=30, border_bottom_pix=40), name='_border_2')
