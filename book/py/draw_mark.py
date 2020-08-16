#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


import pixel_math as pm
from mark import MarkType, Arrow, Text, FieldMarker
import def_mark as dm

from colors import ColorsPair, ColorsShade, ColorsMark, ColorsMarkSimple
from board import BoardType, Board
from board_view import Margin, BoardView
from draw import Draw
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
    assert isinstance(is_light, (bool, type(None)))

    _map = { MarkType.none : None, \
             MarkType.Legal : cmark.legal, \
             MarkType.Illegal : cmark.illegal, \
             MarkType.Action : cmark.action, \
             MarkType.Blocked : cmark.blocked }

    cpair = cshade = _map[ mark_type ]

    if isinstance(cshade, ColorsShade):
        cpair = cshade.light if is_light else cshade.dark

    return cpair


class DrawMark(DrawBoard):

    #
    # arrows

    def calc_arrow(self, arrow, adef=None):
        assert isinstance(arrow, Arrow)
        assert isinstance(adef, (dm.ArrowDef, type(None)))

        _adef = adef or dm.MarkDef[ self.board.type ].arrow_def

        # inv_width_ratio - compared to field size
        inv_width_ratio = _adef.inv_width_ratio # if _adef is not None else dm.DEFAULT_ARROW_INVERSE_WIDTH_RATIO

        # pointy_bit_ratio - compared to arrow width
        pointy_bit_ratio = _adef.pointy_bit_ratio # if _adef is not None else dm.DEFAULT_ARROW_POINTY_BIT_RATIO

        width = 1.0 / inv_width_ratio
        distance = width / 2.0
        length = pm.calc_line_length(arrow.start, arrow.end)

        arrow_size = pointy_bit_ratio * width
        line_division_ratio = abs((length - arrow_size) / arrow_size) # Shouldn't be negative, i.e. outside line segment.

        def _calc_end(do_start):
            if do_start:
                _point, _other, _pointer = arrow.start, arrow.end, arrow.start_pointer
            else:
                _point, _other, _pointer = arrow.end, arrow.start, arrow.end_pointer

            if _pointer:
                _mid_point = pm.calc_division_point(_other, _point, line_division_ratio)
                _inner = pm.calc_distant_points_on_inverse_line(_mid_point, _point, distance)
                _outer = pm.calc_distant_points_on_inverse_line(_mid_point, _point, arrow_size)
                _lst = [ _inner[1], _outer[1], _point, _outer[0], _inner[0] ]
            else:
                _lst = pm.calc_distant_points_on_inverse_line(_point, _other, distance)

            _lst_out = [ self.convert_field_to_user_coords( *tpl ) for tpl in _lst ]
            return _lst_out

        start_lst = _calc_end(True)
        end_lst =  _calc_end(False)
        return start_lst + end_lst

    def draw_arrow(self, arrow, cpair, adef=None):
        # assert isinstance(arrow, Arrow)
        assert isinstance(cpair, ColorsPair)
        # assert isinstance(adef, (ArrowDef, type(None)))

        points = self.calc_arrow(arrow, adef=adef)
        self.draw_polygon(points, interior_str=cpair.interior, outline_str=cpair.outline)

    def draw_all_arrows(self, arrows, adef=None, cmark=None):
        # assert isinstance(adef, (ArrowDef, type(None)))
        # assert isinstance(cmark, (ColorsMarkSimple, type(None)))

        self.clip_board()
        for arrow in arrows:
            cpair = get_mark_color_pair(cmark=cmark, mark_type=arrow.mark_type)
            self.draw_arrow(arrow, cpair, adef=adef)
        self.reset_clip()

    #
    # text

    def draw_text(self, text, cpair, fdef=None):
        assert isinstance(text, Text)
        assert isinstance(cpair, ColorsPair)
        assert isinstance(fdef, (dm.FontDef, type(None)))

        x, y = self.convert_field_to_user_coords( *text.pos )

        _fdef = fdef or dm.MarkDef[ self.board.type ].font_def
        size = 1.0 / _fdef.inv_size_ratio

        Draw.draw_text(self, x, y, text.text, font_family=_fdef.name, size=size, interior_str=cpair.interior, outline_str=cpair.outline)

    def draw_all_texts(self, texts, fdef=None, cmark=None):
        assert isinstance(cmark, (ColorsMark, type(None)))

        self.clip_board()
        for text in texts:
            is_light = self.is_light( *text.pos )
            cpair = get_mark_color_pair(cmark=cmark, mark_type=text.mark_type, is_light=is_light)
            self.draw_text(text, cpair, fdef=fdef)
        self.reset_clip()

    #
    # field marker

    def calc_field_marker(self, field_marker, fmdef=None):
        assert isinstance(field_marker, FieldMarker)
        assert isinstance(fmdef, (dm.FieldMarkerDef, type(None)))

        _fmdef = fmdef or dm.MarkDef[ self.board.type ].field_mark_def

        # inv_width_ratio - compared to field size
        inv_width_ratio = _fmdef.inv_width_ratio # if _fmdef is not None else dm.DEFAULT_FIELD_MARKER_INVERSE_WIDTH_RATIO

        width = 1.0 / inv_width_ratio

        i, j = self.get_field_start( *field_marker.field )
        upper_left_triangle = [ (i, j), (i + width, j), (i, j + width) ]

        x, y = i + 1.0, j
        upper_right_triangle = [ (x, y), (x - width, y), (x, y + width) ]

        x, y = i + 1.0, j + 1.0
        lower_right_triangle = [ (x, y), (x - width, y), (x, y - width) ]

        x, y = i, j + 1.0
        lower_left_triangle = [ (x, y), (x + width, y), (x, y - width) ]

        return [ upper_left_triangle, upper_right_triangle, lower_right_triangle, lower_left_triangle ]

    def draw_field_marker(self, field_marker, cpair, fmdef=None):
        # assert isinstance(field_marker, FieldMarker)
        assert isinstance(cpair, ColorsPair)
        # assert isinstance(fmdef, (dm.FieldMarkerDef, type(None)))
        # assert isinstance(gc, (gtk.gdk.GC, type(None)))

        markers = self.calc_field_marker(field_marker, fmdef=fmdef)

        for points in markers:
            self.draw_polygon(points, interior_str=cpair.interior) # outline_str=cpair.outline

    def draw_all_field_markers(self, field_markers, fmdef=None, cmark=None):
        assert isinstance(cmark, (ColorsMark, type(None)))

        self.clip_board()
        for field_marker in field_markers:
            # assert isinstance(field_marker, FieldMarker)
            is_light = self.is_light( *field_marker.field )
            cpair = get_mark_color_pair(cmark=cmark, mark_type=field_marker.mark_type, is_light=is_light)
            self.draw_field_marker(field_marker, cpair, fmdef=fmdef)
        self.reset_clip()


TEST_BOARD_SIZE_PIX = 1200 # 2400 # 9600
TEST_LINE_WIDTH = 3 # 11 # 3

def test_1(board_type=BoardType.CroatianTies, board_view=None, name=''):
    bt = BoardType(board_type)
    bv = board_view or BoardView(board_type=bt)

    b = Board(bt)
    b.setup()

    d = DrawMark(b, TEST_BOARD_SIZE_PIX, TEST_BOARD_SIZE_PIX, board_view=bv)

    from colors import Colors

    ci = Colors[ bt ]
    d.draw_board(colors_item=ci)

    arws = [ Arrow(1.7, 2.3, 3.4, 5.1, mark_type=MarkType(MarkType.Legal), start_pointer=True, end_pointer=True), \
             Arrow(6.7, 4.3, 9.7, 4.3, mark_type=MarkType(MarkType.Legal)), \
             Arrow(2.7, 6.3, 2.7, 9.3, mark_type=MarkType(MarkType.Legal)), \
             Arrow(4.3, 5.4, 6.7, 0.9, mark_type=MarkType(MarkType.Legal), start_pointer=False, end_pointer=False), ]
    d.draw_all_arrows(arws, cmark=ci.arrow)

    arws2 = [ Arrow(1, 2, 1, 4, mark_type=MarkType(MarkType.Action)), \
              Arrow(3, 9, 4, 9, mark_type=MarkType(MarkType.Action)), \
              Arrow(6, 5, 8, 6, mark_type=MarkType(MarkType.Action), start_pointer=True, end_pointer=True), \
              Arrow(8, 3, 7, 1, mark_type=MarkType(MarkType.Action), start_pointer=False, end_pointer=False), ]
    d.draw_all_arrows(arws2, cmark=ci.arrow)

    file_path = 'temp/arrows%s.IGNORE.png' % name
    d.save_image(file_path)

def test_2(board_type=BoardType.CroatianTies, board_view=None, name=''):
    bt = BoardType(board_type)
    bv = board_view or BoardView(board_type=bt)

    b = Board(bt)
    b.setup()

    d = DrawMark(b, TEST_BOARD_SIZE_PIX, TEST_BOARD_SIZE_PIX, board_view=bv)

    from colors import Colors

    ci = Colors[ bt ]
    d.draw_board(colors_item=ci)

    fdef = dm.FontDef('sans bold', 3.5)
    txts = [ Text("l1", 1.7, 2.3, mark_type=MarkType(MarkType.Legal)), \
             Text("i1", 6.7, 4.3, mark_type=MarkType(MarkType.Illegal)), \
             Text("a1", 2.7, 6.3, mark_type=MarkType(MarkType.Action)), \
             Text("f1", 2.3, 2.4, mark_type=MarkType(MarkType.Blocked)), \
             Text("WW", 5.3, 5.4, mark_type=MarkType(MarkType.Blocked)), \
             Text("MM", 4.3, 6.4, mark_type=MarkType(MarkType.Blocked)), ]
    d.draw_all_texts(txts, fdef=fdef, cmark=ci.text)

    fdef2 = dm.FontDef('sans bold', 4.5)
    txts2 = [ Text("l2", 1, 2, mark_type=MarkType(MarkType.Legal)), \
              Text("i2", 3, 9, mark_type=MarkType(MarkType.Illegal)), \
              Text("a2", 6, 5, mark_type=MarkType(MarkType.Action)), \
              Text("f2", 8, 3, mark_type=MarkType(MarkType.Blocked)), \
              Text("WW", 3, 3, mark_type=MarkType(MarkType.Blocked)),\
              Text("MM", 2, 5, mark_type=MarkType(MarkType.Blocked)), ]
    d.draw_all_texts(txts2, fdef=fdef2, cmark=ci.text)

    file_path = 'temp/texts%s.IGNORE.png' % name
    d.save_image(file_path)

def test_3(board_type=BoardType.CroatianTies, board_view=None, name=''):
    bt = BoardType(board_type)
    bv = board_view or BoardView(board_type=bt)

    b = Board(bt)
    b.setup()

    d = DrawMark(b, TEST_BOARD_SIZE_PIX, TEST_BOARD_SIZE_PIX, board_view=bv)

    from colors import Colors

    ci = Colors[ bt ]
    d.draw_board(colors_item=ci)

    fms = [ FieldMarker(1, 2, mark_type=MarkType(MarkType.Legal)), \
            FieldMarker(6, 4, mark_type=MarkType(MarkType.Illegal)), \
            FieldMarker(2, 6, mark_type=MarkType(MarkType.Action)), \
            FieldMarker(4, 5, mark_type=MarkType(MarkType.Blocked)),  ]
    d.draw_all_field_markers(fms, cmark=ci.marker)

    fmdef = dm.FieldMarkerDef(7.0)
    fms2 = [ FieldMarker(2, 1, mark_type=MarkType(MarkType.Legal)), \
             FieldMarker(7, 5, mark_type=MarkType(MarkType.Illegal)), \
             FieldMarker(3, 7, mark_type=MarkType(MarkType.Action)), \
             FieldMarker(5, 4, mark_type=MarkType(MarkType.Blocked)),  ]
    d.draw_all_field_markers(fms2, fmdef=fmdef, cmark=ci.marker)

    file_path = 'temp/markers%s.IGNORE.png' % name
    d.save_image(file_path)

if __name__ == '__main__':

    bt = BoardType.CroatianTies # BoardType.CroatianTies

    # test_1(board_type=bt)
    # test_1(board_type=bt, board_view=BoardView(margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin')

    # test_1(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0), name='_clipped_2')
    # test_1(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_2')

    # test_1(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0), name='_clipped_3')
    # test_1(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_3')


    test_2(board_type=bt)
    test_2(board_type=bt, board_view=BoardView(margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin')

    test_2(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0), name='_clipped_2')
    test_2(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_2')

    test_2(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0), name='_clipped_3')
    test_2(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_3')


    # test_3(board_type=bt)
    # test_3(board_type=bt, board_view=BoardView(margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin')

    # test_3(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0), name='_clipped_2')
    # test_3(board_type=bt, board_view=BoardView(x=1.7, y=0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_2')

    # test_3(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0), name='_clipped_3')
    # test_3(board_type=bt, board_view=BoardView(x=-0.7, y=-0.3, width=3.6, height=10.0, margin=Margin(left=0.3, top=0.4, right=0.6, bottom=0.7)), name='_margin_3')
