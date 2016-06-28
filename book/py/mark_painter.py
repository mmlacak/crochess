#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import gtk.gdk
import pango

import pixel_math as pm
import mark
import board_painter as bp


class MarkPainter(bp.BoardPainter):
    def __init__(self, drawable, board):
        super(MarkPainter, self).__init__(drawable, board)

    def calc_arrow(self, arrow, inv_width_ratio=None, pointy_bit_ratio=None):
        arrow.start_pix = self.convert_float_coords_to_pixel( *arrow.start )
        arrow.end_pix = self.convert_float_coords_to_pixel( *arrow.end )

        # inv_width_ratio - compared to field size
        inv_width_ratio = inv_width_ratio or arrow.inv_width_ratio or mark.Arrow.DEFAULT_INVERSE_ARROW_WIDTH_RATIO

        # pointy_bit_ratio - compared to arrow width
        pointy_bit_ratio = pointy_bit_ratio or arrow.pointy_bit_ratio or mark.Arrow.DEFAULT_POINTY_ARROW_BIT_RATIO

        width = self.field_width_pix / inv_width_ratio
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

    def draw_arrow(self, arrow, pc, \
                   fg_color=None, \
                   bg_color=None, \
                   inv_width_ratio=None, \
                   pointy_bit_ratio=None):
        # arrow :: mark.Arrow

        points_pix = self.calc_arrow(arrow, inv_width_ratio=inv_width_ratio, pointy_bit_ratio=pointy_bit_ratio)

        fg_color = fg_color or arrow.fg_color or mark.Arrow.DEFAULT_FOREGROUND_COLOR
        bg_color = bg_color or arrow.bg_color or mark.Arrow.DEFAULT_BACKGROUND_COLOR
        gc = pc.get_gc_colors(fg_color, bg_color)

        self.draw_polygon_background_outline(gc, points_pix)

    def draw_all_arrows(self, arrows, pc, \
                        fg_color=None, \
                        bg_color=None, \
                        inv_width_ratio=None, \
                        pointy_bit_ratio=None):
        for arrow in arrows:
            self.draw_arrow(arrow, pc, fg_color=fg_color, bg_color=bg_color, \
                            inv_width_ratio=inv_width_ratio, \
                            pointy_bit_ratio=pointy_bit_ratio)

    def draw_text(self, text, pc, font=None, \
                  fg_color=None, \
                  bg_color=None):
        # text :: mark.Text

        text.pos_pix = self.convert_float_coords_to_pixel( *text.pos )
        x, y = text.pos_pix

        font = font or text.font or mark.Text.DEFAULT_FONT

        fg_color = fg_color or text.fg_color or mark.Text.DEFAULT_FOREGROUND_COLOR
        bg_color = bg_color or text.bg_color or mark.Text.DEFAULT_BACKGROUND_COLOR
        gc = pc.get_gc_colors(fg_color, bg_color)

        screen = gtk.gdk.screen_get_default()
        pango_ctx = gtk.gdk.pango_context_get_for_screen(screen)

        layout = pango.Layout(pango_ctx)
        layout.set_text(text.text)

        font_desc = pango.FontDescription(text.font)
        layout.set_font_description(font_desc)

        x, y = pm.round_floats_to_int((x, y))
        self.drawable.draw_layout(gc, x, y, layout)

    def draw_all_texts(self, texts, pc):
        for text in texts:
            self.draw_text(text, pc)

    def calc_field_marker(self, field_marker, inv_width_ratio=None):
        # inv_width_ratio - compared to field size
        inv_width_ratio = inv_width_ratio or field_marker.inv_width_ratio or mark.FieldMarker.DEFAULT_INVERSE_WIDTH_RATIO

        width_ratio = 1.0 / inv_width_ratio
        width_pix = self.convert_float_width_to_pixel(width_ratio)

        start_dp = self.get_field_start_pix(*field_marker.field)

        x_pix, y_pix = start_dp.x_pix, start_dp.y_pix
        upper_left_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix + width_pix, y_pix), (x_pix, y_pix + width_pix) ])

        x_pix, y_pix = x_pix + self.field_width_pix, y_pix
        upper_right_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix - width_pix, y_pix), (x_pix, y_pix + width_pix) ])

        x_pix, y_pix = x_pix, y_pix + self.field_height_pix
        lower_right_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix - width_pix, y_pix), (x_pix, y_pix - width_pix) ])

        x_pix, y_pix = x_pix - self.field_width_pix, y_pix
        lower_left_triangle = pm.round_coords_to_int([ (x_pix, y_pix), (x_pix + width_pix, y_pix), (x_pix, y_pix - width_pix) ])

        field_markers_pix = [ upper_left_triangle, upper_right_triangle, lower_right_triangle, lower_left_triangle ]

        return field_markers_pix

    def draw_field_marker(self, field_marker, pc, \
                          fg_color=None, \
                          bg_color=None, \
                          inv_width_ratio=None):
        # field_marker :: mark.FieldMarker

        markers_pix = self.calc_field_marker(field_marker, inv_width_ratio=inv_width_ratio)

        fg_color = fg_color or field_marker.fg_color or mark.FieldMarker.DEFAULT_FOREGROUND_COLOR
        bg_color = bg_color or field_marker.bg_color or mark.FieldMarker.DEFAULT_BACKGROUND_COLOR
        gc = pc.get_gc_colors(fg_color, bg_color)

        for points_pix in markers_pix:
            # self.draw_polygon_background_outline(gc, points_pix)
            # self.draw_polygon(gc, points_pix)
            self.draw_polygon_background(gc, points_pix)

    def draw_all_field_markers(self, field_markers, pc):
        for field_marker in field_markers:
            self.draw_field_marker(field_marker, pc)
