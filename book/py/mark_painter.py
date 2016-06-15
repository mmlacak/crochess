#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


import pixel_math as pm
import mark
import board_painter as bp


class MarkPainter(bp.BoardPainter):
    def __init__(self, drawable, board, arrows):
        super(MarkPainter, self).__init__(drawable, board)

        self.arrows = arrows

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
        points_pix = self.calc_arrow(arrow, inv_width_ratio=inv_width_ratio, pointy_bit_ratio=pointy_bit_ratio)

        fg_color = fg_color or arrow.fg_color or mark.Arrow.DEFAULT_FOREGROUND_COLOR
        bg_color = bg_color or arrow.bg_color or mark.Arrow.DEFAULT_BACKGROUND_COLOR
        gc = pc.get_gc_colors(fg_color, bg_color)
        self.draw_polygon_with_background(gc, points_pix)

    def draw_all_arrows(self, arrows, pc, \
                        fg_color=None, \
                        bg_color=None, \
                        inv_width_ratio=None, \
                        pointy_bit_ratio=None):
        for arrow in arrows:
            self.draw_arrow(arrow, pc, fg_color=fg_color, bg_color=bg_color, \
                            inv_width_ratio=inv_width_ratio, \
                            pointy_bit_ratio=pointy_bit_ratio)
