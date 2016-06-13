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

    def calc_pixel_arrow(self, arrow, inv_width_ratio=None, pointy_bit_ratio=None):
        # inv_width_ratio - compared to field size
        inv_width_ratio = inv_width_ratio or arrow.inv_width_ratio or mark.Arrow.DEFAULT_INVERSE_ARROW_WIDTH_RATIO

        # pointy_bit_ratio - compared to arrow width
        pointy_bit_ratio = pointy_bit_ratio or arrow.pointy_bit_ratio or mark.Arrow.DEFAULT_POINTY_ARROW_BIT_RATIO

        width = self.field_width_pix / inv_width_ratio
        distance = width / 2.0
        length = pm.calc_line_length(arrow.start, arrow.end)

        start_lst = pm.calc_distant_points_on_inverse_line(arrow.start, arrow.end, distance)

        arrow_size = pointy_bit_ratio * width # self.board_width_pix / pointy_bit_ratio
        line_division_ratio = abs((length - arrow_size) / arrow_size) # Shouldn't be negative, i.e. outside line segment.
        mid_point = pm.calc_division_point(arrow.start, arrow.end, line_division_ratio)

        mid_lst = pm.calc_distant_points_on_inverse_line(mid_point, arrow.start, distance)
        mid_lst_2 = pm.calc_distant_points_on_inverse_line(mid_point, arrow.start, arrow_size)

        arrow_lst = [ arrow.end, mid_lst_2[0], mid_lst[0], start_lst[0], start_lst[1], mid_lst[1], mid_lst_2[1] ]
        arrow_lst_2 = [ (int(tpl[0]), int(tpl[1])) for tpl in arrow_lst ]
        return arrow_lst_2

    def calc_field_index_arrow(self, arrow, inv_width_ratio=None, pointy_bit_ratio=None):
# TODO :: calculate from field indexes into pixels ...
        pass

    def calc_arrow(self, arrow, inv_width_ratio=None, pointy_bit_ratio=None):
        if arrow.coord_type == mark.CoordinateType.Pixel:
            return self.calc_pixel_arrow(arrow, inv_width_ratio=inv_width_ratio, pointy_bit_ratio=pointy_bit_ratio)
        elif arrow.coord_type == mark.CoordinateType.FieldIndex:
            return self.calc_field_index_arrow(arrow, inv_width_ratio=inv_width_ratio, pointy_bit_ratio=pointy_bit_ratio)

        assert False, "Can't render arrow with coord type '%s'." % str(arrow.coord_type)

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
