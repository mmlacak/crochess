#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from math import pi
import cairo

from util import convert_to_tuple


DEFAULT_LINE_WIDTH = 0.023
DEFAULT_FILE_EXT = '.png'


class Draw:
    def __init__(self, width_pix, height_pix, field_size_in_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF", color_space=cairo.Format.RGB24):
        assert isinstance(width_pix, int)
        assert isinstance(height_pix, int)
        assert isinstance(field_size_in_pix, float) # Scaling factor of device units (pixels), from user coords (== field size).
        assert isinstance(color_space, cairo.Format)

        # Device coords.
        self.width_pix = width_pix
        self.height_pix = height_pix
        self.field_size_in_pix = field_size_in_pix

        self.surface = cairo.ImageSurface(color_space, width_pix, height_pix)
        self.surface.set_device_scale(field_size_in_pix, field_size_in_pix) # Device coords (pixels) are scaled against user units == field size.

        # User coords.
        self.width = self.width_pix / self.field_size_in_pix
        self.height = self.height_pix / self.field_size_in_pix

        self.context = cairo.Context(self.surface)
        self.context.scale(1.0, 1.0) # User coords are in chessboard fields, which are always squares of size 1.0.

        self.set_line(width=line_width, join=cairo.LineJoin.ROUND, cap=cairo.LineCap.ROUND)

        self.clear_image(color_str=color_str)

    def set_line(self, width=DEFAULT_LINE_WIDTH, join=cairo.LineJoin.ROUND, cap=cairo.LineCap.ROUND):
        self.context.set_line_join(join)
        self.context.set_line_cap(cap)
        self.context.set_line_width(width)

    def save_image(self, file_path):
        self.surface.write_to_png(file_path)

    def clear_image(self, color_str="#FFFFFF"):
        self.draw_rectangle(0.0, 0.0, self.width, self.height, interior_str=color_str)

    def draw_last_path(self, interior_str=None, outline_str=None, line_width=DEFAULT_LINE_WIDTH):

        if interior_str:
            interior = convert_to_tuple(interior_str)
            self.context.set_source_rgb(*interior)

            if outline_str:
                self.context.fill_preserve()
            else:
                self.context.fill()

        if outline_str:
            outline = convert_to_tuple(outline_str)
            self.context.set_source_rgb(*outline)

            if line_width is not None:
                self.context.set_line_width(line_width)

            self.context.stroke()

    def draw_rectangle(self, x, y, width, height, interior_str=None, outline_str=None, line_width=DEFAULT_LINE_WIDTH):
        self.context.rectangle(x, y, width, height)
        self.draw_last_path(interior_str=interior_str, outline_str=outline_str, line_width=line_width)

    def draw_lines(self, points, x=0.0, y=0.0, line_width=DEFAULT_LINE_WIDTH, color_str=None):
        x0, y0 = points[ 0 ]
        self.context.move_to(x+x0, y+y0)

        for _x, _y in points[ 1 : ]:
            self.context.line_to(x+_x, y+_y)

        self.draw_last_path(line_width=line_width, interior_str=None, outline_str=color_str)

    def draw_polygon(self, points, x=0.0, y=0.0, interior_str=None, outline_str=None, line_width=DEFAULT_LINE_WIDTH):
        x0, y0 = points[ 0 ]
        self.context.move_to(x+x0, y+y0)

        for _x, _y in points[ 1 : ]:
            self.context.line_to(x+_x, y+_y)
        self.context.close_path()

        self.draw_last_path(interior_str=interior_str, outline_str=outline_str, line_width=line_width)

    def draw_arc(self, x, y, radius, angle1=0.0, angle2=2*pi, interior_str=None, outline_str=None, line_width=DEFAULT_LINE_WIDTH):
        self.context.arc(x, y, radius, angle1, angle2)
        self.draw_last_path(interior_str=interior_str, outline_str=outline_str, line_width=line_width)

    def draw_arc_pos(self, x, y, radius, rel_x=0.5, rel_y=0.5, angle1=0.0, angle2=2*pi, interior_str=None, outline_str=None, line_width=DEFAULT_LINE_WIDTH):
        self.draw_arc(x+rel_x, y+rel_y, radius, angle1=angle1, angle2=angle2, interior_str=interior_str, outline_str=outline_str, line_width=line_width)

    def flip_horizontally(self, points_pct):
        return [ (1.0 - p[0], p[1]) for p in points_pct ]

    def flip_vertically(self, points_pct):
        return [ (p[0], 1.0 - p[1]) for p in points_pct ]

    def rotate_clockwise(self, points_pct):
        return [ (p[1], 1.0 - p[0]) for p in points_pct ]

    def rotate_anticlockwise(self, points_pct):
        return [ (1.0 - p[1], p[0]) for p in points_pct ]

    def translate(self, points_pix, off_x_pix=0, off_y_pix=0):
        return [ (p[0] + off_x_pix, p[1] + off_y_pix) for p in points_pix ]


def test_1():
    d = Draw(600, 400, 100.0)

    d.draw_polygon([ (0.1, 0.1), (0.9, 0.9), (0.1, 0.9) ], interior_str='#FF0000', outline_str='#00FF00')
    d.draw_polygon([ (1.1, 0.1), (1.9, 0.9), (1.1, 0.9) ], interior_str='#FF0000')
    d.draw_polygon([ (2.1, 0.1), (2.9, 0.9), (2.1, 0.9) ], outline_str='#00FF00')

    points = [ (0.1, 0.1), (0.9, 0.9), (0.1, 0.9) ]
    d.draw_polygon(points, x=0.0, y=1.0, interior_str='#00FF00', outline_str='#0000FF')
    d.draw_polygon(points, x=1.0, y=1.0, interior_str='#00FF00')
    d.draw_polygon(points, x=2.0, y=1.0, outline_str='#0000FF')

    d.draw_rectangle( 0.1, 2.0, 0.8, 0.8, interior_str='#00FF00', outline_str='#FF0000' )
    d.draw_rectangle( 1.1, 2.0, 0.8, 0.8, interior_str='#00FF00' )
    d.draw_rectangle( 2.1, 2.0, 0.8, 0.8, outline_str='#FF0000' )

    d.save_image('temp/draw_1.IGNORE.png')

def test_2():
    d = Draw(600, 400, 100.0)

    d.draw_arc(0.5, 0.5, 0.4, interior_str='#FF0000', outline_str='#00FF00')
    d.draw_arc(1.5, 0.5, 0.4, interior_str='#FF0000')
    d.draw_arc(2.5, 0.5, 0.4, outline_str='#00FF00')

    d.draw_lines([ (0.1, 1.1), (0.9, 1.9), (0.1, 1.9) ], color_str='#FF0000')
    d.draw_lines([ (1.1, 1.1), (1.9, 1.9), (1.1, 1.9) ], color_str='#00FF00')
    d.draw_lines([ (2.1, 1.1), (2.9, 1.9), (2.1, 1.9) ], color_str='#0000FF')

    points = [ (0.1, 0.1), (0.9, 0.9), (0.1, 0.9) ]
    d.draw_lines(points, x=0.0, y=2.0, color_str='#0000FF')
    d.draw_lines(points, x=1.0, y=2.0, color_str='#00FF00')
    d.draw_lines(points, x=2.0, y=2.0, color_str='#FF0000')

    d.draw_arc_pos(0.0, 3.0, 0.4, rel_x=0.4, rel_y=0.4, interior_str='#0000FF', outline_str='#00FF00')
    d.draw_arc_pos(1.0, 3.0, 0.4, rel_x=0.4, interior_str='#0000FF')
    d.draw_arc_pos(2.0, 3.0, 0.4, rel_y=0.4, outline_str='#00FF00')

    d.save_image('temp/draw_2.IGNORE.png')

if __name__ == '__main__':
    test_1()
    test_2()
