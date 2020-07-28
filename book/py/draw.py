#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from PIL import Image, ImageDraw, ImageFont
from pixel_math import calc_default_line_width


DEFAULT_COLOR_SPACE = 'RGB'
DEFAULT_FILE_EXT = '.png'
DEFAULT_FILE_TYPE = 'PNG'


def tupletize(points):
    if not points:
        return points
    if isinstance(points[ 0 ], tuple):
        return points
    lst_tpl = list( zip( l[ 0 : : 2 ], l[ 1 : : 2 ] ) ) # [ 1, 2, 3, 4, ... ] --> [ (1, 2), (3, 4), ... ]
    return lst_tpl


class MissingArg:
    pass

DEFAULT_MISSING_ARG = MissingArg()


class Draw:
    def __init__(self, width_pix, height_pix, bg_color="#FFFFFF", color_space=DEFAULT_COLOR_SPACE):
        assert isinstance(width_pix, int)
        assert isinstance(height_pix, int)
        assert isinstance(bg_color, str)
        assert isinstance(color_space, str)

        self.image = Image.new(color_space, (width_pix, height_pix), color=bg_color)
        self.draw = ImageDraw.Draw(self.image)

        size = max(width_pix, height_pix)
        self.default_line_width = calc_default_line_width( size )

    def save_image(self, file_path, file_type=DEFAULT_FILE_TYPE):
        self.image.save(file_path, file_type)

    def clear_area(self, color="#FFFFFF"):
        self.draw_rectangle(0, 0, self.image.width, self.image.height, interior=color, outline=None, line_width=None)

    def draw_lines(self, points, color="#000000", width=1, joint='curve'):
        lw = width or self.default_line_width
        self.draw.line(points, fill=color, width=lw, joint=joint)

    def draw_rectangle(self, x, y, width, height, interior=None, outline=None, line_width=DEFAULT_MISSING_ARG):
        points = [ (x, y), (x+width, y+height) ]
        lw = line_width if line_width is not DEFAULT_MISSING_ARG else self.default_line_width
        self.draw.rectangle(points, fill=interior, outline=outline, width=lw)

    def draw_polygon(self, points, interior=None, outline=None, line_width=DEFAULT_MISSING_ARG, joint='curve'):
        self.draw.polygon(points, fill=interior, outline=outline)

        back2back = tupletize(points)
        last = back2back[ -1 ]
        first = back2back[ 0 ]
        if last != first:
            back2back += [ first, ] # Adds line from end point back to starting point.

        lw = line_width if line_width is not DEFAULT_MISSING_ARG else self.default_line_width
        if outline is not None and lw is not None:
            self.draw.line(back2back, fill=outline, width=lw, joint=joint)

    def draw_ellipse(self, x, y, width, height, interior=None, outline=None, line_width=None):
        points = [ (x, y), (x+width, y+height) ]
        lw = line_width or self.default_line_width
        self.draw.ellipse(points, fill=interior, outline=outline, width=lw)

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
    d = Draw(600, 400)

    d.clear_area(color="#ABCDEF")

    d.draw_polygon([ (10, 10), (100, 100), (10, 100) ], interior='#FF0000', outline=None, line_width=None) # outline='#00FF00'
    d.draw_polygon([ (200, 10), (300, 100), (200, 100) ], interior='#FF0000', outline='#00FF00')

    d.draw_polygon([ (10, 200), (100, 300), (10, 300) ], interior='#00FF00', outline=None, line_width=7) # outline='#0000FF'
    d.draw_polygon([ (200, 200), (300, 300), (200, 300) ], interior='#0000FF', outline='#00FF00', line_width=None)

    d.save_image('temp/draw_1.IGNORE.png')

def test_2():
    d = Draw(600, 400)

    d.clear_area(color="#DEADFB")

    d.draw_ellipse(10, 10, 100, 100, interior='#FF0000', outline=None, line_width=None) # , outline='#00FF00')
    d.draw_ellipse(200, 10, 100, 100, interior='#FF0000', outline='#00FF00')

    d.draw_ellipse(10, 200, 100, 100, interior='#00FF00', outline=None, line_width=7) # outline='#0000FF'
    d.draw_ellipse(200, 200, 100, 100, interior='#0000FF', outline='#00FF00', line_width=7)

    d.save_image('temp/draw_2.IGNORE.png')

if __name__ == '__main__':
    test_1()
    test_2()
