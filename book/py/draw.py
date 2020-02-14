#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk


DEFAULT_FILE_EXT = '.png'
DEFAULT_FILE_TYPE = 'png'


def get_new_drawable(size_x, size_y):
    default = gtk.gdk.screen_get_default()
    root = default.get_root_window()
    drawable = gtk.gdk.Pixmap(root, size_x, size_y)
    return drawable

def get_new_gc(drawable, line_width):
    assert isinstance(drawable, gtk.gdk.Drawable)

    gc = drawable.new_gc()
    gc.set_line_attributes(line_width, gtk.gdk.LINE_SOLID, gtk.gdk.CAP_ROUND, gtk.gdk.JOIN_ROUND)
    return gc

def set_new_colors(gc, fg=None, bg=None):
    assert isinstance(gc, gtk.gdk.GC)

    if fg is not None:
        gc.foreground = gc.get_colormap().alloc_color(fg)

    if bg is not None:
        gc.background = gc.get_colormap().alloc_color(bg)

    return gc

def save_image(drawable, file_path, file_type=DEFAULT_FILE_TYPE):
    pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, *drawable.get_size())
    pixbuf.get_from_drawable(drawable, drawable.get_colormap(), 0, 0, 0, 0, *drawable.get_size())
    pixbuf.save(file_path, file_type)


class DrawableRectangle(object):
    def __init__(self, x_pix, y_pix, width_pix, height_pix):
        assert isinstance(x_pix, int)
        assert isinstance(y_pix, int)
        assert isinstance(width_pix, int)
        assert isinstance(height_pix, int)

        self.x_pix = x_pix
        self.y_pix = y_pix
        self.width_pix = width_pix
        self.height_pix = height_pix

    def as_tuple(self):
        return (self.x_pix, self.y_pix, self.width_pix, self.height_pix)

    @staticmethod
    def from_tuple(tpl):
        return DrawableRectangle( *tpl[ 0 : 4 ] )

    @staticmethod
    def scale(x_pct, y_pct, scale=1.0, center_x=0.5, center_y=0.5):
        if scale == 1.0:
            # 1.0 == 1.0 + 1e-16 --> True
            return (x_pct, y_pct)
        else:
            # 1.0 == 1.0 + 1e-15 --> False
            x = scale * (x_pct - center_x) + center_x
            y = scale * (y_pct - center_y) + center_y
            return (x, y)

    def calc_point(self, x_pct, y_pct):
        x_pix = self.x_pix + x_pct * self.width_pix
        # y = self.top + (1.0 - y_pct) * self.height
        y_pix = self.y_pix + y_pct * self.height_pix
        return (int(x_pix), int(y_pix))

    def calc_size(self, width_pct, height_pct):
        width_pix  = width_pct * self.width_pix
        height_pix  = height_pct * self.height_pix
        return (int(width_pix), int(height_pix))


class Draw(object):
    def __init__(self, drawable, gc, bg_color="#FFFFFF"):
        assert isinstance(drawable, gtk.gdk.Drawable)
        assert isinstance(gc, gtk.gdk.GC)

        self.drawable = drawable
        self.gc = gc

        self.clear_area(color=bg_color, gc=self.gc)

    def set_gc_colors(self, fg=None, bg=None, gc=None):
        self.gc = set_new_colors(gc or self.gc, fg=fg, bg=bg)
        return self.gc

    def save_image(self, file_path, file_type=DEFAULT_FILE_TYPE):
        save_image(self.drawable, file_path, file_type=file_type)

    def clear_area(self, color="#FFFFFF", gc=None):
        self.set_gc_colors(fg=color, gc=gc)
        self.drawable.draw_rectangle(self.gc, True, 0, 0, *self.drawable.get_size())

    def draw_lines(self, points, fg=None, bg=None, gc=None):
        self.set_gc_colors(fg=fg, bg=bg, gc=gc)
        self.drawable.draw_lines(self.gc, points)

    def draw_outlines(self, points, outline=None, gc=None):
        self.draw_lines(points, fg=outline, bg=outline, gc=gc)

    def draw_rectangle(self, x, y, width, height, filled=True, fg=None, bg=None, gc=None):
        self.set_gc_colors(fg=fg, bg=bg, gc=gc)
        self.drawable.draw_rectangle(self.gc, filled, x, y, width, height)

    def draw_polygon(self, points, filled=True, fg=None, bg=None, gc=None):
        self.set_gc_colors(fg=fg, bg=bg, gc=gc)
        self.drawable.draw_polygon(self.gc, filled, points)

    def draw_outlined_polygon(self, points, interior=None, outline=None, gc=None):
        self.draw_polygon(points, filled=True, fg=interior, gc=gc)
        self.draw_polygon(points, filled=False, fg=outline, gc=gc)

    def draw_arc(self, x, y, width, height, filled=True, fg=None, bg=None, angle1=0, angle2=64*360, gc=None):
        self.set_gc_colors(fg=fg, bg=bg, gc=gc)
        self.drawable.draw_arc(self.gc, filled, x, y, width, height, angle1, angle2)

    def draw_outlined_arc(self, x, y, width, height, interior=None, outline=None, angle1=0, angle2=64*360, gc=None):
        self.draw_arc(x, y, width, height, True, fg=interior, angle1=angle1, angle2=angle2, gc=gc)
        self.draw_arc(x, y, width, height, False, fg=outline, angle1=angle1, angle2=angle2, gc=gc)

#    def get_square_size(self):
#        m = min(self.drawable.get_size())
#        return (m, m)

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
    drw = get_new_drawable(600, 400)
    gc = get_new_gc(drw, 5)

    d = Draw(drw, gc)

    d.draw_polygon([ (10, 10), (100, 100), (10, 100) ], filled=True, fg='#FF0000', bg='#00FF00')
    d.draw_polygon([ (200, 10), (300, 100), (200, 100) ], filled=False, fg='#FF0000', bg='#00FF00')

    d.draw_outlined_polygon([ (10, 200), (100, 300), (10, 300) ], interior='#00FF00', outline='#0000FF')
    d.draw_outlined_polygon([ (200, 200), (300, 300), (200, 300) ], interior='#0000FF', outline='#00FF00')

    d.save_image('temp/draw_1.IGNORE.png')

def test_2():
    drw = get_new_drawable(600, 400)
    gc = get_new_gc(drw, 5)

    d = Draw(drw, gc)

    d.draw_arc(10, 10, 100, 100, True, fg='#FF0000', bg='#00FF00')
    d.draw_arc(200, 10, 100, 100, False, fg='#FF0000', bg='#00FF00')

    d.draw_outlined_arc(10, 200, 100, 100, interior='#00FF00', outline='#0000FF')
    d.draw_outlined_arc(200, 200, 100, 100, interior='#0000FF', outline='#00FF00')

    d.save_image('temp/draw_2.IGNORE.png')

if __name__ == '__main__':
    test_1()
    test_2()
