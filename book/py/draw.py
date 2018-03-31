#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk


# DEFAULT_PATH = '../gfx/' # '../tmp/'
DEFAULT_FILE_EXT = '.png'
DEFAULT_FILE_TYPE = 'png'


def get_new_drawable(size_x, size_y):
    default = gtk.gdk.screen_get_default()
    root = default.get_root_window()
    drawable = gtk.gdk.Pixmap(root, size_x, size_y)
    return drawable

def get_new_gc(drawable, line_width):
    gc = drawable.new_gc()
    gc.set_line_attributes(line_width, gtk.gdk.LINE_SOLID, gtk.gdk.CAP_ROUND, gtk.gdk.JOIN_ROUND)
    return gc

def set_new_colors(gc, fg=None, bg=None):
    if fg is not None:
        gc.foreground = gc.get_colormap().alloc_color(fg)

    if bg is not None:
        gc.background = gc.get_colormap().alloc_color(bg)

def save_image(drawable, file_path, file_type=DEFAULT_FILE_TYPE):
    pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, *drawable.get_size())
    pixbuf.get_from_drawable(drawable, drawable.get_colormap(), 0, 0, 0, 0, *drawable.get_size())
    pixbuf.save(file_path, file_type)


class DrawableRectangle(object):
    def __init__(self, left_pix, top_pix, width_pix, height_pix):
        self.left_pix = left_pix
        self.top_pix = top_pix
        self.width_pix = width_pix
        self.height_pix = height_pix

    def calc_point(self, x_pct, y_pct):
        x_pix = self.left_pix + x_pct * self.width_pix
        # y = self.top + (1.0 - y_pct) * self.height
        y_pix = self.top_pix + y_pct * self.height_pix
        return (int(x_pix), int(y_pix))

    def calc_size(self, width_pct, height_pct):
        width_pix  = width_pct * self.width_pix
        height_pix  = height_pct * self.height_pix
        return (int(width_pix ), int(height_pix ))


class Draw(object):
    def __init__(self, size_x, size_y, line_width):
        self.init(size_x, size_y, line_width)

    def init(self, size_x, size_y, line_width):
        self.drawable = get_new_drawable(size_x, size_y)
        self.gc = get_new_gc(self.drawable, line_width)

    def set_new_colors(self, fg=None, bg=None):
        set_new_colors(self.gc, fg=fg, bg=bg)

    def save_image(file_path, file_type=DEFAULT_FILE_TYPE):
        save_image(self.drawable, file_path, file_type=file_type)

    def clear_area(self, color="#FFFFFF"):
#         gc = self.drawable.new_gc()
#         gc.foreground = gc.get_colormap().alloc_color(color)
#         self.gc.foreground = color
        self.set_new_colors(fg=color)
        self.drawable.draw_rectangle(self.gc, True, 0, 0, *self.drawable.get_size())

    def draw_polygon(self, points, fg=None, bg=None, filled=True):
        self.set_new_colors(fg=fg, bg=bg)
        self.drawable.draw_polygon(self.gc, filled, points)

#    def draw_polygon_background_outline(self, points):
#        # Monkeying around limitation of polygon fill being always done with foreground color.
#        fg = self.gc.foreground
#        self.gc.foreground = self.gc.background
#        self.drawable.draw_polygon(self.gc, True, points)
#        self.gc.foreground = fg
#        self.drawable.draw_polygon(self.gc, False, points)

    def draw_polygon_outline(self, points, fg=None, bg=None):
        draw_polygon(self, points, fg=fg, bg=bg, filled=True)
        draw_polygon(self, points, fg=fg, bg=bg, filled=False)

#    def draw_polygon_background(self, points, filled=True):
#        # Monkeying around limitation of polygon fill being always done with foreground color.
#        fg = self.gc.foreground
#        self.gc.foreground = self.gc.background
#        self.drawable.draw_polygon(self.gc, filled, points)
#        self.gc.foreground = fg

#    def draw_arc_with_background(self, x, y, width, height, angle1=0, angle2=64*360):
#        # Monkeying around limitation of arc fill being always done with foreground color.
#        fg = self.gc.foreground
#        self.gc.foreground = self.gc.background
#        self.drawable.draw_arc(self.gc, True, x, y, width, height, angle1, angle2)
#        self.gc.foreground = fg
#        self.drawable.draw_arc(self.gc, False, x, y, width, height, angle1, angle2)

    def draw_arc(self, x, y, width, height, fg=None, bg=None, angle1=0, angle2=64*360):
        self.set_new_colors(fg=fg, bg=bg)
        self.drawable.draw_arc(self.gc, True, x, y, width, height, angle1, angle2)
        self.drawable.draw_arc(self.gc, False, x, y, width, height, angle1, angle2)

#    def get_square_size(self):
#        m = min(self.drawable.get_size())
#        return (m, m)
