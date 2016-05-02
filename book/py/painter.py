#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010, .. 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk

import debug_

class Painter(object):
    def __init__(self, drawable):
        # self.colormap = gtk.gdk.Colormap(gtk.gdk.visual_get_system(), True)
        self.drawable = drawable

    def clear_area(self, color="#FFFFFF"):
        gc = self.drawable.new_gc()
        gc.foreground = gc.get_colormap().alloc_color(color)
        self.drawable.draw_rectangle(gc, True, 0, 0, *self.drawable.get_size())

    def draw_polygon_with_background(self, gc, points):
        # Monkeying around limitation of polygon fill being always done with foreground color.
        fg = gc.foreground
        gc.foreground = gc.background
        self.drawable.draw_polygon(gc, True, points)
        gc.foreground = fg
        self.drawable.draw_polygon(gc, False, points)

    def draw_arc_with_background(self, gc, x, y, width, height, angle1=0, angle2=64*360):
        # Monkeying around limitation of arc fill being always done with foreground color.
        fg = gc.foreground
        gc.foreground = gc.background
        self.drawable.draw_arc(gc, True, x, y, width, height, angle1, angle2)
        gc.foreground = fg
        self.drawable.draw_arc(gc, False, x, y, width, height, angle1, angle2)

    def get_square_size(self):
        w, h = self.drawable.get_size()
        m = min(w, h)
        return (m, m)

    def calc_point(self, rect, x_pct, y_pct):
        x_pix = rect.left_pix + x_pct * rect.width_pix
        # y = rect.top + (1.0 - y_pct) * rect.height
        y_pix = rect.top_pix + y_pct * rect.height_pix
        # return (x, y)
        return (int(x_pix), int(y_pix))

    def calc_size(self, rect, width_pct, height_pct):
        width_pix  = width_pct * rect.width_pix
        height_pix  = height_pct * rect.height_pix
        # return (width_pix , height_pix )
        return (int(width_pix ), int(height_pix ))

    def flip_horizontally(self, points_pct):
        return [ (1.0 - p[0], p[1]) for p in points_pct ]

    def flip_vertically(self, points_pct):
        return [ (p[0], 1.0 - p[1]) for p in points_pct ]

    def rotate_clockwise(self, points_pct):
        return [ (p[1], 1.0 - p[0]) for p in points_pct ]

    def rotate_anticlockwise(self, points_pct):
        return [ (1.0 - p[1], p[0]) for p in points_pct ]
