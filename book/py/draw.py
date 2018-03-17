#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk

from gfx_def import GD


def get_new_drawable(size_x=None, size_y=None):
    size_x = size_x or GD.DEFAULT_BOARD_RENDERING_SIZE
    size_y = size_y or GD.DEFAULT_BOARD_RENDERING_SIZE

    default = gtk.gdk.screen_get_default()
    root = default.get_root_window()
    drawable = gtk.gdk.Pixmap(root, size_x, size_y)

    return drawable

def get_new_gfx_ctx(line_width=None):
    line_width = line_width or GD.DEFAULT_BOARD_LINE_WIDTH

    gc = root.new_gc()
    gc.set_line_attributes(line_width, gtk.gdk.LINE_SOLID, gtk.gdk.CAP_ROUND, gtk.gdk.JOIN_ROUND)

    return gc

def save_image(drawable, file_path, file_type=None):
    file_type = file_type or GD.DEFAULT_FILE_TYPE

    pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, *drawable.get_size())
    pixbuf.get_from_drawable(drawable, drawable.get_colormap(), 0, 0, 0, 0, *drawable.get_size())
    pixbuf.save(file_path, file_type)
