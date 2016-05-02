#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010, .. 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk

from piece import PieceType
from painter_context import PainterContext
# from painter import Painter
from piece_painter import PiecePainter
from board_painter import BoardPainter
from board import BoardType
from board import Board
from rules import Rules
from game import Game
from scene import Scene

import debug_

class GfxRender(object):

    DEFAULT_BOARD_RENDERING_SIZE = 6000
    DEFAULT_PIECE_2_x_2_RENDERING_SIZE = 2000
    DEFAULT_BOARD_LINE_WIDTH = 7
    DEFAULT_PATH = '../gfx/'
    DEFAULT_FILE_EXT = '.png'
    DEFAULT_FILE_TYPE = 'png'

    def render_all_boards(self):
        print
        for bt in xrange(BoardType.Classical, BoardType.One+1, 2):
            # Upper limit is not included in loop.
            # Step is 2 because there is no need to generate odd variants.
            self.init_game(bt, bt)
            file_path = self.get_board_file_path()
            print file_path
            self.save_board_image(file_path)
        print "Finished."

    def init_game(self, board_type_value=BoardType.One, board_type_value_2=BoardType.One):
        bt = BoardType(board_type_value)
        self.game = Game(Rules(Board(bt)))
        bt_2 = BoardType(board_type_value_2)
        self.game.rules.init_board(bt_2)

    def get_board_file_path(self, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or GfxRender.DEFAULT_PATH
        file_ext = file_ext or GfxRender.DEFAULT_FILE_EXT

        bt = self.game.rules.board.type
        index = int(bt)
        name = bt.get_name()
        sanitize = name.replace('\'', '_').replace(' ', '_').lower()
        return '%s%02d_%s%s' % (path_prefix, index, sanitize, file_ext)

    def init_scene(self, board_type_value=BoardType.One):
        bt = BoardType(board_type_value) # BoardType.ConquestOfTlalocan)
        self.scene = Scene(None)
        self.scene.move_shaman_2(bt)

    def save_board_image(self, file_path, size_x=None, size_y=None, line_width=None, file_type=None):
        size_x = size_x or GfxRender.DEFAULT_BOARD_RENDERING_SIZE
        size_y = size_y or GfxRender.DEFAULT_BOARD_RENDERING_SIZE
        line_width = line_width or GfxRender.DEFAULT_BOARD_LINE_WIDTH
        file_type = file_type or GfxRender.DEFAULT_FILE_TYPE

        default = gtk.gdk.screen_get_default()
        root = default.get_root_window()
        drawable = self.off_screen = gtk.gdk.Pixmap(root, size_x, size_y)

        gc = root.new_gc()
        gc.set_line_attributes(line_width, gtk.gdk.LINE_SOLID, gtk.gdk.CAP_ROUND, gtk.gdk.JOIN_ROUND)
        cm = gc.get_colormap()

        painter = BoardPainter(drawable, self.game.rules.board)
        pc = PainterContext(gc, self.game.rules.board, False) # True # False
        pc.cc = pc.get_color_context(self.game.rules.board.type)

        painter.clear_area()
        painter.draw_board(pc)

        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, *drawable.get_size())
        pixbuf.get_from_drawable(drawable, drawable.get_colormap(), 0, 0, 0, 0, *drawable.get_size())
        pixbuf.save(file_path, file_type)
