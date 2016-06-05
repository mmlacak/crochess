#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
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
from gfx import GfxRender

import debug_

class CroChess(object):
    def __init__(self):
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_title("CroChess screen")
        self.window.connect('delete_event', self.delete_event)
        # self.window.set_border_width(10)
        self.window.resize(600, 400)
        self.init()

        self.init_game()
        self.init_scene()

        self.window.show()

        # self.save_game_render()
        self.off_screen = None

        render = GfxRender()
        render.render_all_boards()
        render.render_all_newly_introduced_pieces()
        render.render_all_example_scenes()
        render.render_all_castling_scenes()
        render.render_all_en_passant_scenes()

        print
        print "Finished all renderings."
        print

    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def init(self):
        self.drawing_area = gtk.DrawingArea()
        self.drawing_area.set_size_request(*self.window.get_size())
        # self.drawing_area.set_size_request(10000, 6000)

        self.drawing_area.set_events(gtk.gdk.POINTER_MOTION_MASK |
                                     gtk.gdk.POINTER_MOTION_HINT_MASK )

        # WARNING: GTK+ doc on some occasions mentions 'expose_event',
        #          but it really is 'expose-event'!
        self.drawing_area.connect('expose-event', self.cb_expose_game)
        # self.drawing_area.connect('expose-event', self.cb_expose_scene)

        self.window.add(self.drawing_area)
        self.drawing_area.show()

    def init_game(self):
        bt = BoardType(BoardType.OddDiscovery) # One
        self.game = Game(Rules(Board(bt)))
        bt_2 = BoardType(BoardType.OddDiscovery) # One
        self.game.rules.init_board(bt_2)

    def init_scene(self):
        bt = BoardType(BoardType.ConquestOfTlalocan)
        self.scene = Scene(None)
        self.scene.move_shaman_2(bt)

    def cb_expose_game(self, area, event):
        drawable = self.drawing_area.window

        gc = drawable.new_gc()
        cm = gc.get_colormap()

        if self.off_screen is not None:
            drawable.draw_drawable(gc, self.off_screen, 0, 0, 0, 0, -1, -1)

        return True

    def save_game_render(self):
        default = gtk.gdk.screen_get_default()
        root = default.get_root_window()
        drawable = self.off_screen = gtk.gdk.Pixmap(root, 6000, 6000)

        gc = root.new_gc()
        gc.set_line_attributes(9, gtk.gdk.LINE_SOLID, gtk.gdk.CAP_ROUND, gtk.gdk.JOIN_ROUND)
        cm = gc.get_colormap()

        painter = BoardPainter(drawable, self.game.rules.board)
        pc = PainterContext(gc, self.game.rules.board, False) # True # False
        pc.cc = pc.get_color_context(self.game.rules.board.type)

        painter.clear_area()
        painter.draw_board(pc)

        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, *drawable.get_size())
        pixbuf.get_from_drawable(drawable, drawable.get_colormap(), 0, 0, 0, 0, *drawable.get_size())
        pixbuf.save('../gfx/second.png', 'png')

    def cb_expose_scene(self, area, event):
        drawable = self.drawing_area.window
        gc = drawable.new_gc()
        cm = gc.get_colormap()

        painter = BoardPainter(drawable, self.scene.board)
        pc = PainterContext(gc, self.game.rules.board, False) # True # False
        pc.cc = pc.get_color_context(self.scene.board.type)

        painter.clear_area()
        painter.draw_board(pc)

        return True

def main():
    gtk.main()

if __name__ == "__main__":
    hello = CroChess()
    # hello.init()
    main()
