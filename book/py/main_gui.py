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

import debug_

class CroChess(object):
    def __init__(self):
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_title("CroChess screen")
        self.window.connect('delete_event', self.delete_event)
        # self.window.set_border_width(10)
        self.window.resize(200, 64)
        self.init()

        self.init_game()
        self.init_scene()

        self.window.show()

    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def init(self):
        self.drawing_area = gtk.DrawingArea()
        self.drawing_area.set_size_request(*self.window.get_size())

        self.drawing_area.set_events(gtk.gdk.POINTER_MOTION_MASK |
                                     gtk.gdk.POINTER_MOTION_HINT_MASK )

        # WARNING: GTK+ doc on some occasions mentions 'expose_event',
        #          but it really is 'expose-event'!
        self.drawing_area.connect('expose-event', self.cb_expose_game)
        # self.drawing_area.connect('expose-event', self.cb_expose_scene)

        self.window.add(self.drawing_area)
        self.drawing_area.show()

    def init_game(self):
        bt = BoardType(BoardType.One)
        self.game = Game(Rules(Board(bt)))
        bt_2 = BoardType(BoardType.One)
        self.game.rules.init_board(bt_2)

    def init_scene(self):
        bt = BoardType(BoardType.ConquestOfTlalocan)
        self.scene = Scene(None)
        self.scene.move_shaman_2(bt)

    def cb_expose_game(self, area, event):
        drawable = self.drawing_area.window
        gc = drawable.new_gc()
        cm = gc.get_colormap()

        painter = BoardPainter(drawable, self.game.rules.board)
        pc = PainterContext(gc, self.game.rules.board)

        painter.clear_area()
        painter.draw_board(pc)

#        painter.draw_piece(pc, PieceType(-PieceType.Starchild))

        return True

    def cb_expose_scene(self, area, event):
        drawable = self.drawing_area.window
        gc = drawable.new_gc()
        cm = gc.get_colormap()

        painter = BoardPainter(drawable, self.scene.board)
        pc = PainterContext(gc, self.game.rules.board)

        painter.clear_area()
        painter.draw_board(pc)

#        painter.draw_piece(pc, PieceType(-PieceType.Starchild))

        return True

def main():
    gtk.main()

if __name__ == "__main__":
    hello = CroChess()
    # hello.init()
    main()
