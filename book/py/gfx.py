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
from mark_painter import MarkPainter
from board import BoardType
from board import Board
from rules import Rules
from game import Game
from scene import Scene

import debug_

class GfxRender(object):

    DEFAULT_BOARD_RENDERING_SIZE = 8000
    DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE = 12000 # 50% added to rendering size
    DEFAULT_PIECE_2x2_RENDERING_SIZE = 3200 # 40% of rendering size
    DEFAULT_BOARD_LINE_WIDTH = 11 # >= 1 + (6 * rendering size / 5) // 1000
    DEFAULT_PATH = '../tmp/' # '../gfx/'
    DEFAULT_FILE_EXT = '.png'
    DEFAULT_FILE_TYPE = 'png'

    def render_all_boards(self):
        print
        print "Rendering all boards."
        for bt in xrange(BoardType.Classical, BoardType.One+1, 2):
            # Added +1 because upper limit is not included in loop.
            # Step is 2 because there is no need to generate odd variants.
            self.init_game(bt, bt)
            file_path = self.get_board_file_path()
            print file_path
            self.save_board_image(file_path)
        print "Finished."

    def render_all_newly_introduced_pieces(self):
        print
        print "Rendering all pieces."
        for bt in xrange(BoardType.Classical, BoardType.One+1, 2):
            # Added +1 because upper limit is not included in loop.
            # Step is 2 because there is no need to generate odd variants.
            pt = self.init_intro_piece_scene(bt)
            if pt is not None:
                file_path = self.get_piece_file_path(pt)
                print file_path
                self.save_board_image(file_path, \
                                      is_game_or_scene=False, \
                                      size_x=GfxRender.DEFAULT_PIECE_2x2_RENDERING_SIZE, \
                                      size_y=GfxRender.DEFAULT_PIECE_2x2_RENDERING_SIZE)
        print "Finished."

    def render_all_example_scenes(self):
        print
        print "Rendering all examples."
        self.scene = Scene(None)
        for index, func in enumerate(self.scene.get_example_scene_functions()):
            name = func()
            file_path = self.get_scene_file_path(index+1, name)
            print file_path
            size_x, size_y = self.get_scene_image_size()
            self.save_board_image(file_path, \
                                  is_game_or_scene=False, \
                                  size_x=size_x, \
                                  size_y=size_y)
        print "Finished."

    def render_all_castling_scenes(self):
        print
        print "Rendering all castlings."
        for bt in xrange(BoardType.Classical, BoardType.One+1, 2):
            # Added +1 because upper limit is not included in loop.
            # Step is 2 because there is no need to generate odd variants.
            bt_real = self.init_intro_castling_scene(bt)
            if bt_real is not None:
                file_path = self.get_castling_file_path(bt_real)
                print file_path
                size_x, size_y = self.get_scene_image_size()
                self.save_board_image(file_path, \
                                      is_game_or_scene=False, \
                                      size_x=size_x, \
                                      size_y=size_y)
        print "Finished."

    def render_all_en_passant_scenes(self):
        print
        print "Rendering all en passant."
        for bt in xrange(BoardType.Classical, BoardType.One+1, 2):
            # Added +1 because upper limit is not included in loop.
            # Step is 2 because there is no need to generate odd variants.
            bt_real = self.init_intro_en_passant_scene(bt)
            if bt_real is not None:
                file_path = self.get_en_passant_file_path(bt_real)
                print file_path
                size_x, size_y = self.get_scene_image_size(horizontal_rendering_size=GfxRender.DEFAULT_PIECE_2x2_RENDERING_SIZE)
                self.save_board_image(file_path, \
                                      is_game_or_scene=False, \
                                      size_x=size_x, \
                                      size_y=size_y)
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
        return '%s/boards/%02d_%s%s' % (path_prefix, index, sanitize, file_ext)

    def get_piece_file_path(self, piece_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or GfxRender.DEFAULT_PATH
        file_ext = file_ext or GfxRender.DEFAULT_FILE_EXT

        pt = PieceType(piece_type)
        index = int(pt)
        name = pt.get_name()
        sanitize = name.replace('\'', '_').replace(' ', '_').lower()
        return '%s/pieces/%02d_%s%s' % (path_prefix, index, sanitize, file_ext)

    def get_scene_file_path(self, index, file_name, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or GfxRender.DEFAULT_PATH
        file_ext = file_ext or GfxRender.DEFAULT_FILE_EXT

        return '%s/examples/%02d_%s%s' % (path_prefix, index, file_name, file_ext)

    def get_castling_file_path(self, board_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or GfxRender.DEFAULT_PATH
        file_ext = file_ext or GfxRender.DEFAULT_FILE_EXT

        index = int(board_type)
        name = board_type.get_name()
        sanitize = name.replace('\'', '_').replace(' ', '_').lower()
        return '%s/castlings/%02d_%s_castling%s' % (path_prefix, index, sanitize, file_ext)

    def get_en_passant_file_path(self, board_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or GfxRender.DEFAULT_PATH
        file_ext = file_ext or GfxRender.DEFAULT_FILE_EXT

        index = int(board_type)
        name = board_type.get_name()
        sanitize = name.replace('\'', '_').replace(' ', '_').lower()
        return '%s/en_passants/%02d_%s_en_passant%s' % (path_prefix, index, sanitize, file_ext)

    def get_scene_image_size(self, horizontal_rendering_size=None, vertical_rendering_size=None):
        horizontal_rendering_size = horizontal_rendering_size or GfxRender.DEFAULT_BOARD_RENDERING_SIZE
        vertical_rendering_size = vertical_rendering_size or GfxRender.DEFAULT_BOARD_RENDERING_SIZE

        board = self.scene.board
        width = board.get_width()
        height = board.get_height()

        horizontal_dpi = horizontal_rendering_size // width
        vertical_pixel_size = height * horizontal_dpi

        if vertical_pixel_size < GfxRender.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE:
            return (horizontal_rendering_size, vertical_pixel_size)
        else:
            vertical_dpi = GfxRender.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE // height
            horizontal_pixel_size = width * vertical_dpi

            return (horizontal_pixel_size, GfxRender.DEFAULT_MAX_BOARD_VERTICAL_RENDERING_SIZE)

    def init_intro_piece_scene(self, board_type_value=BoardType.One, piece_type=None):
        bt = BoardType(board_type_value)
        self.scene = Scene(None)
        return self.scene.intro_piece(bt, piece_type=piece_type)

    def init_intro_castling_scene(self, board_type_value=BoardType.One):
        bt = BoardType(board_type_value)
        self.scene = Scene(None)
        return self.scene.intro_castling(bt)

    def init_intro_en_passant_scene(self, board_type_value=BoardType.One):
        bt = BoardType(board_type_value)
        self.scene = Scene(None)
        return self.scene.intro_en_passant(bt)

    def init_scene(self, board_type_value=BoardType.One):
        bt = BoardType(board_type_value) # BoardType.ConquestOfTlalocan)
        self.scene = Scene(None)
        self.scene.move_shaman_2(bt)

    def save_board_image(self, file_path, is_game_or_scene=True, size_x=None, size_y=None, line_width=None, file_type=None):
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

        # painter = BoardPainter(drawable, self.game.rules.board)
        # pc = PainterContext(gc, self.game.rules.board, False) # True # False
        # pc.cc = pc.get_color_context(self.game.rules.board.type)

        board = self.game.rules.board if is_game_or_scene else self.scene.board
        arrows = [] if is_game_or_scene else self.scene.arrows
        # painter = BoardPainter(drawable, board)
        painter = MarkPainter(drawable, board, arrows)
        pc = PainterContext(gc, board, False) # True # False
        pc.cc = pc.get_color_context(board.type)

        painter.clear_area()
        painter.draw_board(pc)
        painter.draw_all_arrows(arrows, pc)

        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, *drawable.get_size())
        pixbuf.get_from_drawable(drawable, drawable.get_colormap(), 0, 0, 0, 0, *drawable.get_size())
        pixbuf.save(file_path, file_type)
