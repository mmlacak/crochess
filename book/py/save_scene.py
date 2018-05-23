#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


DEFAULT_PATH = '../gfx/' # '../tmp/'

from colors import Colors
from board import BoardType, Board
from scene import Scene
from draw import DEFAULT_FILE_EXT, DEFAULT_FILE_TYPE, get_new_drawable, get_new_gc
from draw_scene import DrawScene
from def_mark import MarkDef
from def_render import RenderingSizeEnum, get_rendering_size_item


class SaveScene(object):

    def __init__(self, rendering_size):
        self.rendering_size = RenderingSizeEnum(rendering_size)
        self.rendering_size_item = get_rendering_size_item(self.rendering_size)

    def init_drawable_gc(self, size_x=None, size_y=None, line_width=None):
        size_x = size_x or self.rendering_size_item.board_width_pix
        size_y = size_y or self.rendering_size_item.board_width_pix
        line_width = line_width or self.rendering_size_item.line_width_pix

        drawable = get_new_drawable(size_x, size_y)
        gc = get_new_gc(drawable, line_width)

        return drawable, gc

    def save_scene(self, scene, file_path, board_desc=None, size_x=None, size_y=None, line_width=None, file_type=None):
        assert isinstance(scene, Scene)
        assert isinstance(file_path, str)

        drawable, gc = self.init_drawable_gc(size_x=size_x, size_y=size_y, line_width=line_width)

        draw_scene = DrawScene(drawable, gc, scene, board_desc=board_desc)

        colors_item = Colors[ scene.board.type ]
        mark_def_item = MarkDef[ scene.board.type ]

        draw_scene.draw_scene(colors_item, mark_def_item=mark_def_item, gc=gc)

        file_type = file_type or DEFAULT_FILE_TYPE
        draw_scene.save_image(file_path, file_type=file_type)

    #
    # board

    def get_board_file_path(self, board_type, path_prefix=None, file_ext=None):
        bt = BoardType(board_type)

        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(bt)
        name = bt.get_name()
        sanitize = name.replace('\'', '_').replace(' ', '_').lower()
        return '%s/boards/%02d_%s%s' % (path_prefix, index, sanitize, file_ext)

    def render_all_boards(self, path_prefix=None):
        print
        print "Rendering all boards." if self.rendering_size.needs_rendering() else "Info all boards."

#         for bt in [ BoardType.Classical, ]: # TODO :: DEBUG
        for bt in filter(lambda _bt: s = _bt.get_size(), s > 0 and s % 2 == 0, \
                         iter(BoardType(0))):
            file_path = self.get_board_file_path(bt, path_prefix=path_prefix)
            print file_path

            if self.rendering_size.needs_rendering():
                board = Board(bt)
                board.setup()
                scene = Scene(board=board)

                self.save_scene(scene, file_path)

        print "Finished."


def test_board():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_boards(path_prefix='temp/')

if __name__ == '__main__':
    test_board()
