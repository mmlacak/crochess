#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from types import NoneType

from colors import ColorsItem
from def_mark import MarkDefItem
from draw import Draw
from draw_mark import DrawMark
from scene import Scene


class DrawScene(Draw):

    def __init__(self, drawable, gc, scene, board_desc=None):
        super(DrawScene, self).__init__(drawable, gc)

        self.init_scene(scene, board_desc=board_desc)

    def init_scene(self, scene, board_desc=None):
        assert isinstance(scene, Scene)

        self.scene = scene

        self.draw_mark = DrawMark(self.drawable, self.gc, self.scene.board, board_desc=board_desc)

    def draw_scene(self, colors_item, mark_def_item=None, gc=None):
        assert isinstance(colors_item, ColorsItem)
        assert isinstance(mark_def_item, (MarkDefItem, NoneType))

        self.draw_mark.draw_board.draw_board(colors_item, gc=gc)

        fmdef = mark_def_item.field_mark_def if isinstance(mark_def_item, MarkDefItem) else None
        self.draw_mark.draw_all_field_markers(self.scene.field_markers, fmdef=fmdef, cmark=colors_item.marker, gc=gc) # , draw_outlined=False)

        adef = mark_def_item.arrow_def if isinstance(mark_def_item, MarkDefItem) else None
        self.draw_mark.draw_all_arrows(self.scene.arrows, adef=adef, cmark=colors_item.arrow, gc=gc)

        fdef = mark_def_item.font_def if isinstance(mark_def_item, MarkDefItem) else None
        self.draw_mark.draw_all_texts(self.scene.texts, fdef=fdef, cmark=colors_item.text, gc=gc)


TEST_FIELD_SIZE_PIX = 200 # 100 # 400
TEST_LINE_WIDTH = 5 # 3 # 11

def test_scene(func_name, board_desc=None, name='', include_odd=False, *args, **kwargs):
    sc = SceneCommon()
    func = getattr(sc, func_name)

    for bt in BoardType.iter(include_none=False, include_even=True, include_odd=include_odd):
        func(bt, *args, **kwargs)

        w = int(TEST_FIELD_SIZE_PIX * sc.board.get_width())
        h = int(TEST_FIELD_SIZE_PIX * sc.board.get_height())
        drw = get_new_drawable(w, h)
        gc = get_new_gc(drw, TEST_LINE_WIDTH)

        d = DrawScene(drw, gc, sc, board_desc=board_desc)

        d.draw_scene( Colors[BoardType.Classical] )

        fn = func_name[ 6 : ] if func_name.startswith('intro_') else func_name
        btn = bt.get_name()
        file_path = 'temp/%s.%s.%s.IGNORE.png' % (fn, btn, name)
        d.save_image(file_path)

if __name__ == '__main__':
    from draw import get_new_drawable, get_new_gc
    from board import BoardType
    from colors import Colors
    from scene_common import SceneCommon

    test_scene('intro_piece')
    test_scene('intro_castling')
    test_scene('intro_castling', move_king=-2)
    test_scene('intro_castling', move_king=2)
    test_scene('intro_en_passant')
    test_scene('intro_rush')
