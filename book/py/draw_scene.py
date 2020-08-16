#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from colors import ColorsItem
from def_mark import MarkDefItem
from draw import DEFAULT_LINE_WIDTH
from draw_mark import DrawMark
from scene import Scene


class DrawScene(DrawMark):

    # def __init__(self, drawable, gc, scene, board_desc=None):
    def __init__(self, scene, max_width_pix, max_height_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF"):
        assert isinstance(scene, Scene)
        super(DrawScene, self).__init__(scene.board, max_width_pix, max_height_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF", board_view=scene.board_view)

        self.scene = scene

        # DrawBoard c'tor can instantiate board and view with default arguments; scene can have those None.
        self.scene.update(board=self.board, board_view=self.board_view)

    def draw_scene(self, colors_item, mark_def_item=None):
        assert isinstance(colors_item, ColorsItem)
        assert isinstance(mark_def_item, (MarkDefItem, type(None)))

        self.draw_board(colors_item)

        fmdef = mark_def_item.field_mark_def if isinstance(mark_def_item, MarkDefItem) else None
        self.draw_all_field_markers(self.scene.field_markers, fmdef=fmdef, cmark=colors_item.marker)

        adef = mark_def_item.arrow_def if isinstance(mark_def_item, MarkDefItem) else None
        self.draw_all_arrows(self.scene.arrows, adef=adef, cmark=colors_item.arrow)

        fdef = mark_def_item.font_def if isinstance(mark_def_item, MarkDefItem) else None
        self.draw_all_texts(self.scene.texts, fdef=fdef, cmark=colors_item.text)


TEST_FIELD_SIZE_PIX = 200 # 100 # 400

def test_scene(func_name, board_desc=None, name='', include_odd=False, *args, **kwargs):
    sc = SceneCommon()
    func = getattr(sc, func_name)

    for bt in BoardType.iter(include_none=False, include_even=True, include_odd=include_odd):
        func(bt, *args, **kwargs)

        w = sc.board_view.width * TEST_FIELD_SIZE_PIX
        h = sc.board_view.height * TEST_FIELD_SIZE_PIX

        ds = DrawScene(sc, w, h, TEST_FIELD_SIZE_PIX)

        ci = Colors[ bt ]
        ds.draw_scene(ci)

        fn = func_name[ 6 : ] if func_name.startswith('intro_') else func_name
        btn = bt.get_name()
        file_path = 'temp/%s.%s.%s.IGNORE.png' % (fn, btn, name)
        ds.save_image(file_path)

if __name__ == '__main__':
    from board import BoardType
    from colors import Colors
    from scene_common import SceneCommon

    test_scene('intro_piece')
    test_scene('intro_castling')
    test_scene('intro_castling', move_king=-2)
    test_scene('intro_castling', move_king=2)
    test_scene('intro_en_passant')
    test_scene('intro_rush')
