#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from consts import DEFAULT_LINE_WIDTH

from board import Board
from colors import ColorsItem
from def_mark import MarkDefItem
from board_view import BoardView
from draw_mark import DrawMark
from scene import Scene


class DrawScene( DrawMark ):

    def __init__( self, scene, max_width_pix, max_height_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF" ):
        assert isinstance( scene, Scene )
        assert isinstance( scene.board, Board )
        assert isinstance( scene.board_view, BoardView )

        super( DrawScene, self ).__init__( scene.board, max_width_pix, max_height_pix, line_width=DEFAULT_LINE_WIDTH, color_str="#FFFFFF", board_view=scene.board_view )

        self.scene = scene

    def draw_scene( self, colors_item, mark_def_item=None ):
        assert isinstance( colors_item, ColorsItem )
        assert isinstance( mark_def_item, (MarkDefItem, type( None )) )

        self.draw_board( colors_item )

        fmdef = mark_def_item.field_mark_def if isinstance( mark_def_item, MarkDefItem ) else None
        self.draw_all_field_markers( self.scene.field_markers, fmdef=fmdef, cmark=colors_item.marker )

        adef = mark_def_item.arrow_def if isinstance( mark_def_item, MarkDefItem ) else None
        self.draw_all_arrows( self.scene.arrows, adef=adef, cmark=colors_item.arrow )

        fdef = mark_def_item.font_def if isinstance( mark_def_item, MarkDefItem ) else None
        self.draw_all_texts( self.scene.texts, fdef=fdef, cmark=colors_item.text )


TEST_BOARD_SIZE_PIX = 1200
TEST_FIELD_SIZE_PIX = 200 # 100 # 400

def test_scene( func_name, board_desc=None, name='', *args, **kwargs ):
    sc = SceneCommon()
    func = getattr( sc, func_name )

    for bt in BoardType.iter( include_none=False, include_even=True ):
        scene = func( bt, *args, **kwargs )

        if func_name == 'intro_board':
            w = TEST_BOARD_SIZE_PIX
            h = TEST_BOARD_SIZE_PIX
            fs_px = w / scene.board_view.width
        else:
            w = scene.board_view.width * TEST_FIELD_SIZE_PIX
            h = scene.board_view.height * TEST_FIELD_SIZE_PIX
            fs_px = TEST_FIELD_SIZE_PIX

        ds = DrawScene( scene, w, h, fs_px )

        ci = Colors[ bt ]
        ds.draw_scene(ci)

        fn = func_name[ 6 : ] if func_name.startswith( 'intro_' ) else func_name
        btn = bt.get_symbol()
        file_path = 'temp/%s.%s.%s.IGNORE.png' % (fn, btn, name)
        ds.save_image( file_path )

if __name__ == '__main__':
    from board import BoardType
    from colors import Colors
    from scene_common import SceneCommon

    test_scene( 'intro_piece' )
    test_scene( 'intro_castling', name='K' )
    test_scene( 'intro_castling', move_king=-2, name='-2' )
    test_scene( 'intro_castling', move_king=2, name='2' )
    test_scene( 'intro_en_passant' )
    test_scene( 'intro_rush' )
    test_scene( 'intro_board' )
