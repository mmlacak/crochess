#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


DEFAULT_PATH = '../gfx/' # '../tmp/'

from types import NoneType

from colors import Colors
from piece import PieceType
from board import BoardType, Board
from scene import Scene
from scene_common import SceneCommon
from scene_mix import SceneMix

from draw import DEFAULT_FILE_EXT, DEFAULT_FILE_TYPE, get_new_drawable, get_new_gc
from draw_board import BoardDesc
from draw_scene import DrawScene
from def_mark import MarkDef
from def_render import RenderingSizeEnum, get_rendering_size_item


def sanitize(name):
    assert isinstance(name, str)
    return name.replace('\'', '_').replace(' ', '_').lower()


class SaveScene(object):

    def __init__(self, rendering_size):
        self.rendering_size = RenderingSizeEnum(rendering_size)
        self.rendering_size_item = get_rendering_size_item(self.rendering_size)

    def normalize(self, size_x, size_y):
        size_x = size_x or self.rendering_size_item.board_width_pix
        size_y = size_y or self.rendering_size_item.board_width_pix

        size_x = size_x if size_x < self.rendering_size_item.board_width_pix else self.rendering_size_item.board_width_pix
        size_y = size_y if size_y < self.rendering_size_item.board_max_height_pix else self.rendering_size_item.board_max_height_pix

        return (size_x, size_y)

    def calc_nominal_dpf(self, board_type):
        # dpf == dots-per-field
        bt = BoardType(board_type)
        return self.rendering_size_item.board_width_pix // bt.get_size()

    def recalc_image_size(self, board, board_desc=None, size_x=None, size_y=None):
        assert isinstance(board, Board)

        board_desc = board_desc or BoardDesc()
        assert isinstance(board_desc, (BoardDesc, NoneType))

        size_x, size_y = self.normalize(size_x, size_y)

        board_width = board_desc.off_board_left + board.get_width() + board_desc.off_board_right
        board_height = board_desc.off_board_top + board.get_height() + board_desc.off_board_bottom

        # dots-per-field
        nominal_dpf = self.calc_nominal_dpf(board.type)
        horizontal_dpf = ( size_x - board_desc.border_left_pix - board_desc.border_right_pix ) // board_width
        vertical_dpf = ( size_y - board_desc.border_top_pix - board_desc.border_bottom_pix ) // board_height
        minimal_dpf = min(nominal_dpf, horizontal_dpf, vertical_dpf)

        height_pix = board_height * minimal_dpf + board_desc.border_top_pix + board_desc.border_bottom_pix
        width_pix = board_width * minimal_dpf + board_desc.border_left_pix + board_desc.border_right_pix

        width_pix, height_pix = self.normalize(width_pix, height_pix)

        return (width_pix, height_pix)

    def init_drawable_gc(self, size_x=None, size_y=None, line_width=None, board=None, board_desc=None):
        # Flag is used to trigger recalculation of sizes, because board will always be valid
        # if this method is called from save_scene(), which enforces scene to be valid, and hence board, too.
        sizes_not_specified = (size_x is None) and (size_y is None)

        size_x, size_y = self.normalize(size_x, size_y)
        line_width = line_width or self.rendering_size_item.line_width_pix

        if sizes_not_specified and isinstance(board, Board):
            size_x, size_y = self.recalc_image_size(board, board_desc=board_desc, size_x=size_x, size_y=size_y)

        drawable = get_new_drawable(size_x, size_y)
        gc = get_new_gc(drawable, line_width)

        return drawable, gc

    def save_scene(self, scene, file_path, size_x=None, size_y=None, line_width=None, file_type=None):
        assert isinstance(scene, Scene)
        assert isinstance(file_path, str)

        drawable, gc = self.init_drawable_gc(size_x=size_x, size_y=size_y, line_width=line_width, board=scene.board, board_desc=scene.board_desc)

        draw_scene = DrawScene(drawable, gc, scene, board_desc=scene.board_desc)

        colors_item = Colors.fetch_colors(scene.board.type)
        mark_def_item = MarkDef[ scene.board.type ]

        draw_scene.draw_scene(colors_item, mark_def_item=mark_def_item, gc=gc)

        file_type = file_type or DEFAULT_FILE_TYPE
        draw_scene.save_image(file_path, file_type=file_type)

    #
    # boards

    def get_board_file_path(self, board_type, path_prefix=None, file_ext=None):
        bt = BoardType(board_type)

        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(bt)
        name = bt.get_name()
        sanitized = sanitize(name)
        return '%s/boards/%02d_%s%s' % (path_prefix, index, sanitized, file_ext)

    def render_all_boards(self, path_prefix=None):
        print
        print "Rendering all boards." if self.rendering_size.needs_rendering() else "Info all boards."

        for bt in BoardType.iter():
            file_path = self.get_board_file_path(bt, path_prefix=path_prefix)
            print file_path

            if self.rendering_size.needs_rendering():
                board = Board(bt)
                board.setup()
                scene = Scene(board=board)

                self.save_scene(scene, file_path)

        print "Finished."

    #
    # pieces

    def get_piece_file_path(self, piece_type, board_type=None, path_prefix=None, pieces_folder='pieces', file_ext=None):
        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        is_rendering_one_piece = board_type is not None

        bt = BoardType(board_type) if is_rendering_one_piece else None
        pt = PieceType(piece_type)
        index = int(pt) if not is_rendering_one_piece else int(bt)
        name = pt.get_name() if not is_rendering_one_piece else bt.get_name()
        sanitized = sanitize(name)
        return '%s/%s/%02d_%s%s' % (path_prefix, pieces_folder, index, sanitized, file_ext)

    def render_all_pieces(self, piece_type=None, path_prefix=None):
        is_rendering_one_piece = piece_type is not None

        piece_str = "all" if not is_rendering_one_piece else PieceType(piece_type).get_name()

        print
        print "Rendering %s pieces." % piece_str if self.rendering_size.needs_rendering() else "Info %s pieces." % piece_str

        for bt in BoardType.iter():
            pt = piece_type or bt.get_newly_introduced_piece()

            if pt is not None:
                pf = 'pieces'
                if is_rendering_one_piece:
                    piece_name = PieceType(piece_type).get_name()
                    pf += '/' + sanitize(piece_name)

                _bt = bt if is_rendering_one_piece else None
                file_path = self.get_piece_file_path(pt, board_type=_bt, pieces_folder=pf, path_prefix=path_prefix)
                print file_path

                if self.rendering_size.needs_rendering():
                    scene = SceneCommon()
                    scene.intro_piece(bt, piece_type=pt)

                    self.save_scene(scene, file_path, \
                                    size_x=self.rendering_size_item.piece_2_by_2_pix, \
                                    size_y=self.rendering_size_item.piece_2_by_2_pix)

        print "Finished."

    #
    # en passant

    def get_en_passant_file_path(self, board_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(board_type)
        name = board_type.get_name()
        sanitized = sanitize(name)
        return '%s/en_passants/%02d_%s_en_passant%s' % (path_prefix, index, sanitized, file_ext)

    def render_all_en_passant_scenes(self, path_prefix=None):
        print
        print "Rendering all en passant." if self.rendering_size.needs_rendering() else "Info all en passant."

        for bt in BoardType.iter():
            file_path = self.get_en_passant_file_path(bt, path_prefix=path_prefix)
            print file_path

            if self.rendering_size.needs_rendering():
                scene = SceneCommon()
                scene.intro_en_passant(bt)

                self.save_scene(scene, file_path)

        print "Finished."

    #
    # rush

    def get_rush_file_path(self, board_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(board_type)
        name = board_type.get_name()
        sanitized = sanitize(name)
        return '%s/rush/%02d_%s_rush%s' % (path_prefix, index, sanitized, file_ext)

    def render_all_rush_scenes(self, path_prefix=None):
        print
        print "Rendering all rush." if self.rendering_size.needs_rendering() else "Info all rush."

        for bt in BoardType.iter():
            file_path = self.get_rush_file_path(bt, path_prefix=path_prefix)
            print file_path

            if self.rendering_size.needs_rendering():
                scene = SceneCommon()
                scene.intro_rush(bt)

                self.save_scene(scene, file_path)

        print "Finished."

    #
    # castling

    def get_castling_file_path(self, board_type, path_prefix=None, file_ext=None, move_king=0):
        bt = BoardType(board_type)
        assert isinstance(move_king, int)

        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        mk_str = ""
        if move_king < 0:
            mk_str = "left"
        elif move_king > 0:
            mk_str = "right"

        if move_king != 0:
            mk_str += "_%02d" % abs(move_king)

        name = bt.get_name()
        sf_name = "%02d_%s" % (bt, bt.get_symbol().lower())
        sanitized = sanitize(name)

        if move_king == 0:
            return '%s/castlings/%s/%s_castling%s' % (path_prefix, sf_name, sanitized, file_ext)
        else:
            return '%s/castlings/%s/%s_castling_%s%s' % (path_prefix, sf_name, sanitized, mk_str, file_ext)

    def render_all_castling_scenes(self, move_king=None, path_prefix=None):
        print
        print "Rendering all castlings." if self.rendering_size.needs_rendering() else "Info all castlings."

        for bt in BoardType.iter():
            king_moves = []

            if isinstance(move_king, int):
                king_moves = [move_king]
            else:
                diff_min, diff_max = Board.get_castling_limits(bt)

                king_moves = list(xrange(diff_min, diff_max+1))
                king_moves.append(0)
                king_moves.extend( list(xrange(-diff_min, -diff_max-1, -1)) )

                king_moves.sort()

            for mk in king_moves:
                file_path = self.get_castling_file_path(bt, path_prefix=path_prefix, move_king=mk)
                print file_path

                if self.rendering_size.needs_rendering():
                    scene = SceneCommon()
                    scene.intro_castling(bt, move_king=mk)

                    self.save_scene(scene, file_path)

        print "Finished."

    #
    # scene

    def get_scene_file_path(self, file_name, path_prefix=None, file_ext=None, subfolder_name=None):
        path_prefix = path_prefix or DEFAULT_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        if subfolder_name is None:
            return '%s/examples/%s%s' % (path_prefix, file_name, file_ext)
        else:
            return '%s/examples/%s/%s%s' % (path_prefix, subfolder_name, file_name, file_ext)

    def render_examples(self, do_all_examples=False, path_prefix=None):
        def _render_examples(scene, func):
            name = func()
            sf_name = "%02d_%s" % (scene.board.type, scene.board.type.get_symbol().lower())
            file_path = self.get_scene_file_path(name, path_prefix=path_prefix, subfolder_name=sf_name)
            print file_path

            if self.rendering_size.needs_rendering():
                self.save_scene(scene, file_path)

        _str = "all" if do_all_examples else "recent"
        print
        print "Rendering %s examples." % _str if self.rendering_size.needs_rendering() else "Info %s examples." % _str
        scene = SceneMix()

        scene_funcs = scene.get_all_scene_methods() \
                      if do_all_examples \
                      else scene.get_recent_scene_methods()

        for func in scene_funcs:
            _render_examples(scene, func)

        print "Finished."


def test_boards():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_boards(path_prefix='temp/')

def test_pieces():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_pieces(path_prefix='temp/')
    ss.render_all_pieces(piece_type=PieceType.Star, path_prefix='temp/')

def test_en_passant():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_en_passant_scenes(path_prefix='temp/')

def test_rush():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_rush_scenes(path_prefix='temp/')

def test_castling_init():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_castling_scenes(path_prefix='temp/')

def test_scene_examples():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_examples(do_all_examples=True, path_prefix='temp/')


if __name__ == '__main__':
    test_boards()
    test_pieces()
    test_en_passant()
    test_rush()
    test_castling_init()
    test_scene_examples()
