#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from consts import  DEFAULT_LINE_WIDTH, \
                    DEFAULT_IMAGE_FOLDER_REL_PATH, \
                    DEFAULT_FILE_EXT
from utils import iterate

from pixel_math import assert_floor_2
from colors import Colors
from piece import PieceType
from board import BoardType, Board
from scene import Scene
from scene_common import SceneCommon
from scene_mix import SceneMix
from scene_isa import SceneIsa
from scene_test import SceneTest

from draw_scene import DrawScene
from def_mark import MarkDef
from def_render import RenderingSizeEnum, get_rendering_size_item


def sanitize(name):
    assert isinstance(name, str)
    return name.replace('\'', '_').replace(' ', '_').lower()


class SaveScene:

    def __init__(self, rendering_size):
        self.rendering_size = RenderingSizeEnum(rendering_size)
        self.rendering_size_item = get_rendering_size_item(self.rendering_size)

    def get_default_size_pix(self, scene, max_width_pix=None, max_height_pix=None):
        assert isinstance(scene, Scene)

        w_pix = max_width_pix if max_width_pix is not None else self.rendering_size_item.board_width_pix
        h_pix = max_height_pix if max_height_pix is not None else self.rendering_size_item.board_max_height_pix

        assert isinstance(w_pix, int) and w_pix > 0
        assert isinstance(h_pix, int) and h_pix > 0

        w = w_pix * scene.board_view.width / scene.board.get_width()
        h = h_pix * scene.board_view.height / scene.board.get_height()

        return assert_floor_2(w, h)

    def save_scene(self, scene, file_path, max_width_pix=None, max_height_pix=None, line_width=DEFAULT_LINE_WIDTH, enforce_size=False, enforce_bw=False):
        assert isinstance(scene, Scene)
        assert isinstance(file_path, str)

        w_pix, h_pix = (max_width_pix, max_height_pix) if enforce_size else self.get_default_size_pix(scene, max_width_pix=max_width_pix, max_height_pix=max_height_pix)

        colors_item = Colors.fetch_colors(scene.board.type, enforce_bw=enforce_bw)
        mark_def_item = MarkDef[ scene.board.type ]

        ds = DrawScene(scene, w_pix, h_pix, line_width=line_width, color_str=colors_item.field.light)

        ds.draw_scene(colors_item, mark_def_item=mark_def_item)

        ds.save_image(file_path)

    #
    # boards

    def get_board_file_path(self, board_type, path_prefix=None, file_ext=None):
        bt = BoardType(board_type)

        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(bt)
        name = bt.get_name()
        sanitized = sanitize(name)
        return '%s/boards/%02d_%s%s' % (path_prefix, index, sanitized, file_ext)

    def render_all_boards(self, path_prefix=None):
        print()
        print( "Rendering all boards." if self.rendering_size.needs_rendering() else "Info all boards." )

        sc = SceneCommon()

        for bt in BoardType.iter():
            file_path = self.get_board_file_path(bt, path_prefix=path_prefix)
            print( file_path )

            if self.rendering_size.needs_rendering():
                scene = sc.intro_board(bt)
                self.save_scene(scene, file_path)

        print( "Finished." )

    #
    # pieces

    def get_piece_file_path(self, piece_type, board_type=None, path_prefix=None, pieces_folder='pieces', file_ext=None):
        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
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

        print()
        print( "Rendering %s pieces." % piece_str if self.rendering_size.needs_rendering() else "Info %s pieces." % piece_str )

        sc = SceneCommon()

        for bt in BoardType.iter():
            pts = piece_type or bt.get_newly_introduced_pieces( include_classical=True )

            for pt in iterate( pts ):
                pf = 'pieces'
                if is_rendering_one_piece:
                    piece_name = PieceType(piece_type).get_name()
                    pf += '/' + sanitize(piece_name)

                _bt = bt if is_rendering_one_piece else None
                file_path = self.get_piece_file_path(pt, board_type=_bt, pieces_folder=pf, path_prefix=path_prefix)
                print( file_path )

                if self.rendering_size.needs_rendering():
                    scene = sc.intro_piece(bt, piece_type=pt)

                    self.save_scene(scene, file_path, \
                                    max_width_pix=self.rendering_size_item.piece_2_by_2_pix, \
                                    max_height_pix=self.rendering_size_item.piece_2_by_2_pix, \
                                    enforce_size=True)

        print( "Finished." )

    #
    # en passant

    def get_en_passant_file_path(self, board_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(board_type)
        name = board_type.get_name()
        sanitized = sanitize(name)
        return '%s/en_passants/%02d_%s_en_passant%s' % (path_prefix, index, sanitized, file_ext)

    def render_all_en_passant_scenes(self, path_prefix=None):
        print()
        print( "Rendering all en passant." if self.rendering_size.needs_rendering() else "Info all en passant." )

        sc = SceneCommon()

        for bt in BoardType.iter():
            file_path = self.get_en_passant_file_path(bt, path_prefix=path_prefix)
            print( file_path )

            if self.rendering_size.needs_rendering():
                scene = sc.intro_en_passant(bt)
                self.save_scene(scene, file_path)

        print( "Finished." )

    #
    # rush

    def get_rush_file_path(self, board_type, path_prefix=None, file_ext=None):
        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        index = int(board_type)
        name = board_type.get_name()
        sanitized = sanitize(name)
        return '%s/rush/%02d_%s_rush%s' % (path_prefix, index, sanitized, file_ext)

    def render_all_rush_scenes(self, path_prefix=None):
        print()
        print( "Rendering all rush." if self.rendering_size.needs_rendering() else "Info all rush." )

        sc = SceneCommon()

        for bt in BoardType.iter():
            file_path = self.get_rush_file_path(bt, path_prefix=path_prefix)
            print( file_path )

            if self.rendering_size.needs_rendering():
                scene = sc.intro_rush(bt)
                self.save_scene(scene, file_path)

        print( "Finished." )

    #
    # castling

    # TODO :: OLD :: DELETE
    #
    # def get_castling_file_path(self, board_type, path_prefix=None, file_ext=None, move_king=0):
    #     bt = BoardType(board_type)
    #     assert isinstance(move_king, int)

    #     path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
    #     file_ext = file_ext or DEFAULT_FILE_EXT

    #     mk_str = ""
    #     if move_king < 0:
    #         mk_str = "left"
    #     elif move_king > 0:
    #         mk_str = "right"

    #     if move_king != 0:
    #         mk_str += "_%02d" % abs(move_king)

    #     name = bt.get_name()
    #     sf_name = "%02d_%s" % (bt, bt.get_label())
    #     sanitized = sanitize(name)

    #     if move_king == 0:
    #         return '%s/castlings/%s/%s_castling%s' % (path_prefix, sf_name, sanitized, file_ext)
    #     else:
    #         return '%s/castlings/%s/%s_castling_%s%s' % (path_prefix, sf_name, sanitized, mk_str, file_ext)

    # def render_all_castling_scenes(self, move_king=None, path_prefix=None):
    #     print()
    #     print( "Rendering all castlings." if self.rendering_size.needs_rendering() else "Info all castlings." )

    #     sc = SceneCommon()

    #     for bt in BoardType.iter( include_simple=False ): # TODO :: FIX :: --> include_simple=True --> DELETE
    #         king_moves = []

    #         if isinstance(move_king, int):
    #             king_moves = [move_king]
    #         else:
    #             diff_min, diff_max = Board.get_castling_limits(bt) # TODO :: FIX :: return list

    #             king_moves = list(range(diff_min, diff_max+1))
    #             king_moves.append(0)
    #             king_moves.extend( list(range(-diff_min, -diff_max-1, -1)) )

    #             king_moves.sort()

    #         for mk in king_moves:
    #             file_path = self.get_castling_file_path(bt, path_prefix=path_prefix, move_king=mk)
    #             print( file_path )

    #             if self.rendering_size.needs_rendering():
    #                 scene = sc.intro_castling(bt, move_king=mk) # TODO :: FIX :: add Rook initial position
    #                 self.save_scene(scene, file_path)

    #     print( "Finished." )
    #
    # TODO :: OLD :: DELETE

    def get_castling_file_path( self, board_type, path_prefix=None, file_ext=None, move_king=0, rook_file_init=None ):
        bt = BoardType( board_type )
        assert isinstance( move_king, int )
        assert isinstance( rook_file_init, int )

        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        mk_str = ""
        if move_king < 0:
            mk_str = "_left"
        elif move_king > 0:
            mk_str = "_right"

        if move_king != 0:
            mk_str += "_%02d" % abs( move_king )

        if rook_file_init is not None:
            mk_str += "_%02d" % abs( rook_file_init )

        name = bt.get_name()
        sf_name = "%02d_%s" % ( bt, bt.get_label() )
        sanitized = sanitize( name )

        if mk_str == "":
            return '%s/castlings/%s/%s_castling%s' % ( path_prefix, sf_name, sanitized, file_ext )
        else:
            return '%s/castlings/%s/%s_castling%s%s' % ( path_prefix, sf_name, sanitized, mk_str, file_ext )

    def render_all_castling_scenes(self, path_prefix=None):
        print()
        print( "Rendering all castlings." if self.rendering_size.needs_rendering() else "Info all castlings." )

        sc = SceneCommon()

        for bt in BoardType.iter():
            king_moves = []

            file_king, files_rooks_l, files_rooks_r = Board.get_castling_files( bt )

            for fr in files_rooks_l:
                diff_min, diff_max = Board.get_castling_limits( bt, file_rook_init=fr )

                file_path = self.get_castling_file_path( bt, path_prefix=path_prefix, move_king=0, rook_file_init=fr )
                print( file_path )

                if self.rendering_size.needs_rendering():
                    scene = sc.intro_castling( bt, move_king=0, rook_file_init=fr )
                    self.save_scene( scene, file_path )

            files_rooks = files_rooks_l[ : ]
            files_rooks.extend( files_rooks_r )

            for fr in files_rooks:
                diff_min, diff_max = Board.get_castling_limits( bt, file_rook_init=fr )

                if file_king < fr:
                    king_moves = list( range( diff_min, diff_max+1 ) )
                elif fr < file_king:
                    king_moves = list( range( -diff_min, -diff_max-1, -1 ) )

                # king_moves.sort()

                for mk in king_moves:
                    file_path = self.get_castling_file_path( bt, path_prefix=path_prefix, move_king=mk, rook_file_init=fr )
                    print( file_path )

                    if self.rendering_size.needs_rendering():
                        scene = sc.intro_castling( bt, move_king=mk, rook_file_init=fr )
                        self.save_scene( scene, file_path )

        print( "Finished." )

    #
    # scene

    def get_scene_file_path(self, file_name, path_prefix=None, file_ext=None, subfolder_name=None):
        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT

        if subfolder_name is None:
            return '%s/examples/%s%s' % (path_prefix, file_name, file_ext)
        else:
            return '%s/examples/%s/%s%s' % (path_prefix, subfolder_name, file_name, file_ext)

    def render_example(self, scene, func, board_types=None, path_prefix=None, enforce_in_bw=[]):
        assert isinstance(scene, SceneMix)
        assert callable(func)
        assert isinstance(enforce_in_bw, list)

        scene = func()
        if scene.board.type in board_types:
            sf_name = "%02d_%s" % (scene.board.type, scene.board.type.get_label())
            file_path = self.get_scene_file_path(scene.file_name, path_prefix=path_prefix, subfolder_name=sf_name)
            print( file_path )

            if self.rendering_size.needs_rendering():
                _enforce_bw = scene.board.type.in_variants( enforce_in_bw )
                self.save_scene(scene, file_path, enforce_bw=_enforce_bw)

    def render_examples(self, do_all_examples=False, board_types=None, path_prefix=None, enforce_in_bw=[]):
        _str = "all" if do_all_examples else "recent"
        print()
        print( "Rendering %s examples." % _str if self.rendering_size.needs_rendering() else "Info %s examples." % _str )
        sm = SceneMix()

        scene_funcs = sm.get_all_scene_methods() \
                      if do_all_examples \
                      else sm.get_recent_scene_methods()

        bts = board_types if board_types is not None else BoardType.get_all_list()

        for func in scene_funcs:
            self.render_example(sm, func, board_types=bts, path_prefix=path_prefix, enforce_in_bw=enforce_in_bw)

        print( "Finished." )

    #
    # initial setup analysis

    def get_isa_file_path(self, file_name, board_type, path_prefix=None, file_ext=None, subfolder_name=None):
        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT
        bt = BoardType(board_type)

        if subfolder_name is None:
            return '%s/isa/isa_%02d_%s%s' % (path_prefix, bt, file_name, file_ext)
        else:
            return '%s/isa/%s/isa_%02d_%s%s' % (path_prefix, subfolder_name, bt, file_name, file_ext)

    def render_isa(self, scene, func, do_centaur=False, do_patterns=False, board_types=None, path_prefix=None, enforce_in_bw=[]):
        assert isinstance(scene, SceneIsa)
        assert callable(func)
        assert isinstance(enforce_in_bw, list)

        for index, scene in enumerate( func(do_centaur=do_centaur, do_patterns=do_patterns, board_types=board_types) ):
            board_type = scene.board.type
            sf_name = "%02d_%s" % (scene.board.type, scene.board.type.get_label())
            file_path = self.get_isa_file_path(scene.file_name, board_type, path_prefix=path_prefix, subfolder_name=sf_name)
            print( file_path )

            if self.rendering_size.needs_rendering():
                _enforce_bw = scene.board.type.in_variants( enforce_in_bw )
                self.save_scene(scene, file_path, enforce_bw=_enforce_bw)

    def render_ISAs(self, do_centaur=False, do_patterns=False, board_types=None, path_prefix=None, enforce_in_bw=[]):
        # _str = "all" if do_all_examples else "default"
        print()
        # print( "Rendering %s ISAs." % _str if self.rendering_size.needs_rendering() else "Info %s ISAs." % _str )
        print( "Rendering ISAs." if self.rendering_size.needs_rendering() else "Info ISAs." )
        si = SceneIsa()

        # scene_funcs = si.get_all_scene_methods(prefix='isa_') \
        #               if do_all_examples \
        #               else si.get_recent_scene_methods()
        scene_funcs = si.get_all_scene_methods(prefix='isa_')

        bts = board_types if board_types is not None else BoardType.get_all_list()

        for func in scene_funcs:
            self.render_isa(si, func, do_centaur=do_centaur, do_patterns=do_patterns, board_types=bts, path_prefix=path_prefix, enforce_in_bw=enforce_in_bw)

        print( "Finished." )

    #
    # tests

    def get_test_file_path(self, file_name, path_prefix=None, file_ext=None, subfolder_name=None): # board_type
        path_prefix = path_prefix or DEFAULT_IMAGE_FOLDER_REL_PATH
        file_ext = file_ext or DEFAULT_FILE_EXT
        # bt = BoardType(board_type)

        if subfolder_name is None:
            return '%s/test/%s%s' % (path_prefix, file_name, file_ext)
        else:
            return '%s/test/%s/%s%s' % (path_prefix, subfolder_name, file_name, file_ext)

    def render_test(self, scene, func, path_prefix=None, enforce_cot_in_bw=True):
        assert isinstance(scene, SceneTest)
        assert callable(func)
        assert isinstance(enforce_cot_in_bw, bool)

        scene = func()
        # sf_name = "%02d_%s" % (scene.board.type, scene.board.type.get_label())
        file_path = self.get_test_file_path(scene.file_name, path_prefix=path_prefix, subfolder_name=None) # subfolder_name=sf_name
        print( file_path )

        if self.rendering_size.needs_rendering():
            enforce_bw = True # enforce_cot_in_bw and scene.board.type.is_variant( BoardType.ConquestOfTlalocan )
            self.save_scene(scene, file_path, enforce_bw=enforce_bw)

    def render_tests(self, do_all_tests=True, path_prefix=None, enforce_cot_in_bw=True):
        _str = "all" if do_all_tests else "recent"
        print()
        print( "Rendering %s tests." % _str if self.rendering_size.needs_rendering() else "Info %s tests." % _str )
        st = SceneTest()

        scene_funcs = st.get_all_scene_methods(prefix='test_') \
                      if do_all_tests \
                      else st.get_recent_scene_methods()

        for func in scene_funcs:
            self.render_test(st, func, path_prefix=path_prefix, enforce_cot_in_bw=enforce_cot_in_bw)

        print( "Finished." )


def test_boards():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_boards(path_prefix='temp/')

def test_pieces():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_pieces(path_prefix='temp/')
    ss.render_all_pieces(piece_type=PieceType.Star, path_prefix='temp/')
    ss.render_all_pieces(piece_type=PieceType.Bishop, path_prefix='temp/')

def test_en_passant():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_en_passant_scenes(path_prefix='temp/')

def test_rush():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_all_rush_scenes(path_prefix='temp/')

def test_castling_init():
    ss = SaveScene(RenderingSizeEnum.Draft)
    # ss.render_all_castling_scenes(path_prefix='temp/', move_king=0)
    # ss.render_all_castling_scenes(path_prefix='temp/', move_king=-2)
    # ss.render_all_castling_scenes(path_prefix='temp/', move_king=2)
    ss.render_all_castling_scenes( path_prefix='temp/' )

def test_scene_examples():
    ss = SaveScene(RenderingSizeEnum.Draft)
    ss.render_examples(do_all_examples=True, path_prefix='temp/', enforce_cot_in_bw=True)


if __name__ == '__main__':
    # test_boards()
    # test_pieces()
    # test_en_passant()
    # test_rush()
    test_castling_init()
    # test_scene_examples()
