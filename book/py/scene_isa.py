#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene
from scene_mixin import SceneMixin


class SceneIsa(SceneMixin):

    # overrides
    def _get_recent_scene_method_names(self):
        return  [
                    'isa_one', \
                ]

    def setup_board(self, bt, name):
        bt = BoardType(bt)
        scene = Scene(name, bt)

        scene.board.setup()
        return scene

    def find_piece(self, scene, piece_type, search_light=True, search_queen_side=True):
        pos_0, pos_max = scene.board.get_position_limits() # ((0, 0), (w, h))

        # pos +1 and -1, because range() does not include end limit into iteration.
        i_start, i_end, i_diff = (pos_0[0], pos_max[0]+1, 1) if search_queen_side else (pos_max[0], pos_0[0]-1, -1)
        j = pos_0[1] if search_light else pos_max[1]

        pt = PieceType(piece_type)
        pt = pt.get_light() if search_light else pt.get_dark()

        for i in range(i_start, i_end, i_diff):
            if scene.board.get_piece(i, j) == pt:
                return pt, i, j

        return None, None, None

    def isa_one(self, bt=BoardType.One):
        scene = self.setup_board(bt, 'isa_one')

        pos_C = self.find_piece(scene, PieceType.Centaur, search_light=True, search_queen_side=True)
        print(pos_C)

        yield scene

        yield scene

        yield scene
