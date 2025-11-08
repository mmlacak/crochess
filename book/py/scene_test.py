#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene
from scene_mixin import SceneMixin

import gen_steps as GS


class SceneTest( SceneMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    'test_o_1', \
                ]


    def test_o_1( self, bt=BoardType.One ):

        scene = Scene( 'test_o_1', bt )

        scene.board.set_piece( 12, 12, PieceType.Starchild )

        scene.append_field_marker( 13, 14, mark_type=MarkType.Action )
        scene.append_field_marker( 15, 14, mark_type=MarkType.Action )
        scene.append_field_marker( 13, 16, mark_type=MarkType.Action )
        scene.append_field_marker( 15, 16, mark_type=MarkType.Action )

        return scene
