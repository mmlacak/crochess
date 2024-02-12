#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import enum
from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneClassicalChessMixin:

    def scn_cc_01_init( self, bt=BoardType.Classical ):

        scene = Scene( 'scn_cc_01_init', bt )

        return scene
