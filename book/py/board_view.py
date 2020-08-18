#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from board import BoardType, Board


class Margin:
    def __init__(self, left=0.0, top=0.0, right=0.0, bottom=0.0):
        assert isinstance(left, float)
        assert isinstance(top, float)
        assert isinstance(right, float)
        assert isinstance(bottom, float)

        assert left >= 0.0
        assert top >= 0.0
        assert right >= 0.0
        assert bottom >= 0.0

        self.left = left
        self.top = top
        self.right = right
        self.bottom = bottom

    def as_tuple(self):
        return (self.left, self.top, self.right, self.bottom)

    @staticmethod
    def from_tuple(tpl):
        return Margin( *tpl[ 0 : 4 ] )


class BoardView:
    def __init__(self, x=0.0, y=0.0, width=None, height=None, reverse_off_board_field_colors=False, margin=None, board_type=None):
        assert isinstance(x, float)
        assert isinstance(y, float)
        assert isinstance(width, (float, type(None)))
        assert isinstance(height, (float, type(None)))

        assert isinstance(reverse_off_board_field_colors, bool)
        assert isinstance(margin, (Margin, type(None)))

        self.x = x
        self.y = y

        bt = BoardType(board_type) if board_type is not None else None
        self.width = width if width is not None else ( bt.get_size() if bt is not None else 1.0 )
        self.height = height if height is not None else ( bt.get_size() if bt is not None else 1.0 )

        assert self.width > 0.0
        assert self.height > 0.0

        self.reverse_off_board_field_colors = reverse_off_board_field_colors
        self.margin = margin if margin is not None else Margin()

    def get_position_limits(self):
        return ((self.x, self.y), (self.x + self.width, self.y + self.height))

    def as_tuple(self):
        return (self.x, self.y, self.width, self.height, self.reverse_off_board_field_colors, self.margin)

    @staticmethod
    def from_tuple(tpl):
        return Margin( *tpl[ 0 : 6 ] )
