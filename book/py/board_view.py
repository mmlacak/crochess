#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from board import BoardType, Board


class Border:
    def __init__(self, left=0.0, top=0.0, right=0.0, bottom=0.0):
        assert isinstance(left, float)
        assert isinstance(top, float)
        assert isinstance(right, float)
        assert isinstance(bottom, float)

        self.left = left
        self.top = top
        self.right = right
        self.bottom = bottom

    def as_tuple(self):
        return (self.left, self.top, self.right, self.bottom)

    @staticmethod
    def from_tuple(tpl):
        return Border( *tpl[ 0 : 4 ] )


class BoardView:
    def __init__(self, x=0.0, y=0.0, width=None, height=None, reverse_off_board_field_colors=False, border=None, board_type=None):
        assert isinstance(x, float)
        assert isinstance(y, float)
        assert isinstance(width, (float, type(None)))
        assert isinstance(height, (float, type(None)))

        assert isinstance(reverse_off_board_field_colors, bool)
        assert isinstance(border, (Border, type(None)))
        assert isinstance(board_type, (BoardType, type(None)))

        self.x = x
        self.y = y
        self.width = width or board_type.get_size() if board_type is not None else 1.0
        self.height = height or board_type.get_size() if board_type is not None else 1.0

        self.reverse_off_board_field_colors = reverse_off_board_field_colors
        self.border = border or Border()

    def as_tuple(self):
        return (self.x, self.y, self.width, self.height, self.reverse_off_board_field_colors, self.border)

    @staticmethod
    def from_tuple(tpl):
        return Border( *tpl[ 0 : 6 ] )
