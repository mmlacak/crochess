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
    def __init__(self, board_or_type, left=0.0, top=0.0, width=None, height=None, reverse_off_board_field_colors=False, border=None):
        assert isinstance(board_or_type, (Board, BoardType))

        assert isinstance(left, float)
        assert isinstance(top, float)
        assert isinstance(width, (float, type(None)))
        assert isinstance(height, (float, type(None)))

        assert isinstance(reverse_off_board_field_colors, bool)
        assert isinstance(border, (Border, type(None)))

        self.board = board_or_type if isinstance(board_or_type, Board) else Board(board_or_type)

        self.left = left
        self.top = top
        self.width = width or self.board.get_width()
        self.height = height or self.board.get_height()

        self.reverse_off_board_field_colors = reverse_off_board_field_colors
        self.border = border or Border()

    def as_tuple(self):
        return (self.board, self.left, self.top, self.width, self.height, self.reverse_off_board_field_colors, self.border)

    @staticmethod
    def from_tuple(tpl):
        return Border( *tpl[ 0 : 7 ] )
