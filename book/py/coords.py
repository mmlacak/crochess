#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


class Pos(object):

    def __init__(self, x, y):
        assert isinstance(x, (int, float))
        assert isinstance(y, (int, float))

        assert type(x) is type(y)

        self.x = x
        self.y = y

    def as_tuple(self):
        return (self.x, self.y)

    @staticmethod
    def from_tuple(tpl):
        return RectPos( *tpl[ 0 : 2 ] )


class RectPos(object):

    def __init__(self, left, top, right, bottom):
        assert isinstance(left, (int, float))
        assert isinstance(top, (int, float))
        assert isinstance(right, (int, float))
        assert isinstance(bottom, (int, float))

        assert type(left) is type(top) is type(right) is type(bottom)

        self.left = left
        self.top = top
        self.right = right
        self.bottom = bottom

    def as_tuple(self):
        return (self.left, self.top, self.right, self.bottom)

    @staticmethod
    def from_tuple(tpl):
        return RectPos( *tpl[ 0 : 4 ] )
