#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from board import BoardType
from maps import Map


class ColorsPair(Map)
    def __init__(self, fg=None, bg=None):
        assert isinstance(fg, str)
        assert isinstance(bg, str)

        self.fg = fg
        self.bg = bg

    def as_tuple(self):
        return (self.fg, self.bg)


class ColorsShade(Map):
    def __init__(self, light=None, dark=None):
        assert isinstance(light, ColorsPair)
        assert isinstance(dark, ColorsPair)

        self.light = light
        self.dark = dark

    def as_tuple(self):
        return (self.light, self.dark)


class ColorsMark(Map):
    def __init__(self, legal=None, ilegal=None, action=None, forbidden=None):
        assert isinstance(legal, ColorsPair)
        assert isinstance(ilegal, ColorsPair)
        assert isinstance(action, ColorsPair)
        assert isinstance(forbidden, ColorsPair)

        self.legal = legal
        self.ilegal = ilegal
        self.action = action
        self.forbidden = forbidden

    def as_tuple(self):
        return (self.legal, self.ilegal, self.action, self.forbidden)


class ColorsItem(Map):
    def __init__(self, piece=None, field=None, arrow=None, text=None, marker=None):
        assert isinstance(piece, ColorsShade)
        assert isinstance(field, ColorsShade)
        assert isinstance(arrow, ColorsMark)
        assert isinstance(text, ColorsMark)
        assert isinstance(marker, ColorsMark)

        self.piece = piece
        self.field = field
        self.arrow = arrow
        self.text = text
        self.marker = marker

    def as_tuple(self):
        return (self.piece, self.field, self.arrow, self.text, self.marker)


class Colors(Map):
    pass
