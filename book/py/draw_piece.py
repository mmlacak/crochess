#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from colors import ColorsPair, ColorsShade, ColorsPiece
from draw import DrawableRectangle, Draw


class DrawPiece(Draw):

    def draw_piece(self, points_pct, rect, cpair=None, gc=None):
        assert isinstance(rect, DrawableRectangle)
        assert isinstance(cpair, (ColorsPair, None))

        points_pix = [ rect.calc_point(*t) for t in points_pct ]
        cp = cpair if cpair is not None else ColorsPair(fg=None, bg=None)
        self.draw_polygon_outline(points_pix, fg=cp.fg, bg=cp.bg, gc=gc)

    def draw_none(self, rect, cpiece=None, gc=None):
        pass

    def draw_pawn(self, rect, cpiece=None, gc=None):
        assert isinstance(cpiece, ColorsPiece)

        pawn = [(0.5, 0.5), (0.6, 0.6), (0.3, 0.9), (0.7, 0.9), (0.4, 0.6)]
        self.draw_piece(pawn, rect, cpair=cpiece.own, gc=gc)
