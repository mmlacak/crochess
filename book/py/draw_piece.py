#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from consts import DEFAULT_LINE_WIDTH

from pixel_math import scale_translate, Rectangle
from piece import PieceType
from colors import ColorsPair, ColorsPiece, ColorsItem
from draw import Draw


class DrawPiece(Draw):

    def draw_piece_polygon(self, points, rect, cpair=None, scale=1.0):
        assert isinstance(rect, Rectangle)
        assert isinstance(cpair, ColorsPair)

        _points = [ rect.calc_point( *t, scale=scale ) for t in points ]
        lw = rect.scale_length(DEFAULT_LINE_WIDTH)
        self.draw_polygon(_points, interior_str=cpair.interior, outline_str=cpair.outline, line_width=lw)

    def draw_piece_lines(self, points, rect, cpair=None, scale=1.0):
        assert isinstance(rect, Rectangle)
        assert isinstance(cpair, ColorsPair)

        _points = [ rect.calc_point( *t, scale=scale ) for t in points ]
        lw = rect.scale_length(DEFAULT_LINE_WIDTH)
        self.draw_lines(_points, color_str=cpair.outline, line_width=lw)

    def draw_none(self, rect, cpiece=None):
        pass

    def draw_pawn(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        pawn = [(0.5, 0.5), (0.6, 0.6), (0.3, 0.9), (0.7, 0.9), (0.4, 0.6)]
        self.draw_piece_polygon(pawn, rect, cpair=cpiece.own)

    def draw_bishop(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        bishop = [(0.5, 0.6), (0.45, 0.65), (0.7, 0.9), (0.3, 0.9), (0.55, 0.65)]
        self.draw_piece_polygon(bishop, rect, cpair=cpiece.own)

        # hat = [(0.5, 0.4), (0.6, 0.5), (0.5, 0.6), (0.4, 0.5)]
        hat = [(0.5, 0.4), \
               \
               # mitre cut \
               (0.53, 0.43), (0.47, 0.49), (0.49, 0.51), (0.55, 0.45), \
               \
               (0.6, 0.5), (0.5, 0.6), (0.4, 0.5)]
        self.draw_piece_polygon(hat, rect, cpair=cpiece.opposite)

    def draw_knight(self, rect, cpiece=None, facing_left=True):
        assert isinstance(cpiece, ColorsPiece)

        knight = [(0.5, 0.3), (0.715, 0.39), (0.805, 0.6), (0.74, 0.9), (0.26, 0.9), (0.485, 0.625), \
                  (0.46, 0.55), (0.26, 0.593), (0.22, 0.485)]

        if not facing_left:
            knight = self.flip_horizontally(knight)

        self.draw_piece_polygon(knight, rect, cpair=cpiece.own)

    def draw_rook(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        rook = [ (0.27, 0.3), (0.37, 0.3), # left merlon \
                 (0.37, 0.42), (0.43, 0.42), # left embrasure \
                 (0.43, 0.3), (0.57, 0.3), # center merlon \
                 (0.57, 0.42), (0.63, 0.42), # right embrasure \
                 (0.63, 0.3), (0.73, 0.3), # right merlon \
                 (0.76, 0.5), (0.67, 0.54), # right corbin \
                 (0.71, 0.9), (0.29, 0.9), # floor \
                 (0.33, 0.54), (0.24, 0.5) # left corbin \
               ]

        self.draw_piece_polygon(rook, rect, cpair=cpiece.own)

    def draw_queen(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        queen = [(0.2, 0.3), (0.35, 0.65), (0.35, 0.25), (0.45, 0.6), (0.5, 0.2), (0.55, 0.6), \
                 (0.65, 0.25), (0.65, 0.65), (0.8, 0.3), (0.7, 0.9), (0.3, 0.9)]
        self.draw_piece_polygon(queen, rect, cpair=cpiece.own)

    def draw_king(self, rect, cpiece=None):
        king = [(0.2, 0.4), (0.3, 0.7), (0.4, 0.4), (0.5, 0.7), (0.6, 0.4), (0.7, 0.7), (0.8, 0.4), \
                (0.8, 0.9), (0.2, 0.9)]
        self.draw_piece_polygon(king, rect, cpair=cpiece.own)

    def draw_pegasus(self, rect, cpiece=None):
        self.draw_knight(rect, cpiece=cpiece)

        assert isinstance(cpiece, ColorsPiece)

        wing = [(0.45, 0.87), (0.6, 0.45), (0.93, 0.35), (0.93, 0.44), (0.75, 0.49), (0.84, 0.47), \
                (0.84, 0.56), (0.66, 0.60), (0.75, 0.58), (0.75, 0.67), (0.57, 0.72), (0.5, 0.87)]
        self.draw_piece_polygon(wing, rect, cpair=cpiece.opposite)

    def draw_pyramid(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        pyramid = [(0.1, 0.9), (0.15, 0.79), (0.2, 0.79), (0.25, 0.7), (0.3, 0.7), (0.35, 0.6), \
                   (0.42, 0.6), (0.42, 0.5), (0.58, 0.5), (0.58, 0.6), (0.65, 0.6), (0.7, 0.7), \
                   (0.75, 0.7), (0.8, 0.79), (0.85, 0.79), (0.9, 0.9)]
        self.draw_piece_polygon(pyramid, rect, cpair=cpiece.own)

    def draw_unicorn(self, rect, cpiece=None):
        self.draw_knight(rect, cpiece=cpiece, facing_left=False)

        assert isinstance(cpiece, ColorsPiece)

        # Variant faced left, as used by other knights and derivates.
        # horn = [(0.5, 0.3), (0.49, 0.36), (0.409, 0.361), (0.325, 0.1373)]
        horn = [(0.5, 0.3), (0.51, 0.36), (0.591, 0.361), (0.675, 0.1373)]
        self.draw_piece_polygon(horn, rect, cpair=cpiece.opposite)

    def draw_wave(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        wave = [(0.2, 0.35), (0.25, 0.4), (0.25, 0.68), (0.3, 0.68), (0.3, 0.4), (0.4, 0.3), \
                (0.45, 0.3), (0.55, 0.4), (0.55, 0.68), (0.6, 0.68), (0.6, 0.4), (0.7, 0.3), \
                (0.75, 0.3), (0.85, 0.4), (0.85, 0.7), \
                \
                (0.8, 0.75), (0.75, 0.7), (0.75, 0.42), (0.7, 0.42), (0.7, 0.7), (0.6, 0.8), \
                (0.55, 0.8), (0.45, 0.7), (0.45, 0.42), (0.4, 0.42), (0.4, 0.7), (0.3, 0.8), \
                (0.25, 0.8), (0.15, 0.7), (0.15, 0.4)]
        self.draw_piece_polygon(wave, rect, cpair=cpiece.own)

    def draw_star(self, rect, cpiece=None, scale=1.0):
        assert isinstance(cpiece, ColorsPiece)

        hands = [(0.5, 0.4), (0.7, 0.3), (0.6, 0.5), (0.7, 0.7), (0.5, 0.6), (0.3, 0.7), (0.4, 0.5), \
                 (0.3, 0.3)]
        self.draw_piece_polygon(hands, rect, scale=scale, cpair=cpiece.opposite)

        star = [(0.5, 0.2), (0.57, 0.43), (0.8, 0.5), (0.57, 0.57), (0.5, 0.8), (0.43, 0.57), \
                (0.2, 0.5), (0.43, 0.43)]
        self.draw_piece_polygon(star, rect, scale=scale, cpair=cpiece.own)

    def draw_centaur(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        horseshoe = [(0.5, 0.3), (0.7, 0.4), (0.8, 0.6), (0.7, 0.9), (0.58, 0.83), (0.65, 0.6), (0.6, 0.5), \
                     (0.5, 0.45), (0.4, 0.5), (0.35, 0.6), (0.42, 0.83), (0.3, 0.9), (0.2, 0.6), (0.3, 0.4)]
        self.draw_piece_polygon(horseshoe, rect, cpair=cpiece.own)

        hat = [(0.7, 0.4), (0.65, 0.25), (0.8, 0.2), (0.85, 0.35)]
        self.draw_piece_polygon(hat, rect, cpair=cpiece.opposite)

    def draw_scout(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        # binoculars = [(0.7, 0.6), (0.6, 0.7), (0.4, 0.5), (0.3, 0.6), (0.4, 0.7), (0.6, 0.5)]
        # self.draw_piece_polygon(binoculars, rect, cpair=cpiece.opposite)

        # mono_left = [(0.4, 0.5), (0.3, 0.6), (0.4, 0.7), (0.5, 0.6)]
        mono_left = [(0.37, 0.5), (0.27, 0.6), (0.37, 0.7), (0.47, 0.6)]
        self.draw_piece_polygon(mono_left, rect, cpair=cpiece.opposite)

        mono_right = [(0.63, 0.5), (0.53, 0.6), (0.63, 0.7), (0.73, 0.6)]
        self.draw_piece_polygon(mono_right, rect, cpair=cpiece.opposite)

        self.draw_pawn(rect, cpiece=cpiece)

    def draw_grenadier(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        armor = [(0.4, 0.6), (0.6, 0.6), (0.6, 0.8), (0.4, 0.8)]
        self.draw_piece_polygon(armor, rect, cpair=cpiece.opposite)

        self.draw_pawn(rect, cpiece=cpiece)

        # beret = [(0.5, 0.5), (0.6, 0.4), (0.6, 0.5), (0.3, 0.5), (0.5, 0.4)]
        beret = [(0.5, 0.55), (0.6, 0.45), (0.6, 0.55), (0.3, 0.55), (0.5, 0.45)]
        self.draw_piece_polygon(beret, rect, cpair=cpiece.opposite)

    def draw_serpent(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        serpent = [# right skin
                   (0.45, 0.2), (0.5, 0.15), (0.7, 0.15), (0.8, 0.25), (0.8, 0.35), (0.7, 0.45), \
                   (0.5, 0.45), (0.5, 0.55), (0.7, 0.55), (0.8, 0.65), (0.8, 0.75), (0.7, 0.85), \
                   # head
                   (0.4, 0.85), (0.375, 0.875), (0.175, 0.875), (0.1, 0.85), (0.1, 0.82), (0.23, 0.82), \
                   (0.23, 0.79), (0.1, 0.79), (0.1, 0.75), (0.25, 0.715), (0.35, 0.715), (0.4, 0.75), \
                   # left skin
                   (0.7, 0.75), (0.7, 0.65), (0.5, 0.65), (0.4, 0.55), (0.4, 0.45), (0.5, 0.35), \
                   (0.7, 0.35), (0.7, 0.25), (0.5, 0.25)]
        self.draw_piece_polygon(serpent, rect, cpair=cpiece.own)

    def draw_shaman(self, rect, cpiece=None):
        assert isinstance(cpiece, ColorsPiece)

        drum = [(0.3, 0.3), (0.7, 0.3), (0.7, 0.5), (0.6, 0.9), (0.4, 0.9), (0.3, 0.5)]
        self.draw_piece_polygon(drum, rect, cpair=cpiece.own)

        string = [(0.3, 0.3), (0.4, 0.5), (0.5, 0.3), (0.6, 0.5), (0.7, 0.3)]
        self.draw_piece_lines(string, rect, cpair=cpiece.own)

        string2 = [(0.3, 0.5), (0.4, 0.3), (0.5, 0.5), (0.6, 0.3), (0.7, 0.5)]
        self.draw_piece_lines(string2, rect, cpair=cpiece.own)

    def draw_monolith(self, rect, cpair=None):
        monolith = [(0.32, 0.095), (0.68, 0.095), (0.68, 0.905), (0.32, 0.905)]
        self.draw_piece_polygon(monolith, rect, cpair=cpair)

    def draw_starchild(self, rect, cpiece=None, caura=None):
        assert isinstance(cpiece, ColorsPiece)
        assert isinstance(cpiece.own, ColorsPair)
        assert isinstance(caura, ColorsPair)

        x, y = rect.calc_point(0.5, 0.5)
        lw = rect.scale_length(DEFAULT_LINE_WIDTH)

        r1 = rect.scale_length(0.350001)
        self.draw_arc(x, y, r1, interior_str=cpiece.own.interior, outline_str=cpiece.own.outline, line_width=lw)

        r2 = rect.scale_length(0.300001)
        self.draw_arc(x, y, r2, interior_str=caura.interior, outline_str=cpiece.own.outline, line_width=lw)

        self.draw_star(rect, cpiece=cpiece, scale=0.667)

    def draw_piece(self, piece_type, rect, colors_item):
        pt = PieceType(piece_type)
        assert isinstance(rect, Rectangle)
        assert isinstance(colors_item, ColorsItem)

        _draw = { PieceType.none: self.draw_none,
                  PieceType.Pawn: self.draw_pawn,
                  PieceType.Bishop: self.draw_bishop,
                  PieceType.Knight: self.draw_knight,
                  PieceType.Rook: self.draw_rook,
                  PieceType.Queen: self.draw_queen,
                  PieceType.King: self.draw_king,
                  PieceType.Pegasus: self.draw_pegasus,
                  PieceType.Pyramid: self.draw_pyramid,
                  PieceType.Unicorn: self.draw_unicorn,
                  PieceType.Wave: self.draw_wave,
                  PieceType.Star: self.draw_star,
                  PieceType.Centaur: self.draw_centaur,
                  PieceType.Scout: self.draw_scout,
                  PieceType.Grenadier: self.draw_grenadier,
                  PieceType.Serpent: self.draw_serpent,
                  PieceType.Shaman: self.draw_shaman,
                  PieceType.Monolith: self.draw_monolith,
                  PieceType.Starchild: self.draw_starchild }[ pt.get_enumerated() ]

        if _draw == self.draw_star:
            _draw(rect, cpiece=colors_item.star.to_piece( pt.is_light() ))
        elif _draw == self.draw_monolith:
            _draw(rect, cpair=colors_item.monolith)
        elif _draw == self.draw_starchild:
            _draw(rect, cpiece=colors_item.piece.to_piece( pt.is_light() ), caura=colors_item.aura)
        else:
            _draw(rect, cpiece=colors_item.piece.to_piece( pt.is_light() ))


def test_piece(func_name, size=800):

    d = DrawPiece(size, size, size / 2, color_str='#EFEFEF')

    d.draw_rectangle(1.0, 0.0, 1.0, 1.0, interior_str='#606060')
    d.draw_rectangle(0.0, 1.0, 1.0, 1.0, interior_str='#606060')

    func = getattr(d, func_name)

    cdark = ColorsPiece.from_tuple( ('#202020', '#FFFFFF', '#B0B0B0', '#000000' ) ) # dark
    clight = ColorsPiece.from_tuple( ('#B0B0B0', '#000000', '#202020', '#FFFFFF' ) ) # light
    cmonolith = ColorsPair.from_tuple( ('#000000', '#FFFFFF' ) )
    caura = ColorsPair.from_tuple( ('#FFBFFF', '#FFBFFF' ) )

    def _call(rect, cpiece):
        # func is not d.draw_*, it is just func == d.draw_* (!?)
        if func == d.draw_monolith:
            func(rect, cpair=cmonolith)
        elif func == d.draw_starchild:
            func(rect, cpiece=cpiece, caura=caura)
        else:
            func(rect, cpiece=cpiece)

    r_tl = Rectangle(0.0, 0.0, 1.0, 1.0)
    r_tr = Rectangle(1.0, 0.0, 1.0, 1.0)
    r_bl = Rectangle(0.0, 1.0, 1.0, 1.0)
    r_br = Rectangle(1.0, 1.0, 1.0, 1.0)

    # dark
    _call(r_tl, cpiece=cdark)
    _call(r_tr, cpiece=cdark)

    # light
    _call(r_bl, cpiece=clight)
    _call(r_br, cpiece=clight)

    file_path = 'temp/%s.IGNORE.png' % func_name
    d.save_image(file_path)


def test_piece_contour(func_name, size=1200):

    d = DrawPiece(size, size, size / 2, color_str='#EFEFEF')

    d.draw_rectangle(1.0, 0.0, 1.0, 1.0, interior_str='#606060')
    d.draw_rectangle(0.0, 1.0, 1.0, 1.0, interior_str='#606060')

    func = getattr(d, func_name)

    cdark = ColorsPiece.from_tuple( ('#000000', '#CCCCCC', '#CCCCCC', '#000000' ) ) # dark
    clight = ColorsPiece.from_tuple( ('#CCCCCC', '#000000', '#000000', '#CCCCCC' ) ) # light
    cmonolith = ColorsPair.from_tuple( ('#000000', '#CCCCCC' ) )
    caura = ColorsPair.from_tuple( ('#FFFFFF', '#CCCCCC' ) )

    def _call(rect, cpiece):
        # func is not d.draw_*, it is just func == d.draw_* (!?)
        if func == d.draw_monolith:
            func(rect, cpair=cmonolith)
        elif func == d.draw_starchild:
            func(rect, cpiece=cpiece, caura=caura)
        else:
            func(rect, cpiece=cpiece)

    rect = Rectangle(0.2, 0.2, 1.6, 1.6)
    d.draw_rectangle(*rect.as_tuple(), outline_str='#FF0000', line_width=0.001)

    # dark
    _call(rect, cpiece=cdark)

    # light
    # _call(rect, cpiece=clight)

    file_path = 'temp/%s.CONTOUR.IGNORE.png' % func_name
    d.save_image(file_path)


if __name__ == '__main__':

    test_piece('draw_pawn')
    test_piece('draw_bishop')
    test_piece('draw_knight')
    test_piece('draw_rook')
    test_piece('draw_king')
    test_piece('draw_queen')
    test_piece('draw_pegasus')
    test_piece('draw_pyramid')
    test_piece('draw_unicorn')
    test_piece('draw_wave')
    test_piece('draw_star')
    test_piece('draw_centaur')
    test_piece('draw_serpent')
    test_piece('draw_shaman')
    test_piece('draw_monolith')
    test_piece('draw_starchild')
    test_piece('draw_grenadier')
    test_piece('draw_scout')

    test_piece_contour('draw_pawn')
    test_piece_contour('draw_bishop')
    test_piece_contour('draw_knight')
    test_piece_contour('draw_rook')
    test_piece_contour('draw_king')
    test_piece_contour('draw_queen')
    test_piece_contour('draw_pegasus')
    test_piece_contour('draw_pyramid')
    test_piece_contour('draw_unicorn')
    test_piece_contour('draw_wave')
    test_piece_contour('draw_star')
    test_piece_contour('draw_centaur')
    test_piece_contour('draw_serpent')
    test_piece_contour('draw_shaman')
    test_piece_contour('draw_monolith')
    test_piece_contour('draw_starchild')
    test_piece_contour('draw_grenadier')
    test_piece_contour('draw_scout')
