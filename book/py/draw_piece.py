#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from colors import ColorsPair, ColorsShade, ColorsPiece
from draw import DrawableRectangle, Draw


PIECE_WITH_CHIP_TRANSLATION = 0.11


class DrawPiece(Draw):

    def draw_piece(self, points_pct, rect, cpair=None, scale=1.0, center_x=0.5, center_y=0.5, gc=None):
        assert isinstance(rect, DrawableRectangle)
        assert isinstance(cpair, (ColorsPair, None))

        def _scale(x_pct, y_pct):
            return DrawableRectangle.scale( x_pct, y_pct, scale=scale, center_x=center_x, center_y=center_y )

        points_pix = [ rect.calc_point( *_scale( *t ) ) for t in points_pct ]

        if cpair is not None:
            self.draw_outlined_polygon(points_pix, interior=cpair.interior, outline=cpair.outline, gc=gc)
        else:
            self.draw_outlined_polygon(points_pix, gc=gc)

    def draw_polylines(self, points_pct, rect, cpair=None, scale=1.0, center_x=0.5, center_y=0.5, gc=None):
        assert isinstance(rect, DrawableRectangle)
        assert isinstance(cpair, (ColorsPair, None))

        def _scale(x_pct, y_pct):
            return DrawableRectangle.scale( x_pct, y_pct, scale=scale, center_x=center_x, center_y=center_y )

        points_pix = [ rect.calc_point( *_scale( *t ) ) for t in points_pct ]

        if cpair is not None:
            self.draw_outlined_lines(points_pix, outline=cpair.outline, gc=gc)
        else:
            self.draw_outlined_lines(points_pix, gc=gc)

    def draw_none(self, rect, cpiece=None, gc=None):
        pass

    def draw_pawn(self, rect, cpiece=None, gc=None):
        assert isinstance(cpiece, ColorsPiece)

        pawn = [(0.5, 0.5), (0.6, 0.6), (0.3, 0.9), (0.7, 0.9), (0.4, 0.6)]
        self.draw_piece(pawn, rect, cpair=cpiece.own, gc=gc)

    def draw_bishop(self, rect, cpiece=None, gc=None):
        bishop = [(0.5, 0.6), (0.45, 0.65), (0.7, 0.9), (0.3, 0.9), (0.55, 0.65)]
        self.draw_piece(bishop, rect, cpair=cpiece.own, gc=gc)

        # hat = [(0.5, 0.4), (0.6, 0.5), (0.5, 0.6), (0.4, 0.5)]
        hat = [(0.5, 0.4), \
               \
               # mitre cut \
               (0.53, 0.43), (0.47, 0.49), (0.49, 0.51), (0.55, 0.45), \
               \
               (0.6, 0.5), (0.5, 0.6), (0.4, 0.5)]
        self.draw_piece(hat, rect, cpair=cpiece.opposite, gc=gc)

    def draw_knight(self, rect, cpiece=None, gc=None, left_facing=True):
        knight = [(0.5, 0.3), (0.715, 0.39), (0.805, 0.6), (0.74, 0.9), (0.26, 0.9), (0.485, 0.625), \
                  (0.46, 0.55), (0.26, 0.593), (0.22, 0.485)]

        if not left_facing:
            knight = self.flip_horizontally(knight)

        self.draw_piece(knight, rect, cpair=cpiece.own, gc=gc)

    def draw_rook(self, rect, cpiece=None, gc=None):
        rook = [(0.27, 0.3), (0.37, 0.3), (0.37, 0.42), (0.43, 0.42), (0.43, 0.3), (0.57, 0.3), \
                (0.57, 0.42), (0.63, 0.42), (0.63, 0.3), (0.73, 0.3), (0.76, 0.5), (0.67, 0.54), \
                (0.72, 0.9), (0.28, 0.9), (0.33, 0.54), (0.24, 0.5)]
        self.draw_piece(rook, rect, cpair=cpiece.own, gc=gc)

    def draw_queen(self, rect, cpiece=None, gc=None):
        queen = [(0.2, 0.3), (0.35, 0.65), (0.35, 0.25), (0.45, 0.6), (0.5, 0.2), (0.55, 0.6), \
                 (0.65, 0.25), (0.65, 0.65), (0.8, 0.3), (0.7, 0.9), (0.3, 0.9)]
        self.draw_piece(queen, rect, cpair=cpiece.own, gc=gc)

    def draw_king(self, rect, cpiece=None, gc=None):
        king = [(0.2, 0.4), (0.3, 0.7), (0.4, 0.4), (0.5, 0.7), (0.6, 0.4), (0.7, 0.7), (0.8, 0.4), \
                (0.8, 0.9), (0.2, 0.9)]
        self.draw_piece(king, rect, cpair=cpiece.own, gc=gc)

    def draw_pegasus(self, rect, cpiece=None, gc=None):
        self.draw_knight(rect, cpiece=cpiece, gc=gc)

        wing = [(0.45, 0.87), (0.6, 0.45), (0.93, 0.35), (0.93, 0.44), (0.75, 0.49), (0.84, 0.47), \
                (0.84, 0.56), (0.66, 0.60), (0.75, 0.58), (0.75, 0.67), (0.57, 0.72), (0.5, 0.87)]
        self.draw_piece(wing, rect, cpair=cpiece.opposite, gc=gc)

    def draw_pyramid(self, rect, cpiece=None, gc=None):
        pyramid = [(0.1, 0.9), (0.15, 0.79), (0.2, 0.79), (0.25, 0.7), (0.3, 0.7), (0.35, 0.6), \
                   (0.42, 0.6), (0.42, 0.5), (0.58, 0.5), (0.58, 0.6), (0.65, 0.6), (0.7, 0.7), \
                   (0.75, 0.7), (0.8, 0.79), (0.85, 0.79), (0.9, 0.9)]
        self.draw_piece(pyramid, rect, cpair=cpiece.own, gc=gc)

    def draw_unicorn(self, rect, cpiece=None, gc=None):
        self.draw_knight(rect, cpiece=cpiece, gc=gc, left_facing=False)

        # Variant faced left, as used by other knights and derivates.
        # horn = [(0.5, 0.3), (0.49, 0.36), (0.409, 0.361), (0.325, 0.1373)]
        horn = [(0.5, 0.3), (0.51, 0.36), (0.591, 0.361), (0.675, 0.1373)]
        self.draw_piece(horn, rect, cpair=cpiece.opposite, gc=gc)

    def draw_wave(self, rect, cpiece=None, gc=None):
        wave = [(0.2, 0.35), (0.25, 0.4), (0.25, 0.68), (0.3, 0.68), (0.3, 0.4), (0.4, 0.3), \
                (0.45, 0.3), (0.55, 0.4), (0.55, 0.68), (0.6, 0.68), (0.6, 0.4), (0.7, 0.3), \
                (0.75, 0.3), (0.85, 0.4), (0.85, 0.7), \
                \
                (0.8, 0.75), (0.75, 0.7), (0.75, 0.42), (0.7, 0.42), (0.7, 0.7), (0.6, 0.8), \
                (0.55, 0.8), (0.45, 0.7), (0.45, 0.42), (0.4, 0.42), (0.4, 0.7), (0.3, 0.8), \
                (0.25, 0.8), (0.15, 0.7), (0.15, 0.4)]
        self.draw_piece(wave, rect, cpair=cpiece.own, gc=gc)

    def draw_star(self, rect, cpiece=None, scale=1.0, gc=None):
        hands = [(0.5, 0.4), (0.7, 0.3), (0.6, 0.5), (0.7, 0.7), (0.5, 0.6), (0.3, 0.7), (0.4, 0.5), \
                 (0.3, 0.3)]
        self.draw_piece(hands, rect, scale=scale, center_x=0.5, center_y=0.5, cpair=cpiece.opposite, gc=gc)

        star = [(0.5, 0.2), (0.57, 0.43), (0.8, 0.5), (0.57, 0.57), (0.5, 0.8), (0.43, 0.57), \
                (0.2, 0.5), (0.43, 0.43)]
        self.draw_piece(star, rect, scale=scale, center_x=0.5, center_y=0.5, cpair=cpiece.own, gc=gc)

    def draw_centaur(self, rect, cpiece=None, gc=None):
        horseshoe = [(0.5, 0.3), (0.7, 0.4), (0.8, 0.6), (0.7, 0.9), (0.58, 0.83), (0.65, 0.6), (0.6, 0.5), \
                     (0.5, 0.45), (0.4, 0.5), (0.35, 0.6), (0.42, 0.83), (0.3, 0.9), (0.2, 0.6), (0.3, 0.4)]
        self.draw_piece(horseshoe, rect, cpair=cpiece.own, gc=gc)

        hat = [(0.7, 0.4), (0.65, 0.25), (0.8, 0.2), (0.85, 0.35)]
        self.draw_piece(hat, rect, cpair=cpiece.opposite, gc=gc)

    def draw_serpent(self, rect, cpiece=None, gc=None):
        serpent = [# right skin
                   (0.45, 0.2), (0.5, 0.15), (0.7, 0.15), (0.8, 0.25), (0.8, 0.35), (0.7, 0.45), \
                   (0.5, 0.45), (0.5, 0.55), (0.7, 0.55), (0.8, 0.65), (0.8, 0.75), (0.7, 0.85), \
                   # head
                   (0.4, 0.85), (0.375, 0.875), (0.175, 0.875), (0.1, 0.85), (0.1, 0.82), (0.23, 0.82), \
                   (0.23, 0.79), (0.1, 0.79), (0.1, 0.75), (0.25, 0.715), (0.35, 0.715), (0.4, 0.75), \
                   # left skin
                   (0.7, 0.75), (0.7, 0.65), (0.5, 0.65), (0.4, 0.55), (0.4, 0.45), (0.5, 0.35), \
                   (0.7, 0.35), (0.7, 0.25), (0.5, 0.25)]
        self.draw_piece(serpent, rect, cpair=cpiece.own, gc=gc)

    def draw_shaman(self, rect, cpiece=None, gc=None):
        drum = [(0.3, 0.3), (0.7, 0.3), (0.7, 0.5), (0.6, 0.9), (0.4, 0.9), (0.3, 0.5)]
        self.draw_piece(drum, rect, cpair=cpiece.own, gc=gc)

        string = [(0.3, 0.3), (0.4, 0.5), (0.5, 0.3), (0.6, 0.5), (0.7, 0.3)]
        self.draw_polylines(string, rect, cpair=cpiece.own, gc=gc)

        string2 = [(0.3, 0.5), (0.4, 0.3), (0.5, 0.5), (0.6, 0.3), (0.7, 0.5)]
        self.draw_polylines(string2, rect, cpair=cpiece.own, gc=gc)

    def draw_monolith(self, rect, cpair=None, gc=None):
        monolith = [(0.32, 0.095), (0.68, 0.095), (0.68, 0.905), (0.32, 0.905)]
        self.draw_piece(monolith, rect, cpair=cpair, gc=gc)

    def draw_starchild(self, rect, cpiece=None, caura=None, gc=None):
        x1, y1 = rect.calc_point(0.149999, 0.149999)
        w1, h1 = rect.calc_size(0.700001, 0.700001)
        self.draw_outlined_arc(x1, y1, w1, h1, interior=cpiece.own.interior, outline=cpiece.own.outline, gc=gc)

        x2, y2 = rect.calc_point(0.200001, 0.200001)
        w2, h2 = rect.calc_size(0.600001, 0.600001)
        self.draw_outlined_arc(x2, y2, w2, h2, interior=caura.interior, outline=cpiece.own.outline, gc=gc)

        self.draw_star(rect, cpiece=cpiece, scale=0.667, gc=gc)


def test_piece(func_name, size=300):
    line_width = 1 + (6 * (2*size) / 5) // 1000 # >= 1 + (6 * rendering size / 5) // 1000
    d = DrawPiece(2*size, 2*size, line_width)
    d.clear_area(color='#EFEFEF')

    d.draw_rectangle(size, 0, size, size, fg='#606060')
    d.draw_rectangle(0, size, size, size, fg='#606060')

    func = getattr(d, func_name)

    cdark = ColorsPiece.from_tuple( ('#202020', '#FFFFFF', '#B0B0B0', '#000000') ) # dark
    clight = ColorsPiece.from_tuple( ('#B0B0B0', '#000000', '#202020', '#FFFFFF') ) # light
    cmonolith = ColorsPair.from_tuple( ('#000000', '#FFFFFF') )
    caura = ColorsPair.from_tuple( ('#FFBFFF', '#FFBFFF') )

    def _call(rect, cpiece):
        # In all those clauses func is not ..., it is just func == ... (!?)
        if func == d.draw_monolith:
            func(rect, cpair=cmonolith)
        elif func == d.draw_starchild:
            func(rect, cpiece=cpiece, caura=caura)
        else:
            func(rect, cpiece=cpiece)

    r_tl = DrawableRectangle(0, 0, size, size)
    r_tr = DrawableRectangle(size, 0, size, size)
    r_bl = DrawableRectangle(0, size, size, size)
    r_br = DrawableRectangle(size, size, size, size)

    # dark
    _call(r_tl, cpiece=cdark)
    _call(r_tr, cpiece=cdark)

    # light
    _call(r_bl, cpiece=clight)
    _call(r_br, cpiece=clight)

    file_path = 'temp/%s.IGNORE.png' % func_name
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
