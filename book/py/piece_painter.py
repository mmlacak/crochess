#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk

from piece import PieceType
from painter_context import PainterContext
from painter import Painter

import debug_

class PiecePainter(Painter):
    def __init__(self, drawable):
        super(PiecePainter, self).__init__(drawable)

    def draw_none(self, pc, piece):
        pass

    def draw_pawn(self, pc, piece):
        pawn = [(0.5, 0.5), (0.6, 0.6), (0.3, 0.9), (0.7, 0.9), (0.4, 0.6)]
        pawn = [ self.calc_point(pc.rect, *t) for t in pawn ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), pawn)

    def draw_bishop(self, pc, piece):
        bishop = [(0.5, 0.6), (0.45, 0.65), (0.7, 0.9), (0.3, 0.9), (0.55, 0.65)]
        bishop = [ self.calc_point(pc.rect, *t) for t in bishop ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), bishop)

        bishop2 = [(0.5, 0.4), (0.6, 0.5), (0.5, 0.6), (0.4, 0.5)]
        bishop2 = [ self.calc_point(pc.rect, *t) for t in bishop2 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), bishop2)

    def draw_knight(self, pc, piece):
        knight = [(0.5, 0.3), (0.715, 0.39), (0.805, 0.6), (0.74, 0.9), (0.26, 0.9), (0.485, 0.625), \
                  (0.46, 0.55), (0.26, 0.593), (0.22, 0.485)]
        knight = [ self.calc_point(pc.rect, *t) for t in knight ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), knight)

    def draw_rook(self, pc, piece):
        rook = [(0.27, 0.3), (0.37, 0.3), (0.37, 0.42), (0.43, 0.42), (0.43, 0.3), (0.57, 0.3), \
                (0.57, 0.42), (0.63, 0.42), (0.63, 0.3), (0.73, 0.3), (0.76, 0.5), (0.67, 0.54), \
                (0.72, 0.9), (0.28, 0.9), (0.33, 0.54), (0.24, 0.5)]
        rook = [ self.calc_point(pc.rect, *t) for t in rook ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), rook)

    def draw_queen(self, pc, piece):
        queen = [(0.2, 0.3), (0.35, 0.65), (0.35, 0.25), (0.45, 0.6), (0.5, 0.2), (0.55, 0.6), \
                 (0.65, 0.25), (0.65, 0.65), (0.8, 0.3), (0.7, 0.9), (0.3, 0.9)]
        queen = [ self.calc_point(pc.rect, *t) for t in queen ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), queen)

    def draw_king(self, pc, piece):
        king = [(0.2, 0.4), (0.3, 0.7), (0.4, 0.4), (0.5, 0.7), (0.6, 0.4), (0.7, 0.7), (0.8, 0.4), \
                (0.8, 0.9), (0.2, 0.9)]
        king = [ self.calc_point(pc.rect, *t) for t in king ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), king)

    def draw_pegasus(self, pc, piece):
        pegasus = [(0.5, 0.3), (0.715, 0.39), (0.805, 0.6), (0.74, 0.9), (0.26, 0.9), (0.485, 0.625), \
                   (0.46, 0.55), (0.26, 0.593), (0.22, 0.485)]
        pegasus = [ self.calc_point(pc.rect, *t) for t in pegasus ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), pegasus)

        # pegasus2[0] = (0.45, 0.89), pegasus2[11] = (0.5, 0.89)
        pegasus2 = [(0.45, 0.87), (0.6, 0.45), (0.93, 0.35), (0.93, 0.44), (0.75, 0.49), (0.84, 0.47), \
                    (0.84, 0.56), (0.66, 0.60), (0.75, 0.58), (0.75, 0.67), (0.57, 0.72), (0.5, 0.87)]
        pegasus2 = [ self.calc_point(pc.rect, *t) for t in pegasus2 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), pegasus2)

    def draw_pyramid(self, pc, piece):
        pyramid = [(0.1, 0.9), (0.15, 0.79), (0.2, 0.79), (0.25, 0.7), (0.3, 0.7), (0.35, 0.6), \
                   (0.42, 0.6), (0.42, 0.5), (0.58, 0.5), (0.58, 0.6), (0.65, 0.6), (0.7, 0.7), \
                   (0.75, 0.7), (0.8, 0.79), (0.85, 0.79), (0.9, 0.9)]
        pyramid = [ self.calc_point(pc.rect, *t) for t in pyramid ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), pyramid)

    def draw_unicorn(self, pc, piece):
        # Variant faced left, as used by other knights and derivates.
        # unicorn = [(0.5, 0.3), (0.715, 0.39), (0.805, 0.6), (0.74, 0.9), (0.26, 0.9), (0.485, 0.625), \
        #            (0.46, 0.55), (0.26, 0.593), (0.22, 0.485)]
        unicorn = [(0.5, 0.3), (0.285, 0.39), (0.195, 0.6), (0.26, 0.9), (0.74, 0.9), (0.515, 0.625), \
                   (0.54, 0.55), (0.74, 0.593), (0.78, 0.485)]
        unicorn = [ self.calc_point(pc.rect, *t) for t in unicorn ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), unicorn)

        # Variant faced left, as used by other knights and derivates.
        # unicorn2 = [(0.5, 0.3), (0.49, 0.36), (0.409, 0.361), (0.325, 0.1373)]
        unicorn2 = [(0.5, 0.3), (0.51, 0.36), (0.591, 0.361), (0.675, 0.1373)]
        unicorn2 = [ self.calc_point(pc.rect, *t) for t in unicorn2 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), unicorn2)

    def draw_wave(self, pc, piece):
        wave = [(0.2, 0.35), (0.25, 0.4), (0.25, 0.68), (0.3, 0.68), (0.3, 0.4), (0.4, 0.3), \
                (0.45, 0.3), (0.55, 0.4), (0.55, 0.68), (0.6, 0.68), (0.6, 0.4), (0.7, 0.3), \
                (0.75, 0.3), (0.85, 0.4), (0.85, 0.7), \
                \
                (0.8, 0.75), (0.75, 0.7), (0.75, 0.42), (0.7, 0.42), (0.7, 0.7), (0.6, 0.8), \
                (0.55, 0.8), (0.45, 0.7), (0.45, 0.42), (0.4, 0.42), (0.4, 0.7), (0.3, 0.8), \
                (0.25, 0.8), (0.15, 0.7), (0.15, 0.4)]
        wave = [ self.calc_point(pc.rect, *t) for t in wave ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), wave)

    def draw_star(self, pc, piece):
        star = [(0.5, 0.4), (0.7, 0.3), (0.6, 0.5), (0.7, 0.7), (0.5, 0.6), (0.3, 0.7), (0.4, 0.5), \
                (0.3, 0.3)]
        star = [ self.calc_point(pc.rect, *t) for t in star ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), star)

        star2 = [(0.5, 0.2), (0.57, 0.43), (0.8, 0.5), (0.57, 0.57), (0.5, 0.8), (0.43, 0.57), \
                 (0.2, 0.5), (0.43, 0.43)]
        star2 = [ self.calc_point(pc.rect, *t) for t in star2 ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), star2)

    def draw_centaur(self, pc, piece):
        centaur = [(0.5, 0.3), (0.7, 0.4), (0.8, 0.6), (0.7, 0.9), (0.6, 0.8), (0.65, 0.6), \
                   (0.6, 0.5), (0.5, 0.45), (0.4, 0.5), (0.35, 0.6), (0.4, 0.8), (0.3, 0.9), \
                   (0.2, 0.6), (0.3, 0.4)]
        centaur = [ self.calc_point(pc.rect, *t) for t in centaur ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), centaur)

        centaur2 = [(0.7, 0.4), (0.65, 0.25), (0.8, 0.2), (0.85, 0.35)]
        centaur2 = [ self.calc_point(pc.rect, *t) for t in centaur2 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), centaur2)

    def draw_serpent(self, pc, piece):
        serpent = [# right skin
                   (0.45, 0.2), (0.5, 0.15), (0.7, 0.15), (0.8, 0.25), (0.8, 0.35), (0.7, 0.45), \
                   (0.5, 0.45), (0.5, 0.55), (0.7, 0.55), (0.8, 0.65), (0.8, 0.75), (0.7, 0.85), \
                   # head
                   (0.4, 0.85), (0.375, 0.875), (0.175, 0.875), (0.1, 0.85), (0.1, 0.81), \
                   (0.2, 0.81), (0.1, 0.8), (0.1, 0.75), (0.25, 0.69), (0.35, 0.69), (0.4, 0.75), \
                   # left skin
                   (0.7, 0.75), (0.7, 0.65), (0.5, 0.65), (0.4, 0.55), (0.4, 0.45), (0.5, 0.35), \
                   (0.7, 0.35), (0.7, 0.25), (0.5, 0.25)]
        serpent = [ self.calc_point(pc.rect, *t) for t in serpent ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), serpent)

    def draw_shaman(self, pc, piece):
        shaman = [(0.3, 0.3), (0.7, 0.3), (0.7, 0.5), (0.6, 0.9), (0.4, 0.9), (0.3, 0.5)]
        shaman = [ self.calc_point(pc.rect, *t) for t in shaman ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), shaman)

        shaman2 = [(0.3, 0.3), (0.4, 0.5), (0.5, 0.3), (0.6, 0.5), (0.7, 0.3)]
        shaman2 = [ self.calc_point(pc.rect, *t) for t in shaman2 ]
        self.drawable.draw_lines(pc.get_gc_piece(piece), shaman2)

        shaman3 = [(0.3, 0.5), (0.4, 0.3), (0.5, 0.5), (0.6, 0.3), (0.7, 0.5)]
        shaman3 = [ self.calc_point(pc.rect, *t) for t in shaman3 ]
        self.drawable.draw_lines(pc.get_gc_piece(piece), shaman3)

    def draw_monolith(self, pc, piece):
        monolith = [(0.32, 0.095), (0.68, 0.095), (0.68, 0.905), (0.32, 0.905)]
        monolith = [ self.calc_point(pc.rect, *t) for t in monolith ]
        self.draw_polygon_with_background(pc.get_gc_monolith(), monolith)

    def draw_starchild(self, pc, piece, field_bg="#FFBFFF", field_fg=None):
        x1, y1 = self.calc_point(pc.rect, 0.149999, 0.149999)
        w1, h1 = self.calc_size(pc.rect, 0.700001, 0.700001)
        self.draw_arc_with_background(pc.get_gc_piece(piece), x1, y1, w1, h1)

        x2, y2 = self.calc_point(pc.rect, 0.200001, 0.200001)
        w2, h2 = self.calc_size(pc.rect, 0.600001, 0.600001)
        if field_fg is None:
            field_fg = pc.get_gc_piece(piece).foreground
        self.draw_arc_with_background(pc.get_gc_colors(field_fg, field_bg), x2, y2, w2, h2)

        starchild = [(0.5, 0.44), (0.61, 0.39), (0.56, 0.5), (0.61, 0.61), (0.5, 0.56), (0.39, 0.61), \
                     (0.44, 0.5), (0.39, 0.39)]
        starchild = [ self.calc_point(pc.rect, *t) for t in starchild ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), starchild)

        starchild2 = [(0.5, 0.3), (0.54, 0.46), (0.7, 0.5), (0.54, 0.54), (0.5, 0.7), (0.46, 0.54), \
                      (0.3, 0.5), (0.46, 0.46)]
        starchild2 = [ self.calc_point(pc.rect, *t) for t in starchild2 ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), starchild2)

    def draw_piece(self, pc, piece):
        f = { PieceType.none: self.draw_none,
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
              PieceType.Serpent: self.draw_serpent,
              PieceType.Shaman: self.draw_shaman,
              PieceType.Monolith: self.draw_monolith,
              PieceType.Starchild: self.draw_starchild }[piece.get_enumerated()]
        f(pc, piece)

    def draw_chip(self, pc, piece):
        chip = [(0.1, 0.93), (0.9, 0.93), (0.9, 0.97), (0.1, 0.97)]
        chip = [ self.calc_point(pc.rect, *t) for t in chip ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), chip)

    def draw_spark(self, pc, piece):
        spark = [(0.5, 0.42), (0.65, 0.35), (0.58, 0.5), (0.65, 0.65), (0.5, 0.58), (0.35, 0.65), \
                 (0.42, 0.5), (0.35, 0.35)]
        spark = [ self.calc_point(pc.rect, *t) for t in spark ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), spark)

        spark2 = [(0.5, 0.25), (0.55, 0.45), (0.75, 0.5), (0.55, 0.55), (0.5, 0.75), (0.45, 0.55), \
                  (0.25, 0.5), (0.45, 0.45)]
        spark2 = [ self.calc_point(pc.rect, *t) for t in spark2 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), spark2)

    def draw_griffin(self, pc, piece):
        griffin = [(0.5, 0.3), (0.285, 0.39), (0.195, 0.6), (0.26, 0.9), (0.74, 0.9), (0.515, 0.625), \
                   (0.54, 0.55), (0.74, 0.593), (0.78, 0.485)]
        griffin = [ self.calc_point(pc.rect, *t) for t in griffin ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), griffin)

        # griffin[0] = (0.45, 0.89), griffin[11] = (0.5, 0.89)
        griffin2 = [(0.55, 0.87), (0.4, 0.45), (0.07, 0.35), (0.07, 0.44), (0.25, 0.49), (0.16, 0.47), \
                    (0.16, 0.56), (0.34, 0.60), (0.25, 0.58), (0.25, 0.67), (0.43, 0.72), (0.5, 0.87)]
        griffin2 = [ self.calc_point(pc.rect, *t) for t in griffin2 ]
        self.draw_polygon_with_background(pc.get_gc_piece(not piece), griffin2)

        griffin3 = [(0.5, 0.3), (0.51, 0.36), (0.591, 0.361), (0.675, 0.1373)]
        griffin3 = [ self.calc_point(pc.rect, *t) for t in griffin3 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), griffin3)

    def draw_zigzag(self, pc, piece):
        zigzag = [(0.25, 0.35), (0.35, 0.45), (0.45, 0.35), (0.55, 0.45), (0.65, 0.35), (0.75, 0.45), \
                  (0.75, 0.55), (0.65, 0.45), (0.55, 0.55), (0.45, 0.45), (0.35, 0.55), (0.25, 0.45)]
        zigzag = [ self.calc_point(pc.rect, *t) for t in zigzag ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), zigzag)

        zigzag2 = [(0.25, 0.5), (0.35, 0.6), (0.45, 0.5), (0.55, 0.6), (0.65, 0.5), (0.75, 0.6), \
                   (0.75, 0.7), (0.65, 0.6), (0.55, 0.7), (0.45, 0.6), (0.35, 0.7), (0.25, 0.6)]
        zigzag2 = [ self.calc_point(pc.rect, *t) for t in zigzag2 ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), zigzag2)

    def draw_snake(self, pc, piece):
        snake = [# right skin
                 (0.45, 0.15), (0.5, 0.1), (0.7, 0.1), (0.8, 0.2), (0.8, 0.3), (0.7, 0.4), (0.5, 0.4), \
                 (0.5, 0.5), (0.7, 0.5), (0.8, 0.6), (0.8, 0.7), (0.7, 0.8), \
                 # head
                 (0.4, 0.8), (0.2, 0.9), (0.3, 0.75), (0.2, 0.6), (0.4, 0.7), \
                 # left skin
                 (0.7, 0.7), (0.7, 0.6), (0.5, 0.6), (0.4, 0.5), (0.4, 0.4), (0.5, 0.3), (0.7, 0.3), \
                 (0.7, 0.2), (0.5, 0.2)]
        snake = [ self.calc_point(pc.rect, *t) for t in snake ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), snake)

        # tongue
        snake = [(0.3, 0.75), (0.1, 0.8), (0.2, 0.75), (0.1, 0.7)]
        snake = [ self.calc_point(pc.rect, *t) for t in snake ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), snake)

    def draw_frog(self, pc, piece):
        frog = [(0.7, 0.85), (0.6, 0.9), (0.4, 0.9), (0.3, 0.85), (0.175, 0.575), (0.225, 0.575), \
                (0.4, 0.75), (0.375, 0.475), (0.425, 0.475), (0.5, 0.7), (0.575, 0.475), \
                (0.625, 0.475), (0.6, 0.75), (0.775, 0.575), (0.825, 0.575)]
        frog = [ self.calc_point(pc.rect, *t) for t in frog ]
        self.draw_polygon_with_background(pc.get_gc_piece(piece), frog)

        frog2 = [(0.3, 0.5), (0.2, 0.6), (0.1, 0.5), (0.2, 0.4)]
        frog2 = [ self.calc_point(pc.rect, *t) for t in frog2 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), frog2)

        frog3 = [(0.5, 0.4), (0.4, 0.5), (0.3, 0.4), (0.4, 0.3)]
        frog3 = [ self.calc_point(pc.rect, *t) for t in frog3 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), frog3)

        frog4 = [(0.7, 0.4), (0.6, 0.5), (0.5, 0.4), (0.6, 0.3)]
        frog4 = [ self.calc_point(pc.rect, *t) for t in frog4 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), frog4)

        frog5 = [(0.9, 0.5), (0.8, 0.6), (0.7, 0.5), (0.8, 0.4)]
        frog5 = [ self.calc_point(pc.rect, *t) for t in frog5 ]
        self.draw_polygon_with_background(pc.get_gc_piece_opposite_color(piece), frog5)
