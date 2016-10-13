#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import pygtk
pygtk.require('2.0')
import gtk

from piece import PieceType
from board import BoardType

import debug_

class FieldPosition(object):
    def __init__(self, horizontal, vertical):
        self.horizontal = horizontal
        self.vertical = vertical

class DrawablePosition(object):
    def __init__(self, x_pix, y_pix):
        self.x_pix = x_pix
        self.y_pix = y_pix

class DrawableRectangle(object):
    def __init__(self, left_pix, top_pix, width_pix, height_pix):
        self.left_pix = left_pix
        self.top_pix = top_pix
        self.width_pix = width_pix
        self.height_pix = height_pix

class ColorPair(object):
    def __init__(self, foreground, background):
        self.foreground = foreground
        self.background = background

class ColorContext(object):
    def __init__(self, pair_piece_light, pair_piece_dark, pair_field_light, pair_field_dark, pair_monolith):
        self.pair_piece_light = pair_piece_light
        self.pair_piece_dark = pair_piece_dark
        self.pair_field_light = pair_field_light
        self.pair_field_dark = pair_field_dark
        self.pair_monolith = pair_monolith

    def get_pair_field(self, is_light_or_dark):
        if is_light_or_dark:
            return self.pair_field_light
        else:
            return self.pair_field_dark

    def get_pair_piece(self, is_light_or_dark):
        if is_light_or_dark:
            return self.pair_piece_light
        else:
            return self.pair_piece_dark

class PainterContext(object):
    def __init__(self, gc, board, rect=None, cc=None):
        self.gc = gc
        self.board = board
        self.rect = rect
        self.cc = cc or self.get_color_context(board.type)

    def set_rect(self, left, top, width, height):
        self.rect = DrawableRectangle(left, top, width, height)

    def set_colors(self, fg_piece_light, bg_piece_light, fg_piece_dark, bg_piece_dark, \
                   fg_field_light, bg_field_light, fg_field_dark, bg_field_dark):
        self.cc = ColorContext(ColorPair(fg_piece_light, bg_piece_light), \
                               ColorPair(fg_piece_dark, bg_piece_dark), \
                               ColorPair(fg_field_light, bg_field_light), \
                               ColorPair(fg_field_dark, bg_field_dark))

        #bg_piece_light = self.get_piece_background_color_light(board_type)
        #bg_piece_dark = self.get_piece_background_color_dark(board_type)
        #bg_monolith = self.get_monolith_background_color(board_type)
        #fg_piece_light = self.get_piece_foreground_color_light(board_type)
        #fg_piece_dark = self.get_piece_foreground_color_dark(board_type)
        #fg_monolith = self.get_monolith_foreground_color(board_type)

    def get_gc_piece(self, piece):
        cm = self.gc.get_colormap()
        if self.board.hints.is_universal:
            bt = self.get_intro_board_type(self.board.type.is_even_or_odd(), piece)
            if piece.is_light_or_dark():
                self.gc.foreground = cm.alloc_color(self.get_piece_foreground_color_light(bt))
                self.gc.background = cm.alloc_color(self.get_piece_background_color_light(bt))
            else:
                self.gc.foreground = cm.alloc_color(self.get_piece_foreground_color_dark(bt))
                self.gc.background = cm.alloc_color(self.get_piece_background_color_dark(bt))
        else:
            if piece.is_light_or_dark():
                self.gc.foreground = cm.alloc_color(self.cc.pair_piece_light.foreground)
                self.gc.background = cm.alloc_color(self.cc.pair_piece_light.background)
            else:
                self.gc.foreground = cm.alloc_color(self.cc.pair_piece_dark.foreground)
                self.gc.background = cm.alloc_color(self.cc.pair_piece_dark.background)
        return self.gc

    def get_gc_piece_opposite_color(self, piece):
        cm = self.gc.get_colormap()
        if self.board.hints.is_universal:
            bt = self.get_intro_board_type(self.board.type.is_even_or_odd(), piece)
            if not piece.is_light_or_dark():
                self.gc.foreground = cm.alloc_color(self.get_piece_foreground_color_light(bt))
                self.gc.background = cm.alloc_color(self.get_piece_background_color_light(bt))
            else:
                self.gc.foreground = cm.alloc_color(self.get_piece_foreground_color_dark(bt))
                self.gc.background = cm.alloc_color(self.get_piece_background_color_dark(bt))
        else:
            if not piece.is_light_or_dark():
                self.gc.foreground = cm.alloc_color(self.cc.pair_piece_light.foreground)
                self.gc.background = cm.alloc_color(self.cc.pair_piece_light.background)
            else:
                self.gc.foreground = cm.alloc_color(self.cc.pair_piece_dark.foreground)
                self.gc.background = cm.alloc_color(self.cc.pair_piece_dark.background)
        return self.gc

    def get_gc_monolith(self):
        cm = self.gc.get_colormap()
        if self.board.hints.is_universal:
            bt = self.get_intro_board_type(self.board.type.is_even_or_odd(), PieceType(PieceType.Monolith))
            self.gc.foreground = cm.alloc_color(self.get_monolith_foreground_color(bt))
            self.gc.background = cm.alloc_color(self.get_monolith_background_color(bt))
        else:
            self.gc.foreground = cm.alloc_color(self.cc.pair_monolith.foreground)
            self.gc.background = cm.alloc_color(self.cc.pair_monolith.background)
        return self.gc

    def get_gc_field(self, i, j, is_light_or_dark):
        cm = self.gc.get_colormap()
        if self.board.hints.is_universal:
            bt = self.get_original_board_type(i, j)
            if is_light_or_dark:
                self.gc.foreground = cm.alloc_color(self.get_field_color_light(bt))
                self.gc.background = cm.alloc_color(self.get_field_color_light(bt))
            else:
                self.gc.foreground = cm.alloc_color(self.get_field_color_dark(bt))
                self.gc.background = cm.alloc_color(self.get_field_color_dark(bt))
        else:
            if is_light_or_dark:
                self.gc.foreground = cm.alloc_color(self.cc.pair_field_light.foreground)
                self.gc.background = cm.alloc_color(self.cc.pair_field_light.background)
            else:
                self.gc.foreground = cm.alloc_color(self.cc.pair_field_dark.foreground)
                self.gc.background = cm.alloc_color(self.cc.pair_field_dark.background)
        return self.gc

    def get_gc_reversed(self):
        tmp = self.gc.foreground
        self.gc.foreground = self.gc.background
        self.gc.background = tmp
        return self.gc

    def get_gc_colors(self, foreground, background):
        cm = self.gc.get_colormap()
        self.gc.foreground = cm.alloc_color(foreground)
        self.gc.background = cm.alloc_color(background)
        return self.gc

    def get_intro_even_board_type(self, piece_type):
        return { PieceType.none: BoardType.none,
                 PieceType.Pawn: BoardType.Classical,
                 PieceType.Bishop: BoardType.Classical,
                 PieceType.Knight: BoardType.Classical,
                 PieceType.Rook: BoardType.Classical,
                 PieceType.Queen: BoardType.Classical,
                 PieceType.King: BoardType.Classical,
                 PieceType.Pegasus: BoardType.CroatianTies,
                 PieceType.Pyramid: BoardType.MayanAscendancy,
                 PieceType.Unicorn: BoardType.AgeOfAquarius,
                 PieceType.Wave: BoardType.MirandasVeil,
                 PieceType.Star: BoardType.Nineteen,
                 PieceType.Centaur: BoardType.HemerasDawn,
                 PieceType.Serpent: BoardType.TamoanchanRevisited,
                 PieceType.Shaman: BoardType.ConquestOfTlalocan,
                 PieceType.Monolith: BoardType.Discovery,
                 PieceType.Starchild: BoardType.One }[piece_type.get_enumerated()]

    def get_intro_odd_board_type(self, piece_type):
        return { PieceType.none: BoardType.none,
                 PieceType.Pawn: BoardType.OddClassical,
                 PieceType.Bishop: BoardType.OddClassical,
                 PieceType.Knight: BoardType.OddClassical,
                 PieceType.Rook: BoardType.OddClassical,
                 PieceType.Queen: BoardType.OddClassical,
                 PieceType.King: BoardType.OddClassical,
                 PieceType.Pegasus: BoardType.OddCroatianTies,
                 PieceType.Pyramid: BoardType.OddMayanAscendancy,
                 PieceType.Unicorn: BoardType.OddAgeOfAquarius,
                 PieceType.Wave: BoardType.OddMirandasVeil,
                 PieceType.Star: BoardType.OddNineteen,
                 PieceType.Centaur: BoardType.OddHemerasDawn,
                 PieceType.Serpent: BoardType.OddTamoanchanRevisited,
                 PieceType.Shaman: BoardType.OddConquestOfTlalocan,
                 PieceType.Monolith: BoardType.OddDiscovery,
                 PieceType.Starchild: BoardType.OddOne }[piece_type.get_enumerated()]

    def get_intro_board_type(self, is_even_or_odd, piece_type):
        if is_even_or_odd:
            bt = self.get_intro_even_board_type(piece_type)
        else:
            bt = self.get_intro_odd_board_type(piece_type)
        return bt

    def get_original_board_type(self, i, j):
        if self.board.hints.is_universal:
            if self.board.is_by_the_book():
                off_bt = BoardType.Classical if self.board.type.is_even_or_odd() else BoardType.OddClassical
                off_w = (self.board.get_width() - BoardType(off_bt).get_size()) / 2
                off_h = (self.board.get_height() - BoardType(off_bt).get_size()) / 2

                min_i = min(i, self.board.get_width() - 1 - i)
                min_j = min(j, self.board.get_height() - 1 - j)

                pos_w = off_w - min_i
                pos_h = off_h - min_j

                pos_w = pos_w if pos_w > 0 else 0
                pos_h = pos_h if pos_h > 0 else 0

                bt_w = BoardType(off_bt + 2 * pos_w)
                bt_h = BoardType(off_bt + 2 * pos_h)
                bt = max(bt_w, bt_h)

                # Hack, since Conquest Of Tlalocan board has the same size as Discovery one, but not enum value.
                if self.board.type >= BoardType.OddDiscovery:
                    if bt >= BoardType.OddConquestOfTlalocan:
                        bt += 2
                return BoardType(bt)

        return self.board.type

    def get_field_color_light(self, board_type):
        c = { BoardType.none: "#FFFFFF",
              BoardType.OddClassical: "#EFEFEF",
              BoardType.Classical: "#EFEFEF",
              BoardType.OddCroatianTies: "#EFEFEF",
              BoardType.CroatianTies: "#EFEFEF",
              BoardType.OddMayanAscendancy: "#FFFF00",
              BoardType.MayanAscendancy: "#FFFF00",
              BoardType.OddAgeOfAquarius: "#FFFFDF",
              BoardType.AgeOfAquarius: "#FFFFDF",
              BoardType.OddMirandasVeil: "#FFFFFF",
              BoardType.MirandasVeil: "#FFFFFF",
              BoardType.OddNineteen: "#DFDF7F",
              BoardType.Nineteen: "#DFDF7F",
              BoardType.OddHemerasDawn: "#501008",
              BoardType.HemerasDawn: "#501008",
              BoardType.OddTamoanchanRevisited: "#10F0E0",
              BoardType.TamoanchanRevisited: "#10F0E0",
              BoardType.OddConquestOfTlalocan: "#10F0E0",
              BoardType.ConquestOfTlalocan: "#10F0E0",
              BoardType.OddDiscovery: "#FFFFDF",
              BoardType.Discovery: "#FFFFDF",
              BoardType.OddOne: "#FFFFFF",
              BoardType.One: "#FFFFFF" }[board_type]
        return c

    def get_field_color_dark(self, board_type):
        c = { BoardType.none: "#000000",
              BoardType.OddClassical: "#606060",
              BoardType.Classical: "#606060",
              BoardType.OddCroatianTies: "#FF0000",
              BoardType.CroatianTies: "#FF0000",
              BoardType.OddMayanAscendancy: "#007FCF",
              BoardType.MayanAscendancy: "#007FCF",
              BoardType.OddAgeOfAquarius: "#3FBF3F",
              BoardType.AgeOfAquarius: "#3FBF3F",
              BoardType.OddMirandasVeil: "#500070",
              BoardType.MirandasVeil: "#500070",
              BoardType.OddNineteen: "#FFFFFF",
              BoardType.Nineteen: "#FFFFFF",
              BoardType.OddHemerasDawn: "#909090",
              BoardType.HemerasDawn: "#909090",
              BoardType.OddTamoanchanRevisited: "#0030B0",
              BoardType.TamoanchanRevisited: "#0030B0",
              BoardType.OddConquestOfTlalocan: "#FF0000",
              BoardType.ConquestOfTlalocan: "#FF0000",
              BoardType.OddDiscovery: "#B0B0B0",
              BoardType.Discovery: "#B0B0B0",
              BoardType.OddOne: "#500070",
              BoardType.One: "#500070" }[board_type]
        return c

    def get_piece_background_color_light(self, board_type):
        c = { BoardType.none: "#FFFFFF",
              BoardType.OddClassical: "#B0B0B0",
              BoardType.Classical: "#B0B0B0",
              BoardType.OddCroatianTies: "#B0B0B0",
              BoardType.CroatianTies: "#B0B0B0",
              BoardType.OddMayanAscendancy: "#8F8F00",
              BoardType.MayanAscendancy: "#8F8F00",
              BoardType.OddAgeOfAquarius: "#B0B080",
              BoardType.AgeOfAquarius: "#B0B080",
              BoardType.OddMirandasVeil: "#FF80FF", # "#FF60FF", # "#FF90FF",
              BoardType.MirandasVeil: "#FF80FF", # "#FF60FF", # "#FF90FF",
              BoardType.OddNineteen: "#A0A050",
              BoardType.Nineteen: "#A0A050",
              BoardType.OddHemerasDawn: "#FF0000",
              BoardType.HemerasDawn: "#FF0000",
              BoardType.OddTamoanchanRevisited: "#10F030",
              BoardType.TamoanchanRevisited: "#10F030",
              BoardType.OddConquestOfTlalocan: "#10F030",
              BoardType.ConquestOfTlalocan: "#10F030",
              BoardType.OddDiscovery: "#808080",
              BoardType.Discovery: "#808080",
              BoardType.OddOne: "#CC10EE", # "#DD10FF", # "#FF60FF", # "#FF90FF",
              BoardType.One: "#CC10EE", # "#DD10FF", # "#FF60FF", # "#FF90FF",
            }[board_type]
        return c

    def get_piece_background_color_dark(self, board_type):
        c = { BoardType.none: "#000000",
              BoardType.OddClassical: "#202020",
              BoardType.Classical: "#202020",
              BoardType.OddCroatianTies: "#600000",
              BoardType.CroatianTies: "#600000",
              BoardType.OddMayanAscendancy: "#002F5F",
              BoardType.MayanAscendancy: "#002F5F",
              BoardType.OddAgeOfAquarius: "#1F5F1F",
              BoardType.AgeOfAquarius: "#1F5F1F",
              BoardType.OddMirandasVeil: "#300050", # "#200030",
              BoardType.MirandasVeil: "#300050", # "#200030",
              BoardType.OddNineteen: "#424242",
              BoardType.Nineteen: "#424242",
              BoardType.OddHemerasDawn: "#303030",
              BoardType.HemerasDawn: "#303030",
              BoardType.OddTamoanchanRevisited: "#200070",
              BoardType.TamoanchanRevisited: "#200070",
              BoardType.OddConquestOfTlalocan: "#800000",
              BoardType.ConquestOfTlalocan: "#800000",
              BoardType.OddDiscovery: "#003737",
              BoardType.Discovery: "#003737",
              BoardType.OddOne: "#FF30FF", # "#FF00FF", # "#300050",
              BoardType.One: "#FF30FF", # "#FF00FF", # "#300050",
            }[board_type]
        return c

    def get_piece_foreground_color_light(self, board_type):
        c = { BoardType.none: "#000000",
              BoardType.OddClassical: "#000000",
              BoardType.Classical: "#000000",
              BoardType.OddCroatianTies: "#000000",
              BoardType.CroatianTies: "#000000",
              BoardType.OddMayanAscendancy: "#000000",
              BoardType.MayanAscendancy: "#000000",
              BoardType.OddAgeOfAquarius: "#000000",
              BoardType.AgeOfAquarius: "#000000",
              BoardType.OddMirandasVeil: "#000000",
              BoardType.MirandasVeil: "#000000",
              BoardType.OddNineteen: "#000000",
              BoardType.Nineteen: "#000000",
              BoardType.OddHemerasDawn: "#000000",
              BoardType.HemerasDawn: "#000000",
              BoardType.OddTamoanchanRevisited: "#000000",
              BoardType.TamoanchanRevisited: "#000000",
              BoardType.OddConquestOfTlalocan: "#000000",
              BoardType.ConquestOfTlalocan: "#000000",
              BoardType.OddDiscovery: "#000000",
              BoardType.Discovery: "#000000",
              BoardType.OddOne: "#FFFFFF", # "#000000",
              BoardType.One: "#FFFFFF", # "#000000"
            }[board_type]
        return c

    def get_piece_foreground_color_dark(self, board_type):
        c = { BoardType.none: "#FFFFFF",
              BoardType.OddClassical: "#FFFFFF",
              BoardType.Classical: "#FFFFFF",
              BoardType.OddCroatianTies: "#FFFFFF",
              BoardType.CroatianTies: "#FFFFFF",
              BoardType.OddMayanAscendancy: "#FFFFFF",
              BoardType.MayanAscendancy: "#FFFFFF",
              BoardType.OddAgeOfAquarius: "#FFFFFF",
              BoardType.AgeOfAquarius: "#FFFFFF",
              BoardType.OddMirandasVeil: "#FFFFFF",
              BoardType.MirandasVeil: "#FFFFFF",
              BoardType.OddNineteen: "#FFFFFF",
              BoardType.Nineteen: "#FFFFFF",
              BoardType.OddHemerasDawn: "#FFFFFF",
              BoardType.HemerasDawn: "#FFFFFF",
              BoardType.OddTamoanchanRevisited: "#FFFFFF",
              BoardType.TamoanchanRevisited: "#FFFFFF",
              BoardType.OddConquestOfTlalocan: "#FFFFFF",
              BoardType.ConquestOfTlalocan: "#FFFFFF",
              BoardType.OddDiscovery: "#FFFFFF",
              BoardType.Discovery: "#FFFFFF",
              BoardType.OddOne: "#FFFFFF",
              BoardType.One: "#FFFFFF" }[board_type]
        return c

    def get_monolith_background_color(self, board_type):
        c = { BoardType.none: "#000000",
              BoardType.OddClassical: "#000000",
              BoardType.Classical: "#000000",
              BoardType.OddCroatianTies: "#000000",
              BoardType.CroatianTies: "#000000",
              BoardType.OddMayanAscendancy: "#000000",
              BoardType.MayanAscendancy: "#000000",
              BoardType.OddAgeOfAquarius: "#000000",
              BoardType.AgeOfAquarius: "#000000",
              BoardType.OddMirandasVeil: "#000000",
              BoardType.MirandasVeil: "#000000",
              BoardType.OddNineteen: "#000000",
              BoardType.Nineteen: "#000000",
              BoardType.OddHemerasDawn: "#000000",
              BoardType.HemerasDawn: "#000000",
              BoardType.OddTamoanchanRevisited: "#000000",
              BoardType.TamoanchanRevisited: "#000000",
              BoardType.OddConquestOfTlalocan: "#000000",
              BoardType.ConquestOfTlalocan: "#000000",
              BoardType.OddDiscovery: "#000000",
              BoardType.Discovery: "#000000",
              BoardType.OddOne: "#000000",
              BoardType.One: "#000000" }[board_type]
        return c

    def get_monolith_foreground_color(self, board_type):
        c = { BoardType.none: "#FFFFFF",
              BoardType.OddClassical: "#FFFFFF",
              BoardType.Classical: "#FFFFFF",
              BoardType.OddCroatianTies: "#FFFFFF",
              BoardType.CroatianTies: "#FFFFFF",
              BoardType.OddMayanAscendancy: "#FFFFFF",
              BoardType.MayanAscendancy: "#FFFFFF",
              BoardType.OddAgeOfAquarius: "#FFFFFF",
              BoardType.AgeOfAquarius: "#FFFFFF",
              BoardType.OddMirandasVeil: "#FFFFFF",
              BoardType.MirandasVeil: "#FFFFFF",
              BoardType.OddNineteen: "#FFFFFF",
              BoardType.Nineteen: "#FFFFFF",
              BoardType.OddHemerasDawn: "#FFFFFF",
              BoardType.HemerasDawn: "#FFFFFF",
              BoardType.OddTamoanchanRevisited: "#FFFFFF",
              BoardType.TamoanchanRevisited: "#FFFFFF",
              BoardType.OddConquestOfTlalocan: "#FFFFFF",
              BoardType.ConquestOfTlalocan: "#FFFFFF",
              BoardType.OddDiscovery: "#FFFFFF",
              BoardType.Discovery: "#FFFFFF",
              BoardType.OddOne: "#FFFFFF",
              BoardType.One: "#FFFFFF" }[board_type]
        return c

    def get_color_context(self, board_type):
        field_light = self.get_field_color_light(board_type)
        field_dark = self.get_field_color_dark(board_type)

        bg_piece_light = self.get_piece_background_color_light(board_type)
        bg_piece_dark = self.get_piece_background_color_dark(board_type)
        bg_monolith = self.get_monolith_background_color(board_type)
        fg_piece_light = self.get_piece_foreground_color_light(board_type)
        fg_piece_dark = self.get_piece_foreground_color_dark(board_type)
        fg_monolith = self.get_monolith_foreground_color(board_type)

        cc = ColorContext(ColorPair(fg_piece_light, bg_piece_light), \
                          ColorPair(fg_piece_dark, bg_piece_dark), \
                          ColorPair(field_light, field_light), \
                          ColorPair(field_dark, field_dark), \
                          ColorPair(fg_monolith, bg_monolith))
        return cc
