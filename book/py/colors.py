#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from board import BoardType
from maps import Map


class ColorsPair(Map)
    def __init__(self, fg=None, bg=None):
        assert isinstance(fg, (str, None))
        assert isinstance(bg, (str, None))

        self.fg = fg
        self.bg = bg

    def as_tuple(self):
        return (self.fg, self.bg)

    @staticmethod
    def from_tuple(tpl):
        return ColorsPair(fg=tpl[0], bg=tpl[1])


class ColorsShade(Map):
    def __init__(self, light=None, dark=None):
        assert isinstance(light, ColorsPair)
        assert isinstance(dark, ColorsPair)

        self.light = light
        self.dark = dark

    def as_tuple(self):
        return self.light.as_tuple() + self.dark.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsShade( light=ColorsPair.from_tuple( (tpl[0], tpl[1]) ), \
                            dark=ColorsPair.from_tuple( (tpl[2], tpl[3]) ) )

    def convert_to_piece(self, is_piece_light):
        tpl = self.light.as_tuple() + self.dark.as_tuple() \
              if is_piece_light else \
              self.dark.as_tuple() + self.light.as_tuple()
        return ColorsPiece.from_tuple( tpl )


class ColorsPiece(Map):
    def __init__(self, own=None, opposite=None):
        assert isinstance(own, ColorsPair)
        assert isinstance(opposite, ColorsPair)

        self.own = own
        self.opposite = opposite

    def as_tuple(self):
        return self.own.as_tuple() + self.opposite.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsPiece( own=ColorsPair.from_tuple( (tpl[0], tpl[1]) ), \
                            opposite=ColorsPair.from_tuple( (tpl[2], tpl[3]) ) )

    def convert_to_shade(self, is_piece_light):
        tpl = self.own.as_tuple() + self.opposite.as_tuple() \
              if is_piece_light else \
              self.opposite.as_tuple() + self.own.as_tuple()
        return ColorsShade.from_tuple( tpl )


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
        return self.legal.as_tuple() + self.ilegal.as_tuple() + self.action.as_tuple() + self.forbidden.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsMark( legal=ColorsPair.from_tuple( (tpl[0], tpl[1]) ), \
                           ilegal=ColorsPair.from_tuple( (tpl[2], tpl[3]) ), \
                           action=ColorsPair.from_tuple( (tpl[4], tpl[5]) ), \
                           forbidden=ColorsPair.from_tuple( (tpl[6], tpl[7]) ) )


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
        return self.piece.as_tuple() + self.field.as_tuple() + self.arrow.as_tuple() + self.text.as_tuple() + self.marker.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsItem( piece=ColorsShade.from_tuple( (tpl[0], tpl[1], tpl[2], tpl[3]) ), \
                           field=ColorsShade.from_tuple( (tpl[4], tpl[5], tpl[6], tpl[7]) ), \
                           arrow=ColorsMark.from_tuple( (tpl[8], tpl[9], tpl[10], tpl[11], tpl[12], tpl[13], tpl[14], tpl[15]) ), \
                           text=ColorsMark.from_tuple( (tpl[16], tpl[17], tpl[18], tpl[19], tpl[20], tpl[21], tpl[22], tpl[23]) ), \
                           marker=ColorsMark.from_tuple( (tpl[24], tpl[25], tpl[26], tpl[27], tpl[28], tpl[29], tpl[30], tpl[31]) ) )


class Colors(Map):
    def __init__(self):
        CP = ColorsPair # (<fg>, <bg>)
        CS = ColorsShade # (<light fg>, <light bg>, <dark fg>, <dark bg>)
        CM = ColorsMark # (<legal fg>, <legal bg>, <ilegal fg>, <ilegal bg>, <action fg>, <action bg>, <forbidden fg>, <forbidden bg>)
        CI = ColorsItem # ...

        self[ BoardType.none ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                     field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                     arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                     text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                     marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )

        self[ BoardType.Classical ] = CI( piece=CS.from_tuple(  ('#000000', '#B0B0B0', '#FFFFFF', '#202020') ), \
                                          field=CS.from_tuple(  ('#EFEFEF', '#EFEFEF', '#606060', '#606060') ), \
                                          arrow=CM.from_tuple(  ('#303030', '#00FF00', '#303030', '#FF0000', '#303030', '#FF0000', '#303030', '#FF0000') ), \
                                          text=CM.from_tuple(   ('#0080FF', '#303030', '#101010', '#808080', '#FF0000', '#303030', '#FF0000', '#303030') ), \
                                          marker=CM.from_tuple( ('#0080FF', '#303030', '#101010', '#808080', '#FF0000', '#303030', '#FF0000', '#303030') ) )
        self[ BoardType.OddClassical ] = self[ BoardType.Classical ]

        self[ BoardType.CroatianTies ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                             field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                             arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddCroatianTies ] = self[ BoardType.CroatianTies ]

        self[ BoardType.MayanAscendancy ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                                field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                                arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddMayanAscendancy ] = self[ BoardType.MayanAscendancy ]

        self[ BoardType.AgeOfAquarius ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                              field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                              arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                              text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                              marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddAgeOfAquarius ] = self[ BoardType.AgeOfAquarius ]

        self[ BoardType.MirandasVeil ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                             field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                             arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddMirandasVeil ] = self[ BoardType.MirandasVeil ]

        self[ BoardType.Nineteen ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                         field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                         arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                         text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                         marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddNineteen ] = self[ BoardType.Nineteen ]

        self[ BoardType.HemerasDawn ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                            field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                            arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                            text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                            marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddHemerasDawn ] = self[ BoardType.HemerasDawn ]

        self[ BoardType.TamoanchanRevisited ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                                    field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                                    arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                    text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                    marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddTamoanchanRevisited ] = self[ BoardType.TamoanchanRevisited ]

        self[ BoardType.ConquestOfTlalocan ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                                   field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                                   arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                   text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                   marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddConquestOfTlalocan ] = self[ BoardType.ConquestOfTlalocan ]

        self[ BoardType.Discovery ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                          field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                          arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                          text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                          marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddDiscovery ] = self[ BoardType.Discovery ]

        self[ BoardType.One ] = CI( piece=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                    field=CS.from_tuple(  ('#', '#', '#', '#') ), \
                                    arrow=CM.from_tuple(  ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                    text=CM.from_tuple(   ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                    marker=CM.from_tuple( ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddOne ] = self[ BoardType.One ]

Colors = Colors()
