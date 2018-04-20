#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from board import BoardType


class ColorsPair(object):
    def __init__(self, interior, outline):
        assert isinstance(interior, str)
        assert isinstance(outline, str)

        self.interior = interior
        self.outline = outline

    def as_tuple(self):
        return (self.interior, self.outline)

    @staticmethod
    def from_tuple(tpl):
        return ColorsPair( *tpl[ 0 : 2 ] )


class ColorsShade(object):
    def __init__(self, light, dark):
        assert isinstance(light, ColorsPair)
        assert isinstance(dark, ColorsPair)

        self.light = light
        self.dark = dark

    def as_tuple(self):
        return self.light.as_tuple() + self.dark.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsShade( light=ColorsPair( *tpl[ 0 : 2 ] ), \
                            dark=ColorsPair( *tpl[ 2 : 4 ] ) )

    def to_piece(self, is_piece_light):
        tpl = self.light.as_tuple() + self.dark.as_tuple() \
              if is_piece_light else \
              self.dark.as_tuple() + self.light.as_tuple()
        return ColorsPiece.from_tuple( tpl )


class ColorsPiece(object):
    def __init__(self, own, opposite):
        assert isinstance(own, ColorsPair)
        assert isinstance(opposite, ColorsPair)

        self.own = own
        self.opposite = opposite

    def as_tuple(self):
        return self.own.as_tuple() + self.opposite.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsPiece( own=ColorsPair( *tpl[ 0 : 2 ] ), \
                            opposite=ColorsPair( *tpl[ 2 : 4 ] ) )

    def to_shade(self, is_piece_light):
        tpl = self.own.as_tuple() + self.opposite.as_tuple() \
              if is_piece_light else \
              self.opposite.as_tuple() + self.own.as_tuple()
        return ColorsShade.from_tuple( tpl )


class ColorsMark(object):
    def __init__(self, legal, ilegal, action, forbidden):
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
        return ColorsMark( legal=ColorsPair( *tpl[ 0 : 2 ] ), \
                           ilegal=ColorsPair( *tpl[ 2 : 4 ] ), \
                           action=ColorsPair( *tpl[ 4 : 6 ] ), \
                           forbidden=ColorsPair( *tpl[ 6 : 8 ] ) )


class ColorsItem(object):
    def __init__(self, piece, star, monolith, aura, field, arrow, text, marker):
        assert isinstance(piece, ColorsShade)
        assert isinstance(star, ColorsShade)
        assert isinstance(monolith, ColorsPair)
        assert isinstance(aura, ColorsPair)
        assert isinstance(field, ColorsShade)
        assert isinstance(arrow, ColorsMark)
        assert isinstance(text, ColorsMark)
        assert isinstance(marker, ColorsMark)

        self.piece = piece
        self.star = star
        self.monolith = monolith
        self.aura = aura # used by Starchild
        self.field = field
        self.arrow = arrow
        self.text = text
        self.marker = marker

    def as_tuple(self):
        return  self.piece.as_tuple() + \
                self.star.as_tuple() + \
                self.monolith.as_tuple() + \
                self.aura.as_tuple() + \
                self.field.as_tuple() + \
                self.arrow.as_tuple() + \
                self.text.as_tuple() + \
                self.marker.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return  ColorsItem( piece=ColorsShade( *tpl[ 0 : 4 ] ), \
                            star=ColorsShade( *tpl[ 4 : 8 ] ), \
                            monolith=ColorsPair( *tpl[ 8 : 10 ] ), \
                            aura=ColorsPair(  *tpl[ 10 : 12 ] ), \
                            field=ColorsShade( *tpl[ 12 : 16 ] ), \
                            arrow=ColorsMark( *tpl[ 16 : 24 ] ), \
                            text=ColorsMark( *tpl[ 24 : 32 ] ), \
                            marker=ColorsMark( *tpl[ 32 : 40 ] ) )


class Colors(dict):
    def __init__(self):
        CP = ColorsPair.from_tuple # (<interior>, <outline>)
        CS = ColorsShade.from_tuple # (<light interior>, <light outline>, <dark interior>, <dark outline>)
        CM = ColorsMark.from_tuple # (<legal interior>, <legal outline>, <ilegal interior>, <ilegal outline>, <action interior>, <action outline>, <forbidden interior>, <forbidden outline>)
        CI = ColorsItem # ...

        self[ BoardType.none ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                     star=CS(     ('#', '#', '#', '#') ), \
                                     monolith=CP( ('#', '#') ), \
                                     aura=CP(     ('#', '#') ), \
                                     field=CS(    ('#', '#', '#', '#') ), \
                                     arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                     text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                     marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )

        self[ BoardType.Classical ] = CI( piece=CS(    ('#FFFFFF', '#202020', '#000000', '#B0B0B0') ), \
                                          star=CS(     ('#', '#', '#', '#') ), \
                                          monolith=CP( ('#', '#') ), \
                                          aura=CP(     ('#', '#') ), \
                                          field=CS(    ('#EFEFEF', '#EFEFEF', '#606060', '#606060') ), \
                                          arrow=CM(    ('#00FF00', '#303030', '#0000FF', '#303030', '#FF0000', '#303030', '#101010', '#303030') ), \
                                          text=CM(     ('#0080FF', '#303030', '#101010', '#808080', '#FF0000', '#303030', '#FF0000', '#303030') ), \
                                          marker=CM(   ('#0080FF', '#303030', '#101010', '#808080', '#FF0000', '#303030', '#FF0000', '#303030') ) )
        self[ BoardType.OddClassical ] = self[ BoardType.Classical ]

        self[ BoardType.CroatianTies ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                             star=CS(     ('#', '#', '#', '#') ), \
                                             monolith=CP( ('#', '#') ), \
                                             aura=CP(     ('#', '#') ), \
                                             field=CS(    ('#', '#', '#', '#') ), \
                                             arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddCroatianTies ] = self[ BoardType.CroatianTies ]

        self[ BoardType.MayanAscendancy ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                                star=CS(     ('#', '#', '#', '#') ), \
                                                monolith=CP( ('#', '#') ), \
                                                aura=CP(     ('#', '#') ), \
                                                field=CS(    ('#', '#', '#', '#') ), \
                                                arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddMayanAscendancy ] = self[ BoardType.MayanAscendancy ]

        self[ BoardType.AgeOfAquarius ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                              star=CS(     ('#', '#', '#', '#') ), \
                                              monolith=CP( ('#', '#') ), \
                                              aura=CP(     ('#', '#') ), \
                                              field=CS(    ('#', '#', '#', '#') ), \
                                              arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                              text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                              marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddAgeOfAquarius ] = self[ BoardType.AgeOfAquarius ]

        self[ BoardType.MirandasVeil ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                             star=CS(     ('#', '#', '#', '#') ), \
                                             monolith=CP( ('#', '#') ), \
                                             aura=CP(     ('#', '#') ), \
                                             field=CS(    ('#', '#', '#', '#') ), \
                                             arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                             marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddMirandasVeil ] = self[ BoardType.MirandasVeil ]

        self[ BoardType.Nineteen ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                         star=CS(     ('#', '#', '#', '#') ), \
                                         monolith=CP( ('#', '#') ), \
                                         aura=CP(     ('#', '#') ), \
                                         field=CS(    ('#', '#', '#', '#') ), \
                                         arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                         text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                         marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddNineteen ] = self[ BoardType.Nineteen ]

        self[ BoardType.HemerasDawn ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                            star=CS(     ('#', '#', '#', '#') ), \
                                            monolith=CP( ('#', '#') ), \
                                            aura=CP(     ('#', '#') ), \
                                            field=CS(    ('#', '#', '#', '#') ), \
                                            arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                            text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                            marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddHemerasDawn ] = self[ BoardType.HemerasDawn ]

        self[ BoardType.TamoanchanRevisited ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                                    star=CS(     ('#', '#', '#', '#') ), \
                                                    monolith=CP( ('#', '#') ), \
                                                    aura=CP(     ('#', '#') ), \
                                                    field=CS(    ('#', '#', '#', '#') ), \
                                                    arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                    text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                    marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddTamoanchanRevisited ] = self[ BoardType.TamoanchanRevisited ]

        self[ BoardType.ConquestOfTlalocan ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                                   star=CS(     ('#', '#', '#', '#') ), \
                                                   monolith=CP( ('#', '#') ), \
                                                   aura=CP(     ('#', '#') ), \
                                                   field=CS(    ('#', '#', '#', '#') ), \
                                                   arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                   text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                                   marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddConquestOfTlalocan ] = self[ BoardType.ConquestOfTlalocan ]

        self[ BoardType.Discovery ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                          star=CS(     ('#', '#', '#', '#') ), \
                                          monolith=CP( ('#', '#') ), \
                                          aura=CP(     ('#', '#') ), \
                                          field=CS(    ('#', '#', '#', '#') ), \
                                          arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                          text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                          marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddDiscovery ] = self[ BoardType.Discovery ]

        self[ BoardType.One ] = CI( piece=CS(    ('#', '#', '#', '#') ), \
                                    star=CS(     ('#', '#', '#', '#') ), \
                                    monolith=CP( ('#', '#') ), \
                                    aura=CP(     ('#', '#') ), \
                                    field=CS(    ('#', '#', '#', '#') ), \
                                    arrow=CM(    ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                    text=CM(     ('#', '#', '#', '#', '#', '#', '#', '#') ), \
                                    marker=CM(   ('#', '#', '#', '#', '#', '#', '#', '#') ) )
        self[ BoardType.OddOne ] = self[ BoardType.One ]

Colors = Colors()
