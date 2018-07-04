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

    def __str__(self):
        fmt = "<ColorsPair" + " %s" * 2 + ">"
        return fmt % self.as_tuple()


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
        return ColorsShade( light=ColorsPair.from_tuple( tpl[ 0 : 2 ] ), \
                            dark=ColorsPair.from_tuple( tpl[ 2 : 4 ] ) )

    def to_piece(self, is_piece_light):
        tpl = self.light.as_tuple() + self.dark.as_tuple() \
              if is_piece_light else \
              self.dark.as_tuple() + self.light.as_tuple()
        return ColorsPiece.from_tuple( tpl )

    def __str__(self):
        fmt = "<ColorsShade" + " %s" * 4 + ">"
        return fmt % self.as_tuple()


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
        return ColorsPiece( own=ColorsPair.from_tuple( tpl[ 0 : 2 ] ), \
                            opposite=ColorsPair.from_tuple( tpl[ 2 : 4 ] ) )

    def to_shade(self, is_piece_light):
        tpl = self.own.as_tuple() + self.opposite.as_tuple() \
              if is_piece_light else \
              self.opposite.as_tuple() + self.own.as_tuple()
        return ColorsShade.from_tuple( tpl )

    def __str__(self):
        fmt = "<ColorsPiece" + " %s" * 4 + ">"
        return fmt % self.as_tuple()


class ColorsMark(object):
    def __init__(self, legal, ilegal, action, blocked):

        assert isinstance(legal, ColorsShade)
        assert isinstance(ilegal, ColorsShade)
        assert isinstance(action, ColorsShade)
        assert isinstance(blocked, ColorsShade)

        self.legal = legal
        self.ilegal = ilegal
        self.action = action
        self.blocked = blocked

    def as_tuple(self):
        return self.legal.as_tuple() + \
               self.ilegal.as_tuple() + \
               self.action.as_tuple() + \
               self.blocked.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsMark( legal=ColorsShade.from_tuple( tpl[ 0 : 4 ] ), \
                           ilegal=ColorsShade.from_tuple( tpl[ 4 : 8 ] ), \
                           action=ColorsShade.from_tuple( tpl[ 8 : 12 ] ), \
                           blocked=ColorsShade.from_tuple( tpl[ 12 : 16 ] ) )

    def __str__(self):
        fmt = "<ColorsMark" + " %s" * 16 + ">"
        return fmt % self.as_tuple()


class ColorsMarkSimple(object):
    def __init__(self, legal, ilegal, action, blocked):

        assert isinstance(legal, ColorsPair)
        assert isinstance(ilegal, ColorsPair)
        assert isinstance(action, ColorsPair)
        assert isinstance(blocked, ColorsPair)

        self.legal = legal
        self.ilegal = ilegal
        self.action = action
        self.blocked = blocked

    def as_tuple(self):
        return self.legal.as_tuple() + \
               self.ilegal.as_tuple() + \
               self.action.as_tuple() + \
               self.blocked.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsMarkSimple( legal=ColorsPair.from_tuple( tpl[ 0 : 2 ] ), \
                                 ilegal=ColorsPair.from_tuple( tpl[ 2 : 4 ] ), \
                                 action=ColorsPair.from_tuple( tpl[ 4 : 6 ] ), \
                                 blocked=ColorsPair.from_tuple( tpl[ 6 : 8 ] ) )

    def __str__(self):
        fmt = "<ColorsMarkSimple" + " %s" * 8 + ">"
        return fmt % self.as_tuple()


class ColorsItem(object):
    def __init__(self, piece, star, monolith, aura, field, arrow, text, marker):
        assert isinstance(piece, ColorsShade)
        assert isinstance(star, ColorsShade)
        assert isinstance(monolith, ColorsPair)
        assert isinstance(aura, ColorsPair)
        assert isinstance(field, ColorsShade)
        assert isinstance(arrow, ColorsMarkSimple)
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
        return  ColorsItem( piece=ColorsShade.from_tuple( tpl[ 0 : 4 ] ), \
                            star=ColorsShade.from_tuple( tpl[ 4 : 8 ] ), \
                            monolith=ColorsPair.from_tuple( tpl[ 8 : 10 ] ), \
                            aura=ColorsPair.from_tuple( tpl[ 10 : 12 ] ), \
                            field=ColorsShade.from_tuple( tpl[ 12 : 16 ] ), \
                            arrow=ColorsMarkSimple.from_tuple( tpl[ 16 : 24 ] ), \
                            text=ColorsMark.from_tuple( tpl[ 24 : 40 ] ), \
                            marker=ColorsMark.from_tuple( tpl[ 40 : 56 ] ) )

    def __str__(self):
        fmt = "<ColorsItem" + " %s" * 56 + ">"
        return fmt % self.as_tuple()


class Colors(dict):
    def __init__(self):
        CP = ColorsPair.from_tuple # (<interior>, <outline>)
        CS = ColorsShade.from_tuple # (<light interior>, <light outline>, <dark interior>, <dark outline>)
        CM = ColorsMark.from_tuple # ( <legal light interior>, <legal light outline>, <legal dark interior>, <legal dark outline>, \
                                   #   <ilegal light interior>, <ilegal light outline>, <ilegal dark interior>, <ilegal dark outline>, \
                                   #   <action light interior>, <action light outline>, <action dark interior>, <action dark outline>, \
                                   #   <blocked light interior>, <blocked light outline>, <blocked dark interior>, <blocked dark outline> )
        CMS = ColorsMarkSimple.from_tuple # ( <legal interior>, <legal outline>, <ilegal interior>, <ilegal outline>, <action interior>, <action outline>, <blocked interior>, <blocked outline> )
        CI = ColorsItem # ...


        self[ BoardType.none ] = CI( piece=CS(    ('#FFFFFF', '#000000', '#000000', '#FFFFFF') ), \
                                     star=CS(     ('#FFFFFF', '#000000', '#000000', '#FFFFFF') ), \
                                     monolith=CP( ('#000000', '#FFFFFF') ), \
                                     aura=CP(     ('#FFFFFF', '#FFFFFF') ), \
                                     field=CS(    ('#FFFFFF', '#FFFFFF', '#000000', '#000000') ), \
                                     arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                     text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                     marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )

        self[ BoardType.Classical ] = CI( piece=CS(    ('#B0B0B0', '#000000', '#202020', '#FFFFFF') ), \
                                          star=CS(     ('#B0B0B0', '#000000', '#202020', '#FFFFFF') ), \
                                          monolith=CP( ('#000000', '#FFFFFF') ), \
                                          aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                          field=CS(    ('#EFEFEF', '#EFEFEF', '#606060', '#606060') ), \
                                          arrow=CMS(   ('#00C000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                          text=CM(     ('#00C000', '#000000', '#00C000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                          marker=CM(   ('#00C000', '#000000', '#00C000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddClassical ] = self[ BoardType.Classical ]

        self[ BoardType.CroatianTies ] = CI( piece=CS(    ('#B0B0B0', '#000000', '#600000', '#FFFFFF') ), \
                                             star=CS(     ('#B0B0B0', '#000000', '#600000', '#FFFFFF') ), \
                                             monolith=CP( ('#000000', '#FFFFFF') ), \
                                             aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                             field=CS(    ('#EFEFEF', '#EFEFEF', '#FF0000', '#FF0000') ), \
                                             arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                             text=CM(     ('#00B000', '#000000', '#00FF00', '#000000', '#B00000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                             marker=CM(   ('#00B000', '#000000', '#00FF00', '#000000', '#B00000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddCroatianTies ] = self[ BoardType.CroatianTies ]

        self[ BoardType.MayanAscendancy ] = CI( piece=CS(    ('#8F8F00', '#000000', '#002F5F', '#FFFFFF') ), \
                                                star=CS(     ('#8F8F00', '#000000', '#002F5F', '#FFFFFF') ), \
                                                monolith=CP( ('#000000', '#FFFFFF') ), \
                                                aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                                field=CS(    ('#FFFF00', '#FFFF00', '#007FCF', '#007FCF') ), \
                                                arrow=CMS(   ('#006000', '#000000', '#FF0000', '#000000', '#0000C0', '#000000', '#303030', '#000000') ), \
                                                text=CM(     ('#006000', '#000000', '#006000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000C0', '#000000', '#0000C0', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                                marker=CM(   ('#006000', '#000000', '#006000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000C0', '#000000', '#0000C0', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddMayanAscendancy ] = self[ BoardType.MayanAscendancy ]

        self[ BoardType.AgeOfAquarius ] = CI( piece=CS(    ('#B0B080', '#000000', '#1F5F1F', '#FFFFFF') ), \
                                              star=CS(     ('#B0B080', '#000000', '#1F5F1F', '#FFFFFF') ), \
                                              monolith=CP( ('#000000', '#FFFFFF') ), \
                                              aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                              field=CS(    ('#FFFFDF', '#FFFFDF', '#3FBF3F', '#3FBF3F') ), \
                                              arrow=CMS(   ('#006000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                              text=CM(     ('#006000', '#000000', '#006000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                              marker=CM(   ('#006000', '#000000', '#006000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddAgeOfAquarius ] = self[ BoardType.AgeOfAquarius ]

        self[ BoardType.MirandasVeil ] = CI( piece=CS(    ('#FF80FF', '#000000', '#300050', '#FFFFFF') ), \
                                             star=CS(     ('#FF80FF', '#000000', '#300050', '#FFFFFF') ), \
                                             monolith=CP( ('#000000', '#FFFFFF') ), \
                                             aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                             field=CS(    ('#FFFFFF', '#FFFFFF', '#500070', '#500070') ), \
                                             arrow=CMS(   ('#009000', '#000000', '#FF0000', '#000000', '#3030FF', '#000000', '#909090', '#000000') ), \
                                             text=CM(     ('#009000', '#000000', '#009000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#3030FF', '#000000', '#3030FF', '#000000', '#909090', '#000000', '#909090', '#000000') ), \
                                             marker=CM(   ('#009000', '#000000', '#009000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#3030FF', '#000000', '#3030FF', '#000000', '#909090', '#000000', '#909090', '#000000') ) )
        self[ BoardType.OddMirandasVeil ] = self[ BoardType.MirandasVeil ]

        self[ BoardType.Nineteen ] = CI( piece=CS(    ('#A0A050', '#000000', '#424242', '#FFFFFF') ), \
                                         star=CS(     ('#A0A050', '#000000', '#424242', '#FFFFFF') ), \
                                         monolith=CP( ('#000000', '#FFFFFF') ), \
                                         aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                         field=CS(    ('#DFDF7F', '#DFDF7F', '#FFFFFF', '#FFFFFF') ), \
                                         arrow=CMS(   ('#009000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                         text=CM(     ('#009000', '#000000', '#009000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                         marker=CM(   ('#009000', '#000000', '#009000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddNineteen ] = self[ BoardType.Nineteen ]

        self[ BoardType.HemerasDawn ] = CI( piece=CS(    ('#FF0000', '#000000', '#303030', '#FFFFFF') ), \
                                            star=CS(     ('#2020FF', '#000000', '#FFFFFF', '#000000') ), \
                                            monolith=CP( ('#000000', '#FFFFFF') ), \
                                            aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                            field=CS(    ('#501008', '#501008', '#909090', '#909090') ), \
                                            arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#FFFFFF', '#000000') ), \
                                            text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#FFFFFF', '#000000', '#FFFFFF', '#000000') ), \
                                            marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#FFFFFF', '#000000', '#FFFFFF', '#000000') ) ) # C0C0C0
        self[ BoardType.OddHemerasDawn ] = self[ BoardType.HemerasDawn ]

        self[ BoardType.TamoanchanRevisited ] = CI( piece=CS(    ('#10F030', '#000000', '#200070', '#FFFFFF') ), \
                                                    star=CS(     ('#FFFF00', '#000000', '#FF2020', '#000000') ), \
                                                    monolith=CP( ('#000000', '#FFFFFF') ), \
                                                    aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                                    field=CS(    ('#10F0E0', '#10F0E0', '#0030B0', '#0030B0') ), \
                                                    arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                                    text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                                    marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddTamoanchanRevisited ] = self[ BoardType.TamoanchanRevisited ]

        self[ BoardType.ConquestOfTlalocan ] = CI( piece=CS(    ('#10F030', '#000000', '#800000', '#FFFFFF') ), \
#                                                    star=CS(     ('#2020FF', '#000000', '#FF2020', '#000000') ), \
                                                   star=CS(     ('#2020FF', '#FFFFFF', '#FF2020', '#FFFFFF') ), \
                                                   monolith=CP( ('#000000', '#FFFFFF') ), \
                                                   aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                                   field=CS(    ('#10F0E0', '#10F0E0', '#FF0000', '#FF0000') ), \
                                                   arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                                   text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                                   marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddConquestOfTlalocan ] = self[ BoardType.ConquestOfTlalocan ]

        self[ BoardType.Discovery ] = CI( piece=CS(    ('#808080', '#000000', '#003737', '#FFFFFF') ), \
                                          star=CS(     ('#FF8000', '#000000', '#7000B0', '#FFFFFF') ), \
                                          monolith=CP( ('#000000', '#FFFFFF') ), \
                                          aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                          field=CS(    ('#FFFFDF', '#FFFFDF', '#B0B0B0', '#B0B0B0') ), \
                                          arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                          text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                          marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddDiscovery ] = self[ BoardType.Discovery ]

        self[ BoardType.One ] = CI( piece=CS(    ('#CC10EE', '#FFFFFF', '#FF30FF', '#FFFFFF') ), \
                                    star=CS(     ('#CC10EE', '#FFFFFF', '#FF30FF', '#FFFFFF') ), \
                                    monolith=CP( ('#000000', '#FFFFFF') ), \
                                    aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                    field=CS(    ('#FFFFFF', '#FFFFFF', '#480064', '#480064') ), \
                                    arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#303030', '#000000') ), \
                                    text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ), \
                                    marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#0000FF', '#000000', '#303030', '#000000', '#303030', '#000000') ) )
        self[ BoardType.OddOne ] = self[ BoardType.One ]

Colors = Colors()


def test_1():
    clr = Colors[ BoardType.Classical ]

    print
    print clr
    print
    print clr.piece
    print clr.monolith
    print clr.arrow
    print clr.text
    print


if __name__ == '__main__':
    test_1()
