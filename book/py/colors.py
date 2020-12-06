#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from board import BoardType


class ColorsPair:
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


class ColorsShade:
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


class ColorsPiece:
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


class ColorsMark:
    def __init__(self, legal, illegal, action, blocked):

        assert isinstance(legal, ColorsShade)
        assert isinstance(illegal, ColorsShade)
        assert isinstance(action, ColorsShade)
        assert isinstance(blocked, ColorsShade)

        self.legal = legal
        self.illegal = illegal
        self.action = action
        self.blocked = blocked

    def as_tuple(self):
        return self.legal.as_tuple() + \
               self.illegal.as_tuple() + \
               self.action.as_tuple() + \
               self.blocked.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsMark( legal=ColorsShade.from_tuple( tpl[ 0 : 4 ] ), \
                           illegal=ColorsShade.from_tuple( tpl[ 4 : 8 ] ), \
                           action=ColorsShade.from_tuple( tpl[ 8 : 12 ] ), \
                           blocked=ColorsShade.from_tuple( tpl[ 12 : 16 ] ) )

    def __str__(self):
        fmt = "<ColorsMark" + " %s" * 16 + ">"
        return fmt % self.as_tuple()


class ColorsMarkSimple:
    def __init__(self, legal, illegal, action, blocked):

        assert isinstance(legal, ColorsPair)
        assert isinstance(illegal, ColorsPair)
        assert isinstance(action, ColorsPair)
        assert isinstance(blocked, ColorsPair)

        self.legal = legal
        self.illegal = illegal
        self.action = action
        self.blocked = blocked

    def as_tuple(self):
        return self.legal.as_tuple() + \
               self.illegal.as_tuple() + \
               self.action.as_tuple() + \
               self.blocked.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return ColorsMarkSimple( legal=ColorsPair.from_tuple( tpl[ 0 : 2 ] ), \
                                 illegal=ColorsPair.from_tuple( tpl[ 2 : 4 ] ), \
                                 action=ColorsPair.from_tuple( tpl[ 4 : 6 ] ), \
                                 blocked=ColorsPair.from_tuple( tpl[ 6 : 8 ] ) )

    def __str__(self):
        fmt = "<ColorsMarkSimple" + " %s" * 8 + ">"
        return fmt % self.as_tuple()


class ColorsItem:
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
    def __init__(self, cot_in_bw=False, all_in_bw=False):
        self.cot_in_bw = cot_in_bw # Conquest of Tlalocan
        self.all_in_bw = all_in_bw


        CP = ColorsPair.from_tuple # (<interior>, <outline>)
        CS = ColorsShade.from_tuple # (<light interior>, <light outline>, <dark interior>, <dark outline>)
        CM = ColorsMark.from_tuple # ( <legal light interior>, <legal light outline>, <legal dark interior>, <legal dark outline>, \
                                   #   <illegal light interior>, <illegal light outline>, <illegal dark interior>, <illegal dark outline>, \
                                   #   <action light interior>, <action light outline>, <action dark interior>, <action dark outline>, \
                                   #   <blocked light interior>, <blocked light outline>, <blocked dark interior>, <blocked dark outline> )
        CMS = ColorsMarkSimple.from_tuple # ( <legal interior>, <legal outline>, <illegal interior>, <illegal outline>, <action interior>, <action outline>, <blocked interior>, <blocked outline> )
        CI = ColorsItem # ...


        self[ BoardType.none ] = CI( piece=CS(    ('#FFFFFF', '#000000', '#000000', '#FFFFFF') ), \
                                     star=CS(     ('#FFFFFF', '#000000', '#000000', '#FFFFFF') ), \
                                     monolith=CP( ('#000000', '#FFFFFF') ), \
                                     aura=CP(     ('#FFFFFF', '#FFFFFF') ), \
                                     # field=CS(    ('#FFFFFF', '#FFFFFF', '#EEEEEE', '#EEEEEE') ), \
                                     field=CS(    ('#EEEEEE', '#EEEEEE', '#DDDDDD', '#DDDDDD') ), \
                                     arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#333333', '#000000') ), \
                                     text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                   '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                     marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                   '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )

        self[ BoardType.Classical ] = CI( piece=CS(    ('#AFAFAF', '#000000', '#222222', '#FFFFFF') ), \
                                          star=CS(     ('#AFAFAF', '#000000', '#222222', '#FFFFFF') ), \
                                          monolith=CP( ('#000000', '#FFFFFF') ), \
                                          aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                          field=CS(    ('#EFEFEF', '#EFEFEF', '#666666', '#666666') ), \
#                                           arrow=CMS(   ('#00BF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#333333', '#000000') ), \
                                          arrow=CMS(   ('#00BF00', '#000000', '#FF0000', '#000000', '#3333FF', '#000000', '#333333', '#000000') ), \
                                          text=CM(     ('#00BF00', '#000000', '#00BF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                        '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                          marker=CM(   ('#00BF00', '#000000', '#00BF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                        '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddClassical ] = self[ BoardType.Classical ]

        self[ BoardType.CroatianTies ] = CI( piece=CS(    ('#AFAFAF', '#000000', '#5F0000', '#FFFFFF') ), \
                                             star=CS(     ('#AFAFAF', '#000000', '#5F0000', '#FFFFFF') ), \
                                             monolith=CP( ('#000000', '#FFFFFF') ), \
                                             aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                             field=CS(    ('#EFEFEF', '#EFEFEF', '#FF0000', '#FF0000') ), \
                                             arrow=CMS(   ('#00FF00', '#000000', '#AF0000', '#000000', '#0000FF', '#000000', '#7F7F7F', '#000000') ), \
                                             text=CM(     ('#00AF00', '#000000', '#00DF00', '#000000', '#AF0000', '#000000', '#7F0000', '#000000', \
                                                           '#0000FF', '#000000', '#0000FF', '#000000', '#444444', '#000000', '#444444', '#000000') ), \
                                             marker=CM(   ('#00AF00', '#000000', '#00DF00', '#000000', '#AF0000', '#000000', '#7F0000', '#000000', \
                                                           '#0000FF', '#000000', '#0000FF', '#000000', '#444444', '#000000', '#444444', '#000000') ) )
        self[ BoardType.OddCroatianTies ] = self[ BoardType.CroatianTies ]

        self[ BoardType.MayanAscendancy ] = CI( piece=CS(    ('#8F8F00', '#000000', '#002F5F', '#FFFFFF') ), \
                                                star=CS(     ('#8F8F00', '#000000', '#002F5F', '#FFFFFF') ), \
                                                monolith=CP( ('#000000', '#FFFFFF') ), \
                                                aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                                field=CS(    ('#FFFF00', '#FFFF00', '#007FCF', '#007FCF') ), \
                                                arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#7F7F7F', '#000000') ), \
                                                text=CM(     ('#007F00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                              '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                                marker=CM(   ('#007F00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                              '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddMayanAscendancy ] = self[ BoardType.MayanAscendancy ]

        self[ BoardType.AgeOfAquarius ] = CI( piece=CS(    ('#AFAF7F', '#000000', '#1F5F1F', '#FFFFFF') ), \
                                              star=CS(     ('#AFAF7F', '#000000', '#1F5F1F', '#FFFFFF') ), \
                                              monolith=CP( ('#000000', '#FFFFFF') ), \
                                              aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                              field=CS(    ('#FFFFDF', '#FFFFDF', '#3FBF3F', '#3FBF3F') ), \
                                              arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#7F7F7F', '#000000') ), \
                                              text=CM(     ('#005F00', '#000000', '#005F00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                            '#0000FF', '#000000', '#0000FF', '#000000', '#444444', '#000000', '#444444', '#000000') ), \
                                              marker=CM(   ('#005F00', '#000000', '#005F00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                            '#0000FF', '#000000', '#0000FF', '#000000', '#444444', '#000000', '#444444', '#000000') ) )
        self[ BoardType.OddAgeOfAquarius ] = self[ BoardType.AgeOfAquarius ]

        self[ BoardType.MirandasVeil ] = CI( piece=CS(    ('#FF7FFF', '#000000', '#300050', '#FFFFFF') ), \
                                             star=CS(     ('#FF7FFF', '#000000', '#300050', '#FFFFFF') ), \
                                             monolith=CP( ('#000000', '#FFFFFF') ), \
                                             aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                             field=CS(    ('#FFFFFF', '#FFFFFF', '#500070', '#500070') ), \
                                             arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#3030FF', '#000000', '#909090', '#000000') ), \
                                             text=CM(     ('#007F00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                           '#3030FF', '#000000', '#3030FF', '#000000', '#606060', '#000000', '#C0C0C0', '#000000') ), \
                                             marker=CM(   ('#007F00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                           '#3030FF', '#000000', '#3030FF', '#000000', '#606060', '#000000', '#C0C0C0', '#000000') ) )
        self[ BoardType.OddMirandasVeil ] = self[ BoardType.MirandasVeil ]

        self[ BoardType.Nineteen ] = CI( piece=CS(    ('#A0975F', '#000000', '#424242', '#FFFFFF') ), # #a0955d \
                                         star=CS(     ('#A0975F', '#000000', '#424242', '#FFFFFF') ), \
                                         monolith=CP( ('#000000', '#FFFFFF') ), \
                                         aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                         field=CS(    ('#F7E790', '#F7E790', '#FFFFFF', '#FFFFFF') ), # #fae991 \
                                         arrow=CMS(   ('#009000', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#333333', '#000000') ), \
                                         text=CM(     ('#009000', '#000000', '#009000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                       '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                         marker=CM(   ('#009000', '#000000', '#009000', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                       '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddNineteen ] = self[ BoardType.Nineteen ]

        self[ BoardType.HemerasDawn ] = CI( piece=CS(    ('#FF0000', '#000000', '#FFFF00', '#000000') ), \
                                            star=CS(     ('#2020FF', '#000000', '#FFFFFF', '#000000') ), \
                                            monolith=CP( ('#000000', '#FFFFFF') ), \
                                            aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                            field=CS(    ('#501008', '#501008', '#909090', '#909090') ), \
                                            arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#FFFFFF', '#000000') ), \
                                            text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                          '#0000FF', '#000000', '#0000FF', '#000000', '#FFFFFF', '#000000', '#FFFFFF', '#000000') ), \
                                            marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                          '#0000FF', '#000000', '#0000FF', '#000000', '#FFFFFF', '#000000', '#FFFFFF', '#000000') ) ) # BFBFBF
        self[ BoardType.OddHemerasDawn ] = self[ BoardType.HemerasDawn ]

        self[ BoardType.TamoanchanRevisited ] = CI( piece=CS(    ('#BBBBBB', '#000000', '#666666', '#FFFFFF') ), \
                                                    star=CS(     ('#FFFF00', '#000000', '#FF2020', '#000000') ), \
                                                    monolith=CP( ('#000000', '#FFFFFF') ), \
                                                    aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                                    field=CS(    ('#FFFFFF', '#FFFFFF', '#00CCFF', '#00CCFF') ), \
                                                    arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#999999', '#000000') ), \
                                                    text=CM(     ('#007F00', '#000000', '#007F00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                                  '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                                    marker=CM(   ('#007F00', '#000000', '#007F00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                                  '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddTamoanchanRevisited ] = self[ BoardType.TamoanchanRevisited ]

        self[ BoardType.ConquestOfTlalocan ] = CI( piece=CS(    ('#7F0000', '#FFFFFF', '#10F030', '#000000') ), \
                                                   star=CS(     ('#FF2020', '#FFFFFF', '#2020FF', '#FFFFFF') ), \
                                                   monolith=CP( ('#000000', '#FFFFFF') ), \
                                                   aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                                   field=CS(    ('#FF0000', '#FF0000', '#10F0E0', '#10F0E0') ), \
                                                   arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#333333', '#000000') ), \
                                                   text=CM(     ('#00AA00', '#000000', '#006600', '#000000', '#990000', '#000000', '#990000', '#000000', \
                                                                 '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                                   marker=CM(   ('#00AA00', '#000000', '#006600', '#000000', '#990000', '#000000', '#990000', '#000000', \
                                                                 '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddConquestOfTlalocan ] = self[ BoardType.ConquestOfTlalocan ]

        self[ BoardType.Discovery ] = CI( piece=CS(    ('#6F6F6F', '#000000', '#003737', '#FFFFFF') ), \
                                          star=CS(     ('#FF7F00', '#000000', '#7000AF', '#FFFFFF') ), \
                                          monolith=CP( ('#000000', '#FFFFFF') ), \
                                          aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                          field=CS(    ('#FFFFDF', '#FFFFDF', '#AFAFAF', '#AFAFAF') ), \
                                          arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#333333', '#000000') ), \
                                          text=CM(     ('#009900', '#000000', '#009900', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                        '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ), \
                                          marker=CM(   ('#009900', '#000000', '#009900', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                        '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddDiscovery ] = self[ BoardType.Discovery ]

        # Previously, light piece: CC10EE; dark piece: FF30FF.
        self[ BoardType.One ] = CI( piece=CS(    ('#DD20EE', '#FFFFFF', '#FF47FF', '#FFFFFF') ), \
                                    star=CS(     ('#FF47FF', '#FFFFFF', '#DD20EE', '#FFFFFF') ), \
                                    monolith=CP( ('#000000', '#FFFFFF') ), \
                                    aura=CP(     ('#FFBFFF', '#FFFFFF') ), \
                                    field=CS(    ('#FFFFFF', '#FFFFFF', '#480064', '#480064') ), \
                                    arrow=CMS(   ('#00FF00', '#000000', '#FF0000', '#000000', '#0000FF', '#000000', '#333333', '#000000') ), \
                                    text=CM(     ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                  '#0000FF', '#000000', '#6666FF', '#000000', '#333333', '#000000', '#999999', '#000000') ), \
                                    marker=CM(   ('#00FF00', '#000000', '#00FF00', '#000000', '#FF0000', '#000000', '#FF0000', '#000000', \
                                                  '#0000FF', '#000000', '#0000FF', '#000000', '#333333', '#000000', '#333333', '#000000') ) )
        self[ BoardType.OddOne ] = self[ BoardType.One ]

        # CP = ColorsPair.from_tuple # (<interior>, <outline>)
        # CS = ColorsShade.from_tuple # (<light interior>, <light outline>, <dark interior>, <dark outline>)
        # CM = ColorsMark.from_tuple # ( <legal light interior>, <legal light outline>, <legal dark interior>, <legal dark outline>, \
        #                            #   <illegal light interior>, <illegal light outline>, <illegal dark interior>, <illegal dark outline>, \
        #                            #   <action light interior>, <action light outline>, <action dark interior>, <action dark outline>, \
        #                            #   <blocked light interior>, <blocked light outline>, <blocked dark interior>, <blocked dark outline> )
        # CMS = ColorsMarkSimple.from_tuple # ( <legal interior>, <legal outline>, <illegal interior>, <illegal outline>, <action interior>, <action outline>, <blocked interior>, <blocked outline> )

    def fetch_colors(self, bt, enforce_bw=False):
        if enforce_bw:
            return self[ BoardType.Classical ]

        if self.all_in_bw:
            return self[ BoardType.Classical ]

        if self.cot_in_bw and bt.is_variants(BoardType.ConquestOfTlalocan):
            return self[ BoardType.Classical ]

        return self[ bt ]

Colors = Colors(cot_in_bw=False, all_in_bw=False)


def test_1():
    clr = Colors[ BoardType.Classical ]

    print()
    print( clr )
    print()
    print( clr.piece )
    print( clr.monolith )
    print( clr.arrow )
    print( clr.text )
    print()


if __name__ == '__main__':
    test_1()
