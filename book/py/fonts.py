#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class Font(object):
    def __init__(self, name, size):
        assert isinstance(name, str)
        assert isinstance(size, int)

        self.name = name
        self.size = size

    def as_tuple(self):
        return (self.name, self.size)

    @staticmethod
    def from_tuple(tpl):
        return ColorsPair( *tpl[ 0 : 2 ] )

    def get_font(self):
        return "%s %d" % (self.name, self.size)


class Fonts(dict):
    def __init__(self):
        F = Font # (<name>, <size>)

        self[ BoardType.none ] = F('sans bold', 192)

        self[ BoardType.Classical ] = F('sans bold', 192)
        self[ BoardType.OddClassical ] = F('sans bold', 192)

        self[ BoardType.CroatianTies ] = F('sans bold', 192)
        self[ BoardType.OddCroatianTies ] = F('sans bold', 192)

        self[ BoardType.MayanAscendancy ] = F('sans bold', 192)
        self[ BoardType.OddMayanAscendancy ] = F('sans bold', 192)

        self[ BoardType.AgeOfAquarius ] = F('sans bold', 192)
        self[ BoardType.OddAgeOfAquarius ] = F('sans bold', 192)

        self[ BoardType.MirandasVeil ] = F('sans bold', 192)
        self[ BoardType.OddMirandasVeil ] = F('sans bold', 192)

        self[ BoardType.Nineteen ] = F('sans bold', 192)
        self[ BoardType.OddNineteen ] = F('sans bold', 192)

        self[ BoardType.HemerasDawn ] = F('sans bold', 192)
        self[ BoardType.OddHemerasDawn ] = F('sans bold', 192)

        self[ BoardType.TamoanchanRevisited ] = F('sans bold', 192)
        self[ BoardType.OddTamoanchanRevisited ] = F('sans bold', 192)

        self[ BoardType.ConquestOfTlalocan ] = F('sans bold', 192)
        self[ BoardType.OddConquestOfTlalocan ] = F('sans bold', 192)

        self[ BoardType.Discovery ] = F('sans bold', 192)
        self[ BoardType.OddDiscovery ] = F('sans bold', 192)

        self[ BoardType.One ] = F('sans bold', 192)
        self[ BoardType.OddOne ] = F('sans bold', 192)

Fonts = Fonts()
