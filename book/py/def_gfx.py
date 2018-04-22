#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


DEFAULT_PATH = '../gfx/' # '../tmp/'
# DEFAULT_FILE_EXT = '.png'
# DEFAULT_FILE_TYPE = 'png'


class FontDef(object):
    def __init__(self, name, size):
        assert isinstance(name, str)
        assert isinstance(size, int)

        self.name = name
        self.size = size

    def as_tuple(self):
        return (self.name, self.size)

    @staticmethod
    def from_tuple(tpl):
        return FontDef( *tpl[ 0 : 2 ] )

    def get_font(self):
        return "%s %d" % (self.name, self.size)


class ArrowDef(object):
    def __init__(self, inv_width_ratio, pointy_bit_ratio):
        assert isinstance(inv_width_ratio, float)
        assert isinstance(pointy_bit_ratio, float)

        self.inv_width_ratio = inv_width_ratio
        self.pointy_bit_ratio = pointy_bit_ratio

    def as_tuple(self):
        return (self.inv_width_ratio, self.pointy_bit_ratio)

    @staticmethod
    def from_tuple(tpl):
        return ArrowDef( *tpl[ 0 : 2 ] )


class FieldMarkerDef(object):
    def __init__(self, inv_width_ratio):
        assert isinstance(inv_width_ratio, float)

        self.inv_width_ratio = inv_width_ratio

    def as_tuple(self):
        return (self.inv_width_ratio)

    @staticmethod
    def from_tuple(tpl):
        return ArrowDef( *tpl[ 0 : 1 ] )


class GfxDefItem(object):
    def __init__(self, font_def, arrow_def, field_mark_def):
        assert isinstance(font_def, FontDef)
        assert isinstance(arrow_def, ArrowDef)
        assert isinstance(field_mark_def, FieldMarkerDef)

        self.font_def = font_def
        self.arrow_def = arrow_def
        self.field_mark_def = field_mark_def

    def as_tuple(self):
        return self.font_def.as_tuple() + \
               self.arrow_def.as_tuple() + \
               self.field_mark_def.as_tuple()

    @staticmethod
    def from_tuple(tpl):
        return GfxDefItem( font_def=FontDef( *tpl[ 0 : 2 ] ), \
                           arrow_def=ArrowDef( *tpl[ 2 : 4 ] ), \
                           field_mark_def=FieldMarkerDef( *tpl[ 4 : 5 ] ) )


class GfxDef(dict):

    def __init__(self):
        FD = FontDef # (<name>, <size>)
        AD = ArrowDef
        FMD = FieldMarkerDef
        GDI = GfxDefItem

        self[ BoardType.none ] = GDI( font_def=FD('sans bold', 192), \
                                      arrow_def=AD(12.0, 1.5), \
                                      field_mark_def=FMD(5.0) )

        self[ BoardType.Classical ] = GDI( font_def=FD('sans bold', 192), \
                                           arrow_def=AD(12.0, 1.5), \
                                           field_mark_def=FMD(5.0) )
        self[ BoardType.OddClassical ] = GDI( font_def=FD('sans bold', 192), \
                                              arrow_def=AD(12.0, 1.5), \
                                              field_mark_def=FMD(5.0) )

        self[ BoardType.CroatianTies ] = GDI( font_def=FD('sans bold', 192), \
                                              arrow_def=AD(12.0, 1.5), \
                                              field_mark_def=FMD(5.0) )
        self[ BoardType.OddCroatianTies ] = GDI( font_def=FD('sans bold', 192), \
                                                 arrow_def=AD(12.0, 1.5), \
                                                 field_mark_def=FMD(5.0) )

        self[ BoardType.MayanAscendancy ] = GDI( font_def=FD('sans bold', 192), \
                                                 arrow_def=AD(12.0, 1.5), \
                                                 field_mark_def=FMD(5.0) )
        self[ BoardType.OddMayanAscendancy ] = GDI( font_def=FD('sans bold', 192), \
                                                    arrow_def=AD(12.0, 1.5), \
                                                    field_mark_def=FMD(5.0) )

        self[ BoardType.AgeOfAquarius ] = GDI( font_def=FD('sans bold', 192), \
                                               arrow_def=AD(12.0, 1.5), \
                                               field_mark_def=FMD(5.0) )
        self[ BoardType.OddAgeOfAquarius ] = GDI( font_def=FD('sans bold', 192), \
                                                  arrow_def=AD(12.0, 1.5), \
                                                  field_mark_def=FMD(5.0) )

        self[ BoardType.MirandasVeil ] = GDI( font_def=FD('sans bold', 192), \
                                              arrow_def=AD(12.0, 1.5), \
                                              field_mark_def=FMD(5.0) )
        self[ BoardType.OddMirandasVeil ] = GDI( font_def=FD('sans bold', 192), \
                                                 arrow_def=AD(12.0, 1.5), \
                                                 field_mark_def=FMD(5.0) )

        self[ BoardType.Nineteen ] = GDI( font_def=FD('sans bold', 192), \
                                          arrow_def=AD(12.0, 1.5), \
                                          field_mark_def=FMD(5.0) )
        self[ BoardType.OddNineteen ] = GDI( font_def=FD('sans bold', 192), \
                                             arrow_def=AD(12.0, 1.5), \
                                             field_mark_def=FMD(5.0) )

        self[ BoardType.HemerasDawn ] = GDI( font_def=FD('sans bold', 192), \
                                             arrow_def=AD(12.0, 1.5), \
                                             field_mark_def=FMD(5.0) )
        self[ BoardType.OddHemerasDawn ] = GDI( font_def=FD('sans bold', 192), \
                                                arrow_def=AD(12.0, 1.5), \
                                                field_mark_def=FMD(5.0) )

        self[ BoardType.TamoanchanRevisited ] = GDI( font_def=FD('sans bold', 192), \
                                                     arrow_def=AD(12.0, 1.5), \
                                                     field_mark_def=FMD(5.0) )
        self[ BoardType.OddTamoanchanRevisited ] = GDI( font_def=FD('sans bold', 192), \
                                                        arrow_def=AD(12.0, 1.5), \
                                                        field_mark_def=FMD(5.0) )

        self[ BoardType.ConquestOfTlalocan ] = GDI( font_def=FD('sans bold', 192), \
                                                    arrow_def=AD(12.0, 1.5), \
                                                    field_mark_def=FMD(5.0) )
        self[ BoardType.OddConquestOfTlalocan ] = GDI( font_def=FD('sans bold', 192), \
                                                       arrow_def=AD(12.0, 1.5), \
                                                       field_mark_def=FMD(5.0) )

        self[ BoardType.Discovery ] = GDI( font_def=FD('sans bold', 192), \
                                           arrow_def=AD(12.0, 1.5), \
                                           field_mark_def=FMD(5.0) )
        self[ BoardType.OddDiscovery ] = GDI( font_def=FD('sans bold', 192), \
                                              arrow_def=AD(12.0, 1.5), \
                                              field_mark_def=FMD(5.0) )

        self[ BoardType.One ] = GDI( font_def=FD('sans bold', 192), \
                                     arrow_def=AD(12.0, 1.5), \
                                     field_mark_def=FMD(5.0) )
        self[ BoardType.OddOne ] = GDI( font_def=FD('sans bold', 192), \
                                        arrow_def=AD(12.0, 1.5), \
                                        field_mark_def=FMD(5.0) )

GfxDef = GfxDef()
