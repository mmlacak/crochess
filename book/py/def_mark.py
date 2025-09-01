#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from consts import DEFAULT_ARROW_INVERSE_WIDTH_RATIO, \
    DEFAULT_ARROW_POINTY_BIT_RATIO, \
    DEFAULT_FONT_NAME, \
    DEFAULT_FONT_SIZE_INVERSE_RATIO, \
    DEFAULT_FONT_SIZE, \
    DEFAULT_FIELD_MARKER_INVERSE_WIDTH_RATIO

from board import BoardType


class FontDef(object):
    def __init__(self, name, inv_size_ratio):
        assert isinstance(name, str)
        assert isinstance(inv_size_ratio, float)

        self.name = name
        self.inv_size_ratio = inv_size_ratio

    def as_tuple(self):
        return (self.name, self.inv_size_ratio)

    @staticmethod
    def from_tuple(tpl):
        return FontDef( *tpl[ 0 : 2 ] )


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


class MarkDefItem(object):
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
        return MarkDefItem( font_def=FontDef( *tpl[ 0 : 2 ] ), \
                            arrow_def=ArrowDef( *tpl[ 2 : 4 ] ), \
                            field_mark_def=FieldMarkerDef( *tpl[ 4 : 5 ] ) )


class MarkDef(dict):

    def __init__(self):
        FD = FontDef # (<name>, <inv_size_ratio>)
        AD = ArrowDef # (<inv_width_ratio>, <pointy_bit_ratio>)
        FMD = FieldMarkerDef # (<inv_width_ratio>, )
        MDI = MarkDefItem # (<font_def>, <arrow_def>, <field_mark_def>)

        self[ BoardType.none ] = MDI( font_def=FD('sans bold', 5.0), \
                                      arrow_def=AD(12.0, 1.5), \
                                      field_mark_def=FMD(5.0) )

        self[ BoardType.Classical ] = MDI( font_def=FD('sans bold', 5.0), \
                                           arrow_def=AD(12.0, 1.5), \
                                           field_mark_def=FMD(5.0) )

        self[ BoardType.CroatianTies ] = MDI( font_def=FD('sans bold', 5.0), \
                                              arrow_def=AD(12.0, 1.5), \
                                              field_mark_def=FMD(5.0) )

        self[ BoardType.MayanAscendancy ] = MDI( font_def=FD('sans bold', 5.0), \
                                                 arrow_def=AD(12.0, 1.5), \
                                                 field_mark_def=FMD(5.0) )

        self[ BoardType.AgeOfAquarius ] = MDI( font_def=FD('sans bold', 5.0), \
                                               arrow_def=AD(12.0, 1.5), \
                                               field_mark_def=FMD(5.0) )

        self[ BoardType.MirandasVeil ] = MDI( font_def=FD('sans bold', 5.0), \
                                              arrow_def=AD(12.0, 1.5), \
                                              field_mark_def=FMD(5.0) )

        self[ BoardType.Nineteen ] = MDI( font_def=FD('sans bold', 5.0), \
                                          arrow_def=AD(12.0, 1.5), \
                                          field_mark_def=FMD(5.0) )

        self[ BoardType.HemerasDawn ] = MDI( font_def=FD('sans bold', 5.0), \
                                             arrow_def=AD(12.0, 1.5), \
                                             field_mark_def=FMD(5.0) )

        self[ BoardType.TamoanchanRevisited ] = MDI( font_def=FD('sans bold', 5.0), \
                                                     arrow_def=AD(12.0, 1.5), \
                                                     field_mark_def=FMD(5.0) )

        self[ BoardType.ConquestOfTlalocan ] = MDI( font_def=FD('sans bold', 5.0), \
                                                    arrow_def=AD(12.0, 1.5), \
                                                    field_mark_def=FMD(5.0) )

        self[ BoardType.Discovery ] = MDI( font_def=FD('sans bold', 5.0), \
                                           arrow_def=AD(12.0, 1.5), \
                                           field_mark_def=FMD(5.0) )

        self[ BoardType.One ] = MDI( font_def=FD('sans bold', 5.0), \
                                     arrow_def=AD(12.0, 1.5), \
                                     field_mark_def=FMD(5.0) )

        self[ BoardType.Classic_14 ] = MDI( font_def=FD('sans bold', 5.0), \
                                            arrow_def=AD(12.0, 1.5), \
                                            field_mark_def=FMD(5.0) )

        self[ BoardType.Classic_20 ] = MDI( font_def=FD('sans bold', 5.0), \
                                            arrow_def=AD(12.0, 1.5), \
                                            field_mark_def=FMD(5.0) )

        self[ BoardType.Classic_26 ] = MDI( font_def=FD('sans bold', 5.0), \
                                            arrow_def=AD(12.0, 1.5), \
                                            field_mark_def=FMD(5.0) )

        self[ BoardType.Croatian_14 ] = MDI( font_def=FD('sans bold', 5.0), \
                                             arrow_def=AD(12.0, 1.5), \
                                             field_mark_def=FMD(5.0) )

        self[ BoardType.Croatian_20 ] = MDI( font_def=FD('sans bold', 5.0), \
                                             arrow_def=AD(12.0, 1.5), \
                                             field_mark_def=FMD(5.0) )

        self[ BoardType.Croatian_26 ] = MDI( font_def=FD('sans bold', 5.0), \
                                             arrow_def=AD(12.0, 1.5), \
                                             field_mark_def=FMD(5.0) )

MarkDef = MarkDef()
