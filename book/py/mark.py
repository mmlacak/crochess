#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class MarkType(int):
    none = 0
    Legal = 1
    Ilegal = 2
    Action = 3
    Blocked = 4

    def __new__(cls, value):
        if MarkType._is_valid(value):
            return super(MarkType, cls).__new__(cls, value)
        else:
            raise ValueError("No such a mark type, received '%s'." % (str(value), ))

    @staticmethod
    def _is_valid(mark_type):
        return MarkType.none <= mark_type <= MarkType.Blocked

    @staticmethod
    def iter(include_none=False):
        lst =  [ MarkType.Legal, \
                 MarkType.Ilegal, \
                 MarkType.Action, \
                 MarkType.Blocked ]

        if include_none:
            lst.insert(0, MarkType.none)

        lst.sort()
        return [ MarkType(mt) for mt in lst ]


class Arrow(object):

    def __init__(self, start_x, start_y, end_x, end_y, mark_type=MarkType(MarkType.Legal), \
                 start_pointer=False, \
                 end_pointer=True):
        assert isinstance(start_x, (int, float))
        assert isinstance(start_y, (int, float))
        assert isinstance(end_x, (int, float))
        assert isinstance(end_y, (int, float))
        assert isinstance(mark_type, MarkType)
        assert isinstance(start_pointer, bool)
        assert isinstance(end_pointer, bool)

        assert type(start_x) is type(start_y)
        assert type(end_x) is type(end_y)

        self.start = (start_x, start_y)
        self.end = (end_x, end_y)
        self.mark_type = mark_type
        self.start_pointer = start_pointer
        self.end_pointer = end_pointer

    def reverse(self):
        self.start, self.end = self.end, self.start
        self.start_pointer, self.end_pointer = self.end_pointer, self.start_pointer


class Text(object):

    def __init__(self, text, pos_x, pos_y, mark_type=MarkType(MarkType.Legal)):
        assert isinstance(text, str)
        assert isinstance(pos_x, (int, float))
        assert isinstance(pos_y, (int, float))
        # assert isinstance(mark_type, MarkType)

        assert type(pos_x) is type(pos_y)

        mt = MarkType(mark_type)

        self.text = text
        self.pos = (pos_x, pos_y)
        self.mark_type = mt


class FieldMarker(object):

    def __init__(self, field_i, field_j, mark_type=MarkType(MarkType.Legal)):
        assert isinstance(field_i, int)
        assert isinstance(field_j, int)
        assert isinstance(mark_type, MarkType)

        self.field = (field_i, field_j)
        self.mark_type = mark_type
