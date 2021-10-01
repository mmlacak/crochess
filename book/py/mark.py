#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

from pixel_math import is_any, q_same_rounded_floats


class MarkType(int):
    none = 0
    Legal = 1
    Illegal = 2
    Action = 3
    Blocked = 4

    def __new__(cls, value):
        if MarkType._is_valid(value):
            return super(MarkType, cls).__new__(cls, value)
        else:
            raise ValueError("No such a mark type, received '%s'." % (str(value), ))

    @staticmethod
    def iter(include_none=False, do_construct=True):
        lst =  [ MarkType.Legal, \
                 MarkType.Illegal, \
                 MarkType.Action, \
                 MarkType.Blocked ]

        if include_none:
            lst.insert(0, MarkType.none)

        lst.sort()
        return [ MarkType(mt) if do_construct else mt for mt in lst ]

    @staticmethod
    def _is_valid(mark_type):
        return mark_type in MarkType.iter(include_none=True, do_construct=False)


class Arrow(object):

    def __init__(self, start_x, start_y, end_x, end_y, mark_type=MarkType(MarkType.Legal), \
                 start_pointer=False, \
                 end_pointer=True):
        assert isinstance(start_x, (int, float))
        assert isinstance(start_y, (int, float))
        assert isinstance(end_x, (int, float))
        assert isinstance(end_y, (int, float))
        # assert isinstance(mark_type, MarkType)
        assert isinstance(start_pointer, bool)
        assert isinstance(end_pointer, bool)

        assert type(start_x) is type(start_y)
        assert type(end_x) is type(end_y)

        mt = MarkType(mark_type)

        self.start = (start_x, start_y)
        self.end = (end_x, end_y)
        self.mark_type = mt
        self.start_pointer = start_pointer
        self.end_pointer = end_pointer

    def reverse(self):
        self.start, self.end = self.end, self.start
        self.start_pointer, self.end_pointer = self.end_pointer, self.start_pointer

    def same_position(self, other, digits=6):
        assert isinstance(other, Arrow)

        if is_any( other.start + other.end + self.start + self.end, type_=float ):
            o_x1, o_y1 = other.start
            o_x2, o_y2 = other.end
            s_x1, s_y1 = self.start
            s_x2, s_y2 = self.end

            return ( q_same_rounded_floats(o_x1, s_x1, digits=digits) and \
                     q_same_rounded_floats(o_y1, s_y1, digits=digits) and \
                     q_same_rounded_floats(o_x2, s_x2, digits=digits) and \
                     q_same_rounded_floats(o_y2, s_y2, digits=digits) ) or \
                   ( q_same_rounded_floats(o_x1, s_x2, digits=digits) and \
                     q_same_rounded_floats(o_y1, s_y2, digits=digits) and \
                     q_same_rounded_floats(o_x2, s_x1, digits=digits) and \
                     q_same_rounded_floats(o_y2, s_y1, digits=digits) )
        else:
            return (other.start == self.start and other.end == self.end) or \
                   (other.start == self.end and other.end == self.start)


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

    def same_position(self, other):
        assert isinstance(other, Text)

        return other.pos == self.pos


class FieldMarker(object):

    def __init__(self, field_i, field_j, mark_type=MarkType(MarkType.Legal)):
        assert isinstance(field_i, int)
        assert isinstance(field_j, int)
        # assert isinstance(mark_type, MarkType)

        mt = MarkType(mark_type)

        self.field = (field_i, field_j)
        self.mark_type = mt

    def same_position(self, other):
        assert isinstance(other, FieldMarker)

        return other.field == self.field
