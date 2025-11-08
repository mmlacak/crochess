#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from consts import  DEFAULT_FONT_SIZE, \
                    DEFAULT_FONT_SPACER, \
                    DEFAULT_FIELD_MARKER_SIZE

from utils import xor
import pixel_math as pm
from coords import Pos, RectPos
from corner import Corner
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType, Arrow, Text, FieldMarker


DEFAULT_CORNER_MARGINS = RectPos( 0.0 + 2 * DEFAULT_FONT_SPACER, \
                                  1.0 - DEFAULT_FONT_SPACER - DEFAULT_FONT_SIZE, \
                                  1.0 - DEFAULT_FONT_SPACER - DEFAULT_FONT_SIZE, \
                                  0.0 + 5 * DEFAULT_FONT_SPACER ) # left, top, right, bottom

DEFAULT_CORNER_MARGINS_WITH_FIELD_MARKER = RectPos( 0.0 + 2 * DEFAULT_FONT_SPACER + DEFAULT_FIELD_MARKER_SIZE, \
                                                    1.0 - DEFAULT_FONT_SPACER - DEFAULT_FONT_SIZE, \
                                                    1.0 - DEFAULT_FONT_SPACER - DEFAULT_FONT_SIZE - DEFAULT_FIELD_MARKER_SIZE, \
                                                    0.0 + 5 * DEFAULT_FONT_SPACER ) # left, top, right, bottom


def get_coord_offset( coord, offset=0.5 ):
    return float( coord + offset) if isinstance( coord, int ) else float(coord )

def recalc_arrow_ends( start_i, start_j, end_i, end_j ):
    starts_are_ints = bool( isinstance( start_i, int ) and isinstance( start_j, int ) )
    starts_are_floats = bool( isinstance( start_i, float ) and isinstance( start_j, float ) )
    assert xor( starts_are_ints, starts_are_floats, default=False ), \
           "Unexpected types for starting i and j (or, x and y), found ('%s', '%s'), expected both to be either ints or floats." % (type( start_i ), type( start_j ))

    ends_are_ints = bool( isinstance( end_i, int ) and isinstance( end_j, int ) )
    ends_are_floats = bool( (isinstance( end_i, float ) and isinstance( end_j, float )) )
    assert xor( ends_are_ints, ends_are_floats, default=False ), \
           "Unexpected types for ending i and j (or, x and y), found ('%s', '%s'), expected both to be either ints or floats." % (type( end_i ), type( end_j ))

    if starts_are_ints or ends_are_ints:
        diff_i = end_i - start_i
        diff_j = end_j - start_j

        start_x_off = start_x = get_coord_offset( start_i )
        start_y_off = start_y = get_coord_offset( start_j )
        end_x_off = end_x = get_coord_offset( end_i )
        end_y_off = end_y = get_coord_offset( end_j )

        offset_x = 0.9 if diff_i > 0.0 else 0.1
        offset_y = 0.9 if diff_j > 0.0 else 0.1

        if pm.q_same_rounded_floats( end_x, start_x ):
            start_y_off = get_coord_offset( start_j, offset=offset_y )
            end_y_off = get_coord_offset( end_j, offset=(1.0 - offset_y) )
        elif pm.q_same_rounded_floats( end_y, start_y ):
            start_x_off = get_coord_offset( start_i, offset=offset_x )
            end_x_off = get_coord_offset( end_i, offset=(1.0 - offset_x) )
        else:
            a, b = pm.calc_straight_line( (start_x, start_y), (end_x, end_y) )

            is_x_crossed = bool( abs( diff_i) > abs(diff_j ) )

            if is_x_crossed:
                start_x_off = get_coord_offset( start_i, offset=offset_x )
                end_x_off = get_coord_offset( end_i, offset=(1.0 - offset_x) )

                start_y_off = a * start_x_off + b
                end_y_off = a * end_x_off + b
            else:
                start_y_off = get_coord_offset( start_j, offset=offset_y )
                end_y_off = get_coord_offset( end_j, offset=(1.0 - offset_y) )

                start_x_off = (start_y_off - b) / a
                end_x_off = (end_y_off - b) / a

    if starts_are_floats:
        start_x_off = start_i
        start_y_off = start_j

    if ends_are_floats:
        end_x_off = end_i
        end_y_off = end_j

    return [start_x_off, start_y_off, end_x_off, end_y_off]


def get_func_get_text_position( left=None, top=None, right=None, bottom=None ):
    assert isinstance( left, (float, type( None) ) )
    assert isinstance( top, (float, type( None) ) )
    assert isinstance( right, (float, type( None) ) )
    assert isinstance( bottom, (float, type( None) ) )

    def get_text_position( pos_i, pos_j, corner ):
        assert isinstance( pos_i, (int, float) )
        assert isinstance( pos_j, (int, float) )

        crnr = Corner( corner )

        if crnr.is_position():
            return (float( pos_i), float( pos_j ) )

        _default = DEFAULT_CORNER_MARGINS_WITH_FIELD_MARKER if crnr.is_with_field_marker() else DEFAULT_CORNER_MARGINS

        _left = left if left is not None else _default.left
        _top = top if top is not None else _default.top
        _right = right if right is not None else _default.right
        _bottom = bottom if bottom is not None else _default.bottom

        x = _left if crnr.is_left() else _right
        y = _top if crnr.is_upper() else _bottom

        return (float( pos_i + x), float( pos_j + y ) )

    return get_text_position


class Scene:

    def __init__( self, file_name, board_type, x=0.0, y=0.0, width=None, height=None, reverse_off_board_field_colors=False, margin=None, skip_if_rendering_board=None, board_view=None, *args, **kwargs ):
        assert isinstance( file_name, str )

        super( Scene, self).__init__(*args, **kwargs )

        self.file_name = file_name

        bt = BoardType( board_type )
        self.board = Board(bt)

        w = float( width ) if width is not None else None
        h = float( height ) if height is not None else None
        self.board_view = board_view or BoardView( x=float( x ), y=float( y ), width=w, height=h, reverse_off_board_field_colors=reverse_off_board_field_colors, margin=margin, board_type=bt, skip_if_rendering_board=skip_if_rendering_board )

        self.arrows = [] # :: [ mark.Arrow, ... ]
        self.texts = [] # :: [ mark.Text, ... ]
        self.field_markers = [] # :: [ mark.FieldMarker, ... ]

    def new_text(self, txt, pos_i, pos_j, \
                 corner=Corner( Corner.UpperLeft ), \
                 mark_type=MarkType( MarkType.Legal ), \
                 rect=None):
        # assert isinstance( txt, str )
        assert isinstance( pos_i, (int, float) )
        assert isinstance( pos_j, (int, float) )
        # assert isinstance( corner, (Corner, int) )
        # assert isinstance( mark_type, MarkType )
        assert isinstance( rect, (tuple, RectPos, type( None) ) )

        assert type( pos_i) is type(pos_j )

        crnr = Corner( corner )
        _default = DEFAULT_CORNER_MARGINS_WITH_FIELD_MARKER if crnr.is_with_field_marker() else DEFAULT_CORNER_MARGINS
        _rect = rect if rect is not None else _default
        left, top, right, bottom = _rect.as_tuple() if isinstance( _rect, RectPos ) else _rect

        get_text_position = get_func_get_text_position( left=left, top=top, right=right, bottom=bottom )
        pos_x, pos_y = get_text_position( pos_i, pos_j, crnr )

        txt_mark = Text( txt, pos_x, pos_y, mark_type=mark_type )

        return txt_mark

    def append_text(self, txt, pos_i, pos_j, \
                    corner=Corner( Corner.UpperLeft ), \
                    mark_type=MarkType( MarkType.Legal ), \
                    rect=None):

        txt_mark = self.new_text( txt, pos_i, pos_j, corner=corner, mark_type=mark_type, rect=rect )
        self.texts.append( txt_mark )

        return txt_mark

    def replace_text(self, txt, pos_i, pos_j, \
                     corner=Corner( Corner.UpperLeft ), \
                     mark_type=MarkType( MarkType.Legal ), \
                     rect=None):

        txt_mark = self.new_text( txt, pos_i, pos_j, corner=corner, mark_type=mark_type, rect=rect )

        for i, tm in enumerate( self.texts ):
            if tm.same_position( txt_mark ):
                self.texts[ i ] = txt_mark

        return txt_mark

    def new_arrow(self, start_i, start_j, end_i, end_j, \
                  mark_type=MarkType( MarkType.Legal ), \
                  start_pointer=False, \
                  end_pointer=True, \
                  do_recalc_arrow_ends=True):
        # assert isinstance( start_i, (int, float) )
        # assert isinstance( start_j, (int, float) )
        # assert isinstance( end_i, (int, float) )
        # assert isinstance( end_j, (int, float) )
        # assert isinstance( mark_type, MarkType )

        start_x, start_y, end_x, end_y = recalc_arrow_ends( start_i, start_j, end_i, end_j ) \
                                         if do_recalc_arrow_ends \
                                         else (start_i, start_j, end_i, end_j)

        arw_mark = Arrow(start_x, start_y, end_x, end_y, mark_type=mark_type, \
                         start_pointer=start_pointer, \
                         end_pointer=end_pointer)

        return arw_mark

    def append_arrow(self, start_i, start_j, end_i, end_j, \
                     mark_type=MarkType( MarkType.Legal ), \
                     start_pointer=False, \
                     end_pointer=True, \
                     do_recalc_arrow_ends=True):

        arw_mark = self.new_arrow(start_i, start_j, end_i, end_j, mark_type=mark_type, \
                                  start_pointer=start_pointer, \
                                  end_pointer=end_pointer, \
                                  do_recalc_arrow_ends=do_recalc_arrow_ends)

        self.arrows.append( arw_mark )

        return arw_mark

    def replace_arrow(self, start_i, start_j, end_i, end_j, \
                      mark_type=MarkType( MarkType.Legal ), \
                      start_pointer=False, \
                      end_pointer=True, \
                      do_recalc_arrow_ends=True):

        arw_mark = self.new_arrow(start_i, start_j, end_i, end_j, mark_type=mark_type, \
                                  start_pointer=start_pointer, \
                                  end_pointer=end_pointer, \
                                  do_recalc_arrow_ends=do_recalc_arrow_ends)

        for i, am in enumerate( self.arrows ):
            if am.same_position( arw_mark ):
                self.arrows[ i ] = arw_mark

        return arw_mark

    def new_field_marker( self, field_i, field_j, corner=None, mark_type=MarkType( MarkType.Legal ) ):
        # assert isinstance( mark_type, MarkType )

        fld_mark = FieldMarker( field_i, field_j, corner=corner, mark_type=mark_type )

        return fld_mark

    def append_field_marker( self, field_i, field_j, corner=None, mark_type=MarkType( MarkType.Legal ), force_unique=False ):
        # assert isinstance( mark_type, MarkType )

        fld_mark = self.new_field_marker( field_i, field_j, corner=corner, mark_type=mark_type )

        if force_unique:
            for i, fm in enumerate( self.field_markers ):
                if fm.same_position( fld_mark ):
                    raise ValueError( "Field marker not unique, at position (%d, %d)." % (fm.field[0], fm.field[1]) )

        self.field_markers.append( fld_mark )

        return fld_mark

    def replace_field_marker( self, field_i, field_j, corner=None, mark_type=MarkType( MarkType.Legal ) ):

        fld_mark = self.new_field_marker( field_i, field_j, corner=corner, mark_type=mark_type )

        for i, fm in enumerate( self.field_markers ):
            if fm.same_position( fld_mark ):
                self.field_markers[ i ] = fld_mark

        return fld_mark
