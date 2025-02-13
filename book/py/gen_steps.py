#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import gen_next, iterate, xor

from corner import Corner


def convert_single_step_into_multi_rels(rels):
    return [ [ tpl ] for tpl in rels ]


DEFAULT_MONOLITH_REL_LEFT_MOVES = [ ( 2,  1),  \
                                    (-1,  2),  \
                                    (-2, -1),  \
                                    ( 1, -2)   ]
DEFAULT_MONOLITH_MULTI_REL_LEFT_MOVES = convert_single_step_into_multi_rels(DEFAULT_MONOLITH_REL_LEFT_MOVES)

DEFAULT_MONOLITH_REL_RIGHT_MOVES = [ ( 1,  2),  \
                                     (-2,  1),  \
                                     (-1, -2),  \
                                     ( 2, -1)   ]
DEFAULT_MONOLITH_MULTI_REL_RIGHT_MOVES = convert_single_step_into_multi_rels(DEFAULT_MONOLITH_REL_RIGHT_MOVES)

DEFAULT_KNIGHT_REL_MOVES = [ ( 2,  1),  \
                             ( 1,  2),  \
                                        \
                             (-1,  2),  \
                             (-2,  1),  \
                                        \
                             (-2, -1),  \
                             (-1, -2),  \
                                        \
                             ( 1, -2),  \
                             ( 2, -1)   ]
DEFAULT_KNIGHT_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_KNIGHT_REL_MOVES)

DEFAULT_CENTAUR_SHORT_I_III_REL_MOVES =   [ ( 2,  1),  \
                                            ( 1,  2),  \
                                                       \
                                            (-2, -1),  \
                                            (-1, -2),  ]
DEFAULT_CENTAUR_SHORT_I_III_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_CENTAUR_SHORT_I_III_REL_MOVES)

DEFAULT_CENTAUR_SHORT_II_IV_REL_MOVES =   [ (-1,  2),  \
                                            (-2,  1),  \
                                                       \
                                            ( 1, -2),  \
                                            ( 2, -1)   ]
DEFAULT_CENTAUR_SHORT_II_IV_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_CENTAUR_SHORT_II_IV_REL_MOVES)

DEFAULT_CENTAUR_SHORT_REL_MOVES = DEFAULT_CENTAUR_SHORT_I_III_REL_MOVES \
                                + DEFAULT_CENTAUR_SHORT_II_IV_REL_MOVES
DEFAULT_CENTAUR_SHORT_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_CENTAUR_SHORT_REL_MOVES)

DEFAULT_UNICORN_REL_LONG_MOVES = [ ( 4,  1),    \
                                   ( 3,  2),    \
                                   ( 2,  3),    \
                                   ( 1,  4),    \
                                                \
                                   (-1,  4),    \
                                   (-2,  3),    \
                                   (-3,  2),    \
                                   (-4,  1),    \
                                                \
                                   (-4, -1),    \
                                   (-3, -2),    \
                                   (-2, -3),    \
                                   (-1, -4),    \
                                                \
                                   ( 1, -4),    \
                                   ( 2, -3),    \
                                   ( 3, -2),    \
                                   ( 4, -1)     ]
DEFAULT_UNICORN_MULTI_REL_LONG_MOVES = convert_single_step_into_multi_rels(DEFAULT_UNICORN_REL_LONG_MOVES)

DEFAULT_CENTAUR_LONG_I_III_REL_MOVES =    [ ( 4,  1),    \
                                            ( 3,  2),    \
                                            ( 2,  3),    \
                                            ( 1,  4),    \
                                                         \
                                            (-4, -1),    \
                                            (-3, -2),    \
                                            (-2, -3),    \
                                            (-1, -4),    ]
DEFAULT_CENTAUR_LONG_I_III_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_CENTAUR_LONG_I_III_REL_MOVES)

DEFAULT_CENTAUR_LONG_II_IV_REL_MOVES =    [ (-1,  4),    \
                                            (-2,  3),    \
                                            (-3,  2),    \
                                            (-4,  1),    \
                                                         \
                                            ( 1, -4),    \
                                            ( 2, -3),    \
                                            ( 3, -2),    \
                                            ( 4, -1)     ]
DEFAULT_CENTAUR_LONG_II_IV_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_CENTAUR_LONG_II_IV_REL_MOVES)

DEFAULT_CENTAUR_LONG_REL_MOVES  = DEFAULT_CENTAUR_LONG_I_III_REL_MOVES \
                                + DEFAULT_CENTAUR_LONG_II_IV_REL_MOVES
DEFAULT_CENTAUR_LONG_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_CENTAUR_LONG_REL_MOVES)

DEFAULT_ROOK_REL_MOVES = [  ( 1,  0), \
                            ( 0,  1), \
                            (-1,  0), \
                            ( 0, -1)  ]
DEFAULT_ROOK_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_ROOK_REL_MOVES)

DEFAULT_KING_REL_MOVES = [ ( 1,  0), \
                           ( 1,  1), \

                           ( 0,  1), \
                           (-1,  1), \

                           (-1,  0), \
                           (-1, -1), \

                           ( 0, -1), \
                           ( 1, -1)  ]
DEFAULT_KING_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_KING_REL_MOVES)

DEFAULT_BISHOP_REL_MOVES = [ ( 1,  1), \
                             (-1,  1), \
                             (-1, -1), \
                             ( 1, -1)  ]
DEFAULT_BISHOP_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_BISHOP_REL_MOVES)


# left turning --> spiraling right
LIGHT_SHAMAN_REL_MOVES = [ ( 2,  1), \
                           (-2, -1), ]
LIGHT_SHAMAN_MULTI_REL_MOVES = convert_single_step_into_multi_rels(LIGHT_SHAMAN_REL_MOVES)

LIGHT_SHAMAN_REL_LEGS_LEFT =  [ (-1,  0), \
                                (-1, -1), \

                                ( 0, -1), \
                                ( 1, -1), \

                                ( 1,  0), \
                                ( 1,  1), \

                                ( 0,  1), \
                                (-1,  1), ]

LIGHT_SHAMAN_REL_LEGS_RIGHT = [ ( 1,  0), \
                                ( 1,  1), \

                                ( 0,  1), \
                                (-1,  1), \

                                (-1,  0), \
                                (-1, -1), \

                                ( 0, -1), \
                                ( 1, -1)  ]

LIGHT_SHAMAN_CORNER_LEFT =    [ Corner.UpperLeft,  \
                                Corner.LowerLeft,  \
                                Corner.LowerRight, \
                                Corner.UpperRight  ]

LIGHT_SHAMAN_CORNER_RIGHT =   [ Corner.LowerRight, \
                                Corner.UpperRight, \
                                Corner.UpperLeft , \
                                Corner.LowerLeft   ]

LIGHT_SHAMAN_CORNER_LEFT_FIELD_MARKER =   [ Corner.UpperLeftFieldMarker,  \
                                            Corner.LowerLeftFieldMarker,  \
                                            Corner.LowerRightFieldMarker, \
                                            Corner.UpperRightFieldMarker  ]

LIGHT_SHAMAN_CORNER_RIGHT_FIELD_MARKER =  [ Corner.LowerRightFieldMarker, \
                                            Corner.UpperRightFieldMarker, \
                                            Corner.UpperLeftFieldMarker,  \
                                            Corner.LowerLeftFieldMarker   ]


# right turning --> spiraling left
DARK_SHAMAN_REL_MOVES = [ ( 1,  2), \
                          (-1, -2), ]
DARK_SHAMAN_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DARK_SHAMAN_REL_MOVES)

DARK_SHAMAN_REL_LEGS_UP = [ ( 0,  1), \
                            ( 1,  1), \

                            ( 1,  0), \
                            ( 1, -1), \

                            ( 0, -1), \
                            (-1, -1), \

                            (-1,  0), \
                            (-1,  1), ]

DARK_SHAMAN_REL_LEGS_DOWN = [ ( 0, -1), \
                              (-1, -1), \

                              (-1,  0), \
                              (-1,  1), \

                              ( 0,  1), \
                              ( 1,  1), \

                              ( 1,  0), \
                              ( 1, -1), ]

DARK_SHAMAN_CORNER_UP =   [ Corner.UpperLeft,  \
                            Corner.UpperRight, \
                            Corner.LowerRight, \
                            Corner.LowerLeft   ]

DARK_SHAMAN_CORNER_DOWN = [ Corner.LowerRight, \
                            Corner.LowerLeft,  \
                            Corner.UpperLeft , \
                            Corner.UpperRight  ]

DARK_SHAMAN_CORNER_UP_FIELD_MARKER =  [ Corner.UpperLeftFieldMarker,  \
                                        Corner.UpperRightFieldMarker, \
                                        Corner.LowerRightFieldMarker, \
                                        Corner.LowerLeftFieldMarker   ]

DARK_SHAMAN_CORNER_DOWN_FIELD_MARKER =    [ Corner.LowerRightFieldMarker, \
                                            Corner.LowerLeftFieldMarker,  \
                                            Corner.UpperLeftFieldMarker,  \
                                            Corner.UpperRightFieldMarker  ]


DEFAULT_DISPLACEMENT_REL_MOVES =  [ (  3,   1),  \
                                    (  2,   2),  \
                                    (  1,   3),  \
                                                 \
                                    (  4,   2),  \
                                    (  3,   3),  \
                                    (  2,   4),  \
                                                 \
                                    (  6,   2),  \
                                    (  5,   3),  \
                                    (  4,   4),  \
                                    (  3,   5),  \
                                    (  2,   6),  \
                                                 \
                                                 \
                                    ( -1,   3),  \
                                    ( -2,   2),  \
                                    ( -3,   1),  \
                                                 \
                                    ( -2,   4),  \
                                    ( -3,   3),  \
                                    ( -4,   2),  \
                                                 \
                                    ( -2,   6),  \
                                    ( -3,   5),  \
                                    ( -4,   4),  \
                                    ( -5,   3),  \
                                    ( -6,   2),  \
                                                 \
                                                 \
                                    ( -3,  -1),  \
                                    ( -2,  -2),  \
                                    ( -1,  -3),  \
                                                 \
                                    ( -4,  -2),  \
                                    ( -3,  -3),  \
                                    ( -2,  -4),  \
                                                 \
                                    ( -6,  -2),  \
                                    ( -5,  -3),  \
                                    ( -4,  -4),  \
                                    ( -3,  -5),  \
                                    ( -2,  -6),  \
                                                 \
                                                 \
                                    (  1,  -3),  \
                                    (  2,  -2),  \
                                    (  3,  -1),  \
                                                 \
                                    (  2,  -4),  \
                                    (  3,  -3),  \
                                    (  4,  -2),  \
                                                 \
                                    (  2,  -6),  \
                                    (  3,  -5),  \
                                    (  4,  -4),  \
                                    (  5,  -3),  \
                                    (  6,  -2)   ]
DEFAULT_DISPLACEMENT_MULTI_REL_MOVES = convert_single_step_into_multi_rels(DEFAULT_DISPLACEMENT_REL_MOVES)


def separate_poss(arrow):
    x0, y0, x1, y1 = arrow
    return (x0, y0), (x1, y1)

def combine_poss(start, end):
    # x0, y0 = start
    # x1, y1 = end
    # return (x0, y0, x1, y1)
    return start + end


def add_rel(pos, rel_x, rel_y):
    if pos is None or rel_x is None or rel_y is None:
        return None
    return ( pos[0] + rel_x, pos[1] + rel_y )

def sub_rel(pos, rel_x, rel_y):
    if pos is None or rel_x is None or rel_y is None:
        return None
    return ( pos[0] - rel_x, pos[1] - rel_y )

def append_pos(pos, x, y):
    if pos is None or x is None or y is None:
        return None
    return ( pos[0], pos[1], x, y )

def append_pos_rel(pos, rel_x, rel_y):
    if pos is None or rel_x is None or rel_y is None:
        return None
    return ( pos[0], pos[1], pos[0] + rel_x, pos[1] + rel_y )

def prepend_pos_rel(rel_x, rel_y, pos):
    if pos is None or rel_x is None or rel_y is None:
        return None
    return ( pos[0] - rel_x, pos[1] - rel_y, pos[0], pos[1] )

def add_step(pos, step):
    if pos is None or step is None:
        return None
    return ( pos[0] + step[0], pos[1] + step[1] )

def sub_step(pos, step):
    if pos is None or step is None:
        return None
    return ( pos[0] - step[0], pos[1] - step[1] )

def get_start(arrow):
    return (arrow[0], arrow[1])

def get_end(arrow):
    return (arrow[2], arrow[3])

def add_end_rel(arrow, rel_x, rel_y):
    x0, y0, x1, y1 = arrow
    return (x1, y1, x1 + rel_x, y1 + rel_y)

def sub_end_rel(arrow, rel_x, rel_y):
    x0, y0, x1, y1 = arrow
    return (x1, y1, x1 - rel_x, y1 - rel_y)

def add_arrow_rel(arrow, rel_x, rel_y):
    x0, y0, x1, y1 = arrow
    return (x0 + rel_x, y0 + rel_y, x1 + rel_x, y1 + rel_y)


def adder(pos, include_prev=False):
    _current = pos
    _next = pos

    def _adder(rel_i, rel_j, do_advance=True):
        nonlocal _current, _next
        _current = _next
        _n = add_step(_current, (rel_i, rel_j))
        if do_advance:
            _next = _n

        if include_prev:
            return _current + _n # (i, j) + (k, l) --> (i, j, k, l)
        else:
            return _n
    return _adder


def remove(coords, to_remove=[]):
    return [ pos for pos in coords if pos not in to_remove ] # preserves order

def linear_all(coords, factor, offset):
    t = type(coords)
    c = [ factor * x + offset for x in coords ]
    return t(c)

def multiply_all(coords, factor):
    t = type(coords)
    c = [ factor * x for x in coords ]
    return t(c)

def add_to_all(coords, offset):
    t = type(coords)
    c = [ x + offset for x in coords ]
    return t(c)

def negate(coords):
    t = type(coords)
    c = [ (-x) for x in coords ]
    return t(c)


def check_valid(bounds=None, func=None):
    # bounds :: ((i_min, j_min), (i_max, j_max)) # ((0, 0), (25, 25))
    #
    # func :: pos --> bool
    #         pos :: (i ,j)

    def _check_valid(pos):
        # pos :: (int, int) # (i, j)

        if pos is None:
            return None

        if bounds is not None:
            l = len(pos) - 2
            i = pos[0 + l]
            j = pos[1 + l]

            i_min, j_min = bounds[0]
            i_max, j_max = bounds[1]

            if (i < i_min) or (i_max < i) or (j < j_min) or (j_max < j):
                return False

        if func is not None:
            if not func(pos):
                return False

        return True

    return _check_valid

def gen_items(items, count=None):
    # items :: generator
    #       || [ a, b, c, ... ] # e.g. [ (i, j), ... ]
    #
    # count :: int
    #       || None

    def _gen_items():
        i = 0
        while count is None or i < count:
            if callable(items):
                for item in items():
                    if count is None or i < count:
                        yield item
                        i += 1
                    else:
                        return
            else:
                for item in items:
                    if count is None or i < count:
                        yield item
                        i += 1
                    else:
                        return

    return _gen_items

def gen_steps(rels, start=None, end=None, include_prev=False, include_start=False, bounds=None, func_valid=None, count=None):
    # rels :: generator
    #      ||  [ (i, j), ... ]
    #
    # bounds :: ((i_min, j_min), (i_max, j_max)) # ((0, 0), (25, 25))
    #
    # func_valid :: pos --> bool
    #               pos :: (i ,j)

    assert xor(start is not None, end is not None)

    def _gen_steps():
        _reverse = start is None
        _current = start or end

        if include_start and not include_prev:
            yield _current

        _rels = rels if callable(rels) else gen_items(rels, count=count)
        _valid = check_valid(bounds=bounds, func=func_valid)

        for _rel in _rels():
            if _reverse:
                _rel = negate(_rel)
            _prev = _next = add_step(_current, _rel)

            if not _valid(_next):
                break

            if include_prev:
                if not _reverse:
                    yield _current + _next # (i, j) + (k, l) --> (i, j, k, l)
                else:
                    yield _prev + _current # (k, l) + (i, j) --> (k, l, i, j)
            else:
                yield _next

            _current = _next

    return _gen_steps

def gen_multi_steps(multi_rels, start=None, end=None, include_prev=False, include_start=False, bounds=None, func_valid=None, count=None):
    # multi_rels :: [ rels, ... ]
    #
    # rels :: generator
    #      || [ (i, j), ... ]
    #
    # start :: (i, j)
    # end :: (i, j)
    #
    # bounds :: ((i_min, j_min), (i_max, j_max)) # ((0, 0), (25, 25))
    #
    # func_valid :: pos --> bool
    #               pos :: (i ,j)

    def _gen_multi_steps():
        for rels in multi_rels:
            _steps = gen_steps(rels, start=start, end=end, include_prev=include_prev, include_start=include_start, bounds=bounds, func_valid=func_valid, count=count)

            for _step in _steps():
                if _step is None:
                    break

                yield _step

    return _gen_multi_steps

#
# shaman generators

def gen_shaman_rel_legs(rel, count=None):
    # generate legs of relative steps

    start_horizontal = rel in LIGHT_SHAMAN_REL_MOVES
    start_vertical = rel in DARK_SHAMAN_REL_MOVES

    assert xor(start_horizontal, start_vertical)

    legs = None

    if start_horizontal:
        if rel[0] > 0:
            legs = LIGHT_SHAMAN_REL_LEGS_RIGHT
        else:
            legs = LIGHT_SHAMAN_REL_LEGS_LEFT
    else: # start_vertical
        if rel[1] > 0:
            legs = DARK_SHAMAN_REL_LEGS_UP
        else:
            legs = DARK_SHAMAN_REL_LEGS_DOWN

    def _gen_legs():
        length = 1
        i = 0
        loop = True

        while loop:
            # legs :: [ (i, j), ... ]
            for leg in legs:
                yield multiply_all(leg, length)

                i += 1
                if count is not None and count <= i:
                    loop = False
                    break

                if i % 2 == 0:
                    length += 1

    return _gen_legs

def gen_shaman_rels(rel, count=None):

    g = gen_next( gen_shaman_rel_legs(rel, count=None) )

    def _gen_shaman_rels():
        i = 0
        while count is None or i < count:
            rel_1 = g()
            rel_2 = g()
            rel_new = add_step(rel_1, rel_2)

            if rel_new is None:
                break

            yield rel_new
            i += 1

    return _gen_shaman_rels

def gen_shaman_corners(rel, count=None, is_with_field_marker=True):

    start_horizontal = rel in LIGHT_SHAMAN_REL_MOVES
    start_vertical = rel in DARK_SHAMAN_REL_MOVES

    assert xor(start_horizontal, start_vertical)

    corners = None

    if start_horizontal:
        if rel[0] > 0:
            corners = LIGHT_SHAMAN_CORNER_RIGHT_FIELD_MARKER if is_with_field_marker else LIGHT_SHAMAN_CORNER_RIGHT
        else:
            corners = LIGHT_SHAMAN_CORNER_LEFT_FIELD_MARKER if is_with_field_marker else LIGHT_SHAMAN_CORNER_LEFT
    else: # start_vertical
        if rel[1] > 0:
            corners = DARK_SHAMAN_CORNER_UP_FIELD_MARKER if is_with_field_marker else DARK_SHAMAN_CORNER_UP
        else:
            corners = DARK_SHAMAN_CORNER_DOWN_FIELD_MARKER if is_with_field_marker else DARK_SHAMAN_CORNER_DOWN

    return gen_items(corners, count=count)

#
# monolith generators

def gen_monolith_default_steps(step_number):

    steps = []

    count = 2 * step_number

    #
    # 1st quadrant

    x = count
    y = 1

    for i in range( count ):
        field_1 = ( x, y )
        steps.append( field_1 )

        x -= 1
        y += 1

    #
    # 2nd quadrant

    x = -1
    y = count

    for i in range( count ):
        field_2 = ( x, y )
        steps.append( field_2 )

        x -= 1
        y -= 1

    #
    # 3rd quadrant

    x = -count
    y = -1

    for i in range( count ):
        field_3 = ( x, y )
        steps.append( field_3 )

        x += 1
        y -= 1

    #
    # 4th quadrant

    x = 1
    y = -count

    for i in range( count ):
        field_4 = ( x, y )
        steps.append( field_4 )

        x += 1
        y += 1

    return steps

#
# tests

def test_print(gen, length=8, as_next=True):
    if as_next:
        g = gen_next(gen) # , default='pero'

        print()
        print( "-" * 42 )
        print( g )
        # print()
        for i in range(61):
            if i % length == 0:
                print()
            print( i, g() )
        print( "-" * 42 )
        print()
    else:
        g = gen

        print()
        print( "-" * 42 )
        print( g )
        # print()
        for i, t in enumerate(g()):
            if i % length == 0:
                print()
            print( i, t )
            if i > 59:
                break
        print( "-" * 42 )
        print()

def test_1(as_next=True):
    # rels = [(3, 1), ]
    rels = DEFAULT_KNIGHT_REL_MOVES
    # rels = DEFAULT_UNICORN_REL_LONG_MOVES
    # rels = [(-2, 1), (3, 2)]
    ln = len(rels)

    g = gen_items(rels, count=3)
    g = gen_items(g)

    test_print(g, length=ln, as_next=as_next)

def test_2(as_next=True):
    rels = [(-2, 1), (3, 2)]
    ln = len(rels)
    start = (7, 9)
    bounds = ((0, 0), (20, 20))

    # g = gen_steps(rels, start=start, bounds=bounds)
    # g = gen_steps(rels, start=start, include_prev=True, bounds=bounds)
    g = gen_steps(rels, start=start, include_prev=True, count=2)
    # g = gen_steps(rels, end=start, include_prev=True, bounds=bounds)

    g = gen_items(g)

    test_print(g, length=ln, as_next=as_next)

def test_3(as_next=True):
    start = (7, 9)

    rel1 = [(-2, -1), (3, 2)]
    # rel2 = [(-2, -1), (-3, -2), (-1, -1)]
    rel2 = [(2, 1), (1, 2), (-1, 2)]
    # rel1 = [(2, 1), ]
    # rel2 = [(3, 2), (-1, 4), ]
    ln = len(rel1) + len(rel2)

    multi_rels = [ rel1, rel2 ]
    bounds = ((0, 0), (25, 25))

    # g = gen_multi_steps(multi_rels, start=start)
    # g = gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=bounds)
    g = gen_multi_steps(multi_rels, start=start, include_prev=True, count=2)
    # g = gen_multi_steps(multi_rels, end=start, include_prev=True)

    # g = gen_items(g)

    test_print(g, length=ln, as_next=as_next)

def test_4(as_next=False):
    start = (2, 2)
    bounds = ((0, 0), (4, 4))
    ln = len(DEFAULT_KNIGHT_REL_MOVES)

    multi_rels = [ [t] for t in DEFAULT_KNIGHT_REL_MOVES ]

    # g = gen_multi_steps(DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, bounds=bounds)
    # g = gen_multi_steps(DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, include_prev=True, count=3)
    g = gen_multi_steps(DEFAULT_KNIGHT_MULTI_REL_MOVES, end=start, include_prev=True, count=3)
    # g = gen_items(g)

    test_print(g, length=ln, as_next=as_next)

def test_5(as_next=True):
    rel = (2, 1)
    ln = 2

    g = gen_shaman_rel_legs(rel, count=21)

    test_print(g, length=ln, as_next=as_next)

def test_6(as_next=True):
    rel = (2, 1)
    ln = 8

    g = gen_shaman_rels(rel, count=21)

    test_print(g, length=ln, as_next=as_next)

def test_7(as_next=True):
    rel = (2, 1)
    ln = 8

    start = (7, 9)
    bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

    f = gen_shaman_rels(rel)

    g = gen_steps(f, start=start, include_prev=False, bounds=bounds)
    # g = gen_steps(f, start=start, bounds=bounds)
    # g = gen_steps(f, start=start, include_prev=True, bounds=bounds)
    # g = gen_steps(f, end=start, include_prev=True, bounds=bounds)

    test_print(g, length=ln, as_next=as_next)

def test_8(as_next=True):
    start = (7, 9)
    ln = 8

    rel1 = gen_shaman_rels(LIGHT_SHAMAN_REL_MOVES[0])
    rel2 = gen_shaman_rels(LIGHT_SHAMAN_REL_MOVES[1])

    multi_rels = [ rel1, rel2 ]
    bounds = ((-42, -42), (99, 99)) # ((0, 0), (25, 25))

    # g = gen_multi_steps(multi_rels, start=start)
    g = gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=bounds)
    # g = gen_multi_steps(multi_rels, start=start, include_prev=True, count=3)
    # g = gen_multi_steps(multi_rels, end=start, include_prev=True, bounds=bounds)

    g = gen_items(g)

    test_print(g, length=ln, as_next=as_next)

def test_monolith_gen():
    print()
    for i in range( 4 ):
        lst = gen_monolith_default_steps( i+1 )
        print( lst )
    print()


if __name__ == '__main__':
    # test_1(as_next=True)
    # test_1(as_next=False)

    # test_2(as_next=True)
    # test_2(as_next=False)

    # test_3(as_next=True)
    # test_3(as_next=False)

    # test_4(as_next=True)
    # test_4(as_next=False)

    # test_5(as_next=True)
    # test_5(as_next=False)

    # test_6(as_next=True)
    # test_6(as_next=False)

    test_7(as_next=True)
    test_7(as_next=False)

    # test_8(as_next=True)
    # test_8(as_next=False)

    # test_monolith_gen()
