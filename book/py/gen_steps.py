#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

# import inspect as I

from util import xor


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

DEFAULT_NEIGHBOURING_REL_MOVES = [ ( 1,  0), \
                                   ( 0,  1), \
                                   (-1,  0), \
                                   ( 0, -1)  ]

DEFAULT_KING_REL_MOVES = [ ( 1,  0), \
                           ( 1,  1), \

                           ( 0,  1), \
                           (-1,  1), \

                           (-1,  0), \
                           (-1, -1), \

                           ( 0, -1), \
                           ( 1, -1)  ]


def add(step, rel):
    if step is None or rel is None:
        return None
    return ( step[0] + rel[0], step[1] + rel[1] )

def remove(coords, to_remove=[]):
    return list( set(coords) - set(to_remove) )

def negate(coords):
    t = type(coords)
    c = [ (-x) for x in coords ]
    return t(c)

def gen_next(gen, default=None):
    g = gen()

    def _gen_next():
        return next(g, default) # g.next() raises StopIteration

    return _gen_next

def check_valid(bounds=None, func=None):
    # bounds :: ((i_min, j_min), (i_max, j_max)) # ((0, 0), (25, 25))
    #
    # func :: pos --> bool
    #         pos :: (i ,j)

    def _check_valid(pos):
        # pos :: (int, int) # (i, j)

        if pos is not None:
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

def gen_rels(rels, inf_seq=True):
    # rels :: generator
    #      ||  [ (i, j), ... ]
    #
    # inf_seq :: bool

    def _gen_rels():
        if callable(rels):
            # rels :: generator
            for _rel in rels():
                yield _rel
        else:
            _repeat = True
            # rels :: [ (i, j), ... ]
            while _repeat:
                for rel in rels:
                    yield rel
                _repeat = inf_seq

    return _gen_rels

def gen_steps(rels, start=None, end=None, include_prev=False, bounds=None, func_valid=None):
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

        _rels = rels if callable(rels) else gen_rels(rels)
        _valid = check_valid(bounds=bounds, func=func_valid)

        for _rel in _rels():
            if _reverse:
                _rel = negate(_rel)
            _prev = _next = add(_current, _rel)

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

def gen_multi_steps(multi_rels, start=None, end=None, include_prev=False, bounds=None, func_valid=None):
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
            _steps = gen_steps(rels, start=start, end=end, include_prev=include_prev, bounds=bounds, func_valid=func_valid)

            for _step in _steps():
                if _step is None:
                    break

                yield _step

    return _gen_multi_steps


#
# tests

def test_1(as_next=True):
    # rels = [(3, 1), ]
    rels = DEFAULT_KNIGHT_REL_MOVES
    # rels = DEFAULT_UNICORN_REL_LONG_MOVES
    # rels = [(-2, 1), (3, 2)]
    ln = len(rels)

    g = gen_rels(rels, inf_seq=False)
    g = gen_rels(g)

    if as_next:
        g = gen_next(g, default='pero')

        print
        print "-" * 42
        print g
        # print
        for i in xrange(60):
            if i % ln == 0:
                print
            print i, g()
        print "-" * 42
        print
    else:
        print
        print "-" * 42
        print g
        # print
        for i, t in enumerate(g()):
            if i % ln == 0:
                print
            print i, t
            if i > 60:
                break
        print "-" * 42
        print

def test_2(as_next=True):
    rels = [(-2, 1), (3, 2)]
    ln = len(rels)
    start = (7, 9)
    bounds = ((0, 0), (20, 20))

    # g = gen_steps(rels, start=start, bounds=bounds)
    g = gen_steps(rels, start=start, include_prev=True, bounds=bounds)
    # g = gen_steps(rels, end=start, include_prev=True, bounds=bounds)

    g = gen_rels(g)

    if as_next:
        g = gen_next(g)

        print
        print "-" * 42
        print g
        print start
        for i in xrange(60):
            if i % ln == 0:
                print
            print i, g()
        print "-" * 42
        print
    else:
        print
        print "-" * 42
        print g
        # print
        for i, t in enumerate(g()):
            if i % ln == 0:
                print
            print i, t
            if i > 60:
                break
        print "-" * 42
        print

def test_3(as_next=True):
    start = (7, 9)

    rel1 = [(-2, -1), (3, 2)]
    # rel2 = [(-2, -1), (-3, -2), (-1, -1)]
    rel2 = [(2, 1), (1, 2), (-1, 2)]
    ln = len(rel1) + len(rel2)

    multi_rels = [ rel1, rel2 ]
    bounds = ((0, 0), (25, 25))

    # g = gen_multi_steps(multi_rels, start=start)
    g = gen_multi_steps(multi_rels, start=start, include_prev=True, bounds=bounds)
    # g = gen_multi_steps(multi_rels, end=start, include_prev=True)

    # g = gen_rels(g)

    if as_next:
        g = gen_next(g)

        print
        print "-" * 42
        print g
        print start
        for i in xrange(60):
            if i % ln == 0:
                print
            print i, g()
        print "-" * 42
        print
    else:
        print
        print "-" * 42
        print g
        # print
        for i, t in enumerate(g()):
            if i % ln == 0:
                print
            print i, t
            if i > 60:
                break
        print "-" * 42
        print

def test_4():
    start = (2, 2)
    bounds = ((0, 0), (4, 4))
    ln = len(DEFAULT_KNIGHT_REL_MOVES)

    multi_rels = [ DEFAULT_KNIGHT_REL_MOVES, ]

    g = gen_multi_steps(multi_rels, start=start, bounds=bounds)

    g = gen_rels(g)

    print
    print "-" * 42
    print g
    print start
    for i, pos in enumerate( g() ):
        if i % ln == 0:
            print
        print i, pos
    print "-" * 42
    print

if __name__ == '__main__':
    # test_1(as_next=True)
    # test_1(as_next=False)

    # test_2(as_next=True)
    # test_2(as_next=False)

    # test_3(as_next=True)
    # test_3(as_next=False)

    test_4()
