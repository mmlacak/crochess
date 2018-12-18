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
        return next(g, default) # return g.next()

    return _gen_next

#def check_valid(valid=None):
#    # valid :: bounds | func | count
#    #
#    # bounds :: ((i_min, j_min), (i_max, j_max)) # ((0, 0), (25, 25))
#    #
#    # func :: index --> pos --> bool
#    #         index :: int
#    #         pos :: (i ,j)
#    #
#    # count :: int

#    def _check_valid(index, pos):
#        if valid is None:
#            return True

#        if pos is not None:
#            if isinstance(valid, tuple):
#                l = len(pos) - 2
#                i = pos[0 + l]
#                j = pos[1 + l]

#                i_min, j_min = valid[0]
#                i_max, j_max = valid[1]

#                if (i < i_min) or (i_max < i) or (j < j_min) or (j_max < j):
#                    return False
#            elif callable(valid):
#                if not valid(index, pos):
#                    return False

#        if isinstance(valid, int):
#            if index >= valid:
#                return False

#        return True

#    return _check_valid

def check_valid(count=None, bounds=None, func=None):
    # count :: int
    #
    # bounds :: ((i_min, j_min), (i_max, j_max)) # ((0, 0), (25, 25))
    #
    # func :: index --> pos --> bool
    #         index :: int
    #         pos :: (i ,j)

    def _check_valid(index, pos):
        # index :: int
        #
        # pos :: (int, int) # (i, j)

        if count is not None:
            if index < 0 or index >= count:
                return False

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
            if not func(index, pos):
                return False

        return True

    return _check_valid

def gen_rels(rels, count=None, default=None):

    def _gen_rels():
        index = 0

        while count is None or index < count:
            if callable(rels):
                # rels :: generator.next()
                yield rels()
            else:
                # rels :: [ (i, j), ... ]
                for rel in rels:
                    yield rel
            index += 1

    return gen_next(_gen_rels, default=default)

def gen_steps(rels, start=None, end=None, include_prev=False, count=None, default=None):

    assert xor(start is not None, end is not None)

    def _gen_steps():
        _reverse = start is None
        _current = start or end
        _rels = rels if callable(rels) else gen_rels(rels, default=default)

        index = 0
        while count is None or index < count:
            _rel = _rels()
            if _reverse:
                _rel = negate(_rel)
            _prev = _next = add(_current, _rel)

            if include_prev:
                if not _reverse:
                    yield _current + _next # (i, j) + (k, l) --> (i, j, k, l)
                else:
                    yield _prev + _current # (k, l) + (i, j) --> (k, l, i, j)
            else:
                yield _next

            _current = _next
            index += 1

    return gen_next(_gen_steps, default=default)

def gen_multi_steps(multi_rels, start=None, end=None, include_prev=False, default=None):
    # start :: (i, j)
    # end :: (i, j)
    #
    # multi_rels :: [ ( rels, count ), ... ]
    #
    # rels :: generator.next()
    # ... or ...
    # rels :: [ (i, j), ... ]

    def _gen_multi_steps():
        for rels, _count in multi_rels:
            _rels = gen_steps(rels, start=start, end=end, include_prev=include_prev, count=_count, default=default)

            while True:
                _rel = _rels()

                if _rel is None:
                    break

                yield _rel

    return gen_next(_gen_multi_steps, default=default)


#
# tests

def test_1():
    # rels = [(3, 1), ]
    # rels = DEFAULT_KNIGHT_REL_MOVES
    rels = DEFAULT_UNICORN_REL_LONG_MOVES
    # rels = [(-2, 1), (3, 2)]
    ln = len(rels)

    g = gen_rels(rels, count=2, default='pero')

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

def test_2():
    rels = [(-2, 1), (3, 2)]
    ln = len(rels)

    # g = gen_rels(rels, count=5)

    f = gen_rels(rels, count=3)
    g = gen_rels(f)

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

def test_3():
    rels = [(-2, 1), (3, 2)]
    ln = len(rels)
    start = (7, 4)

    # g = gen_steps(rels, start=start)
    g = gen_steps(rels, start=start, include_prev=True)
    # g = gen_steps(rels, end=start, include_prev=True)

    # f = gen_rels(rels)
    # g = gen_steps(f, start=start)

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

def test_4():
    start = (4, 7)
    rel1 = [(2, 1), (3, 2)]
    rel2 = [(-2, -1), (-3, -2), (-1, -1)]
    ln = len(rel1) + len(rel2)
    multi_rels = [(rel1, 7), (rel2, 11)]

    # g = gen_multi_steps(multi_rels, start=start)
    # g = gen_multi_steps(multi_rels, start=start, include_prev=True)
    g = gen_multi_steps(multi_rels, end=start, include_prev=True)

    # e = gen_rels(rel1)
    # f = gen_rels(rel2)
    # mr = [(e, 5), (f, 9)]
    #
    # g = gen_multi_steps(mr, start=start)

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


if __name__ == '__main__':
    # test_1()
    # test_2()
    # test_3()
    test_4()
