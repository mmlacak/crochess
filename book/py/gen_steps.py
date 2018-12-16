#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 - 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

# import inspect as I


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

def subtract_steps(steps_lst, subtract_lst=[]):
    return list( set(steps_lst) - set(subtract_lst) )

def gen_next(gen, default=None):
    g = gen()

    def _gen_next():
        return next(g, default) # return g.next()

    return _gen_next

def gen_rels(rels, count=None, default=None):

    def _gen_rels():
        index = 0

        while count is None or index < count:
            if callable(rels):
                # rels == generator.next()
                yield rels()
            else:
                # rels == [ (i, j), ... ]
                for rel in rels:
                    yield rel
            index += 1

    return gen_next(_gen_rels, default=default)

def gen_steps(start, rels, count=None, default=None):

    def _gen_steps():
        _current = start
        _rels = rels if callable(rels) else gen_rels(rels, default=default)

        index = 0
        while count is None or index < count:
            _rel = _rels()
            _next = add(_current, _rel)

            yield _next

            _current = _next
            index += 1

    return gen_next(_gen_steps, default=default)

def gen_multi_steps(start, multi_rels, count=None, default=None):
    # multi_rels == [ ( [ (i, j), ... ], count ), ... ]

    def _gen_multi_steps():
        for rels, _count in multi_rels:
            _rels = gen_steps(start, rels, count=_count, default=default)

            while True:
                _rel = _rels()

                if _rel is None:
                    break

                yield _rel

    return gen_next(_gen_multi_steps, default=default)



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

    g = gen_steps(start, rels)

    # f = gen_rels(rels)
    # g = gen_steps(start, f)

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

    g = gen_multi_steps(start, multi_rels)

    # e = gen_rels(rel1)
    # f = gen_rels(rel2)
    # mr = [(e, 5), (f, 9)]
    #
    # g = gen_multi_steps(start, mr)

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
