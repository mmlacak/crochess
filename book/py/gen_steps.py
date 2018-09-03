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
                                   ( 0, -1) ]

DEFAULT_KING_REL_MOVES = [ ( 1,  0), \
                           ( 1,  1), \

                           ( 0,  1), \
                           (-1,  1), \

                           (-1,  0), \
                           (-1, -1), \

                           ( 0, -1), \
                           ( 1, -1) ]



def add(pos1, pos2):
    return ( pos1[0] + pos2[0], pos1[1] + pos2[1] )

def subtract_steps(steps_lst, subtract_lst=[]):
    return list( set(steps_lst) - set(subtract_lst) )

def call_gen(gen):
    g = gen()

    def call_func():
        return g.next()

    return call_func

def get_gen_steps(start=(0, 0), rel=(1, 1)):
    def gen_steps():
        current = start
        while True:
            current = add(current, rel)
            yield current

    return gen_steps

def get_gen_steps_prev(start=None, rel=(1, 1), end=None):
    def gen_steps():
        prev = current = start or end
        reverse = start is None
        _rel = tuple( (-i) for i in rel ) if reverse else rel
        while True:
            current = add(current, _rel)
            if not reverse:
                yield prev + current # (i, j) + (k, l) --> (i, j, k, l)
            else:
                yield current + prev # (k, l) + (i, j)  --> (k, l, i, j)
            prev = current

    return gen_steps

def get_func_is_valid(pos_bounds=((0, 0), (25, 25))):
    def is_valid(pos=(-1, -1)):
        # pos can also be (i, j, k, l), in that case only destination is checked, i.e. (k, l)
        l = len(pos) - 2
        return (pos_bounds[0][0] <= pos[0 + l] <= pos_bounds[1][0]) and \
               (pos_bounds[0][1] <= pos[1 + l] <= pos_bounds[1][1])

    return is_valid

def get_gen_multi_steps(start=(0, 0), rel_lst=[], pos_bounds=((0, 0), (25, 25)), gen_steps=None):
    is_valid = get_func_is_valid(pos_bounds=pos_bounds)
    gen = get_gen_steps if gen_steps is None else gen_steps

    def gen_multi_steps():
        for rel in rel_lst:
            gs = gen(start=start, rel=rel)
            for pos in gs():
                if is_valid(pos):
                    yield pos
                else:
                    break

    return gen_multi_steps


def test_1():
    g = get_gen_steps( start=(3, 1), rel=(1, 5) )
    f = call_gen(g)

    print
    print g
    print g()
    print
    print f
    print
    print f()
    print f()
    print f()
    print f()
    print f()
    print f()
    print f()
    print f()
    print

def test_2():
    gp = get_gen_steps_prev( start=(3, 1), rel=(1, 5) )
    fp = call_gen(gp)

    print
    print gp
    print gp()
    print
    print fp
    print
    print fp()
    print fp()
    print fp()
    print fp()
    print fp()
    print fp()
    print fp()
    print fp()
    print

def test_3():
    gms = get_gen_multi_steps(start=(2, 3), rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((-5, -3), (7, 9)))

    print
    print gms
    print gms()
    print

    for p in gms():
        print p

    print

def test_4():
    gmps = get_gen_multi_steps(start=(2, 3), rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((-5, -3), (7, 9)), gen_steps=get_gen_steps_prev)

    print
    print gmps
    print gmps()
    print

    for p in gmps():
        print p

    print

def test_5():
    pos = (4, 5)
    rel_lst = subtract_steps(DEFAULT_KNIGHT_REL_MOVES, [(1, 2), (-1, -2)])
    pos_bounds = (add(pos, (-2, -2)), add(pos, (2, 2)))

    print
    print "rel_lst:", rel_lst
    print "pos_bounds:", pos_bounds
    print

    coords = get_gen_multi_steps(start=pos, rel_lst=rel_lst, pos_bounds=pos_bounds, gen_steps=get_gen_steps_prev)

    print
    print "coords"
    for c in coords():
        print c
    print

    dest = get_gen_multi_steps(start=pos, rel_lst=rel_lst, pos_bounds=pos_bounds)

    print
    print "dest"
    for d in dest():
        print d
    print

def test_6():
    start = (2, 2)
    pos_bounds=((0, 0), (4, 4))

    gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=pos_bounds)

    print
    print "get_position_limits:", pos_bounds
    for pos in gen_abs_pos():
        print "pos", pos
    print


if __name__ == '__main__':
    # test_1()

    # test_2()

    # test_3()

    # test_4()

    test_5()

    test_6()
