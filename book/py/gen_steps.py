#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import inspect as I


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


def add(pos1, pos2):
    return ( pos1[0] + pos2[0], pos1[1] + pos2[1] )

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

def get_gen_steps_prev(start=(0, 0), rel=(1, 1)):
    def gen_steps():
        prev = current = start
        while True:
            current = add(current, rel)
            yield prev + current # (i, j) + (k, l) --> (i, j, k, l)
            prev = current

    return gen_steps

def get_func_is_valid(pos_bounds=((0, 25), (0, 25))):
    def is_valid(pos=(-1, -1)):
        return (pos_bounds[0][0] <= pos[0] <= pos_bounds[0][1]) and \
               (pos_bounds[1][0] <= pos[1] <= pos_bounds[1][1])

    return is_valid

def get_func_multi_steps(start=(0, 0), rel_lst=[], pos_bounds=((0, 25), (0, 25)), gen_steps=None):
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


if __name__ == '__main__':
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


    gms = get_func_multi_steps(start=(2, 3), rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((-5, 7), (-3, 9)))

    print
    print gms
    print gms()
    print

    for p in gms():
        print p

    print


    gmps = get_func_multi_steps(start=(2, 3), rel_lst=DEFAULT_KNIGHT_REL_MOVES, pos_bounds=((-5, 7), (-3, 9)), gen_steps=get_gen_steps_prev)

    print
    print gmps
    print gmps()
    print

    for p in gmps():
        print p

    print
