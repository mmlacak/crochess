#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import inspect as I


def add_coords(pos, rel, factor=1):
    return (pos[0] + factor * rel[0], pos[1] + factor * rel[1])

def sub_coords(pos, rel, factor=1):
    return (pos[0] - factor * rel[0], pos[1] - factor * rel[1])

def is_coord_within(coord, limits=(0, 25)):
    return bool( limits[0] <= coord <= limits[1] )

def is_pos_within(pos, pos_limits=((0, 25), (0, 25))):
    return is_coord_within(pos[0], limits=pos_limits[0]) and \
           is_coord_within(pos[1], limits=pos_limits[1])

def gen_relative_moves(moves):
    for i in xrange(len(moves)):
        yield moves[i]

DEFAULT_KNIGHT_REL_MOVES = [ (2, 1), \
                             (1, 2), \
                             \
                             (-1, 2), \
                             (-2, 1), \
                             \
                             (-2, -1), \
                             (-1, -2), \
                             \
                             (1, -2), \
                             (2, -1) ]

def gen_knight_rel_moves(moves=DEFAULT_KNIGHT_REL_MOVES):
    return gen_relative_moves(moves)

DEFAULT_UNICORN_REL_LONG_MOVES = [ (4, 1), \
                                   (3, 2), \
                                   (2, 3), \
                                   (1, 4), \
                                   \
                                   (-1, 4), \
                                   (-2, 3), \
                                   (-3, 2), \
                                   (-4, 1), \
                                   \
                                   (-4, -1), \
                                   (-3, -2), \
                                   (-2, -3), \
                                   (-1, -4), \
                                   \
                                   (1, -4), \
                                   (2, -3), \
                                   (3, -2), \
                                   (4, -1) ]

def gen_unicorn_rel_long_moves(moves=DEFAULT_UNICORN_REL_LONG_MOVES):
    return gen_relative_moves(moves)

def get_gen_move(rel, pos=(0, 0)):
    def gen_move():
        yield add_coords(pos, rel, 1)

    return gen_move

def get_gen_moves(rel, pos=(0, 0)):
    def gen_moves():
        i = 1
        while True:
            yield add_coords(pos, rel, i)
            i += 1

    return gen_moves

def get_gen_abs_pos(gen_rel, start=(0, 0), pos_limits=((0, 25), (0, 25))):
    def gen_abs_pos():
        for rel in gen_rel if I.isgenerator(gen_rel) else gen_rel():
            gen_moves = get_gen_moves(rel, pos=start)

            for pos in gen_moves():
                if is_pos_within(pos, pos_limits=pos_limits):
                    yield pos
                else:
                    break

    return gen_abs_pos
