#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

""" straight_line :: (float, float) # (a, b) --> y = a * x + b
    point :: (float, float) # (x, y)
"""

def round_float_to_int(pos):
    return int(round(pos))

def round_floats_to_int(itr):
    lst = [ int(round(itm)) for itm in itr ]
    cls = type(itr)
    return cls(lst)

def round_coords_to_int(coords):
    lst = [ round_floats_to_int(coord) for coord in coords ]
    cls = type(coords)
    return cls(lst)

def floatify_iterable(itr):
    lst = [ float(itm) for itm in itr ]
    cls = type(itr)
    return cls(lst)

def calc_rounded_str_value(num, digits=6):
    n = round(float(num), digits)
    return str(n)

def q_same_rounded_floats(num0, num1, digits=6):
    n0 = calc_rounded_str_value(num0, digits)
    n1 = calc_rounded_str_value(num1, digits)
    return n0 == n1

def calc_straight_line(start, end):
    x0, y0 = floatify_iterable(start)
    x1, y1 = floatify_iterable(end)

    if q_same_rounded_floats(x1, x0):
        return None

    a = (y1 - y0) / (x1 - x0)
    b = (-a) * x0 + y0
    return (a, b)

def calc_inverse_line(straight_line, point):
    a, b = floatify_iterable(straight_line)
    x, y = floatify_iterable(point)

    if q_same_rounded_floats(a, 0.0):
        return None

    a_inv = (-1) / a
    b_inv = y - a_inv * x
    return (a_inv, b_inv)

def calc_distant_points_on_inverse_line(point, other, distance):
    x0, y0 = floatify_iterable(point)
    x1, y1 = floatify_iterable(other)

    if q_same_rounded_floats(x1, x0):
        y0_up = y0 + distance
        y0_down = y0 - distance
        return [(x0, y0_down), (x0, y0_up)]
    elif q_same_rounded_floats(y1, y0):
        x0_left = x0 - distance
        x0_right = x0 + distance
        return [(x0_left, y0), (x0_right, y0)]
    else:
        straight_line = calc_straight_line(point, other)
        inverse_line = calc_inverse_line(straight_line, point)

        a_inv, b_inv = inverse_line
        delta = math.atan(a_inv)
        alpha = abs(delta)

        x_dist = distance * math.cos(alpha)
        x_2 = x0 - x_dist
        x_3 = x0 + x_dist

        y_2 = a_inv * x_2 + b_inv
        y_3 = a_inv * x_3 + b_inv

        y_dist = distance * math.sin(alpha)
        assert q_same_rounded_floats(y_2, y0 - y_dist) or q_same_rounded_floats(y_2, y0 + y_dist)
        assert q_same_rounded_floats(y_3, y0 - y_dist) or q_same_rounded_floats(y_3, y0 + y_dist)

        return [(x_2, y_2), (x_3, y_3)]

def calc_division_point(start, end, ratio):
    x0, y0 = floatify_iterable(start)
    x1, y1 = floatify_iterable(end)

    x = (x0 + ratio * x1) / (1.0 + ratio)
    y = (y0 + ratio * y1) / (1.0 + ratio)

    return (x, y)

def calc_line_length(point, other):
    x0, y0 = floatify_iterable(point)
    x1, y1 = floatify_iterable(other)

    sqr_dist = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)
    return math.sqrt(sqr_dist)

if __name__ == '__main__':
    print
    print floatify_iterable( (1, 2, 3) )
    print floatify_iterable( [1, 2, 3] )
    print

    print
    print q_same_rounded_floats(3.0, 3.0)
    print q_same_rounded_floats(3.0, -3.0)
    print q_same_rounded_floats(3.14, math.pi)
    print q_same_rounded_floats(3.1415926535, math.pi)
    print

    start0 = (3, 2)
    end0 = (7, 5)
    print start0, end0, calc_distant_points_on_inverse_line(start0, end0, 2.0)
    print

    start1 = (3.0, 2.0)
    end1 = (3.0, 5.0)
    print start1, end1, calc_distant_points_on_inverse_line(start1, end1, 1.42)
    print

    start2 = (3.0, 2.0)
    end2 = (7.0, 2.0)
    print start2, end2, calc_distant_points_on_inverse_line(start2, end2, 1.608)
    print

    start3 = (2, 1)
    end3 = (5, 7)
    print start3, end3, calc_division_point(start3, end3, 3.5)
    print
