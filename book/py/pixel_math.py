#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2016, 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import math

""" straight_line :: (float, float) # (a, b) --> y = a * x + b
    point :: (float, float) # (x, y)
"""

def is_any(itr, type_=float):
    return any( [ isinstance(x, type_) for x in itr ] )

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
    n0 = calc_rounded_str_value(num0, digits=digits)
    n1 = calc_rounded_str_value(num1, digits=digits)
    return n0 == n1


# TODO :: cairo coords are all float!
def translate_scale(x_pct, y_pct, scale=1.0, center_x=0.5, center_y=0.5):
    if scale == 1.0:
        # 1.0 == 1.0 + 1e-16 --> True
        return (x_pct, y_pct)
    else:
        # 1.0 == 1.0 + 1e-15 --> False
        x = scale * (x_pct - center_x) + center_x
        y = scale * (y_pct - center_y) + center_y
        return (x, y)

# TODO :: cairo coords are all float!
def calc_default_line_width(size_pix):
    # return math.ceil( 1 + ( 6 * size_pix / 5000 ) )
    return 0.04


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
    # Returns points in positive, i.e. anti-clockwise order.

    assert distance > 0.0

    x0, y0 = floatify_iterable(point)
    x1, y1 = floatify_iterable(other)

    if q_same_rounded_floats(x1, x0):
        x0_left = x0 - distance
        x0_right = x0 + distance
        if y1 > y0:
            return [(x0_left, y0), (x0_right, y0)]
        else:
            return [(x0_right, y0), (x0_left, y0)]
    elif q_same_rounded_floats(y1, y0):
        y0_up = y0 - distance
        y0_down = y0 + distance
        if x1 > x0:
            return [(x0, y0_up), (x0, y0_down)]
        else:
            return [(x0, y0_down), (x0, y0_up)]
    else:
        straight_line = calc_straight_line(point, other)
        inverse_line = calc_inverse_line(straight_line, point)

        a_inv, b_inv = inverse_line
        delta = math.atan(a_inv)
        # alpha = abs(delta)

        x_dist = distance * math.cos(delta) # alpha
        x_2 = x0 - x_dist
        x_3 = x0 + x_dist

        y_2 = a_inv * x_2 + b_inv
        y_3 = a_inv * x_3 + b_inv

        y_dist = distance * math.sin(delta) # alpha
        assert q_same_rounded_floats(y_2, y0 - y_dist) or q_same_rounded_floats(y_2, y0 + y_dist)
        assert q_same_rounded_floats(y_3, y0 - y_dist) or q_same_rounded_floats(y_3, y0 + y_dist)

        # https://stackoverflow.com/questions/6989100/sort-points-in-clockwise-order
        det = (x_2 - x0) * (y1 - y0) - (x1 - x0) * (y_2 - y0)

        if det < 0.0:
            return [(x_2, y_2), (x_3, y_3)]
        else:
            return [(x_3, y_3), (x_2, y_2)]

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


# TODO :: cairo coords are all float!
class RectanglePix:
    def __init__(self, x_pix, y_pix, width_pix, height_pix):
        assert isinstance(x_pix, int)
        assert isinstance(y_pix, int)
        assert isinstance(width_pix, int)
        assert isinstance(height_pix, int)

        self.x_pix = x_pix
        self.y_pix = y_pix
        self.width_pix = width_pix
        self.height_pix = height_pix

    def as_tuple(self):
        return (self.x_pix, self.y_pix, self.width_pix, self.height_pix)

    @staticmethod
    def from_tuple(tpl):
        return RectanglePix( *tpl[ 0 : 4 ] )

    def calc_point(self, x_pct, y_pct):
        x_pix = self.x_pix + x_pct * self.width_pix
        # y = self.top + (1.0 - y_pct) * self.height
        y_pix = self.y_pix + y_pct * self.height_pix
        return (int(x_pix), int(y_pix))

    def calc_size(self, width_pct, height_pct):
        width_pix  = width_pct * self.width_pix
        height_pix  = height_pct * self.height_pix
        return (int(width_pix), int(height_pix))


if __name__ == '__main__':
    print()
    print( floatify_iterable( (1, 2, 3) ) )
    print( floatify_iterable( [1, 2, 3] ) )
    print()

    print()
    print( q_same_rounded_floats(3.0, 3.0) )
    print( q_same_rounded_floats(3.0, -3.0) )
    print( q_same_rounded_floats(3.14, math.pi) )
    print( q_same_rounded_floats(3.1415926535, math.pi) )
    print( q_same_rounded_floats(3.1415, math.pi, digits=3) )
    print()

    start0 = (3, 2)
    end0 = (7, 5)
    dist0 = 2.0
    print( start0, end0, dist0, calc_distant_points_on_inverse_line(start0, end0, dist0) )
    print( end0, start0, dist0, calc_distant_points_on_inverse_line(end0, start0, dist0) )
    print()

    # start0 = (3, 2)
    # end0 = (7, 5)
    dist0 = 5.0
    print( start0, end0, dist0, calc_distant_points_on_inverse_line(start0, end0, dist0) )
    print( end0, start0, dist0, calc_distant_points_on_inverse_line(end0, start0, dist0) )
    print()

    start1 = (3.0, 2.0)
    end1 = (3.0, 5.0)
    dist1 = 1.42
    print( start1, end1, dist1, calc_distant_points_on_inverse_line(start1, end1, dist1) )
    print( end1, start1, dist1, calc_distant_points_on_inverse_line(end1, start1, dist1) )
    print()

    start2 = (3.0, 2.0)
    end2 = (7.0, 2.0)
    dist2 = 1.608
    print( start2, end2, dist2, calc_distant_points_on_inverse_line(start2, end2, dist2) )
    print( end2, start2, dist2, calc_distant_points_on_inverse_line(end2, start2, dist2) )
    print()

    start3 = (2, 1)
    end3 = (5, 7)
    dist3 = 3.5
    print( start3, end3, dist3, calc_division_point(start3, end3, dist3) )
    print( end3, start3, dist3, calc_division_point(end3, start3, dist3) )
    print()
