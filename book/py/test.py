#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

# import board
from piece import PieceType
from board import BoardType

#class A(object):
    #b = 0
    #c = 1
    #d = 2
    #e = 3
#
#a = A.c
#
#print "a", a
#
#b = A.d
#
#print "a+b", a, b, a+b


#pt = board.PieceType.Bishop
#
#print pt
#print board.PieceType.get_symbol(pt)
#print board.PieceType.get_name(pt)

pt = PieceType(PieceType.Bishop)

print pt
print pt.get_symbol()
print pt.get_name()

bs = BoardType(BoardType.AgeOfAquarius)

print bs
print bs.get_name()
print bs.get_size()
