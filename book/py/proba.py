#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from piece import PieceType
from board import BoardType
from board import Board

b = Board(BoardType(BoardType.Nineteen))
off_bt = BoardType.Classical if b.type.is_even_or_odd() else BoardType.OddClassical
i = 9
j = 1

off_w = (b.get_width() - BoardType(off_bt).get_size()) / 2
off_h = (b.get_height() - BoardType(off_bt).get_size()) / 2
print "off:", off_bt, off_w,  off_h

min_i = min(i, b.get_width() - 1 - i)
min_j = min(j, b.get_height() - 1 - j)
print "min:", min_i, min_j

pos_w = off_w - min_i
pos_h = off_h - min_j
print "pos:", pos_w,  pos_h

pos_w = pos_w if pos_w > 0 else 0
pos_h = pos_h if pos_h > 0 else 0
print "pos:", pos_w,  pos_h

bt_w = BoardType(off_bt + 2 * pos_w)
bt_h = BoardType(off_bt + 2 * pos_h)
bt = max(bt_w, bt_h)
print "bt:", off_bt, bt_w, bt_h, bt
print "bt:", BoardType(off_bt).get_name(), bt_w.get_name(), bt_h.get_name(), bt.get_name()
