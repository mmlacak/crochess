#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010, .. 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

from rules import Rules

class Game(object):
    def __init__(self, rules):
        self.rules = rules

    # TODO :: this is where fusion with Ai and user responses comes
