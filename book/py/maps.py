#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class Map(dict):
    def __getattr__(self, name):
        return self[name]

    def __setattr__(self, name, value):
        self[name] = value

    def __delattr__(self, name):
        del self[name]

    def as_tuple(self):
        return tuple(self.itervalues())


if __name__ == '__main__':
    print
    # m = Map({'foo' : 11, 'bar' : 42, 'baz' : 77})
    m = Map(foo=11, bar=42, baz=77)
    print m

    print
    m.new = 33
    print m

    print
    m.foo = 99
    print m

    print
    del m.bar
    print m

    print
