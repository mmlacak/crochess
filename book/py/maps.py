#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


class Map( dict ):
    def __getattr__( self, name ):
        return self[name]

    def __setattr__( self, name, value ):
        self[name] = value

    def __delattr__( self, name ):
        del self[name]

#    def as_tuple( self ):
#        return tuple( self.itervalues() )

#    @staticmethod
#    def from_tuple( tpl ):
#        raise NotImplementedError( "from_tuple" )


if __name__ == '__main__':
    print
    # m = Map( {'foo' : 11, 'bar' : 42, 'baz' : 77} )
    m = Map( foo=11, bar=42, baz=77 )
    print( m )

    print
    m.new = 33
    print( m )

    print
    m.foo = 99
    print( m )

    print
    del m.bar
    print( m )

    print
