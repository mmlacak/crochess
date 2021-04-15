// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>

#include "piece_type.h"
#include "chip_type.h"
#include "board_type.h"
#include "board.h"


bool is_field_light( int i, int j )
{
    return ( (i + j) % 2 == 0 );
}


Board * brd_alloc_new(BoardType const bt)
{
    Board * b = malloc( sizeof( Board ) );
    if ( !b ) return NULL;

    brd_init(b, bt);

    return b;
}

bool brd_init( Board * const restrict b, BoardType const bt )
{
    if ( !b ) return false;

    b->type = bt;
    b->size = bt_size( b->type );

    return brd_clear( b );
}

bool brd_clear(Board * const restrict b)
{
    if ( !b ) return false;

    for ( int i = 0; i < BOARD_SIZE_MAXIMUM; ++i )
    {
        for ( int j = 0; i < BOARD_SIZE_MAXIMUM; ++i )
        {
            b->board[ i ][ j ] = PT_None;
            b->chips[ i ][ j ] = CT_None;
        }
    }

    return true;
}

bool brd_is_on_board( Board const * const restrict b, int i, int j )
{
    if ( !b ) return false;
    return ( ( 0 <= i ) && ( i < b->size ) && ( 0 <= j ) && ( j < b->size ) );
}

PieceType brd_get_piece( Board const * const restrict b, int i, int j )
{
    if ( brd_is_on_board( b, i, j ) )
    {
        return b->board[ i ][ j ];
    }

    return PT_None;
}

ChipType brd_get_chip( Board const * const restrict b, int i, int j )
{
    if ( brd_is_on_board( b, i, j ) )
    {
        return b->chips[ i ][ j ];
    }

    return CT_None;
}

bool brd_set_piece_chip( Board * const restrict b, int i, int j, PieceType pt, ChipType ct )
{
    if ( !b ) return false;

    if ( brd_is_on_board( b, i, j ) )
    {
        b->board[ i ][ j ] = pt;
        b->chips[ i ][ j ] = ct;
    }

    return true;
}

bool brd_set_piece( Board * const restrict b, int i, int j, PieceType pt )
{
    return brd_set_piece_chip( b, i, j, pt, CT_None );
}
