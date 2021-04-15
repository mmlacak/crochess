// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "piece_type.h"
#include "chip_type.h"
#include "board_type.h"
#include "board.h"


// char const BOARD_DISPLAY_DIVIDER[] = "-----------------------------------------------------";


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

static char * brd_get_divider_alloc( Board const * const restrict b )
{
    if ( !b ) return NULL;

    size_t len = 3 + 2 * b->size + 3 + 1;
    char * divider = calloc( 1, len );
    if ( !divider ) return NULL;

    for ( int i = 0; i < len; ++i )
    {
        if ( i < 3 ) divider[ i ] = ' ';
        else if ( i < 3 + 2 * b->size - 1 ) divider[ i ] = '-';
        else if ( i < len ) divider[ i ] = ' ';
    }

    divider[ len - 2 ] = '\n';
    divider[ len - 1 ] = '\0';

    return divider;
}

static char * brd_get_horizontal_ruler_alloc( Board const * const restrict b )
{
    if ( !b ) return NULL;

    size_t len = 3 + 2 * b->size + 3 + 1;
    char * hr = calloc( 1, len );
    if ( !hr ) return NULL;

    char c = 'a';
    for ( int i = 0; i < len; ++i )
    {
        if ( i < 3 ) hr[ i ] = ' ';
        else if ( i < 3 + 2 * b->size )
        {
            if ( i % 2 == 0 ) hr[ i ] = ' ';
            else
            {
                hr[ i ] = c;
                ++c;
            }
        }
        else if ( i < len ) hr[ i ] = ' ';
    }

    hr[ len - 2 ] = '\n';
    hr[ len - 1 ] = '\0';

    return hr;
}

char * brd_as_string_alloc( Board const * const restrict b, bool is_board_or_chips )
{
    if ( !b ) return NULL;

    char * s = calloc( 1, 2048 );
    if ( !s ) return NULL;

    char * horizontal_ruler = brd_get_horizontal_ruler_alloc(b);
    if ( !horizontal_ruler )
    {
        free(s);
        return NULL;
    }

    strcat( s, horizontal_ruler );

    char * divider = brd_get_divider_alloc(b);
    if ( !divider )
    {
        free(s);
        free(horizontal_ruler);
        return NULL;
    }

    strcat( s, divider );

    char * row = calloc(1, 4);
    if ( !row )
    {
        free(s);
        free(horizontal_ruler);
        free(divider);
        return NULL;
    }

    char * field = calloc(1, 3);
    if ( !field )
    {
        free(s);
        free(horizontal_ruler);
        free(divider);
        free(row);
        return NULL;
    }

    for ( int i = 0; i < b->size; ++i )
    {
        char r = b->size - i;
        sprintf( row, "%2hhu|", r );
        strcat( s, row );

        for ( int j = 0; j < b->size; ++j )
        {
            char c;

            if ( is_board_or_chips )
                c = pt_as_char( b->board[ i ][ j ] );
            else
                c = ct_as_char( b->chips[ i ][ j ] );

            if ( c == ' ' )
            {
                if ( is_field_light( i, j ) ) c = ',';
                else c = '.';
            }

            if ( j < b->size - 1 )
                sprintf( field, "%c ", c );
            else
                sprintf( field, "%c", c );

            strcat( s, field );
        }

        sprintf( row, "|%2hhu\n", r );
        strcat( s, row );
    }

    strcat( s, divider );
    strcat( s, horizontal_ruler );

    free(horizontal_ruler);
    free(divider);
    free(row);
    free(field);

    return s;
}
