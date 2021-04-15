// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "piece_type.h"
#include "chip_type.h"
#include "board_type.h"
#include "chessboard.h"


bool is_field_light( int i, int j )
{
    return ( (i + j) % 2 == 0 );
}


Chessboard * brd_alloc_new( BoardType const bt )
{
    Chessboard * b = malloc( sizeof( Chessboard ) );
    if ( !b ) return NULL;

    brd_init(b, bt);

    return b;
}

bool brd_init( Chessboard * const restrict cb, BoardType const bt )
{
    if ( !cb ) return false;

    cb->type = bt;
    cb->size = bt_size( cb->type );

    return brd_clear( cb );
}

bool brd_clear( Chessboard * const restrict cb )
{
    if ( !cb ) return false;

    for ( int i = 0; i < BOARD_SIZE_MAXIMUM; ++i )
    {
        for ( int j = 0; i < BOARD_SIZE_MAXIMUM; ++i )
        {
            cb->board[ i ][ j ] = PT_None;
            cb->chips[ i ][ j ] = CT_None;
        }
    }

    return true;
}

bool brd_is_on_board( Chessboard const * const restrict cb, int i, int j )
{
    if ( !cb ) return false;
    return ( ( 0 <= i ) && ( i < cb->size ) && ( 0 <= j ) && ( j < cb->size ) );
}

PieceType brd_get_piece( Chessboard const * const restrict cb, int i, int j )
{
    if ( brd_is_on_board( cb, i, j ) )
    {
        return cb->board[ i ][ j ];
    }

    return PT_None;
}

ChipType brd_get_chip( Chessboard const * const restrict cb, int i, int j )
{
    if ( brd_is_on_board( cb, i, j ) )
    {
        return cb->chips[ i ][ j ];
    }

    return CT_None;
}

bool brd_set_piece_chip( Chessboard * const restrict cb, int i, int j, PieceType pt, ChipType ct )
{
    if ( !cb ) return false;

    if ( brd_is_on_board( cb, i, j ) )
    {
        cb->board[ i ][ j ] = pt;
        cb->chips[ i ][ j ] = ct;
    }

    return true;
}

bool brd_set_piece( Chessboard * const restrict cb, int i, int j, PieceType pt )
{
    return brd_set_piece_chip( cb, i, j, pt, CT_None );
}

static char * brd_get_divider_alloc( Chessboard const * const restrict cb )
{
    if ( !cb ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * divider = calloc( 1, len );
    if ( !divider ) return NULL;

    for ( int i = 0; i < len; ++i )
    {
        if ( i < 3 ) divider[ i ] = ' ';
        else if ( i < 3 + 2 * cb->size - 1 ) divider[ i ] = '-';
        else if ( i < len ) divider[ i ] = ' ';
    }

    divider[ len - 2 ] = '\n';
    divider[ len - 1 ] = '\0';

    return divider;
}

static char * brd_get_horizontal_ruler_alloc( Chessboard const * const restrict cb )
{
    if ( !cb ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * hr = calloc( 1, len );
    if ( !hr ) return NULL;

    char c = 'a';
    for ( int i = 0; i < len; ++i )
    {
        if ( i < 3 ) hr[ i ] = ' ';
        else if ( i < 3 + 2 * cb->size )
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

char * brd_as_string_alloc( Chessboard const * const restrict cb, bool is_board_or_chips )
{
    if ( !cb ) return NULL;

    char * s = calloc( 1, 2048 );
    if ( !s ) return NULL;

    char * horizontal_ruler = brd_get_horizontal_ruler_alloc( cb );
    if ( !horizontal_ruler )
    {
        free(s);
        return NULL;
    }

    strcat( s, horizontal_ruler );

    char * divider = brd_get_divider_alloc( cb );
    if ( !divider )
    {
        free(s);
        free(horizontal_ruler);
        return NULL;
    }

    strcat( s, divider );

    char * row = calloc(1, 6);
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

    for ( int i = 0; i < cb->size; ++i )
    {
        char r = (char)( cb->size - i );
        sprintf( row, "%2hhu|", r );
        strcat( s, row );

        for ( int j = 0; j < cb->size; ++j )
        {
            char c;

            if ( is_board_or_chips )
                c = pt_as_char( cb->board[ i ][ j ] );
            else
                c = ct_as_char( cb->chips[ i ][ j ] );

            if ( c == ' ' )
            {
                if ( is_field_light( i, j ) ) c = ',';
                else c = '.';
            }

            if ( j < cb->size - 1 )
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
