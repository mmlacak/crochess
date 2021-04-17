// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "piece_type.h"
#include "tag_type.h"
#include "board_type.h"
#include "setup_board.h"
#include "chessboard.h"


bool is_field_light( int i, int j )
{
    return ( (i + j) % 2 == 0 );
}


Chessboard * cb_alloc( BoardType const bt )
{
    Chessboard * b = malloc( sizeof( Chessboard ) );
    if ( !b ) return NULL;

    cb_init(b, bt);

    return b;
}

bool cb_init( Chessboard * const restrict cb, BoardType const bt )
{
    if ( !cb ) return false;

    cb->type = bt;
    cb->size = bt_size( cb->type );

    // return cb_clear( cb );
    return cb_setup( cb );
}

bool cb_clear( Chessboard * const restrict cb )
{
    if ( !cb ) return false;

    for ( int i = 0; i < BOARD_SIZE_MAXIMUM; ++i )
    {
        for ( int j = 0; j < BOARD_SIZE_MAXIMUM; ++j )
        {
            cb->board[ i ][ j ] = PT_None;
            cb->tags[ i ][ j ] = TT_None;
        }
    }

    return true;
}

bool cb_setup( Chessboard * const restrict cb )
{
    if ( !cb ) return false;

    if ( !cb_clear( cb ) ) return false;

    PieceType const * const su = get_setup_board( cb->type );
    // PieceType const * const * const su = (PieceType const * const * const)brd;

    for ( int i = 0; i < cb->size; ++i )
    {
        for ( int j = 0; j < cb->size; ++j )
        {
            cb->board[ i ][ j ] = su[ cb->size * i + j ]; // su[ i ][ j ];

// TODO
            // cb->tags[ i ][ j ] = TT_None;
        }
    }

    return true;
}

bool cb_is_on_board( Chessboard const * const restrict cb, int i, int j )
{
    if ( !cb ) return false;
    return ( ( 0 <= i ) && ( i < cb->size ) && ( 0 <= j ) && ( j < cb->size ) );
}

PieceType brd_get_piece( Chessboard const * const restrict cb, int i, int j )
{
    if ( cb_is_on_board( cb, i, j ) )
    {
        return cb->board[ i ][ j ];
    }

    return PT_None;
}

TagType brd_get_chip( Chessboard const * const restrict cb, int i, int j )
{
    if ( cb_is_on_board( cb, i, j ) )
    {
        return cb->tags[ i ][ j ];
    }

    return TT_None;
}

bool cb_set_piece_chip( Chessboard * const restrict cb, int i, int j, PieceType pt, TagType ct )
{
    if ( !cb ) return false;

    if ( cb_is_on_board( cb, i, j ) )
    {
        cb->board[ i ][ j ] = pt;
        cb->tags[ i ][ j ] = ct;
    }

    return true;
}

bool cb_set_piece( Chessboard * const restrict cb, int i, int j, PieceType pt )
{
    return cb_set_piece_chip( cb, i, j, pt, TT_None );
}

static char * cb_get_divider_alloc( Chessboard const * const restrict cb )
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

static char * cb_get_horizontal_ruler_alloc( Chessboard const * const restrict cb )
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

char * cb_as_string_alloc( Chessboard const * const restrict cb, bool is_board_or_chips )
{
    if ( !cb ) return NULL;

    char * s = calloc( 1, 2048 );
    if ( !s ) return NULL;

    char * horizontal_ruler = cb_get_horizontal_ruler_alloc( cb );
    if ( !horizontal_ruler )
    {
        free( s );
        return NULL;
    }

    strcat( s, horizontal_ruler );

    char * divider = cb_get_divider_alloc( cb );
    if ( !divider )
    {
        free( s );
        free( horizontal_ruler );
        return NULL;
    }

    strcat( s, divider );

    char * row = calloc(1, 6);
    if ( !row )
    {
        free( s );
        free( horizontal_ruler );
        free( divider );
        return NULL;
    }

    char * field = calloc(1, 3);
    if ( !field )
    {
        free( s );
        free( horizontal_ruler );
        free( divider );
        free( row );
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
                c = tt_as_char( cb->tags[ i ][ j ] );

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

    free( horizontal_ruler );
    free( divider );
    free( row );
    free( field );

    return s;
}
