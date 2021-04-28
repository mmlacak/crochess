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
#include "setup_tags.h"
#include "chessboard.h"


bool is_field_light( int i, int j )
{
    return ( (i + j) % 2 != 0 );
}


Chessboard * cb_new_alx( BoardType const bt, bool do_setup )
{
    Chessboard * b = malloc( sizeof( Chessboard ) );
    if ( !b ) return NULL;

    cb_init( b, bt, do_setup );

    return b;
}

bool cb_init( Chessboard * const restrict cb, BoardType const bt, bool do_setup )
{
    if ( !cb ) return false;

    cb->type = bt;
    cb->size = bt_size( cb->type );

    if ( do_setup ) return cb_setup( cb );
    else return cb_clear( cb );
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

    PieceType const * const su = get_board_setup( cb->type );
    if ( !su ) return false;

    TagType const * const tu = get_tags_setup( cb->type );
    if ( !tu ) return false;

    for ( int i = 0; i < (int)cb->size; ++i )
    {
        for ( int j = 0; j < (int)cb->size; ++j )
        {
            int x = j;
            int y = cb->size - i - 1;
            int z = cb->size * i + j;

            cb->board[ x ][ y ] = su[ z ];
            cb->tags[ x ][ y ] = tu[ z ];
        }
    }

    return true;
}

bool cb_is_on_board( Chessboard const * const restrict cb, int i, int j )
{
    if ( !cb ) return false;
    return ( ( 0 <= i ) && ( i < (int)cb->size ) && ( 0 <= j ) && ( j < (int)cb->size ) );
}

PieceType cb_get_piece( Chessboard const * const restrict cb, int i, int j )
{
    if ( cb_is_on_board( cb, i, j ) )
    {
        return cb->board[ i ][ j ];
    }

    return PT_None;
}

TagType cb_get_tag( Chessboard const * const restrict cb, int i, int j )
{
    if ( cb_is_on_board( cb, i, j ) )
    {
        return cb->tags[ i ][ j ];
    }

    return TT_None;
}

bool cb_set_piece_tag( Chessboard * const restrict cb, int i, int j, PieceType pt, TagType tt )
{
    if ( !cb ) return false;

    if ( cb_is_on_board( cb, i, j ) )
    {
        cb->board[ i ][ j ] = pt;
        cb->tags[ i ][ j ] = tt;
    }

    return true;
}

bool cb_set_piece( Chessboard * const restrict cb, int i, int j, PieceType pt )
{
    return cb_set_piece_tag( cb, i, j, pt, TT_None );
}

bool cb_set_tag( Chessboard * const restrict cb, int i, int j, TagType tt )
{
    if ( !cb ) return false;

    if ( cb_is_on_board( cb, i, j ) )
    {
        cb->tags[ i ][ j ] = tt;
    }

    return true;
}

static char * cb_get_divider_alx( Chessboard const * const restrict cb )
{
    if ( !cb ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * divider = calloc( 1, len );
    if ( !divider ) return NULL;

    for ( int i = 0; i < (int)len; ++i )
    {
        if ( i < 3 ) divider[ i ] = ' ';
        else if ( i < 3 + 2 * (int)cb->size - 1 ) divider[ i ] = '-';
        else if ( i < (int)len ) divider[ i ] = ' ';
    }

    divider[ len - 2 ] = '\n';
    divider[ len - 1 ] = '\0';

    return divider;
}

static char * cb_get_horizontal_ruler_alx( Chessboard const * const restrict cb )
{
    if ( !cb ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * hr = calloc( 1, len );
    if ( !hr ) return NULL;

    char ch = 'a';
    for ( int i = 0; i < (int)len; ++i )
    {
        if ( i < 3 ) hr[ i ] = ' ';
        else if ( i < 3 + 2 * (int)cb->size )
        {
            if ( i % 2 == 0 ) hr[ i ] = ' ';
            else
            {
                hr[ i ] = ch;
                ++ch;
            }
        }
        else if ( i < (int)len ) hr[ i ] = ' ';
    }

    hr[ len - 2 ] = '\n';
    hr[ len - 1 ] = '\0';

    return hr;
}

char * cb_as_string_alx( Chessboard const * const restrict cb, bool is_board_or_chips )
{
    if ( !cb ) return NULL;

    char * s = calloc( 1, 2048 );
    if ( !s ) return NULL;

    char * horizontal_ruler = cb_get_horizontal_ruler_alx( cb );
    if ( !horizontal_ruler )
    {
        free( s );
        return NULL;
    }

    strcat( s, horizontal_ruler );

    char * divider = cb_get_divider_alx( cb );
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

    for ( int i = 0; i < (int)cb->size; ++i )
    {
        char r = (char)( cb->size - i );
        sprintf( row, "%2hhu|", r );
        strcat( s, row );

        for ( int j = 0; j < (int)cb->size; ++j )
        {
            char ch;
            int x = j;
            int y = cb->size - i - 1;

            if ( is_board_or_chips )
                ch = pt_as_char( cb->board[ x ][ y ] );
            else
                ch = tt_as_char( cb->tags[ x ][ y ] );

            if ( ch == ' ' )
            {
                if ( is_field_light( x, y ) ) ch = '.';
                else ch = ',';
            }

            if ( j < (int)cb->size - 1 )
                sprintf( field, "%c ", ch );
            else
                sprintf( field, "%c", ch );

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

bool cb_print( Chessboard const * const restrict cb, bool is_board_or_chips )
{
    if ( !cb ) return false;

    char * s = cb_as_string_alx( cb, is_board_or_chips );

    if ( !s ) return false;

    printf( "%s", s );
    free( s );

    return true;
}
