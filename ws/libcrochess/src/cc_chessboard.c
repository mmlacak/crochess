// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"
#include "cc_setup_board.h"
#include "cc_setup_tags.h"
#include "cc_chessboard.h"

/**
    @file cc_chessboard.c
    @brief Chessboard related functions.
*/


bool cc_is_field_light( int const i, int const j )
{
    return ( (i + j) % 2 != 0 );
}


CcChessboard * cc_chessboard_new( CcVariantEnum const ve, bool const do_setup )
{
    CcChessboard * cb = malloc( sizeof( CcChessboard ) );
    if ( !cb ) return NULL;

    if ( !cc_chessboard_init( cb, ve, do_setup ) )
    {
        free( cb );
        return NULL;
    }

    return cb;
}

bool cc_chessboard_init( CcChessboard * const restrict cb_io,
                         CcVariantEnum const ve,
                         bool const do_setup )
{
    if ( !cb_io ) return false;

    cb_io->type = ve;
    cb_io->size = cc_variant_board_size( cb_io->type );

    if ( do_setup ) return cc_chessboard_setup( cb_io );
    else return cc_chessboard_clear( cb_io );
}

bool cc_chessboard_clear( CcChessboard * const restrict cb_io )
{
    if ( !cb_io ) return false;

    for ( int i = 0; i < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++i )
    {
        for ( int j = 0; j < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++j )
        {
            cb_io->board[ i ][ j ] = CC_PE_None;
            cb_io->tags[ i ][ j ] = CC_TE_None;
        }
    }

    return true;
}

bool cc_chessboard_setup( CcChessboard * const restrict cb_io )
{
    if ( !cb_io ) return false;

    if ( !cc_chessboard_clear( cb_io ) ) return false;

    CcPieceEnum const * const su = cc_board_setup_get( cb_io->type );
    if ( !su ) return false;

    CcTagEnum const * const tu = cc_tags_setup_get( cb_io->type );
    if ( !tu ) return false;

    for ( int i = 0; i < (int)cb_io->size; ++i )
    {
        for ( int j = 0; j < (int)cb_io->size; ++j )
        {
            int x = j;
            int y = cb_io->size - i - 1;
            int z = cb_io->size * i + j;

            cb_io->board[ x ][ y ] = su[ z ];
            cb_io->tags[ x ][ y ] = tu[ z ];
        }
    }

    return true;
}


bool cc_chessboard_copy( CcChessboard * const restrict into_io,
                         CcChessboard const * const restrict from )
{
    if ( !into_io ) return false;
    if ( !from ) return false;

    if ( !cc_chessboard_init( into_io, from->type, false ) ) return false;

    for ( int i = 0; i < (int)into_io->size; ++i )
    {
        for ( int j = 0; j < (int)into_io->size; ++j )
        {
            into_io->board[ i ][ j ] = from->board[ i ][ j ];
            into_io->tags[ i ][ j ] = from->tags[ i ][ j ];
        }
    }

    return true;
}

CcChessboard * cc_chessboard_duplicate_new( CcChessboard const * const restrict from )
{
    if ( !from ) return NULL;

    CcChessboard * cb = malloc( sizeof( CcChessboard ) );
    if ( !cb ) return NULL;

    CcVariantEnum ve = from->type;
    cc_chessboard_init( cb, ve, false );

    if ( !cc_chessboard_copy( cb, from ) )
    {
        free( cb );
        return NULL;
    }

    return cb;
}

bool cc_chessboard_free_all( CcChessboard ** const restrict cb_f )
{
    if ( !cb_f ) return false;
    if ( !*cb_f ) return true;

    free( *cb_f );
    *cb_f = NULL;

    return true;
}

bool cc_chessboard_is_coord_on_board( CcChessboard const * const restrict cb,
                                      int const coord )
{
    if ( !cb ) return false;
    return ( ( 0 <= coord ) && ( coord < (int)cb->size ) );
}

bool cc_chessboard_is_pos_on_board( CcChessboard const * const restrict cb,
                                    int const i,
                                    int const j )
{
    if ( !cb ) return false;

    return ( cc_chessboard_is_coord_on_board( cb, i )
          && cc_chessboard_is_coord_on_board( cb, j ) );
}

CcPieceEnum cc_chessboard_get_piece( CcChessboard const * const restrict cb,
                                     int const i,
                                     int const j )
{
    if ( cc_chessboard_is_pos_on_board( cb, i, j ) )
    {
        return cb->board[ i ][ j ];
    }

    return CC_PE_None;
}

CcTagEnum cc_chessboard_get_tag( CcChessboard const * const restrict cb,
                                 int const i,
                                 int const j )
{
    if ( cc_chessboard_is_pos_on_board( cb, i, j ) )
    {
        return cb->tags[ i ][ j ];
    }

    return CC_TE_None;
}

bool cc_chessboard_set_piece_tag( CcChessboard * const restrict cb_io,
                                  int const i,
                                  int const j,
                                  CcPieceEnum const pe,
                                  CcTagEnum const tt )
{
    if ( !cb_io ) return false;

    if ( cc_chessboard_is_pos_on_board( cb_io, i, j ) )
    {
        cb_io->board[ i ][ j ] = pe;
        cb_io->tags[ i ][ j ] = tt;

        return ( ( cb_io->board[ i ][ j ] == pe ) && ( cb_io->tags[ i ][ j ] == tt ) ); // cb_io volatile ?
    }

    return false;
}

bool cc_chessboard_set_piece( CcChessboard * const restrict cb_io,
                              int const i,
                              int const j,
                              CcPieceEnum const pe )
{
    return cc_chessboard_set_piece_tag( cb_io, i, j, pe, CC_TE_None );
}

bool cc_chessboard_set_tag( CcChessboard * const restrict cb_io,
                            int const i,
                            int const j,
                            CcTagEnum const tt )
{
    if ( !cb_io ) return false;

    if ( cc_chessboard_is_pos_on_board( cb_io, i, j ) )
    {
        cb_io->tags[ i ][ j ] = tt;

        return ( cb_io->tags[ i ][ j ] == tt );
    }

    return false;
}


static char * cc_chessboard_get_divider_new( CcChessboard const * const restrict cb )
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

static char * cc_chessboard_get_horizontal_ruler_new( CcChessboard const * const restrict cb )
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

char * cc_chessboard_as_string_new( CcChessboard const * const restrict cb,
                                    bool const is_board_or_tag )
{
    if ( !cb ) return NULL;

    char * s = calloc( 1, 2048 );
    if ( !s ) return NULL;

    char * horizontal_ruler = cc_chessboard_get_horizontal_ruler_new( cb );
    if ( !horizontal_ruler )
    {
        free( s );
        return NULL;
    }

    strcat( s, horizontal_ruler );

    char * divider = cc_chessboard_get_divider_new( cb );
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

            if ( is_board_or_tag )
                ch = cc_piece_as_char( cb->board[ x ][ y ] );
            else
                ch = cc_tag_as_char( cb->tags[ x ][ y ] );

            if ( ch == ' ' )
            {
                if ( cc_is_field_light( x, y ) ) ch = '.';
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

bool cc_chessboard_print( CcChessboard const * const restrict cb,
                          bool const is_board_or_tag )
{
    if ( !cb ) return false;

    char * s = cc_chessboard_as_string_new( cb, is_board_or_tag );
    if ( !s ) return false;

    printf( "%s", s );
    free( s );

    return true;
}
