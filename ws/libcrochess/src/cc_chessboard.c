// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_variant.h"
#include "cc_setup_board.h"
#include "cc_setup_tags.h"
#include "cc_chessboard.h"

/**
    @file cc_chessboard.c
    @brief Chessboard functions.
*/


bool cc_is_field_light( int i, int j )
{
    return ( (i + j) % 2 != 0 );
}


CcChessboard * cc_chessboard_new( CcVariantEnum ve, bool do_setup )
{
    CcChessboard * cb__a = malloc( sizeof( CcChessboard ) );
    if ( !cb__a ) return NULL;

    if ( !cc_chessboard_init( cb__a, ve, do_setup ) )
    {
        CC_FREE( cb__a );
        return NULL;
    }

    return cb__a;
}

bool cc_chessboard_init( CcChessboard * restrict cb__io,
                         CcVariantEnum ve,
                         bool do_setup )
{
    if ( !cb__io ) return false;

    cb__io->type = ve;
    cb__io->size = cc_variant_board_size( cb__io->type );

    if ( do_setup )
        return cc_chessboard_setup( cb__io );
    else
        return cc_chessboard_clear( cb__io );
}

bool cc_chessboard_clear( CcChessboard * restrict cb__io )
{
    if ( !cb__io ) return false;

    for ( int i = 0; i < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++i )
    {
        for ( int j = 0; j < CC_VARIANT_BOARD_SIZE_MAXIMUM; ++j )
        {
            cb__io->board[ i ][ j ] = CC_PE_None;
            cb__io->tags[ i ][ j ] = CC_TE_None;
        }
    }

    return true;
}

bool cc_chessboard_setup( CcChessboard * restrict cb__io )
{
    if ( !cb__io ) return false;

    if ( !cc_chessboard_clear( cb__io ) ) return false;

    CcPieceEnum const * su = cc_setup_board_get( cb__io->type );
    if ( !su ) return false;

    CcTagEnum const * tu = cc_setup_tags_get( cb__io->type );
    if ( !tu ) return false;

    for ( int i = 0; i < (int)cb__io->size; ++i )
    {
        for ( int j = 0; j < (int)cb__io->size; ++j )
        {
            int x = j;
            int y = cb__io->size - i - 1;
            int z = cb__io->size * i + j;

            cb__io->board[ x ][ y ] = su[ z ];
            cb__io->tags[ x ][ y ] = tu[ z ];
        }
    }

    return true;
}


bool cc_chessboard_copy( CcChessboard * restrict into__io,
                         CcChessboard * restrict from )
{
    if ( !into__io ) return false;
    if ( !from ) return false;

    if ( !cc_chessboard_init( into__io, from->type, false ) )
        return false;

    for ( int i = 0; i < (int)into__io->size; ++i )
    {
        for ( int j = 0; j < (int)into__io->size; ++j )
        {
            into__io->board[ i ][ j ] = from->board[ i ][ j ];
            into__io->tags[ i ][ j ] = from->tags[ i ][ j ];
        }
    }

    return true;
}

CcChessboard * cc_chessboard_duplicate_new( CcChessboard * restrict from )
{
    if ( !from ) return NULL;

    CcChessboard * cb__a = malloc( sizeof( CcChessboard ) );
    if ( !cb__a ) return NULL;

    cc_chessboard_init( cb__a, from->type, false );

    if ( !cc_chessboard_copy( cb__a, from ) )
    {
        CC_FREE( cb__a );
        return NULL;
    }

    return cb__a;
}

bool cc_chessboard_free_all( CcChessboard ** restrict cb__f )
{
    if ( !cb__f ) return false;
    if ( !*cb__f ) return true;

    CC_FREE_NULL( cb__f );
    return true;
}

CcPieceEnum cc_chessboard_get_piece( CcChessboard * restrict cb,
                                     int i,
                                     int j )
{
    if ( cc_variant_is_pos_on_board( cb->type, i, j ) )
    {
        return cb->board[ i ][ j ];
    }

    return CC_PE_None;
}

CcTagEnum cc_chessboard_get_tag( CcChessboard * restrict cb,
                                 int i,
                                 int j )
{
    if ( cc_variant_is_pos_on_board( cb->type, i, j ) )
    {
        return cb->tags[ i ][ j ];
    }

    return CC_TE_None;
}

bool cc_chessboard_set_piece_tag( CcChessboard * restrict cb__io,
                                  int i,
                                  int j,
                                  CcPieceEnum pe,
                                  CcTagEnum tt )
{
    if ( !cb__io ) return false;

    if ( cc_variant_is_pos_on_board( cb__io->type, i, j ) )
    {
        cb__io->board[ i ][ j ] = pe;
        cb__io->tags[ i ][ j ] = tt;

        return  ( ( cb__io->board[ i ][ j ] == pe )
               && ( cb__io->tags[ i ][ j ] == tt ) ); // cb__io volatile ?
    }

    return false;
}

bool cc_chessboard_set_piece( CcChessboard * restrict cb__io,
                              int i,
                              int j,
                              CcPieceEnum pe )
{
    return cc_chessboard_set_piece_tag( cb__io, i, j, pe, CC_TE_None );
}

bool cc_chessboard_set_tag( CcChessboard * restrict cb__io,
                            int i,
                            int j,
                            CcTagEnum tt )
{
    if ( !cb__io ) return false;

    if ( cc_variant_is_pos_on_board( cb__io->type, i, j ) )
    {
        cb__io->tags[ i ][ j ] = tt;

        return ( cb__io->tags[ i ][ j ] == tt );
    }

    return false;
}


static char * cc_chessboard_get_divider_new( CcChessboard * restrict cb )
{
    if ( !cb ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * divider__a = calloc( 1, len );
    if ( !divider__a ) return NULL;

    for ( int i = 0; i < (int)len; ++i )
    {
        if ( i < 3 ) divider__a[ i ] = ' ';
        else if ( i < 3 + 2 * (int)cb->size - 1 ) divider__a[ i ] = '-';
        else if ( i < (int)len ) divider__a[ i ] = ' ';
    }

    divider__a[ len - 2 ] = '\n';
    divider__a[ len - 1 ] = '\0';

    return divider__a;
}

static char * cc_chessboard_get_horizontal_ruler_new( CcChessboard * restrict cb )
{
    if ( !cb ) return NULL;

    size_t len = 3 + 2 * cb->size + 3 + 1;
    char * hr__a = calloc( 1, len );
    if ( !hr__a ) return NULL;

    char ch = 'a';
    for ( int i = 0; i < (int)len; ++i )
    {
        if ( i < 3 ) hr__a[ i ] = ' ';
        else if ( i < 3 + 2 * (int)cb->size )
        {
            if ( i % 2 == 0 ) hr__a[ i ] = ' ';
            else
            {
                hr__a[ i ] = ch;
                ++ch;
            }
        }
        else if ( i < (int)len ) hr__a[ i ] = ' ';
    }

    hr__a[ len - 2 ] = '\n';
    hr__a[ len - 1 ] = '\0';

    return hr__a;
}

char * cc_chessboard_as_string_new( CcChessboard * restrict cb,
                                    bool is_board_or_tag )
{
    if ( !cb ) return NULL;

    char * show__a = calloc( 1, 2048 );
    if ( !show__a ) return NULL;

    char * horizontal_ruler = cc_chessboard_get_horizontal_ruler_new( cb );
    if ( !horizontal_ruler )
    {
        CC_FREE( show__a );
        return NULL;
    }

    strcat( show__a, horizontal_ruler );

    char * divider__a = cc_chessboard_get_divider_new( cb );
    if ( !divider__a )
    {
        CC_FREE( show__a );
        CC_FREE( horizontal_ruler );
        return NULL;
    }

    strcat( show__a, divider__a );

    char * row__a = calloc(1, 6);
    if ( !row__a )
    {
        CC_FREE( show__a );
        CC_FREE( horizontal_ruler );
        CC_FREE( divider__a );
        return NULL;
    }

    char * field__a = calloc(1, 3);
    if ( !field__a )
    {
        CC_FREE( show__a );
        CC_FREE( horizontal_ruler );
        CC_FREE( divider__a );
        CC_FREE( row__a );
        return NULL;
    }

    for ( int i = 0; i < (int)cb->size; ++i )
    {
        char r = (char)( cb->size - i );
        sprintf( row__a, "%2hhu|", r );
        strcat( show__a, row__a );

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
                sprintf( field__a, "%c ", ch );
            else
                sprintf( field__a, "%c", ch );

            strcat( show__a, field__a );
        }

        sprintf( row__a, "|%2hhu\n", r );
        strcat( show__a, row__a );
    }

    strcat( show__a, divider__a );
    strcat( show__a, horizontal_ruler );

    CC_FREE( horizontal_ruler );
    CC_FREE( divider__a );
    CC_FREE( row__a );
    CC_FREE( field__a );

    return show__a;
}

bool cc_chessboard_print( CcChessboard * restrict cb,
                          bool is_board_or_tag )
{
    if ( !cb ) return false;

    char * show__a = cc_chessboard_as_string_new( cb, is_board_or_tag );
    if ( !show__a ) return false;

    printf( "%s", show__a );
    CC_FREE( show__a );

    return true;
}
