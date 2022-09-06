// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_math.h"
#include "cc_pos.h"

/**
    @file cc_pos.c
    @brief Functions for positions, linked list.
*/


CcPos cc_pos( int i, int j )
{
    CcPos pos = { .i = i, .j = j };
    return pos;
}

CcPos cc_pos_disambiguation_file( int i )
{
    return cc_pos( i, CC_INVALID_COORD );
}

CcPos cc_pos_disambiguation_rank( int j )
{
    return cc_pos( CC_INVALID_COORD, j );
}

bool cc_pos_is_valid( CcPos pos )
{
    return ( CC_IS_POS_VALID( pos.i, pos.j ) );
}

bool cc_pos_is_static_step( CcPos pos )
{
    return ( ( pos.i == 0 ) && ( pos.j == 0 ) );
}

bool cc_pos_is_disambiguation_file( CcPos pos )
{
    return ( CC_IS_COORD_VALID( pos.i ) && ( !CC_IS_COORD_VALID( pos.j ) ) );
}

bool cc_pos_is_disambiguation_rank( CcPos pos )
{
    return ( ( !CC_IS_COORD_VALID( pos.i ) ) && CC_IS_COORD_VALID( pos.j ) );
}

bool cc_pos_is_disambiguation( CcPos pos )
{
    return ( cc_pos_is_disambiguation_file( pos ) ||
             cc_pos_is_disambiguation_rank( pos ) );
}

bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 )
{
    return ( ( pos_1.i == pos_2.i ) && ( pos_1.j == pos_2.j ) );
}

bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 )
{
    bool is_file = ( CC_IS_COORD_VALID( pos_1.i ) &&
                     CC_IS_COORD_VALID( pos_2.i ) );

    if ( is_file && ( pos_1.i != pos_2.i ) )
        return false;

    bool is_rank = ( CC_IS_COORD_VALID( pos_1.j ) &&
                     CC_IS_COORD_VALID( pos_2.j ) );

    if ( is_rank && ( pos_1.j != pos_2.j ) )
        return false;

    return is_file || is_rank;
}

CcPos cc_pos_add( CcPos pos, CcPos step )
{
    if ( cc_pos_is_valid( pos ) && cc_pos_is_valid( step ) )
        return cc_pos( pos.i + step.i, pos.j + step.j );
    else if ( cc_pos_is_disambiguation_file( pos ) &&
              cc_pos_is_disambiguation_file( step ) )
        return cc_pos_disambiguation_file( pos.i + step.i );
    else if ( cc_pos_is_disambiguation_rank( pos ) &&
              cc_pos_is_disambiguation_rank( step ) )
        return cc_pos_disambiguation_rank( pos.j + step.j );
    else
        return CC_POS_CAST_INVALID;
}

CcPos cc_pos_subtract( CcPos pos, CcPos step )
{
    if ( cc_pos_is_valid( pos ) && cc_pos_is_valid( step ) )
        return cc_pos( pos.i - step.i, pos.j - step.j );
    else if ( cc_pos_is_disambiguation_file( pos ) &&
              cc_pos_is_disambiguation_file( step ) )
        return cc_pos_disambiguation_file( pos.i - step.i );
    else if ( cc_pos_is_disambiguation_rank( pos ) &&
              cc_pos_is_disambiguation_rank( step ) )
        return cc_pos_disambiguation_rank( pos.j - step.j );
    else
        return CC_POS_CAST_INVALID;
}

CcPos cc_pos_step( CcPos start, CcPos destination )
{
    int diff_i = destination.i - start.i;
    int diff_j = destination.j - start.j;

    int gcd = cc_gcd( diff_i, diff_j );
    if ( gcd == 0 ) return CC_POS_CAST_INVALID;

    diff_i /= gcd;
    diff_j /= gcd;

    return cc_pos( diff_i, diff_j );
}

bool cc_pos_to_short_string( CcPos pos,
                             cc_char_8 * restrict pos_str__o )
{
    if ( !pos_str__o ) return false;

    if ( CC_IS_POS_ON_BOARD( CC_MAX_BOARD_SIZE, pos.i, pos.j ) )
    {
        snprintf( *pos_str__o,
                  CC_MAX_LEN_CHAR_8,
                  "%c%hhd",
                  CC_CONVERT_BYTE_INTO_FILE_CHAR( pos.i ),
                  (signed char)(pos.j + 1) );
    }
    else
    {
        int count = 0; // snprintf() doesn't count '\0'

        if ( ( -100 < pos.i ) && ( pos.i < 1000 ) )
            count = snprintf( *pos_str__o,
                              CC_MAX_LEN_CHAR_8,
                              "%hd,",
                              (signed short)pos.i );
        else
            count = snprintf( *pos_str__o, CC_MAX_LEN_CHAR_8, "*," );

        if ( count < 1 ) return false; // count can't be > 4

        char * p = ( (char *)pos_str__o + count );
        size_t size = CC_MAX_LEN_CHAR_8 - count;

        if ( ( -100 < pos.j ) && ( pos.j < 1000 ) )
            count = snprintf( p, size, "%hd", (signed short)pos.j );
        else
            count = snprintf( p, size, "*" );

        if ( count < 1 ) return false; // count can't be > 4
    }

    return true;
}


CcPosLink * cc_pos_link__new( CcPos pos )
{
    CcPosLink * pl__t = malloc( sizeof( CcPosLink ) );
    if ( !pl__t ) return NULL;

    pl__t->pos = pos;

    pl__t->next = NULL;

    return pl__t;
}

CcPosLink * cc_pos_link_append( CcPosLink * restrict pos_link__io,
                                CcPos pos )
{
    if ( !pos_link__io ) return NULL;

    CcPosLink * pl__t = cc_pos_link__new( pos );
    if ( !pl__t ) return NULL;

    CcPosLink * pl = pos_link__io;

    while ( pl->next ) pl = pl->next; // rewind

    pl->next = pl__t; // append // Ownership transfer --> pl__t is now weak pointer.

    return pl__t;
}

CcPosLink * cc_pos_link_append_if( CcPosLink ** restrict pos_link__io,
                                   CcPos pos )
{
    if ( !pos_link__io ) return NULL;

    CcPosLink * pl__w = NULL;

    if ( !*pos_link__io )
        *pos_link__io = pl__w = cc_pos_link__new( pos );
    else
        pl__w = cc_pos_link_append( *pos_link__io, pos );

    return pl__w;
}

bool cc_pos_link_free_all( CcPosLink ** restrict pos_link__f )
{
    if ( !pos_link__f ) return false;
    if ( !*pos_link__f ) return true;

    CcPosLink * pl = *pos_link__f;
    CcPosLink * tmp = NULL;

    while ( pl )
    {
        tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *pos_link__f = NULL;
    return true;
}

size_t cc_pos_link_len( CcPosLink * restrict pos_link )
{
    if ( !pos_link ) return 0;

    size_t len = 0;
    CcPosLink * pl = pos_link;

    while ( pl )
    {
        ++len;
        pl = pl->next;
    }

    return len;
}

char * cc_pos_link_to_short_string__new( CcPosLink * restrict pos_link )
{
    if ( !pos_link ) return NULL;

    // len is certainly > 0, because pos_link != NULL
    size_t len = cc_pos_link_len( pos_link ) *
                 ( CC_MAX_LEN_CHAR_8 + 1 ); // +1, for separator '.' between positions

    size_t size = len + 1;
    char * pl_str__a = malloc( size ); // == len + 1, to have room for '\0'
    if ( !pl_str__a ) return NULL;

    *pl_str__a = '\0';

    char * pl_str = pl_str__a;
    cc_char_8 pos_str = CC_CHAR_8_EMPTY;
    CcPosLink * pl = pos_link;

    while ( pl )
    {
        if ( pl != pos_link ) // Not 1st pos ...
        {
            *pl_str++ = '.';
            *pl_str = '\0';
        }

        if ( !cc_pos_to_short_string( pl->pos, &pos_str ) )
        {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl_str = cc_str_append_into( pl_str, size, pos_str, CC_MAX_LEN_CHAR_8 );
        if ( !pl_str )
        {
            CC_FREE( pl_str__a );
            return NULL;
        }

        pl = pl->next;
    }

    return pl_str__a;
}
