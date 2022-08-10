// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

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
    return cc_pos( i, CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcPos cc_pos_disambiguation_rank( int j )
{
    return cc_pos( CC_INVALID_OFF_BOARD_COORD_MIN, j );
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
    if ( cc_pos_is_valid( pos_1 ) && cc_pos_is_valid( pos_2 ) )
        return ( ( pos_1.i == pos_2.i ) && ( pos_1.j == pos_2.j ) );
    else if ( cc_pos_is_disambiguation_file( pos_1 ) &&
              cc_pos_is_disambiguation_file( pos_2 ) )
        return ( pos_1.i == pos_2.i );
    else if ( cc_pos_is_disambiguation_rank( pos_1 ) &&
              cc_pos_is_disambiguation_rank( pos_2 ) )
        return ( pos_1.j == pos_2.j );
    else
        return false;
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
        return CC_POS_INVALID_CAST;
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
        return CC_POS_INVALID_CAST;
}

CcPos cc_pos_step( CcPos start, CcPos destination )
{
    int diff_i = destination.i - start.i;
    int diff_j = destination.j - start.j;

    int gcd = cc_gcd( diff_i, diff_j );
    if ( gcd == 0 ) return CC_POS_INVALID_CAST;

    diff_i /= gcd;
    diff_j /= gcd;

    return cc_pos( diff_i, diff_j );
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

CcPosLink * cc_pos_link_append_or_init( CcPosLink ** restrict pos_link__io,
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
