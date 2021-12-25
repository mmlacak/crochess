// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_pos.h"


CcPos cc_pos( int i, int j )
{
    CcPos pos = { .i = i, .j = j };
    return pos;
}

CcPos cc_pos_empty()
{
    return cc_pos( CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcPos cc_pos_add( CcPos augend, CcPos addend )
{
    return cc_pos( augend.i + addend.i, augend.j + addend.j );
}

CcPos cc_pos_subtract( CcPos minuend, CcPos subtrahend )
{
    return cc_pos( minuend.i - subtrahend.i, minuend.j - subtrahend.j );
}

bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 )
{
    return ( ( pos_1.i == pos_2.i ) && ( pos_1.j == pos_2.j ) );
}

bool cc_pos_is_not_equal( CcPos pos_1, CcPos pos_2 )
{
    return ( ( pos_1.i != pos_2.i ) || ( pos_1.j != pos_2.j ) );
}

bool cc_pos_is_empty( CcPos pos )
{
    return ( ( pos.i == CC_INVALID_OFF_BOARD_COORD_MIN ) &&
             ( pos.j == CC_INVALID_OFF_BOARD_COORD_MIN ) );
}


CcPosLink * cc_pos_link_new( int i, int j )
{
    CcPosLink * pl__a = malloc( sizeof( CcPosLink ) );
    if ( !pl__a ) return NULL;

    pl__a->pos = cc_pos( i, j );
    pl__a->next = NULL;

    return pl__a;
}

CcPosLink * cc_pos_link_append( CcPosLink * restrict pos_link__io,
                                int i,
                                int j )
{
    if ( !pos_link__io ) return NULL;

    CcPosLink * pl__t = cc_pos_link_new( i, j );
    if ( !pl__t ) return NULL;

    CcPosLink * pl = pos_link__io;
    while ( pl->next ) pl = pl->next; // rewind
    pl->next = pl__t; // append // Ownership transfer --> pl__t is now weak pointer.

    return pl__t;
}

CcPosLink * cc_pos_link_append_or_init( CcPosLink ** restrict pos_link__io,
                                        int i,
                                        int j )
{
    if ( !pos_link__io ) return NULL;

    CcPosLink * pl__w = NULL;

    if ( !*pos_link__io )
        *pos_link__io = pl__w = cc_pos_link_new( i, j );
    else
        pl__w = cc_pos_link_append( *pos_link__io, i, j );

    return pl__w;
}

bool cc_pos_link_free_all( CcPosLink ** restrict pos_link__f )
{
    if ( !pos_link__f ) return false;
    if ( !*pos_link__f ) return true;

    CcPosLink * pl = *pos_link__f;

    while ( pl )
    {
        CcPosLink * tmp = pl->next;
        CC_FREE( pl );
        pl = tmp;
    }

    *pos_link__f = NULL;
    return true;
}
