// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_pos.h"


CcPos cc_pos( int const i, int const j )
{
    CcPos pos = { .i = i, .j = j };
    return pos;
}

CcPos cc_pos_empty()
{
    return cc_pos( CC_INVALID_OFF_BOARD_COORD_MIN, CC_INVALID_OFF_BOARD_COORD_MIN );
}


CcPosLink * cc_pos_link_new( int const i, int const j )
{
    CcPosLink * pl = malloc( sizeof( CcPosLink ) );
    if ( !pl ) return NULL;

    pl->i = i;
    pl->j = j;
    pl->next = NULL;

    return pl;
}

CcPosLink * cc_pos_link_append( CcPosLink * const restrict pos_link,
                                int const i,
                                int const j )
{
    if ( !pos_link ) return NULL;

    CcPosLink * new = cc_pos_link_new( i, j );
    if ( !new ) return NULL;

    CcPosLink * pl = pos_link;
    while ( pl->next ) pl = pl->next; // rewind
    pl->next = new; // append

    return new;
}

CcPosLink * cc_pos_link_append_or_init( CcPosLink ** const restrict pos_link_io,
                                        int const i,
                                        int const j )
{
    if ( !pos_link_io ) return NULL;

    CcPosLink * new = NULL;

    if ( !*pos_link_io )
        *pos_link_io = new = cc_pos_link_new( i, j );
    else
        new = cc_pos_link_append( *pos_link_io, i, j );

    return new;
}
