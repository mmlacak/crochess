// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"

/**
    @file cc_ply.c
    @brief Ply related functions.
*/


CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece, CcStep ** restrict steps_n )
{
    CcPly * ply = calloc( 1, sizeof( CcPly ) );
    if ( !ply ) return NULL;

    ply->link = link;
    ply->piece = piece;

    if ( steps_n )
    {
        ply->steps = *steps_n;
        *steps_n = NULL;
    }
    else
        ply->steps = NULL;

    ply->next = NULL;

    return ply;
}

CcPly * cc_ply_append_new( CcPly * const restrict plies,
                           CcPlyLinkEnum link,
                           CcPieceEnum piece,
                           CcStep ** restrict steps_n )
{
    CcPly * new = cc_ply_new( link, piece, steps_n );
    if ( !new ) return NULL;
    if ( !plies ) return new;

    CcPly * p = plies;
    while ( p->next ) p = p->next; // rewind
    p->next = new; // append

    return new;
}

bool cc_ply_free_all_plies( CcPly ** const plies_f )
{
    if ( !plies_f ) return false;
    if ( !*plies_f ) return true;

    bool result = true;
    CcPly * ply = *plies_f;

    while ( ply )
    {
        CcStep ** steps = &( ply->steps );
        result = result && cc_step_free_all_steps( steps );

        CcPly * tmp = ply->next;
        free( ply );
        ply = tmp;
    }

    *plies_f = NULL;
    return result;
}


bool cc_ply_contains_side_effects( CcPly const * const restrict ply )
{
    if ( !ply ) return false;

    CcStep const * steps = ply->steps;
    if ( !steps ) return false;

    CcStep const * s = steps;
    while ( s->next )
    {
        if ( s->side_effect.type != CC_SEE_None ) return true;
        s = s->next;
    }

    return false;
}

size_t cc_ply_step_count( CcPly const * const restrict ply, bool include_starting_pos )
{
    if ( !ply ) return 0;

    CcStep const * steps = ply->steps;
    if ( !steps ) return 0;

    size_t count = 1;
    CcStep const * s = steps;

    while ( s->next )
    {
        if ( s->link == CC_SLE_Start )
        {
            if ( include_starting_pos ) ++count;
        }
        else
            ++count;

        s = s->next;
    }

    return count;
}
