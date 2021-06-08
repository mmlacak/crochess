// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_ply.h"
#include "cc_move.h"


CcMove * cc_move_new( char const * const restrict notation,
                      CcPly * const restrict plies,
                      CcMoveStatusEnum status )
{
    CcMove * mv = malloc( sizeof( CcMove ) );
    if ( !mv ) return NULL;

    mv->notation = notation;
    mv->plies = plies;
    mv->status = status;
    mv->next = NULL;

    return mv;
}

CcMove * cc_move_append_new( CcMove * const restrict moves,
                             char const * const restrict notation,
                             CcPly * const restrict plies,
                             CcMoveStatusEnum status )
{
    CcMove * new = cc_move_new( notation, plies, status );
    if ( !new ) return NULL;
    if ( !moves ) return new;

    CcMove * mv = moves;
    while ( mv->next ) mv = mv->next; // rewind
    mv->next = new; // append

    return new;
}

bool cc_move_free_all_moves( CcMove ** const moves )
{
    if ( !moves ) return true;
    if ( !*moves ) return false;

    bool result = true;

    CcMove * mv = *moves;

    while ( mv )
    {
        free( (char *)mv->notation ); // free() doesn't do pointers to const.

        CcPly ** plies = &( mv->plies );
        result = result && cc_ply_free_all_plies( plies );

        CcMove * tmp = mv->next;
        free( mv );
        mv = tmp;
    }

    *moves = NULL;
    return result;
}
