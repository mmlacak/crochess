// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "ply.h"
#include "move.h"


Move * mv_new_alx( Ply * const restrict plies, MoveStatus status )
{
    Move * mv = malloc( sizeof( Move ) );
    if ( !mv ) return NULL;

    mv->plies = plies;
    mv->status = status;

    return mv;
}

bool mv_free_move( Move ** move )
{
    if ( !move ) return true;
    if ( !*move ) return false;

    bool result = true;

    Ply ** plies = &( ( *move )->plies );
    result = result && ply_free_all_plies( plies );

    free( *move );
    move = NULL;

    return result;
}
