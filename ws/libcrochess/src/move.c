// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_ply.h"
#include "move.h"


Move * mv_new_alx( CcPly * const restrict plies, MoveStatus status )
{
    Move * mv = malloc( sizeof( Move ) );
    if ( !mv ) return NULL;

    mv->plies = plies;
    mv->status = status;

    return mv;
}

bool mv_free_complete_move( Move ** const move )
{
    if ( !move ) return true;
    if ( !*move ) return false;

    bool result = true;

    CcPly ** plies = &( ( *move )->plies );
    result = result && cc_ply_free_all_plies( plies );

    free( *move );
    *move = NULL;

    return result;
}
