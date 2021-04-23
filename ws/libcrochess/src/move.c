// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "move.h"


Move * mv_new_alx( Ply * const restrict plies, MoveStatus status )
{
    Move * mv = malloc( sizeof( Move ) );
    if ( !mv ) return NULL;

    mv->plies = plies;
    mv->status = status;

    return mv;
}
