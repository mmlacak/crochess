// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <string.h>

#include "cc_print_moves.h"


char * cc_print_step( CcChessboard * const restrict cb,
                      CcMove const * const restrict move,
                      CcPly const * const restrict ply,
                      CcStep const * const restrict step )
{
    return NULL;
}

char * cc_print_ply( CcChessboard * const restrict cb,
                     CcMove const * const restrict move,
                     CcPly const * const restrict ply )
{
    return NULL;
}

char * cc_print_moves( CcChessboard * const restrict cb,
                       CcMove const * const restrict moves,
                       CcPrintMoveEnum do_spec )
{
    if ( !cb ) return NULL;
    if ( !moves ) return NULL;

    char * result = "";
    CcMove const * mv = moves;

    if ( do_spec == CC_PME_PrintOnlyLastMove ) while ( mv->next ) mv = mv->next; // moves != NULL --> mv != NULL

    while ( mv )
    {
        if ( !mv->plies ) return NULL;

        CcPly * p = mv->plies;
        while ( p )
        {
            /* result = result && */ cc_print_ply( cb, mv, p );
            p = p->next;
        }

        if ( do_spec != CC_PME_PrintAllMoves ) break;

        mv = mv->next;
    }

    return NULL;
}
