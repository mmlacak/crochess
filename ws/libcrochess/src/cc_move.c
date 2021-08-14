// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_str_utils.h"

#include "cc_move.h"

/**
    @file cc_move.c
    @brief Move enumeration, structure, and related functions.
*/


CcMove * cc_move_new( char const * const restrict notation,
                      CcPly ** const restrict plies_n,
                      CcMoveStatusEnum const status )
{
    CcMove * mv = malloc( sizeof( CcMove ) );
    if ( !mv ) return NULL;

    mv->notation = cc_str_duplicate_len_new( notation, BUFSIZ );

    if ( plies_n )
    {
        mv->plies = *plies_n;
        *plies_n = NULL; // Taking ownership.
    }
    else
        mv->plies = NULL;

    mv->status = status;
    mv->next = NULL;

    return mv;
}

CcMove * cc_move_append_new( CcMove * const restrict moves,
                             char const * const restrict notation,
                             CcPly ** const restrict plies_n,
                             CcMoveStatusEnum const status )
{
    if ( !moves ) return NULL;

    CcMove * new = cc_move_new( notation, plies_n, status );
    if ( !new ) return NULL;

    CcMove * mv = moves;
    while ( mv->next ) mv = mv->next; // rewind
    mv->next = new; // append

    return new;
}

bool cc_move_free_all_moves( CcMove ** const restrict moves_f )
{
    if ( !moves_f ) return false;
    if ( !*moves_f ) return true;

    bool result = true;

    CcMove * mv = *moves_f;

    while ( mv )
    {
        free( (char *)mv->notation ); // free() doesn't do pointers to const.

        CcPly ** plies = &( mv->plies );
        result = result && cc_ply_free_all_plies( plies );

        CcMove * tmp = mv->next;
        free( mv );
        mv = tmp;
    }

    *moves_f = NULL;
    return result;
}

size_t cc_move_ply_count( CcMove const * const restrict move )
{
    if ( !move ) return 0;
    if ( !move->plies ) return 0;

    size_t count = 1;
    CcPly const * p = move->plies;

    while ( p->next )
    {
        ++count;
        p = p->next;
    }

    return count;
}
