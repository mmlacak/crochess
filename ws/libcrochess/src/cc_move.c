// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_move.h"

/**
    @file cc_move.c
    @brief Move enumeration, structure, and related functions.
*/


CcMove * cc_move_new( char const * const restrict notation,
                      CcPly ** const restrict plies__n,
                      CcMoveStatusEnum const status )
{
    CcMove * mv__t = malloc( sizeof( CcMove ) );
    if ( !mv__t ) return NULL;

    mv__t->notation = cc_str_duplicate_new( notation, false, BUFSIZ );
    if ( notation && ( !mv__t->notation ) )
    {
        CC_FREE( mv__t );
        return NULL;
    }

    if ( plies__n )
    {
        mv__t->plies = *plies__n;
        *plies__n = NULL; // Taking ownership.
    }
    else
        mv__t->plies = NULL;

    mv__t->status = status;
    mv__t->next = NULL;

    return mv__t;
}

CcMove * cc_move_append( CcMove * const restrict moves__io,
                         char const * const restrict notation,
                         CcPly ** const restrict plies__n,
                         CcMoveStatusEnum const status )
{
    if ( !moves__io ) return NULL;

    CcMove * new = cc_move_new( notation, plies__n, status );
    if ( !new ) return NULL;

    CcMove * mv = moves__io;
    while ( mv->next ) mv = mv->next; // rewind
    mv->next = new; // append

    return new;
}

bool cc_move_extend_or_init( CcMove ** const restrict moves__io,
                             CcMove ** const restrict moves__n )
{
    if ( !moves__io ) return false;
    if ( !moves__n ) return false;
    if ( !*moves__n ) return false;

    if ( *moves__io )
    {
        CcMove * mv = *moves__io;
        while ( mv->next ) mv = mv->next; // rewind
        mv->next = *moves__n; // append
    }
    else
        *moves__io = *moves__n;

    *moves__n = NULL;

    return true;
}

CcMove * cc_move_duplicate_all_new( CcMove const * const restrict moves )
{
    if ( !moves ) return NULL;

    CcPly * plies__t = cc_ply_duplicate_all_new( moves->plies );
    if ( !plies__t ) return NULL;

    CcMove * new__o = cc_move_new( moves->notation, &plies__t, moves->status );
    if ( !new__o )
    {
        cc_ply_free_all_plies( CC_CAST_TC_P_PC( CcPly, &plies__t ) );
        return NULL;
    }

    CcMove const * from = moves->next;

    while ( from )
    {
        CcPly * p__t = cc_ply_duplicate_all_new( from->plies );
        if ( !p__t )
        {
            cc_move_free_all_moves( CC_CAST_TC_P_PC( CcMove, &new__o ) );
            return NULL;
        }

        CcMove * n__w = cc_move_append( new__o, from->notation, &p__t, from->status );
        if ( !n__w )
        {
            cc_ply_free_all_plies( CC_CAST_TC_P_PC( CcPly, &p__t ) );
            cc_move_free_all_moves( CC_CAST_TC_P_PC( CcMove, &new__o ) );
            return NULL;
        }

        from = from->next;
    }

    return new__o;
}

bool cc_move_free_all_moves( CcMove const ** const restrict moves__f )
{
    if ( !moves__f ) return false;
    if ( !*moves__f ) return true;

    bool result = true;

    CcMove const * mv = *moves__f;

    while ( mv )
    {
        CC_FREE( mv->notation );

        CcPly const ** const plies = CC_CAST_TC_P_PC( CcPly, &( mv->plies ) );
        result = result && cc_ply_free_all_plies( plies );

        CcMove * tmp = mv->next;
        CC_FREE( mv );
        mv = tmp;
    }

    *moves__f = NULL;
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
