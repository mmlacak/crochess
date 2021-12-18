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


CcMove * cc_move_new( char const * restrict notation,
                      CcPly ** restrict plies__n,
                      CcMoveStatusEnum status )
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

CcMove * cc_move_append( CcMove * restrict moves__io,
                         char const * restrict notation,
                         CcPly ** restrict plies__n,
                         CcMoveStatusEnum status )
{
    if ( !moves__io ) return NULL;

    CcMove * new = cc_move_new( notation, plies__n, status );
    if ( !new ) return NULL;

    CcMove * mv = moves__io;
    while ( mv->next ) mv = mv->next; // rewind
    mv->next = new; // append

    return new;
}

bool cc_move_extend_or_init( CcMove ** restrict moves__io,
                             CcMove ** restrict moves__n )
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

CcMove * cc_move_duplicate_all_new( CcMove * restrict moves )
{
    if ( !moves ) return NULL;

    CcPly * plies__t = cc_ply_duplicate_all_new( moves->plies );
    if ( !plies__t ) return NULL;

    CcMove * new__o = cc_move_new( moves->notation, &plies__t, moves->status );
    if ( !new__o )
    {
        cc_ply_free_all_plies( &plies__t );
        return NULL;
    }

    CcMove * from = moves->next;

    while ( from )
    {
        CcPly * p__t = cc_ply_duplicate_all_new( from->plies );
        if ( !p__t )
        {
            cc_move_free_all_moves( &new__o );
            return NULL;
        }

        CcMove * n__w = cc_move_append( new__o, from->notation, &p__t, from->status );
        if ( !n__w )
        {
            cc_ply_free_all_plies( &p__t );
            cc_move_free_all_moves( &new__o );
            return NULL;
        }

        from = from->next;
    }

    return new__o;
}

bool cc_move_free_all_moves( CcMove ** restrict moves__f )
{
    if ( !moves__f ) return false;
    if ( !*moves__f ) return true;

    bool result = true;

    CcMove * mv = *moves__f;

    while ( mv )
    {
        CC_FREE( mv->notation );

        CcPly ** plies = &( mv->plies );
        result = result && cc_ply_free_all_plies( plies );

        CcMove * tmp = mv->next;
        CC_FREE( mv );
        mv = tmp;
    }

    *moves__f = NULL;
    return result;
}

size_t cc_move_ply_count( CcMove * restrict move )
{
    if ( !move ) return 0;
    if ( !move->plies ) return 0;

    size_t count = 1;
    CcPly * p = move->plies;

    while ( p->next )
    {
        ++count;
        p = p->next;
    }

    return count;
}
