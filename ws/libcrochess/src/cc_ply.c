// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"

/**
    @file cc_ply.c
    @brief Ply related functions.
*/


char const * cc_ply_link_symbol( CcPlyLinkEnum const ple )
{
    switch ( ple )
    {
        case CC_PLE_Ply : return "~";
        case CC_PLE_Teleportation : return "|";
        case CC_PLE_FailedTeleportation : return "||";
        case CC_PLE_TranceJourney : return "@";
        case CC_PLE_DualTranceJourney : return "@@";
        case CC_PLE_FailedTranceJourney : return "@@@";
        case CC_PLE_PawnSacrifice : return "::";

        default : return NULL; // gcc complains otherwise, it's good default anyway.
    }
}


CcPly * cc_ply_new( CcPlyLinkEnum const link,
                    CcPieceEnum const piece,
                    CcStep ** const restrict steps__n )
{
    CcPly * ply = calloc( 1, sizeof( CcPly ) );
    if ( !ply ) return NULL;

    ply->link = link;
    ply->piece = piece;

    if ( steps__n )
    {
        ply->steps = *steps__n;
        *steps__n = NULL;
    }
    else
        ply->steps = NULL;

    ply->next = NULL;

    return ply;
}

CcPly * cc_ply_append( CcPly * const restrict plies,
                       CcPlyLinkEnum const link,
                       CcPieceEnum const piece,
                       CcStep ** const restrict steps__n )
{
    if ( !plies ) return NULL;

    CcPly * new = cc_ply_new( link, piece, steps__n );
    if ( !new ) return NULL;

    CcPly * p = plies;
    while ( p->next ) p = p->next; // rewind
    p->next = new; // append

    return new;
}

CcPly * cc_ply_duplicate_all_new( CcPly const * const restrict plies )
{
    if ( !plies ) return NULL;

    CcStep * steps__t = cc_step_duplicate_all_new( plies->steps );
    if ( !steps__t ) return NULL;

    CcPly * new__o = cc_ply_new( plies->link, plies->piece, &steps__t );
    if ( !new__o )
    {
        cc_step_free_all_steps( &steps__t );
        return NULL;
    }

    CcPly const * from = plies->next;

    while ( from )
    {
        CcStep * s__t = cc_step_duplicate_all_new( from->steps );
        if ( !s__t )
        {
            cc_ply_free_all_plies( &new__o );
            return NULL;
        }

        CcPly * n__w = cc_ply_append( new__o, from->link, from->piece, &s__t );
        if ( !n__w )
        {
            cc_step_free_all_steps( &s__t );
            cc_ply_free_all_plies( &new__o );
            return NULL;
        }

        from = from->next;
    }

    return new__o;
}

bool cc_ply_free_all_plies( CcPly ** const restrict plies__f )
{
    if ( !plies__f ) return false;
    if ( !*plies__f ) return true;

    bool result = true;
    CcPly * ply = *plies__f;

    while ( ply )
    {
        CcStep ** steps = &( ply->steps );
        result = result && cc_step_free_all_steps( steps );

        CcPly * tmp = ply->next;
        free( ply );
        ply = tmp;
    }

    *plies__f = NULL;
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

size_t cc_ply_step_count( CcPly const * const restrict ply,
                          CcFormatStepUsageEnum const usage,
                          bool const include_starting_pos )
{
    if ( !ply ) return 0;

    CcStep const * steps = ply->steps;
    if ( !steps ) return 0;

    size_t count = 1;
    CcStep const * s = steps;

    while ( s->next )
    {
        if ( s->usage <= usage )
        {
            if ( s->link == CC_SLE_Start )
            {
                if ( include_starting_pos ) ++count;
            }
            else
                ++count;
        }

        s = s->next;
    }

    return count;
}
