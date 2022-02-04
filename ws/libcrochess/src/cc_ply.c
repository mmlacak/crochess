// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"

/**
    @file cc_ply.c
    @brief Functions for plies linked list.
*/


char const * cc_ply_link_symbol( CcPlyLinkEnum ple )
{
    switch ( ple )
    {
        case CC_PLE_Ply : return "~";
        case CC_PLE_Teleportation : return "|";
        case CC_PLE_FailedTeleportation : return "||";
        case CC_PLE_TranceJourney : return "@";
        case CC_PLE_DualTranceJourney : return "@@";
        case CC_PLE_FailedTranceJourney : return "@@@";
        case CC_PLE_PawnSacrifice : return ":::";

        default : return NULL; // gcc complains otherwise, it's good default anyway.
    }
}


CcPly * cc_ply__new( CcPlyLinkEnum link,
                     CcPieceEnum piece,
                     CcStep ** restrict steps__n )
{
    CcPly * ply__a = malloc( sizeof( CcPly ) );
    if ( !ply__a ) return NULL;

    ply__a->link = link;
    ply__a->piece = piece;

    if ( steps__n )
    {
        ply__a->steps = *steps__n;
        *steps__n = NULL;
    }
    else
        ply__a->steps = NULL;

    ply__a->next = NULL;

    return ply__a;
}

CcPly * cc_ply_append( CcPly * restrict plies__io,
                       CcPlyLinkEnum link,
                       CcPieceEnum piece,
                       CcStep ** restrict steps__n )
{
    if ( !plies__io ) return NULL;

    CcPly * ply__t = cc_ply__new( link, piece, steps__n );
    if ( !ply__t ) return NULL;

    CcPly * p = plies__io;
    while ( p->next ) p = p->next; // rewind
    p->next = ply__t; // append // Ownership transfer --> ply__t is now weak pointer.

    return ply__t;
}

CcPly * cc_ply_append_or_init( CcPly ** restrict plies__io,
                               CcPlyLinkEnum link,
                               CcPieceEnum piece,
                               CcStep ** restrict steps__n )
{
    if ( !plies__io ) return NULL;

    CcPly * ply__w = NULL;

    if ( !*plies__io )
        *plies__io = ply__w = cc_ply__new( link, piece, steps__n );
    else
        ply__w = cc_ply_append( *plies__io, link, piece, steps__n );

    return ply__w;
}

CcPly * cc_plies_duplicate_all__new( CcPly * restrict plies )
{
    if ( !plies ) return NULL;

    CcPly * ply__a = NULL;
    CcPly * from = plies;

    do
    {
        CcStep * steps__t = cc_steps_duplicate_all__new( from->steps );
        if ( !steps__t )
        {
            cc_plies_free_all( &ply__a );
            return NULL;
        }

        CcPly * ply__w = cc_ply_append_or_init( &ply__a, from->link, from->piece, &steps__t );
        if ( !ply__w )
        {
            cc_steps_free_all( &steps__t ); // Failed append --> ownership not transferred ...
            cc_plies_free_all( &ply__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return ply__a;
}

bool cc_plies_free_all( CcPly ** restrict plies__f )
{
    if ( !plies__f ) return false;
    if ( !*plies__f ) return true;

    bool result = true;
    CcPly * ply = *plies__f;

    while ( ply )
    {
        CcStep ** steps = &( ply->steps );
        result = cc_steps_free_all( steps ) && result;

        CcPly * tmp = ply->next;
        CC_FREE( ply );
        ply = tmp;
    }

    *plies__f = NULL;
    return result;
}


bool cc_ply_contains_side_effects( CcPly * restrict ply )
{
    if ( !ply ) return false;
    if ( !ply->steps ) return false;

    CcStep * s = ply->steps;
    while ( s->next )
    {
        if ( s->side_effect.type != CC_SEE_None ) return true;
        s = s->next;
    }

    return false;
}

size_t cc_ply_step_count( CcPly * restrict ply,
                          CcFormatStepUsageEnum usage,
                          bool include_starting_pos )
{
    if ( !ply ) return 0;

    CcStep * steps = ply->steps;
    if ( !steps ) return 0;

    size_t count = 1;
    CcStep * s = steps;

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
