// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_ply.h"

/**
    @file cc_ply.c
    @brief Ply, plies linked list functions.
*/


char const * cc_ply_link_symbol( CcPlyLinkEnum ple )
{
    switch ( ple )
    {
        case CC_PLE_None : return NULL;
        case CC_PLE_StartingPly : return "";
        case CC_PLE_CascadingPly : return "~";
        case CC_PLE_Teleportation : return "|";
        case CC_PLE_FailedTeleportation : return "||";
        case CC_PLE_TranceJourney : return "@";
        case CC_PLE_DualTranceJourney : return "@@";
        case CC_PLE_FailedTranceJourney : return "@@@";
        case CC_PLE_PawnSacrifice : return ";;";

        default : return NULL;
    }
}


CcPly * cc_ply__new( char const * restrict start_an__d,
                     char const * restrict end_an__d,
                     size_t max_len__d,
                     CcPlyLinkEnum link,
                     CcPieceEnum piece,
                     CcStep ** restrict steps__n )
{
    CcPly * ply__a = malloc( sizeof( CcPly ) );
    if ( !ply__a ) return NULL;

    ply__a->notation = cc_str_copy__new( start_an__d, end_an__d, max_len__d );

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
                       char const * restrict start_an__d,
                       char const * restrict end_an__d,
                       size_t max_len__d,
                       CcPlyLinkEnum link,
                       CcPieceEnum piece,
                       CcStep ** restrict steps__n )
{
    if ( !plies__io ) return NULL;

    CcPly * ply__t = cc_ply__new( start_an__d, end_an__d, max_len__d, link, piece, steps__n );
    if ( !ply__t ) return NULL;

    CcPly * p = plies__io;
    while ( p->next ) p = p->next; // rewind
    p->next = ply__t; // append // Ownership transfer --> ply__t is now weak pointer.

    return ply__t;
}

CcPly * cc_ply_append_if( CcPly ** restrict plies__io,
                          char const * restrict start_an__d,
                          char const * restrict end_an__d,
                          size_t max_len__d,
                          CcPlyLinkEnum link,
                          CcPieceEnum piece,
                          CcStep ** restrict steps__n )
{
    if ( !plies__io ) return NULL;

    CcPly * ply__w = NULL;

    if ( !*plies__io )
        *plies__io = ply__w = cc_ply__new( start_an__d,
                                           end_an__d,
                                           max_len__d,
                                           link,
                                           piece,
                                           steps__n );
    else
        ply__w = cc_ply_append( *plies__io,
                                start_an__d,
                                end_an__d,
                                max_len__d,
                                link,
                                piece,
                                steps__n );

    return ply__w;
}

CcPly * cc_ply_duplicate_all__new( CcPly * restrict plies )
{
    if ( !plies ) return NULL;

    CcPly * ply__a = NULL;
    CcPly * from = plies;

    do
    {
        CcStep * steps__t = cc_step_duplicate_all__new( from->steps );
        if ( !steps__t )
        {
            cc_ply_free_all( &ply__a );
            return NULL;
        }

        CcPly * ply__w = cc_ply_append_if( &ply__a,
                                           from->notation,
                                           NULL,
                                           CC_MAX_LEN_ZERO_TERMINATED,
                                           from->link,
                                           from->piece,
                                           &steps__t );
        if ( !ply__w )
        {
            cc_step_free_all( &steps__t ); // Failed append --> ownership not transferred ...
            cc_ply_free_all( &ply__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return ply__a;
}

// TODO :: reconsider
//
bool cc_ply_is_valid( CcPly * restrict ply, unsigned int board_size )
{
    if ( !ply ) return false;

    if ( ply->piece == CC_PE_None ) return false;

    if ( ply->link == CC_PLE_StartingPly )
    {
        if ( !ply->steps ) return false;
    }
    else if ( ply->link == CC_PLE_CascadingPly )
    {
        if ( !ply->steps ) return false;
    }
    else if ( ply->link == CC_PLE_Teleportation )
    {
        if ( !ply->steps ) return false;

        if ( ( ply->next ) && ( !CC_PIECE_IS_WAVE( ply->piece ) ) )
            return false;
        else if ( ( !ply->next ) && ( CC_PIECE_IS_WAVE( ply->piece ) ) )
            return false;
    }
    else if ( ply->link == CC_PLE_FailedTeleportation )
    {
        if ( ( ply->steps ) &&
             ( ( !CC_PIECE_IS_STARCHILD( ply->piece ) ) ||
               ( !CC_PIECE_IS_WAVE( ply->piece ) ) ) )
            // If Wave was activated by Starchild is checked in cc_ply_are_all_valid().
            return false;

        if ( ply->next ) return false;
    }
    else if ( ply->link == CC_PLE_TranceJourney )
    {
        if ( !ply->steps ) return false;
        if ( ply->next ) return false;
    }
    else if ( ply->link == CC_PLE_DualTranceJourney )
    {
        if ( ply->next ) return false;
    }
    else if ( ply->link == CC_PLE_FailedTranceJourney )
    {
        if ( ply->steps ) return false;
        if ( ply->next ) return false;
    }
    if ( ply->link == CC_PLE_PawnSacrifice )
    {
        if ( !ply->steps ) return false;
    }
    else
        return false;

    if ( !cc_step_are_all_valid( ply->steps, board_size ) ) return false;

    return true;
}
//
// TODO :: reconsider
//
bool cc_ply_are_all_valid( CcPly * restrict plies, unsigned int board_size )
{
    if ( !plies ) return false;

    // Start of a plies.
    if ( !CC_PIECE_IS_ACTIVE( plies->piece ) ) return false;
    if ( plies->link != CC_PLE_StartingPly ) return false;

    CcPieceEnum prev_piece = CC_PE_None;
    CcPieceEnum last_active_piece = CC_PE_None;
    CcPly * p = plies;

    while ( p )
    {
        if ( CC_PIECE_IS_ACTIVE( p->piece ) )
            last_active_piece = p->piece;

// TODO :: add last active piece checks for activated pieces in cascades

        if ( ( !CC_PIECE_CAN_BE_ACTIVATED( p->piece ) ) ) // Kings, Monolith cannot be activated, ...
            if ( p != plies ) return false; // ... so, can't be in the middle of a cascade.

        // if ( CC_PIECE_IS_PYRAMID( p->piece ) )
        // {
        //     // <*> If last active piece was Pawn, Starchild or Shaman, it can't activate
        //     //     Pyramid on its step-fields, neither directly, nor via Wave(s). Also,
        //     //     Serpent can't activate Pyramid on its color-changing ply.
        //     //
        //     //     Legality check, needs positions --> deferred to rules.
        // }

        if ( !cc_ply_is_valid( p, board_size ) )
            return false;

        if ( p->link == CC_PLE_FailedTeleportation )
        {
            if ( ( p->steps ) && ( CC_PIECE_IS_WAVE( p->piece ) ) )
                if ( !CC_PIECE_IS_STARCHILD( last_active_piece ) )
                    return false;
        }

        prev_piece = p->piece;
        p = p->next;
    }

    return true;
}
//
// TODO :: reconsider

bool cc_ply_free_all( CcPly ** restrict plies__f )
{
    if ( !plies__f ) return false;
    if ( !*plies__f ) return true;

    bool result = true;
    CcPly * ply = *plies__f;

    while ( ply )
    {
        CC_FREE( ply->notation );

        CcStep ** steps = &( ply->steps );
        result = cc_step_free_all( steps ) && result;

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
                          bool include_starting_pos )
{
    if ( !ply ) return 0;

    CcStep * steps = ply->steps;
    if ( !steps ) return 0;

    size_t count = 1;
    CcStep * s = steps;

    while ( s->next )
    {
        if ( s->link == CC_SLE_Start )
        {
            if ( include_starting_pos ) ++count;
        }
        else
            ++count;

        s = s->next;
    }

    return count;
}

CcPieceEnum cc_ply_last_active_piece( CcPly * restrict plies,
                                      CcPly * restrict ply__d )
{
    if ( !plies ) return CC_PE_None;

    if ( plies == ply__d ) // First ply in a linked list.
        return CC_PIECE_IS_ACTIVE( plies->piece ) ? plies->piece
                                                  : CC_PE_None;

    // <!> Shadows issue if ply is not contained in plies.
    //
    // if ( ply__d && CC_PIECE_IS_ACTIVE( ply__d->piece ) )
    //     return ply__d->piece;

    CcPieceEnum last_active_piece = CC_PE_None;
    bool ply_encountered = ( !ply__d );
    CcPly * p = plies;

    while ( p )
    {
        if ( CC_PIECE_IS_ACTIVE( p->piece ) )
            last_active_piece = p->piece;

        if ( p == ply__d )
        {
            ply_encountered = true;
            break;
        }

        p = p->next;
    }

    return ply_encountered ? last_active_piece : CC_PE_None;
}
