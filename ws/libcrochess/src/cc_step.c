// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"

/**
    @file cc_step.c
    @brief Step related functions.
*/


char const * cc_step_link_symbol( CcStepLinkEnum sle )
{
    switch ( sle )
    {
        case CC_SLE_None : return NULL;
        case CC_SLE_Start : return "";
        case CC_SLE_Reposition : return ",";
        case CC_SLE_Next : return ".";
        case CC_SLE_Distant : return "..";
        case CC_SLE_Destination : return "-";

        default : return NULL;
    }
}

CcStep * cc_step__new( CcStepLinkEnum link,
                       int i, int j, CcSideEffect side_effect )
{
    CcStep * step__a = malloc( sizeof( CcStep ) );
    if ( !step__a ) return NULL;

    step__a->link = link;
    step__a->i = i;
    step__a->j = j;
    step__a->side_effect = side_effect;
    step__a->next = NULL;

    return step__a;
}

CcStep * cc_step_append( CcStep * restrict steps__io,
                         CcStepLinkEnum link, int i, int j, CcSideEffect side_effect )
{
    if ( !steps__io ) return NULL;

    CcStep * step__t = cc_step__new( link, i, j, side_effect );
    if ( !step__t ) return NULL;

    CcStep * s = steps__io;
    while ( s->next ) s = s->next; // rewind
    s->next = step__t; // append // Ownership transfer --> step__t is now weak pointer.

    return step__t;
}

CcStep * cc_step_append_if( CcStep ** restrict steps__io,
                            CcStepLinkEnum link, int i, int j, CcSideEffect side_effect )
{
    if ( !steps__io ) return NULL;

    CcStep * step__w = NULL;

    if ( !*steps__io )
        *steps__io = step__w = cc_step__new( link, i, j, side_effect );
    else
        step__w = cc_step_append( *steps__io, link, i, j, side_effect );

    return step__w;
}

// TODO :: REWRITE :: using cc_step_append_if
CcStep * cc_step_duplicate_all__new( CcStep * restrict steps__io )
{
    if ( !steps__io ) return NULL;

    CcStep * steps__a = cc_step__new( steps__io->link,
                                      steps__io->i,
                                      steps__io->j,
                                      steps__io->side_effect );
    if ( !steps__a ) return NULL;

    CcStep * from = steps__io->next;

    while ( from )
    {
        CcStep * step__w = cc_step_append( steps__a,
                                           from->link,
                                           from->i,
                                           from->j,
                                           from->side_effect );
        if ( !step__w )
        {
            cc_step_free_all( &steps__a );
            return NULL;
        }

        from = from->next;
    }

    return steps__a;
}
// TODO :: REWRITE :: using cc_step_append_if

bool cc_step_is_valid( CcStep * restrict step, unsigned int board_size )
{
    if ( !step ) return false;

    if ( !CC_IS_COORD_2_ON_BOARD( board_size, step->i, step->j ) ) return false;

    if ( !cc_side_effect_is_valid( step->side_effect, board_size ) ) return false;

    return true;
}

bool cc_step_all_are_valid( CcStep * restrict steps, unsigned int board_size )
{
    if ( !steps ) return false;

    if ( !cc_step_is_valid( steps, board_size ) ) return false;
    if ( !steps->next ) return ( steps->link != CC_SLE_Destination ); // The only step must be destination.

    bool is_starting = ( steps->link == CC_SLE_Start );
    bool is_repositioning = ( steps->link == CC_SLE_Reposition );

    CcStep * s = steps->next;
    while ( s )
    {
        if ( s->link == CC_SLE_Start ) return false; // Only first step can be starting.
        if ( ( s->link == CC_SLE_Destination ) && ( s->next ) ) return false; // Destination step must not be in the middle.
        if ( ( !s->next ) && ( s->link != CC_SLE_Destination ) ) return false; // The last step must be destination.

        if ( s->link == CC_SLE_Reposition ) // Repositioning can be only on first step or second step, if following starting step.
        {
            if ( is_repositioning ) return false; // Already repositioning, but it can be only one.

            if ( s != steps->next ) return false; // If not on second step, repositioning is misplaced.
            if ( !is_starting ) return false; // Repositioning is on second step, but not following starting step.
        }

        if ( !cc_step_is_valid( s, board_size ) ) return false;

        s = s->next;
    }

    return true;
}

bool cc_step_free_all( CcStep ** restrict steps__f )
{
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcStep * s = *steps__f;

    while ( s )
    {
        CcStep * tmp = s->next;
        CC_FREE( s );
        s = tmp;
    }

    *steps__f = NULL;
    return true;
}

//
// new conveniences

CcStep * cc_step_none__new( CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_capture__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, CcTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_displacement__new( CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, int i, int j,
                                  CcPieceEnum piece, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_castle__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_promote__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_convert__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, CcTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_demote__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step__new( link, i, j, se );
}

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step__new( link, i, j, se );
}

//
// append conveniences

CcStep * cc_step_none_append( CcStep * restrict steps__io,
                              CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_capture_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_displacement_append( CcStep * restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_en_passant_append( CcStep * restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_castle_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_promote_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_tag_for_promotion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_convert_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_failed_conversion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_demote_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_resurrect_append( CcStep * restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se );
}

CcStep * cc_step_failed_resurrection_append( CcStep * restrict steps__io,
                                             CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append( steps__io, link, i, j, se );
}

//
// append or init conveniences

CcStep * cc_step_none_append_if( CcStep ** restrict steps__io,
                                 CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_capture_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_displacement_append_if( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_en_passant_append_if( CcStep ** restrict steps__io,
                                       CcStepLinkEnum link, int i, int j,
                                       CcPieceEnum piece, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_castle_append_if( CcStep ** restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_promote_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_tag_for_promotion_append_if( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_convert_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_failed_conversion_append_if( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_demote_append_if( CcStep ** restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_resurrect_append_if( CcStep ** restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, int dest_i, int dest_j )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append_if( steps__io, link, i, j, se );
}

CcStep * cc_step_failed_resurrection_append_if( CcStep ** restrict steps__io,
                                                CcStepLinkEnum link, int i, int j )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append_if( steps__io, link, i, j, se );
}


size_t cc_step_count( CcStep * restrict steps )
{
    if ( !steps ) return 0;

    size_t count = 0;
    CcStep * s = steps;

    while ( s )
    {
        ++count;
        s = s->next;
    }

    return count;
}
