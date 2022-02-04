// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
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
        case CC_SLE_Start : return "";
        case CC_SLE_Reposition : return ",";
        case CC_SLE_Next : return ".";
        case CC_SLE_Distant : return "..";
        case CC_SLE_Destination : return "-";

        default : return NULL; // gcc complains without default.
    }
}

CcStep * cc_step_new( CcStepLinkEnum link,
                      int i, int j, CcSideEffect side_effect,
                      CcFormatStepUsageEnum usage )
{
    CcStep * step__a = malloc( sizeof( CcStep ) );
    if ( !step__a ) return NULL;

    step__a->link = link;
    step__a->i = i;
    step__a->j = j;
    step__a->side_effect = side_effect;
    step__a->usage = usage;
    step__a->next = NULL;

    return step__a;
}

CcStep * cc_step_append( CcStep * restrict steps__io,
                         CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                         CcFormatStepUsageEnum usage )
{
    if ( !steps__io ) return NULL;

    CcStep * step__t = cc_step_new( link, i, j, side_effect, usage );
    if ( !step__t ) return NULL;

    CcStep * s = steps__io;
    while ( s->next ) s = s->next; // rewind
    s->next = step__t; // append // Ownership transfer --> step__t is now weak pointer.

    return step__t;
}

CcStep * cc_step_append_or_init( CcStep ** restrict steps__io,
                                 CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                                 CcFormatStepUsageEnum usage )
{
    if ( !steps__io ) return NULL;

    CcStep * step__w = NULL;

    if ( !*steps__io )
        *steps__io = step__w = cc_step_new( link, i, j, side_effect, usage );
    else
        step__w = cc_step_append( *steps__io, link, i, j, side_effect, usage );

    return step__w;
}

// TODO :: REWRITE :: using cc_step_append_or_init
CcStep * cc_step_duplicate_all_new( CcStep * restrict steps__io )
{
    if ( !steps__io ) return NULL;

    CcStep * steps__a = cc_step_new( steps__io->link, steps__io->i, steps__io->j, steps__io->side_effect, steps__io->usage );
    if ( !steps__a ) return NULL;

    CcStep * from = steps__io->next;

    while ( from )
    {
        CcStep * step__w = cc_step_append( steps__a, from->link, from->i, from->j, from->side_effect, from->usage );
        if ( !step__w )
        {
            cc_step_free_all_steps( &steps__a );
            return NULL;
        }

        from = from->next;
    }

    return steps__a;
}
// TODO :: REWRITE :: using cc_step_append_or_init

bool cc_step_is_valid( CcStep * restrict step, unsigned int board_size )
{
    if ( !step ) return false;

    if ( !CC_IS_POS_VALID( step->i, step->j ) ) return false;

    if ( !cc_side_effect_is_valid( step->side_effect, board_size ) ) return false;

    return true;
}

bool cc_steps_are_valid( CcStep * restrict steps, unsigned int board_size )
{
    if ( !steps ) return false;

    if ( !cc_step_is_valid( steps, board_size ) ) return false;
    if ( !steps->next ) return ( steps->link != CC_SLE_Reposition ); // The only step can't be repositioning.

    bool is_starting = ( steps->link == CC_SLE_Start );
    bool is_repositioning = ( steps->link == CC_SLE_Reposition );

    CcStep * s = steps->next;
    while ( s )
    {
        if ( s->link == CC_SLE_Start ) return false; // Starting step must be the first one.
        if ( ( s->link == CC_SLE_Destination ) && ( s->next ) ) return false; // Destination step must be the last one.

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

bool cc_step_free_all_steps( CcStep ** restrict steps__f )
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

CcStep * cc_step_none_new( CcStepLinkEnum link, int i, int j,
                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_capture_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_displacement_new( CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_en_passant_new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_castle_new( CcStepLinkEnum link, int i, int j,
                             CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                             CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_promote_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_new( CcStepLinkEnum link, int i, int j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_convert_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_new( CcStepLinkEnum link, int i, int j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_demote_new( CcStepLinkEnum link, int i, int j,
                             CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                             CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_resurrect_new( CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_new( CcStepLinkEnum link, int i, int j,
                                          CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_new( link, i, j, se, usage );
}

//
// append conveniences

CcStep * cc_step_none_append( CcStep * restrict steps__io,
                              CcStepLinkEnum link, int i, int j,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_capture_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_displacement_append( CcStep * restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                      CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_en_passant_append( CcStep * restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_castle_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_promote_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_convert_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_demote_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_resurrect_append( CcStep * restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, int dest_i, int dest_j,
                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_append( CcStep * restrict steps__io,
                                             CcStepLinkEnum link, int i, int j,
                                             CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append( steps__io, link, i, j, se, usage );
}

//
// append or init conveniences

CcStep * cc_step_none_append_or_init( CcStep ** restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_capture_append_or_init( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, CcTagEnum lost_tag,
                                         CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_displacement_append_or_init( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, int i, int j,
                                              CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_en_passant_append_or_init( CcStep ** restrict steps__io,
                                            CcStepLinkEnum link, int i, int j,
                                            CcPieceEnum piece, int dest_i, int dest_j,
                                            CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_castle_append_or_init( CcStep ** restrict steps__io,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_promote_append_or_init( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece,
                                         CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_append_or_init( CcStep ** restrict steps__io,
                                                   CcStepLinkEnum link, int i, int j,
                                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_convert_append_or_init( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, CcTagEnum lost_tag,
                                         CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_append_or_init( CcStep ** restrict steps__io,
                                                   CcStepLinkEnum link, int i, int j,
                                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_demote_append_or_init( CcStep ** restrict steps__io,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_resurrect_append_or_init( CcStep ** restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcPieceEnum piece, int dest_i, int dest_j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_append_or_init( CcStep ** restrict steps__io,
                                                     CcStepLinkEnum link, int i, int j,
                                                     CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append_or_init( steps__io, link, i, j, se, usage );
}


size_t cc_step_count_usage( CcStep * restrict steps,
                            CcFormatStepUsageEnum usage )
{
    if ( !steps ) return 0;

    size_t count = 0;
    CcStep * s = steps;

    while ( s )
    {
        if ( s->usage <= usage ) ++count;
        s = s->next;
    }

    return count;
}
