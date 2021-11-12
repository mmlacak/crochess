// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"

/**
    @file cc_step.c
    @brief Step related functions.
*/


char const * cc_step_link_symbol( CcStepLinkEnum const sle )
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


bool cc_side_effect_enum_is_castling( CcSideEffectEnum const see )
{
    return ( see == CC_SEE_Castle );
}


CcSideEffect cc_side_effect( CcSideEffectEnum const type,
                             CcPieceEnum const piece,
                             CcTagEnum const lost_tag,
                             int const start_i, int const start_j,
                             int const dest_i, int const dest_j )
{
    CcSideEffect sse = { .type = type, };

    // Nothing more to do if type == CC_SEE_None.
    if ( sse.type == CC_SEE_Capture )
    {
        sse.capture.piece = piece;
        sse.capture.lost_tag = lost_tag;
    }
    else if ( sse.type == CC_SEE_Displacement )
    {
        sse.displacement.piece = piece;
        sse.displacement.lost_tag = lost_tag;
        sse.displacement.dest_i = dest_i;
        sse.displacement.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_EnPassant )
    {
        sse.en_passant.piece = piece;
        sse.en_passant.dest_i = dest_i;
        sse.en_passant.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_Castle )
    {
        sse.castle.rook = piece;
        sse.castle.start_i = start_i;
        sse.castle.start_j = start_j;
        sse.castle.dest_i = dest_i;
        sse.castle.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_Promotion )
    {
        sse.promote.piece = piece;
    }
    // Nothing more to do if type == CC_SEE_TagForPromotion.
    else if ( sse.type == CC_SEE_Conversion )
    {
        sse.convert.piece = piece;
        sse.convert.lost_tag = lost_tag;
    }
    // Nothing more to do if type == CC_SEE_FailedConversion.
    else if ( sse.type == CC_SEE_Demotion )
    {
        sse.demote.piece = piece;
        sse.demote.dest_i = dest_i;
        sse.demote.dest_j = dest_j;
    }
    else if ( sse.type == CC_SEE_Resurrection )
    {
        sse.resurrect.piece = piece;
        sse.resurrect.dest_i = dest_i;
        sse.resurrect.dest_j = dest_j;
    }
    // Nothing more to do if type == CC_SEE_FailedResurrection.

    return sse;
}

CcSideEffect cc_side_effect_none()
{
    return cc_side_effect( CC_SEE_None, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_capture( CcPieceEnum const piece, CcTagEnum const lost_tag )
{
    return cc_side_effect( CC_SEE_Capture, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_displacement( CcPieceEnum const piece, CcTagEnum const lost_tag, int const dest_i, int const dest_j )
{
    return cc_side_effect( CC_SEE_Displacement, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_en_passant( CcPieceEnum const piece, int const dest_i, int const dest_j )
{
    return cc_side_effect( CC_SEE_EnPassant, piece, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_castle( CcPieceEnum const rook, int const start_i, int const start_j, int const dest_i, int const dest_j )
{
    return cc_side_effect( CC_SEE_Castle, rook, CC_TE_None, start_i, start_j, dest_i, dest_j );
}

CcSideEffect cc_side_effect_promote( CcPieceEnum const piece )
{
    return cc_side_effect( CC_SEE_Promotion, piece, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_tag_for_promotion()
{
    return cc_side_effect( CC_SEE_TagForPromotion, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_convert( CcPieceEnum const piece, CcTagEnum const lost_tag )
{
    return cc_side_effect( CC_SEE_Conversion, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_failed_conversion()
{
    return cc_side_effect( CC_SEE_FailedConversion, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}

CcSideEffect cc_side_effect_demote( CcPieceEnum const piece, CcTagEnum lost_tag, int const dest_i, int const dest_j )
{
    return cc_side_effect( CC_SEE_Demotion, piece, lost_tag,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_resurrect( CcPieceEnum const piece, int const dest_i, int const dest_j )
{
    return cc_side_effect( CC_SEE_Resurrection, piece, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           dest_i,
                           dest_j );
}

CcSideEffect cc_side_effect_failed_resurrection()
{
    return cc_side_effect( CC_SEE_FailedResurrection, CC_PE_None, CC_TE_None,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN,
                           CC_INVALID_OFF_BOARD_COORD_MIN );
}


CcStep * cc_step_new( CcStepLinkEnum const link,
                      int const i, int const j, CcSideEffect const side_effect,
                      CcFormatStepUsageEnum const usage )
{
    CcStep * step = malloc( sizeof( CcStep ) );
    if ( !step ) return NULL;

    step->link = link;
    step->i = i;
    step->j = j;
    step->side_effect = side_effect;
    step->usage = usage;
    step->next = NULL;

    return step;
}

CcStep * cc_step_append( CcStep * const restrict steps,
                         CcStepLinkEnum const link, int const i, int const j, CcSideEffect const side_effect,
                         CcFormatStepUsageEnum const usage )
{
    if ( !steps ) return NULL;

    CcStep * new = cc_step_new( link, i, j, side_effect, usage );
    if ( !new ) return NULL;

    CcStep * s = steps;
    while ( s->next ) s = s->next; // rewind
    s->next = new; // append

    return new;
}

CcStep * cc_step_append_or_init( CcStep ** const restrict steps_io,
                                 CcStepLinkEnum const link, int const i, int const j, CcSideEffect const side_effect,
                                 CcFormatStepUsageEnum const usage )
{
    if ( !steps_io ) return NULL;

    CcStep * new = NULL;

    if ( !*steps_io )
        *steps_io = new = cc_step_new( link, i, j, side_effect, usage );
    else
        new = cc_step_append( *steps_io, link, i, j, side_effect, usage );

    return new;
}

CcStep * cc_step_duplicate_all_new( CcStep const * const restrict steps )
{
    if ( !steps ) return NULL;

    CcStep * new__o = cc_step_new( steps->link, steps->i, steps->j, steps->side_effect, steps->usage );
    if ( !new__o ) return NULL;

    CcStep const * from = steps->next;

    while ( from )
    {
        CcStep * n__w = cc_step_append( new__o, from->link, from->i, from->j, from->side_effect, from->usage );
        if ( !n__w )
        {
            cc_step_free_all_steps( &new__o );
            return NULL;
        }

        from = from->next;
    }

    return new__o;
}

bool cc_step_free_all_steps( CcStep ** const restrict steps__f )
{
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcStep * s = *steps__f;

    while ( s )
    {
        CcStep * tmp = s->next;
        free( s );
        s = tmp;
    }

    *steps__f = NULL;
    return true;
}


// new

CcStep * cc_step_none_new( CcStepLinkEnum const link, int const i, int const j,
                           CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_capture_new( CcStepLinkEnum const link, int const i, int const j,
                              CcPieceEnum const piece, CcTagEnum const lost_tag,
                              CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_displacement_new( CcStepLinkEnum const link, int const i, int const j,
                                   CcPieceEnum const piece, CcTagEnum const lost_tag, int const dest_i, int const dest_j,
                                   CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_en_passant_new( CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece, int const dest_i, int const dest_j,
                                 CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_castle_new( CcStepLinkEnum const link, int const i, int const j,
                             CcPieceEnum const rook, int const start_i, int const start_j, int const dest_i, int const dest_j,
                             CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_promote_new( CcStepLinkEnum const link, int const i, int const j,
                              CcPieceEnum const piece,
                              CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_new( CcStepLinkEnum const link, int const i, int const j,
                                        CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_convert_new( CcStepLinkEnum const link, int const i, int const j,
                              CcPieceEnum const piece, CcTagEnum const lost_tag,
                              CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_new( CcStepLinkEnum const link, int const i, int const j,
                                        CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_demote_new( CcStepLinkEnum const link, int const i, int const j,
                             CcPieceEnum const piece, CcTagEnum lost_tag, int const dest_i, int const dest_j,
                             CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_resurrect_new( CcStepLinkEnum const link, int const i, int const j,
                                CcPieceEnum const piece, int const dest_i, int const dest_j,
                                CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_new( link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_new( CcStepLinkEnum const link, int const i, int const j,
                                          CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_new( link, i, j, se, usage );
}


// append

CcStep * cc_step_none_append( CcStep * const restrict steps,
                              CcStepLinkEnum const link, int const i, int const j,
                              CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_capture_append( CcStep * const restrict steps,
                                 CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece, CcTagEnum const lost_tag,
                                 CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, lost_tag );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_displacement_append( CcStep * const restrict steps,
                                      CcStepLinkEnum const link, int const i, int const j,
                                      CcPieceEnum const piece, CcTagEnum const lost_tag, int const dest_i, int const dest_j,
                                      CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_en_passant_append( CcStep * const restrict steps,
                                    CcStepLinkEnum const link, int const i, int const j,
                                    CcPieceEnum const piece, int const dest_i, int const dest_j,
                                    CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_castle_append( CcStep * const restrict steps,
                                CcStepLinkEnum const link, int const i, int const j,
                                CcPieceEnum const rook, int const start_i, int const start_j, int const dest_i, int const dest_j,
                                CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_promote_append( CcStep * const restrict steps,
                                 CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece,
                                 CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_append( CcStep * const restrict steps,
                                           CcStepLinkEnum const link, int const i, int const j,
                                           CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_convert_append( CcStep * const restrict steps,
                                 CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece, CcTagEnum const lost_tag,
                                 CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, lost_tag );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_append( CcStep * const restrict steps,
                                           CcStepLinkEnum const link, int const i, int const j,
                                           CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_demote_append( CcStep * const restrict steps,
                                CcStepLinkEnum const link, int const i, int const j,
                                CcPieceEnum const piece, CcTagEnum lost_tag, int const dest_i, int const dest_j,
                                CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, lost_tag, dest_i, dest_j );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_resurrect_append( CcStep * const restrict steps,
                                   CcStepLinkEnum const link, int const i, int const j,
                                   CcPieceEnum const piece, int const dest_i, int const dest_j,
                                   CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append( steps, link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_append( CcStep * const restrict steps,
                                             CcStepLinkEnum const link, int const i, int const j,
                                             CcFormatStepUsageEnum const usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append( steps, link, i, j, se, usage );
}


size_t cc_step_count_usage( CcStep const * const restrict steps,
                            CcFormatStepUsageEnum const usage )
{
    if ( !steps ) return 0;

    size_t count = 0;
    CcStep const * s = steps;

    while ( s )
    {
        if ( s->usage <= usage ) ++count;
        s = s->next;
    }

    return count;
}
