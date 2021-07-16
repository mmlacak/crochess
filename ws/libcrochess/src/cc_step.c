// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"


CcSideEffect cc_side_effect( CcSideEffectEnum type,
                             CcPieceEnum piece,
                             bool is_promo_tag_lost,
                             int start_i, int start_j,
                             int dest_i, int dest_j )
{
    CcSideEffect sse = { .type = type, };

    // Nothing more to do if type == CC_SEE_None.
    if ( sse.type == CC_SEE_Capture )
    {
        sse.capture.piece = piece;
        sse.capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( sse.type == CC_SEE_Displacement )
    {
        sse.displacement.piece = piece;
        sse.displacement.is_promo_tag_lost = is_promo_tag_lost;
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
        sse.convert.is_promo_tag_lost = is_promo_tag_lost;
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
    return cc_side_effect( CC_SEE_None, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcSideEffect cc_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost )
{
    return cc_side_effect( CC_SEE_Capture, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Displacement, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcSideEffect cc_side_effect_en_passant( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_EnPassant, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcSideEffect cc_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Castle, rook, false, start_i, start_j, dest_i, dest_j );
}

CcSideEffect cc_side_effect_promote( CcPieceEnum piece )
{
    return cc_side_effect( CC_SEE_Promotion, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcSideEffect cc_side_effect_tag_for_promotion()
{
    return cc_side_effect( CC_SEE_TagForPromotion, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcSideEffect cc_side_effect_convert( CcPieceEnum piece, bool is_promo_tag_lost )
{
    return cc_side_effect( CC_SEE_Conversion, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcSideEffect cc_side_effect_failed_conversion()
{
    return cc_side_effect( CC_SEE_FailedConversion, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcSideEffect cc_side_effect_demote( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Demotion, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j )
{
    return cc_side_effect( CC_SEE_Resurrection, piece, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, dest_i, dest_j );
}

CcSideEffect cc_side_effect_failed_resurrection()
{
    return cc_side_effect( CC_SEE_FailedResurrection, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}


CcStep * cc_step__new( CcStepLinkEnum link,
                       int i, int j, CcSideEffect side_effect,
                       CcFormatStepUsageEnum usage )
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

CcStep * cc_step_append__new( CcStep * const restrict steps,
                              CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                              CcFormatStepUsageEnum usage )
{
    CcStep * new = cc_step__new( link, i, j, side_effect, usage );
    if ( !new ) return NULL;
    if ( !steps ) return new;

    CcStep * s = steps;
    while ( s->next ) s = s->next; // rewind
    s->next = new; // append

    return new;
}

bool cc_step_free_all_steps( CcStep ** const steps )
{
    if ( !steps ) return true;
    if ( !*steps ) return false;

    CcStep * s = *steps;

    while ( s )
    {
        CcStep * tmp = s->next;
        free( s );
        s = tmp;
    }

    *steps = NULL;
    return true;
}


// new

CcStep * cc_step_none__new( CcStepLinkEnum link, int i, int j,
                            CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_capture__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, bool is_promo_tag_lost,
                               CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, is_promo_tag_lost );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_displacement__new( CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, is_promo_tag_lost, dest_i, dest_j );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, int i, int j,
                                  CcPieceEnum piece, int dest_i, int dest_j,
                                  CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_castle__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_promote__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece,
                               CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, int i, int j,
                                         CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_convert__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, bool is_promo_tag_lost,
                               CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, is_promo_tag_lost );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, int i, int j,
                                         CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_demote__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, int dest_i, int dest_j,
                              CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, dest_i, dest_j );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j,
                                 CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step__new( link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step__new( link, i, j, se, usage );
}


// append

CcStep * cc_step_none_append__new( CcStep * const restrict steps,
                                   CcStepLinkEnum link, int i, int j,
                                   CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_none();
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_capture_append__new( CcStep * const restrict steps,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, bool is_promo_tag_lost,
                                      CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_capture( piece, is_promo_tag_lost );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_displacement_append__new( CcStep * const restrict steps,
                                           CcStepLinkEnum link, int i, int j,
                                           CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j,
                                           CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_displacement( piece, is_promo_tag_lost, dest_i, dest_j );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_en_passant_append__new( CcStep * const restrict steps,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, int dest_i, int dest_j,
                                         CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_en_passant( piece, dest_i, dest_j );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_castle_append__new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                     CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_castle( rook, start_i, start_j, dest_i, dest_j );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_promote_append__new( CcStep * const restrict steps,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece,
                                      CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_promote( piece );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_tag_for_promotion_append__new( CcStep * const restrict steps,
                                                CcStepLinkEnum link, int i, int j,
                                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_tag_for_promotion();
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_convert_append__new( CcStep * const restrict steps,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, bool is_promo_tag_lost,
                                      CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_convert( piece, is_promo_tag_lost );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_failed_conversion_append__new( CcStep * const restrict steps,
                                                CcStepLinkEnum link, int i, int j,
                                                CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_conversion();
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_demote_append__new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum piece, int dest_i, int dest_j,
                                     CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_demote( piece, dest_i, dest_j );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_resurrect_append__new( CcStep * const restrict steps,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum piece, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_resurrect( piece, dest_i, dest_j );
    return cc_step_append__new( steps, link, i, j, se, usage );
}

CcStep * cc_step_failed_resurrection_append__new( CcStep * const restrict steps,
                                                  CcStepLinkEnum link, int i, int j,
                                                  CcFormatStepUsageEnum usage )
{
    CcSideEffect se = cc_side_effect_failed_resurrection();
    return cc_step_append__new( steps, link, i, j, se, usage );
}


size_t cc_step_count_usage( CcStep const * const restrict steps, CcFormatStepUsageEnum usage )
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
