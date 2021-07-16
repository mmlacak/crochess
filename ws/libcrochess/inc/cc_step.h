// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_STEP_H__
#define __CC_STEP_H__


#include <stdbool.h>

#include "cc_piece.h"


typedef enum CcStepLinkEnum
{
    CC_SLE_Start,
    CC_SLE_Next,
    CC_SLE_Distant,
    CC_SLE_Destination,
} CcStepLinkEnum;


typedef enum CcSideEffectEnum
{
    CC_SEE_None,
    CC_SEE_Capture,
    CC_SEE_Displacement,
    CC_SEE_EnPassant,
    CC_SEE_Castle,
    CC_SEE_Promotion,
    CC_SEE_TagForPromotion,
    CC_SEE_Conversion,
    CC_SEE_FailedConversion,
    CC_SEE_Demotion,
    CC_SEE_Resurrection,
    CC_SEE_FailedResurrection,
} CcSideEffectEnum;

typedef struct CcSideEffect
{
    CcSideEffectEnum type;

    union
    {
        struct { CcPieceEnum piece; bool is_promo_tag_lost; } capture;
        struct { CcPieceEnum piece; bool is_promo_tag_lost; int dest_i; int dest_j; } displacement;
        struct { CcPieceEnum piece; int dest_i; int dest_j; } en_passant;
        struct { CcPieceEnum rook; int start_i; int start_j; int dest_i; int dest_j; } castle;
        struct { CcPieceEnum piece; } promote;
        struct { CcPieceEnum piece; bool is_promo_tag_lost; } convert;
        struct { CcPieceEnum piece; int dest_i; int dest_j; } demote;
        struct { CcPieceEnum piece; int dest_i; int dest_j; } resurrect;
    };
} CcSideEffect;

CcSideEffect cc_side_effect( CcSideEffectEnum type,
                             CcPieceEnum piece,
                             bool is_promo_tag_lost,
                             int start_i, int start_j,
                             int dest_i, int dest_j );

CcSideEffect cc_side_effect_none();
CcSideEffect cc_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost );
CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j );
CcSideEffect cc_side_effect_en_passant( CcPieceEnum piece, int dest_i, int dest_j );
CcSideEffect cc_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );
CcSideEffect cc_side_effect_promote( CcPieceEnum piece );
CcSideEffect cc_side_effect_tag_for_promotion();
CcSideEffect cc_side_effect_convert( CcPieceEnum piece, bool is_promo_tag_lost );
CcSideEffect cc_side_effect_failed_conversion();
CcSideEffect cc_side_effect_demote( CcPieceEnum piece, int dest_i, int dest_j );
CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j );
CcSideEffect cc_side_effect_failed_resurrection();


typedef enum CcFormatStepUsageEnum
{
    CC_FSUE_User,
    CC_FSUE_Clarification,
    CC_FSUE_Clarification_NoOutput,
    CC_FSUE_Addition,
    CC_FSUE_Debug,
} CcFormatStepUsageEnum;


typedef struct CcStep
{
    CcStepLinkEnum link;
    int i;
    int j;
    CcSideEffect side_effect;
    CcFormatStepUsageEnum usage;
    struct CcStep * next;
} CcStep;

CcStep * cc_step__new( CcStepLinkEnum link,
                       int i, int j, CcSideEffect side_effect,
                       CcFormatStepUsageEnum usage );

CcStep * cc_step_append__new( CcStep * const restrict steps,
                              CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                              CcFormatStepUsageEnum usage );

bool cc_step_free_all_steps( CcStep ** const steps );


CcStep * cc_step_none__new( CcStepLinkEnum link, int i, int j,
                            CcFormatStepUsageEnum usage );

CcStep * cc_step_capture__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, bool is_promo_tag_lost,
                               CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement__new( CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, int i, int j,
                                  CcPieceEnum piece, int dest_i, int dest_j,
                                  CcFormatStepUsageEnum usage );

CcStep * cc_step_castle__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_promote__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece,
                               CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, int i, int j,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_convert__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, bool is_promo_tag_lost,
                               CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, int i, int j,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_demote__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, int dest_i, int dest_j,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j,
                                 CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage );


CcStep * cc_step_none_append__new( CcStep * const restrict steps,
                                   CcStepLinkEnum link, int i, int j,
                                   CcFormatStepUsageEnum usage );

CcStep * cc_step_capture_append__new( CcStep * const restrict steps,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, bool is_promo_tag_lost,
                                      CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement_append__new( CcStep * const restrict steps,
                                           CcStepLinkEnum link, int i, int j,
                                           CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j,
                                           CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant_append__new( CcStep * const restrict steps,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, int dest_i, int dest_j,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_castle_append__new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                     CcFormatStepUsageEnum usage );

CcStep * cc_step_promote_append__new( CcStep * const restrict steps,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece,
                                      CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion_append__new( CcStep * const restrict steps,
                                                CcStepLinkEnum link, int i, int j,
                                                CcFormatStepUsageEnum usage );

CcStep * cc_step_convert_append__new( CcStep * const restrict steps,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, bool is_promo_tag_lost,
                                      CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion_append__new( CcStep * const restrict steps,
                                                CcStepLinkEnum link, int i, int j,
                                                CcFormatStepUsageEnum usage );

CcStep * cc_step_demote_append__new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum piece, int dest_i, int dest_j,
                                     CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect_append__new( CcStep * const restrict steps,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum piece, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection_append__new( CcStep * const restrict steps,
                                                  CcStepLinkEnum link, int i, int j,
                                                  CcFormatStepUsageEnum usage );


size_t cc_step_count_usage( CcStep const * const restrict steps, CcFormatStepUsageEnum usage );


#endif /* __CC_STEP_H__ */
