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

typedef struct CcStep
{
    CcStepLinkEnum link;
    int i;
    int j;
    struct CcStep * next;
} CcStep;

CcStep * cc_step_new( CcStepLinkEnum link, int i, int j );
CcStep * cc_step_append_new( CcStep * const restrict steps, CcStepLinkEnum link, int i, int j );
bool cc_step_free_all_steps( CcStep ** const steps );


typedef enum CcStepSideEffectEnum
{
    CC_SSEE_None,
    CC_SSEE_Capture,
    CC_SSEE_Displacement,
} CcStepSideEffectEnum;

typedef struct CcStepSideEffect
{
    CcStepSideEffectEnum type;

    union
    {
        struct { CcPieceEnum piece; bool is_promo_tag_lost; } capture;
        struct { CcPieceEnum piece; bool is_promo_tag_lost; int i; int j; } displacement;
    };
} CcStepSideEffect;

CcStepSideEffect cc_step_side_effect( CcStepSideEffectEnum type, CcPieceEnum piece, bool is_promo_tag_lost, int i, int j );

CcStepSideEffect cc_step_side_effect_none();
CcStepSideEffect cc_step_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost );
CcStepSideEffect cc_step_side_effect_displacement( CcPieceEnum piece, bool is_promo_tag_lost, int i, int j );


typedef struct CcSideEffectStep
{
    CcStepLinkEnum link;
    int i;
    int j;
    CcStepSideEffect side_effect;
    struct CcSideEffectStep * next;
} CcSideEffectStep;

CcSideEffectStep * cc_step_side_effect_step_new( CcStepLinkEnum link, int i, int j, CcStepSideEffect side_effect );
CcSideEffectStep * cc_step_append_side_effect_step_new( CcSideEffectStep * const restrict steps, CcStepLinkEnum link, int i, int j, CcStepSideEffect side_effect );
bool cc_step_free_all_side_effect_steps( CcSideEffectStep ** const steps );


#endif /* __CC_STEP_H__ */
