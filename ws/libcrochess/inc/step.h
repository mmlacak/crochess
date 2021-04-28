// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __STEP_H__
#define __STEP_H__


#include <stdbool.h>

#include "piece_type.h"


typedef enum StepLink
{
    SL_Start,
    SL_Next,
    SL_Distant,
    SL_Destination,
} StepLink;

typedef struct Step
{
    StepLink link;
    int i;
    int j;
    struct Step * next;
} Step;

Step * step_new_alx(StepLink link, int i, int j);
bool step_free_all_steps( Step ** const restrict steps );


typedef enum StepSideEffectType
{
    SSET_None,
    SSET_Capture,
    SSET_Displacement,
} StepSideEffectType;

typedef struct StepSideEffect
{
    StepSideEffectType type;

    union
    {
        struct { PieceType piece; bool is_promo_tag_lost; } capture;
        struct { PieceType piece; bool is_promo_tag_lost; int i; int j; } displacement;
    };
} StepSideEffect;

StepSideEffect * step_new_side_effect_alx( StepSideEffectType type, PieceType piece, bool is_promo_tag_lost, int i, int j );

StepSideEffect * step_new_side_effect_none_alx();
StepSideEffect * step_new_side_effect_capture_alx( PieceType piece, bool is_promo_tag_lost );
StepSideEffect * step_new_side_effect_displacement_alx( PieceType piece, bool is_promo_tag_lost, int i, int j );


typedef struct TranceJourneyStep
{
    StepLink link;
    int i;
    int j;
    StepSideEffect side_effect;
    struct TranceJourneyStep * next;
} TranceJourneyStep;

TranceJourneyStep * step_new_trance_journey_alx(StepLink link, int i, int j, StepSideEffect side_effect);
bool step_free_all_trance_journey_steps( TranceJourneyStep ** const restrict steps );


typedef struct PawnSacrificeCaptureStep
{
    StepLink link;
    int i;
    int j;
    StepSideEffect side_effect; // Only SSET_None, SSET_Capture are valid.
    struct PawnSacrificeCaptureStep * next;
} PawnSacrificeCaptureStep;

PawnSacrificeCaptureStep * step_new_pawn_sacrifice_capture_alx(StepLink link, int i, int j, StepSideEffect side_effect);
bool step_free_all_pawn_sacrifice_steps( PawnSacrificeCaptureStep ** const restrict steps );


#endif /* __STEP_H__ */
