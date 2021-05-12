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

Step * step_new_alx( StepLink link, int i, int j );
Step * step_append_alx( Step * const restrict steps, StepLink link, int i, int j );
bool step_free_all_steps( Step ** const steps );


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

StepSideEffect step_side_effect( StepSideEffectType type, PieceType piece, bool is_promo_tag_lost, int i, int j );

StepSideEffect step_side_effect_none();
StepSideEffect step_side_effect_capture( PieceType piece, bool is_promo_tag_lost );
StepSideEffect step_side_effect_displacement( PieceType piece, bool is_promo_tag_lost, int i, int j );


typedef struct SideEffectStep
{
    StepLink link;
    int i;
    int j;
    StepSideEffect side_effect;
    struct SideEffectStep * next;
} SideEffectStep;

SideEffectStep * step_new_side_effect_alx( StepLink link, int i, int j, StepSideEffect side_effect );
SideEffectStep * step_append_side_effect_alx( SideEffectStep * const restrict steps, StepLink link, int i, int j, StepSideEffect side_effect );
bool step_free_all_side_effect_steps( SideEffectStep ** const steps );


#endif /* __STEP_H__ */
