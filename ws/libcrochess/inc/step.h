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


typedef enum StepSideEffectType
{
    SSET_None,
    SSET_Capture,
    SSET_Displacement,
} StepSideEffectType;


typedef struct StepSideEffect
{
    StepSideEffectType type;
    bool is_promo_tag_lost;

    union
    {
        struct { PieceType piece; } capture;
        struct { PieceType piece; int i; int j; } displacement;
    };
} StepSideEffect;


typedef struct Step
{
    StepLink link;
    int i;
    int j;
    StepSideEffect side_effect;
} Step;


#endif /* __STEP_H__ */
