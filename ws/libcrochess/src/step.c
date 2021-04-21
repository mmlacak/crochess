// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "step.h"


Step * step_new_alx(StepLink link, int i, int j)
{
    Step * step = malloc( sizeof( Step ) );
    if ( !step ) return NULL;

    step->link = link;
    step->i = i;
    step->j = j;
    step->next = NULL;

    return step;
}


StepSideEffect * step_new_side_effect_alx( StepSideEffectType type, PieceType piece, bool is_promo_tag_lost, int i, int j )
{
    StepSideEffect * sse = calloc( 1, sizeof( StepSideEffect ) );
    if ( !sse ) return NULL;

    sse->type = type;

    // Nothing more to do if type == SSET_None.
    if ( sse->type == SSET_Capture )
    {
        sse->capture.piece = piece;
        sse->capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( sse->type == SSET_Displacement )
    {
        sse->displacement.piece = piece;
        sse->displacement.is_promo_tag_lost = is_promo_tag_lost;
        sse->displacement.i = i;
        sse->displacement.j = j;
    }

    return sse;
}

StepSideEffect * step_new_side_effect_none_alx()
{
    return step_new_side_effect_alx(SSET_None, PT_None, false, -1, -1);
}

StepSideEffect * step_new_side_effect_capture_alx( PieceType piece, bool is_promo_tag_lost )
{
    return step_new_side_effect_alx(SSET_Capture, piece, is_promo_tag_lost, -1, -1);
}

StepSideEffect * step_new_side_effect_displacement_alx( PieceType piece, bool is_promo_tag_lost, int i, int j )
{
    return step_new_side_effect_alx(SSET_Displacement, piece, is_promo_tag_lost, i, j);
}


TranceJourneyStep * step_new_trance_journey_alx(StepLink link, int i, int j, StepSideEffect side_effect)
{
    TranceJourneyStep * tjs = malloc( sizeof( TranceJourneyStep ) );
    if ( !tjs ) return NULL;

    tjs->link = link;
    tjs->i = i;
    tjs->j = j;
    tjs->side_effect = side_effect;
    tjs->next = NULL;

    return tjs;
}
