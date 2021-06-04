// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "step.h"


Step * step_new_alx( StepLink link, int i, int j )
{
    Step * step = malloc( sizeof( Step ) );
    if ( !step ) return NULL;

    step->link = link;
    step->i = i;
    step->j = j;
    step->next = NULL;

    return step;
}

Step * step_append_alx( Step * const restrict steps, StepLink link, int i, int j )
{
    Step * new = step_new_alx( link, i, j );
    if ( !new ) return NULL;
    if ( !steps ) return new;

    Step * s = steps;
    while ( s->next ) s = s->next; // rewind
    s->next = new; // append

    return new;
}

bool step_free_all_steps( Step ** const steps )
{
    if ( !steps ) return true;
    if ( !*steps ) return false;

    Step * s = *steps;

    while ( s )
    {
        Step * tmp = s->next;
        free( s );
        s = tmp;
    }

    *steps = NULL;
    return true;
}


StepSideEffect step_side_effect( StepSideEffectType type, PieceType piece, bool is_promo_tag_lost, int i, int j )
{
    StepSideEffect sse = { .type = type, };

    // Nothing more to do if type == SSET_None.
    if ( sse.type == SSET_Capture )
    {
        sse.capture.piece = piece;
        sse.capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( sse.type == SSET_Displacement )
    {
        sse.displacement.piece = piece;
        sse.displacement.is_promo_tag_lost = is_promo_tag_lost;
        sse.displacement.i = i;
        sse.displacement.j = j;
    }

    return sse;
}

StepSideEffect step_side_effect_none()
{
    return step_side_effect( SSET_None, PT_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

StepSideEffect step_side_effect_capture( PieceType piece, bool is_promo_tag_lost )
{
    return step_side_effect( SSET_Capture, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

StepSideEffect step_side_effect_displacement( PieceType piece, bool is_promo_tag_lost, int i, int j )
{
    return step_side_effect( SSET_Displacement, piece, is_promo_tag_lost, i, j );
}


SideEffectStep * step_new_side_effect_alx( StepLink link, int i, int j, StepSideEffect side_effect )
{
    SideEffectStep * tjs = malloc( sizeof( SideEffectStep ) );
    if ( !tjs ) return NULL;

    tjs->link = link;
    tjs->i = i;
    tjs->j = j;
    tjs->side_effect = side_effect;
    tjs->next = NULL;

    return tjs;
}

SideEffectStep * step_append_side_effect_alx( SideEffectStep * const restrict steps, StepLink link, int i, int j, StepSideEffect side_effect )
{
    SideEffectStep * new = step_new_side_effect_alx( link, i, j, side_effect );
    if ( !new ) return NULL;
    if ( !steps ) return new;

    SideEffectStep * s = steps;
    while ( s->next ) s = s->next; // rewind
    s->next = new; // append

    return new;
}

bool step_free_all_side_effect_steps( SideEffectStep ** const steps )
{
    if ( !steps ) return true;
    if ( !*steps ) return false;

    SideEffectStep * s = *steps;

    while ( s )
    {
        SideEffectStep * tmp = s->next;
        free( s );
        s = tmp;
    }

    *steps = NULL;
    return true;
}
