// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_step.h"


CcStep * cc_step_new( CcStepLinkEnum link, int i, int j )
{
    CcStep * step = malloc( sizeof( CcStep ) );
    if ( !step ) return NULL;

    step->link = link;
    step->i = i;
    step->j = j;
    step->next = NULL;

    return step;
}

CcStep * cc_step_append_new( CcStep * const restrict steps, CcStepLinkEnum link, int i, int j )
{
    CcStep * new = cc_step_new( link, i, j );
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


CcStepSideEffect cc_step_side_effect( CcStepSideEffectEnum type, CcPieceEnum piece, bool is_promo_tag_lost, int i, int j )
{
    CcStepSideEffect sse = { .type = type, };

    // Nothing more to do if type == CC_SSEE_None.
    if ( sse.type == CC_SSEE_Capture )
    {
        sse.capture.piece = piece;
        sse.capture.is_promo_tag_lost = is_promo_tag_lost;
    }
    else if ( sse.type == CC_SSEE_Displacement )
    {
        sse.displacement.piece = piece;
        sse.displacement.is_promo_tag_lost = is_promo_tag_lost;
        sse.displacement.i = i;
        sse.displacement.j = j;
    }

    return sse;
}

CcStepSideEffect cc_step_side_effect_none()
{
    return cc_step_side_effect( CC_SSEE_None, CC_PE_None, false, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcStepSideEffect cc_step_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost )
{
    return cc_step_side_effect( CC_SSEE_Capture, piece, is_promo_tag_lost, CC_OFF_BOARD_COORD, CC_OFF_BOARD_COORD );
}

CcStepSideEffect cc_step_side_effect_displacement( CcPieceEnum piece, bool is_promo_tag_lost, int i, int j )
{
    return cc_step_side_effect( CC_SSEE_Displacement, piece, is_promo_tag_lost, i, j );
}


CcSideEffectStep * cc_step_side_effect_step_new( CcStepLinkEnum link, int i, int j, CcStepSideEffect side_effect )
{
    CcSideEffectStep * tjs = malloc( sizeof( CcSideEffectStep ) );
    if ( !tjs ) return NULL;

    tjs->link = link;
    tjs->i = i;
    tjs->j = j;
    tjs->side_effect = side_effect;
    tjs->next = NULL;

    return tjs;
}

CcSideEffectStep * cc_step_append_side_effect_step_new( CcSideEffectStep * const restrict steps, CcStepLinkEnum link, int i, int j, CcStepSideEffect side_effect )
{
    CcSideEffectStep * new = cc_step_side_effect_step_new( link, i, j, side_effect );
    if ( !new ) return NULL;
    if ( !steps ) return new;

    CcSideEffectStep * s = steps;
    while ( s->next ) s = s->next; // rewind
    s->next = new; // append

    return new;
}

bool cc_step_free_all_side_effect_steps( CcSideEffectStep ** const steps )
{
    if ( !steps ) return true;
    if ( !*steps ) return false;

    CcSideEffectStep * s = *steps;

    while ( s )
    {
        CcSideEffectStep * tmp = s->next;
        free( s );
        s = tmp;
    }

    *steps = NULL;
    return true;
}
