// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_steps.h"

/**
    @file cc_steps.c
    @brief Functions for positions, linked list.
*/


CcSteps * cc_steps__new( CcStepLinkEnum step_link,
                         CcPos pos )
{
    CcSteps * step__t = malloc( sizeof( CcSteps ) );
    if ( !step__t ) return NULL;

    step__t->step_link = step_link;
    step__t->pos = pos;

    step__t->prev = NULL;
    step__t->next = NULL;

    return step__t;
}

CcSteps * cc_steps_append( CcSteps * restrict steps__io,
                           CcStepLinkEnum step_link,
                           CcPos pos )
{
    if ( !steps__io ) return NULL;

    CcSteps * step__t = cc_steps__new( step_link, pos );
    if ( !step__t ) return NULL;

    CcSteps * s = steps__io;
    while ( s->next ) s = s->next; // rewind

    s->next = step__t; // append // Ownership transfer --> step__t is now weak pointer.
    step__t->prev = s;

    return step__t;
}

CcSteps * cc_steps_append_or_init( CcSteps ** restrict steps__io,
                                   CcStepLinkEnum step_link,
                                   CcPos pos )
{
    if ( !steps__io ) return NULL;

    CcSteps * step__w = NULL;

    if ( !*steps__io )
        *steps__io = step__w = cc_steps__new( step_link, pos );
    else
        step__w = cc_steps_append( *steps__io, step_link, pos );

    return step__w;
}

bool cc_steps_free_all( CcSteps ** restrict steps__f )
{
    if ( !steps__f ) return false;
    if ( !*steps__f ) return true;

    CcSteps * s = *steps__f;
    CcSteps * tmp = NULL;

    while ( s )
    {
        tmp = s->next;
        CC_FREE( s );
        s = tmp;
    }

    *steps__f = NULL;
    return true;
}
