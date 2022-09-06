// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>

#include "cc_defines.h"
#include "cc_steps.h"

/**
    @file cc_steps.c
    @brief Functions for steps queue.
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

CcSteps * cc_steps_append_if( CcSteps ** restrict steps__io,
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

bool cc_steps_are_congruent( CcSteps * restrict steps,
                             CcPosLink * restrict positions )
{
    if ( !steps ) return false;
    if ( !positions ) return false;

    CcPosLink * p = positions;
    CcSteps * prev_s = NULL;
    CcSteps * s = steps;

    while ( s->prev ) s = s->prev; // rewind

    while ( s )
    {
        switch ( s->step_link )
        {
            case CC_SLE_Start :
            {
                if ( !s->prev && ( p == positions ) ) // First step, and position?
                {
                    if ( !cc_pos_is_congruent( s->pos, p->pos ) )
                        return false;
                }
                else
                    return false;

                break;
            }

            case CC_SLE_Reposition :
            case CC_SLE_Next :
            {
                if ( p && p->next )
                {
                    // If reposition, position before must be the first one.
                    if ( ( s->step_link == CC_SLE_Reposition ) &&
                         ( p != positions ) )
                            return false;

                    p = p->next;

                    if ( !cc_pos_is_equal( s->pos, p->pos ) )
                        return false;
                }
                else
                    return false;

                break;
            }

            case CC_SLE_Distant :
            {
                if ( p )
                    p = p->next;
                else
                    return false;

                bool found = false;

                while ( p )
                {
                    if ( cc_pos_is_equal( s->pos, p->pos ) )
                    {
                        found = true;
                        break;
                    }

                    p = p->next;
                }

                if ( !found )
                    return false;

                break;
            }

            case CC_SLE_Destination :
            {
                if ( s->next ) return false; // Not the last one?

                while ( p && p->next ) p = p->next; // rewind

                if ( !cc_pos_is_equal( s->pos, p->pos ) )
                    return false;

                break;
            }

            case CC_SLE_None :
            default :
                return false;
        }

        prev_s = s;
        s = s->next;
    }

    return ( prev_s && !prev_s->next && p && !p->next );
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

size_t cc_steps_len( CcSteps * restrict steps )
{
    if ( !steps ) return 0;

    size_t len = 0;
    CcSteps * s = steps;

    while ( s )
    {
        ++len;
        s = s->next;
    }

    return len;
}

char * cc_steps_to_short_string__new( CcSteps * restrict steps )
{
    if ( !steps ) return NULL;

    size_t len = cc_steps_len( steps ) *
                 ( CC_MAX_LEN_CHAR_8 + 2 ); // +2 for separator ".." between steps

    size_t size = len + 1;
    char * steps_str__a = malloc( size ); // == len + 1, to have room for '\0'
    if ( !steps_str__a ) return NULL;

    // *steps_str__a = '\0'; // Not needed, done after a switch below.

    char * steps_str = steps_str__a;
    cc_char_8 pos_str = CC_CHAR_8_EMPTY;
    CcSteps * s = steps;

    while ( s )
    {
        switch ( s->step_link )
        {
            case CC_SLE_None :
            {
                *steps_str++ = '?';
                break;
            }

            case CC_SLE_Start :
            {
                *steps_str++ = ' ';
                break;
            }

            case CC_SLE_Reposition :
            {
                *steps_str++ = ',';
                break;
            }

            case CC_SLE_Next :
            {
                *steps_str++ = '.';
                break;
            }

            case CC_SLE_Distant :
            {
                *steps_str++ = '.';
                *steps_str++ = '.';
                break;
            }

            case CC_SLE_Destination :
            {
                *steps_str++ = '-';
                break;
            }

            default :
            {
                *steps_str++ = '!';
                break;
            }
        }

        *steps_str = '\0';

        if ( !cc_pos_to_short_string( s->pos, &pos_str ) )
        {
            CC_FREE( steps_str__a );
            return NULL;
        }

        steps_str = cc_str_append_into( steps_str, size, pos_str, CC_MAX_LEN_CHAR_8 );
        if ( !steps_str )
        {
            CC_FREE( steps_str__a );
            return NULL;
        }

        s = s->next;
    }

    return steps_str__a;
}
