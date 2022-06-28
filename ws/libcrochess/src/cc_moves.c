// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_moves.h"

/**
    @file cc_moves.c
    @brief Functions for move queue.
*/


CcMoves * cc_moves__new( char const * restrict an,
                         size_t max_len__d )
{
    CcMoves * mv__a = malloc( sizeof( CcMoves ) );
    if ( !mv__a ) return NULL;

    mv__a->an = cc_str_duplicate__new( an, false, max_len__d );

    mv__a->prev = NULL;
    mv__a->next = NULL;

    return mv__a;
}

CcMoves * cc_moves_append( CcMoves * restrict moves__io,
                           char const * restrict an,
                           size_t max_len__d )
{
    if ( !moves__io ) return NULL;

    CcMoves * mv__t = cc_moves__new( an, max_len__d );
    if ( !mv__t ) return NULL;

    CcMoves * m = moves__io;
    while ( m->next ) m = m->next; // rewind

    m->next = mv__t; // append // Ownership transfer --> mv__t is now weak pointer.
    mv__t->prev = m;

    return mv__t;
}

CcMoves * cc_moves_append_or_init( CcMoves ** restrict moves__io,
                                   char const * restrict an,
                                   size_t max_len__d )
{
    if ( !moves__io ) return NULL;

    CcMoves * mv__w = NULL;

    if ( !*moves__io )
        *moves__io = mv__w = cc_moves__new( an, max_len__d );
    else
        mv__w = cc_moves_append( *moves__io, an, max_len__d );

    return mv__w;
}

CcMoves * cc_moves_duplicate_all__new( CcMoves * restrict moves )
{
    if ( !moves ) return NULL;

    CcMoves * new__a = cc_moves__new( moves->an, CC_MAX_LEN_ZERO_TERMINATED );
    if ( !new__a ) return NULL;

    CcMoves * m = moves->next;

    while ( m )
    {
        if ( !cc_moves_append( new__a, m->an, CC_MAX_LEN_ZERO_TERMINATED ) )
        {
            cc_moves_free_all( &new__a );
            return NULL;
        }

        ++m;
    }

    return new__a;
}

bool cc_moves_free_all( CcMoves ** restrict moves__f )
{
    if ( !moves__f ) return false;
    if ( !*moves__f ) return true;

    CcMoves * m = *moves__f;
    CcMoves * tmp = NULL;

    while ( m )
    {
        CC_FREE( m->an );

        tmp = m->next;
        CC_FREE( m );
        m = tmp;
    }

    *moves__f = NULL;
    return true;
}

bool cc_moves_print( CcMoves * restrict moves )
{
    if ( !moves ) return false;

    CcMoves * m = moves;
    CcMoves * l = NULL;
    CcMoves * d = NULL;

    size_t i = 0;
    size_t index = 0;

    while ( m )
    {
        if ( i++ % 2 == 0 )
        {
            l = m;

            if ( !m->next )
            {
                printf( "%lu %s ...\n", index+1, l->an );
                break;
            }
        }
        else
        {
            d = m;
            printf( "%lu %s %s\n", ++index, l->an, d->an );
        }

        m = m->next;
    }

    return true;
}
