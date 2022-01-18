// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_context.h"

/**
    @file cc_context.c
    @brief Context functions, used while parsing AN notation.
*/


CcContextPly * cc_context_ply_new( char const * restrict ply_start__w,
                                   char const * restrict ply_end__w )
{
    CcContextPly * context_ply__a = malloc( sizeof( CcContextPly ) );
    if ( !context_ply__a ) return NULL;

    context_ply__a->chessboard = NULL;

    context_ply__a->ply_start__w = ply_start__w;
    context_ply__a->ply_end__w = ply_end__w;

    context_ply__a->next = NULL;

    return context_ply__a;
}

CcContextPly * cc_context_ply_append( CcContextPly * restrict context_ply__io,
                                      char const * restrict ply_start__w,
                                      char const * restrict ply_end__w )
{
    if ( !context_ply__io ) return NULL;

    CcContextPly * context_ply__t = cc_context_ply_new( ply_start__w, ply_end__w );
    if ( !context_ply__t ) return NULL;

    CcContextPly * p = context_ply__io;
    while ( p->next ) p = p->next; // rewind
    p->next = context_ply__t; // append // Ownership transfer --> context_ply__t is now weak pointer.

    return context_ply__t;
}

CcContextPly * cc_context_ply_append_or_init( CcContextPly ** restrict context_ply__io,
                                              char const * restrict ply_start__w,
                                              char const * restrict ply_end__w )
{
    if ( !context_ply__io ) return NULL;

    CcContextPly * context_ply__w = NULL;

    if ( !*context_ply__io )
        *context_ply__io = context_ply__w = cc_context_ply_new( ply_start__w, ply_end__w );
    else
        context_ply__w = cc_context_ply_append( *context_ply__io, ply_start__w, ply_end__w );

    return context_ply__w;
}

bool cc_context_ply_free_all( CcContextPly ** restrict context_ply__f )
{
    if ( !context_ply__f ) return false;
    if ( !*context_ply__f ) return true;

    CcContextPly * cp = *context_ply__f;

    while ( cp )
    {
        CcContextPly * tmp = cp->next;
        CC_FREE( cp );
        cp = tmp;
    }

    *context_ply__f = NULL;
    return true;
}


CcContext * cc_context_new( CcGame * restrict game__w,
                            char const * restrict user_move_an )
{
    CcContext * context__a = malloc( sizeof( CcContext ) );
    if ( !context__a ) return NULL;

    context__a->game__w = game__w;

    context__a->user_move_an = cc_str_duplicate_new( user_move_an, false, BUFSIZ );
    context__a->converted_an = NULL; // TODO :: convert user_move_an, if neccessary

    context__a->move_an__w =
        ( context__a->converted_an ) ? context__a->converted_an
                                     : context__a->user_move_an;

    context__a->context_ply = NULL;

    return context__a;
}

bool cc_context_free_all( CcContext ** restrict context__f )
{
    if ( !context__f ) return false;
    if ( !*context__f ) return true;

    bool result = true;
    CcContext * ctx = *context__f;

    CC_FREE( ctx->user_move_an );
    CC_FREE( ctx->converted_an );

    result = cc_context_ply_free_all( &( ctx->context_ply ) ) && result;

    CC_FREE_NULL( context__f );
    return result;
}
