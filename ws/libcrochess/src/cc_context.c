// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


#include "cc_defines.h"
#include "cc_context.h"


CcPlyContext * cc_ply_context_new( char const * restrict ply_start__w,
                                   char const * restrict ply_end__w )
{
    CcPlyContext * ply_context__a = calloc( 1, sizeof( CcPlyContext ) );
    if ( !ply_context__a ) return NULL;

    ply_context__a->ply_start__w = ply_start__w;
    ply_context__a->ply_end__w = ply_end__w;

    ply_context__a->next = NULL;

    return ply_context__a;
}

CcPlyContext * cc_ply_context_append( CcPlyContext * restrict ply_context__io,
                                      char const * restrict ply_start__w,
                                      char const * restrict ply_end__w )
{
    if ( !ply_context__io ) return NULL;

    CcPlyContext * ply_context__t = cc_ply_context_new( ply_start__w, ply_end__w );
    if ( !ply_context__t ) return NULL;

    CcPlyContext * p = ply_context__io;
    while ( p->next ) p = p->next; // rewind
    p->next = ply_context__t; // append // Ownership transfer --> ply_context__t is now weak pointer.

    return ply_context__t;
}

CcPlyContext * cc_ply_context_append_or_init( CcPlyContext ** restrict ply_context__io,
                                              char const * restrict ply_start__w,
                                              char const * restrict ply_end__w )
{
    if ( !ply_context__io ) return NULL;

    CcPlyContext * ply_context__w = NULL;

    if ( !*ply_context__io )
        *ply_context__io = ply_context__w = cc_ply_context_new( ply_start__w, ply_end__w );
    else
        ply_context__w = cc_ply_context_append( *ply_context__io, ply_start__w, ply_end__w );

    return ply_context__w;
}

bool cc_ply_context_free_all( CcPlyContext ** restrict ply_context__f )
{
    if ( !ply_context__f ) return false;
    if ( !*ply_context__f ) return true;

    CcPlyContext * pc = *ply_context__f;

    while ( pc )
    {
        CcPlyContext * tmp = pc->next;
        CC_FREE( pc );
        pc = tmp;
    }

    *ply_context__f = NULL;
    return true;
}
