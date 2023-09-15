// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_move.h"

/**
    @file cc_move.c
    @brief Functions for move linked list, storage.
*/


CcMove * cc_move__new( char const * restrict notation,
                       size_t max_len__d,
                       CcPly ** restrict plies__n,
                       CcMoveStatusEnum status ) {
    CcMove * mv__a = malloc( sizeof( CcMove ) );
    if ( !mv__a ) return NULL;

    mv__a->notation = cc_str_duplicate__new( notation, false, max_len__d );
    if ( notation && ( !mv__a->notation ) ) {
        CC_FREE( mv__a );
        return NULL;
    }

    if ( plies__n ) {
        mv__a->plies = *plies__n;
        *plies__n = NULL; // Taking ownership.
    }
    else
        mv__a->plies = NULL;

    mv__a->status = status;

    mv__a->prev = NULL;
    mv__a->next = NULL;

    return mv__a;
}

CcMove * cc_move_append( CcMove * restrict moves__io,
                         char const * restrict notation,
                         size_t max_len__d,
                         CcPly ** restrict plies__n,
                         CcMoveStatusEnum status ) {
    if ( !moves__io ) return NULL;

    CcMove * mv__t = cc_move__new( notation, max_len__d, plies__n, status );
    if ( !mv__t ) return NULL;

    CcMove * m = moves__io;
    while ( m->next ) m = m->next; // rewind

    m->next = mv__t; // append // Ownership transfer --> mv__t is now weak pointer.
    mv__t->prev = m;

    return mv__t;
}

CcMove * cc_move_append_if( CcMove ** restrict moves__io,
                            char const * restrict notation,
                            size_t max_len__d,
                            CcPly ** restrict plies__n,
                            CcMoveStatusEnum status ) {
    if ( !moves__io ) return NULL;

    CcMove * move__w = NULL;

    if ( !*moves__io )
        *moves__io = move__w = cc_move__new( notation, max_len__d, plies__n, status );
    else
        move__w = cc_move_append( *moves__io, notation, max_len__d, plies__n, status );

    return move__w;
}

CcMove * cc_move_duplicate_all__new( CcMove * restrict moves ) {
    if ( !moves ) return NULL;

    CcMove * mv__a = NULL;
    CcMove * from = moves;

    while ( from->prev ) from = from->prev; // rewind

    do {
        CcPly * plies__t = cc_ply_duplicate_all__new( from->plies );
        if ( !plies__t ) {
            cc_move_free_all( &mv__a );
            return NULL;
        }

        CcMove * mv__w = cc_move_append_if( &mv__a,
                                            from->notation,
                                            CC_MAX_LEN_ZERO_TERMINATED,
                                            &plies__t,
                                            from->status );
        if ( !mv__w ) {
            cc_ply_free_all( &plies__t ); // Failed append --> no ownership transfer ...
            cc_move_free_all( &mv__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return mv__a;
}

bool cc_move_free_all( CcMove ** restrict moves__f ) {
    if ( !moves__f ) return false;
    if ( !*moves__f ) return true;

    bool result = true;
    CcMove * mv = *moves__f;

    while ( mv->prev ) mv = mv->prev; // rewind

    while ( mv ) {
        CC_FREE( mv->notation );

        CcPly ** plies = &( mv->plies );
        result = cc_ply_free_all( plies ) && result;

        CcMove * tmp = mv->next;
        CC_FREE( mv );
        mv = tmp;
    }

    *moves__f = NULL;
    return result;
}


size_t cc_move_plies_count( CcMove * restrict move ) {
    if ( !move ) return 0;
    if ( !move->plies ) return 0;

    size_t count = 1;
    CcPly * p = move->plies;

    while ( p->next ) {
        ++count;
        p = p->next;
    }

    return count;
}
