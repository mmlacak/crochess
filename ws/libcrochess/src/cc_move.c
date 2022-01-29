// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
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


CcMove * cc_move_new( char const * restrict notation,
                      CcMovePreStatusEnum prestatus,
                      CcPly ** restrict plies__n,
                      CcMoveStatusEnum status )
{
    if ( CC_MOVE_PRESTATUS_IS_GAME_END( prestatus ) )
    {
        if ( plies__n ) return NULL;
        if ( status != CC_MSE_None ) return NULL;
    }

    CcMove * mv__a = malloc( sizeof( CcMove ) );
    if ( !mv__a ) return NULL;

    mv__a->notation = cc_str_duplicate_new( notation, false, BUFSIZ );
    if ( notation && ( !mv__a->notation ) )
    {
        CC_FREE( mv__a );
        return NULL;
    }

    mv__a->prestatus = prestatus;

    if ( plies__n )
    {
        mv__a->plies = *plies__n;
        *plies__n = NULL; // Taking ownership.
    }
    else
        mv__a->plies = NULL;

    mv__a->status = status;
    mv__a->next = NULL;

    return mv__a;
}

CcMove * cc_move_append( CcMove * restrict moves__io,
                         char const * restrict notation,
                         CcMovePreStatusEnum prestatus,
                         CcPly ** restrict plies__n,
                         CcMoveStatusEnum status )
{
    if ( !moves__io ) return NULL;

    CcMove * mv__a = cc_move_new( notation, prestatus, plies__n, status );
    if ( !mv__a ) return NULL;

    CcMove * mv = moves__io;
    while ( mv->next ) mv = mv->next; // rewind
    mv->next = mv__a; // append

    return mv__a;
}

CcMove * cc_move_append_or_init( CcMove ** restrict moves__io,
                                 char const * restrict notation,
                                 CcMovePreStatusEnum prestatus,
                                 CcPly ** restrict plies__n,
                                 CcMoveStatusEnum status )
{
    if ( !moves__io ) return NULL;

    CcMove * move__w = NULL;

    if ( !*moves__io )
        *moves__io = move__w = cc_move_new( notation, prestatus, plies__n, status );
    else
        move__w = cc_move_append( *moves__io, notation, prestatus, plies__n, status );

    return move__w;
}

bool cc_move_extend_or_init( CcMove ** restrict moves__io,
                             CcMove ** restrict moves__n )
{
    if ( !moves__io ) return false;
    if ( !moves__n ) return false;
    if ( !*moves__n ) return false;

    if ( *moves__io )
    {
        CcMove * mv = *moves__io;
        while ( mv->next ) mv = mv->next; // rewind
        mv->next = *moves__n; // append
    }
    else
        *moves__io = *moves__n;

    *moves__n = NULL;

    return true;
}

CcMove * cc_move_duplicate_all_new( CcMove * restrict moves )
{
    if ( !moves ) return NULL;

    CcMove * mv__a = NULL;
    CcMove * from = moves;

    do
    {
        CcPly * plies__t = cc_ply_duplicate_all_new( moves->plies );
        if ( !plies__t )
        {
            cc_move_free_all_moves( &mv__a );
            return NULL;
        }

        CcMove * mv__w = cc_move_append_or_init( &mv__a,
                                                 from->notation,
                                                 from->prestatus,
                                                 &plies__t,
                                                 from->status );
        if ( !mv__w )
        {
            cc_ply_free_all_plies( &plies__t ); // Failed append --> no ownership transfer ...
            cc_move_free_all_moves( &mv__a );
            return NULL;
        }

        from = from->next;
    }
    while ( from );

    return mv__a;
}

bool cc_move_free_all_moves( CcMove ** restrict moves__f )
{
    if ( !moves__f ) return false;
    if ( !*moves__f ) return true;

    bool result = true;

    CcMove * mv = *moves__f;

    while ( mv )
    {
        CC_FREE( mv->notation );

        CcPly ** plies = &( mv->plies );
        result = cc_ply_free_all_plies( plies ) && result;

        CcMove * tmp = mv->next;
        CC_FREE( mv );
        mv = tmp;
    }

    *moves__f = NULL;
    return result;
}

//
// new conveniences

CcMove * cc_move_on_new( char const * restrict notation,
                         CcMovePreStatusEnum prestatus,
                         CcPly ** restrict plies__n,
                         CcMoveStatusEnum status )
{
    if ( !CC_MOVE_PRESTATUS_IS_GAME_ON( prestatus ) ) return NULL;
    return cc_move_new( notation, prestatus, plies__n, status );
}

CcMove * cc_move_end_new( char const * restrict notation,
                          CcMovePreStatusEnum prestatus )
{
    if ( !CC_MOVE_PRESTATUS_IS_GAME_END( prestatus ) ) return NULL;
    return cc_move_new( notation, prestatus, NULL, CC_MSE_None );
}

//
// append conveniences

CcMove * cc_move_on_append( CcMove * restrict moves__io,
                            char const * restrict notation,
                            CcMovePreStatusEnum prestatus,
                            CcPly ** restrict plies__n,
                            CcMoveStatusEnum status )
{
    if ( !CC_MOVE_PRESTATUS_IS_GAME_ON( prestatus ) ) return NULL;
    return cc_move_append( moves__io, notation, prestatus, plies__n, status );
}

CcMove * cc_move_end_append( CcMove * restrict moves__io,
                             char const * restrict notation,
                             CcMovePreStatusEnum prestatus )
{
    if ( !CC_MOVE_PRESTATUS_IS_GAME_END( prestatus ) ) return NULL;
    return cc_move_append( moves__io, notation, prestatus, NULL, CC_MSE_None );
}

//
// append or init conveniences

CcMove * cc_move_on_append_or_init( CcMove ** restrict moves__io,
                                    char const * restrict notation,
                                    CcMovePreStatusEnum prestatus,
                                    CcPly ** restrict plies__n,
                                    CcMoveStatusEnum status )
{
    if ( !CC_MOVE_PRESTATUS_IS_GAME_ON( prestatus ) ) return NULL;
    return cc_move_append_or_init( moves__io, notation, prestatus, plies__n, status );
}

CcMove * cc_move_end_append_or_init( CcMove ** restrict moves__io,
                                     char const * restrict notation,
                                     CcMovePreStatusEnum prestatus )
{
    if ( !CC_MOVE_PRESTATUS_IS_GAME_END( prestatus ) ) return NULL;
    return cc_move_append_or_init( moves__io, notation, prestatus, NULL, CC_MSE_None );
}


size_t cc_move_ply_count( CcMove * restrict move )
{
    if ( !move ) return 0;
    if ( !move->plies ) return 0;

    size_t count = 1;
    CcPly * p = move->plies;

    while ( p->next )
    {
        ++count;
        p = p->next;
    }

    return count;
}
