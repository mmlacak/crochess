// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CONTEXT_H__
#define __CC_CONTEXT_H__

#include <stddef.h>

#include "cc_chessboard.h"
#include "cc_game.h"


typedef struct CcContextPly
{
    CcChessboard chessboard;
    char const * ply_start__w;
    char const * ply_end__w;
    struct CcContextPly * next;
} CcContextPly;


CcContextPly * cc_context_ply_new( char const * restrict ply_start__w,
                                   char const * restrict ply_end__w );

CcContextPly * cc_context_ply_append( CcContextPly * restrict context_ply__io,
                                      char const * restrict ply_start__w,
                                      char const * restrict ply_end__w );

CcContextPly * cc_context_ply_append_or_init( CcContextPly ** restrict context_ply__io,
                                              char const * restrict ply_start__w,
                                              char const * restrict ply_end__w );

bool cc_context_ply_free_all( CcContextPly ** restrict context_ply__f );


typedef struct CcContext
{
    CcGame * game__w;
    char * user_move_an;
    char * converted_an;
    char const * move_an__w;
    CcContextPly * context_ply;
} CcContext;


CcContext * cc_context_new( CcGame * restrict game__w,
                            char const * restrict user_move_an );

bool cc_context_free_all( CcContext ** restrict context__f );



#endif /* __CC_CONTEXT_H__ */
