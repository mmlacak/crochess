// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CONTEXT_H__
#define __CC_CONTEXT_H__

#include <stddef.h>

#include "cc_chessboard.h"
#include "cc_game.h"


typedef struct CcPlyContext
{
    CcChessboard chessboard;
    char const * ply_start__w;
    char const * ply_end__w;
    struct CcPlyContext * next;
} CcPlyContext;


CcPlyContext * cc_ply_context_new( char const * restrict ply_start__w,
                                   char const * restrict ply_end__w );

CcPlyContext * cc_ply_context_append( CcPlyContext * restrict ply_context__io,
                                      char const * restrict ply_start__w,
                                      char const * restrict ply_end__w );

CcPlyContext * cc_ply_context_append_or_init( CcPlyContext ** restrict ply_context__io,
                                              char const * restrict ply_start__w,
                                              char const * restrict ply_end__w );

bool cc_ply_context_free_all( CcPlyContext ** restrict ply_context__f );


typedef struct CcContext
{
    CcGame game;
    char * user_move_an;
    char * converted_an;
    char const * move_an__w;
    CcPlyContext * ply_context;
} CcContext;




#endif /* __CC_CONTEXT_H__ */
