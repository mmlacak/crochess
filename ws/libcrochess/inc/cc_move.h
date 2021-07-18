// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_MOVE_H__
#define __CC_MOVE_H__

#include "cc_ply.h"


typedef enum CcMoveStatusEnum
{
    CC_MSE_None,
    CC_MSE_Check,
    CC_MSE_Checkmate,
} CcMoveStatusEnum;


typedef struct CcMove
{
    char const * notation;
    CcPly * plies;
    CcMoveStatusEnum status;

    struct CcMove * next;
} CcMove;


CcMove * cc_move_new( char const * const restrict notation,
                      CcPly ** restrict plies_n,
                      CcMoveStatusEnum status );

CcMove * cc_move_append_new( CcMove * const restrict moves,
                             char const * const restrict notation,
                             CcPly ** restrict plies_n,
                             CcMoveStatusEnum status );

bool cc_move_free_all_moves( CcMove ** const moves );

size_t cc_move_ply_count( CcMove const * const restrict move );


#endif /* __CC_MOVE_H__ */
