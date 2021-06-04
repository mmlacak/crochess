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
    CcPly * plies;
    CcMoveStatusEnum status;
} CcMove;


CcMove * cc_move_new( CcPly * const restrict plies, CcMoveStatusEnum status );
bool cc_mv_free_complete_move( CcMove ** const move );


#endif /* __CC_MOVE_H__ */
