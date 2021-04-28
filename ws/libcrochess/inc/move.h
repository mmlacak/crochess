// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __MOVE_H__
#define __MOVE_H__

#include "ply.h"


typedef enum MoveStatus
{
    MS_None,
    MS_Check,
    MS_Checkmate,
} MoveStatus;


typedef struct Move
{
    Ply * plies;
    MoveStatus status;
} Move;


Move * mv_new_alx( Ply * const restrict plies, MoveStatus status );
bool mv_free_move( Move ** move );


#endif /* __MOVE_H__ */
