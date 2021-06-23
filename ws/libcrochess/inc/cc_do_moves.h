// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_DO_MOVES_H__
#define __CC_DO_MOVES_H__

#include "cc_chessboard.h"
#include "cc_move.h"


typedef enum CcDoMoveEnum
{
    CC_DME_OnlyCurrentMove,
    CC_DME_OnlyLastMove,
    CC_DME_AllMoves,
} CcDoMoveEnum;


CcPlyLinkEnum * cc_get_next_ply_link( CcPly const * const restrict ply );
bool cc_is_teleporting_next( CcPly const * const restrict ply, bool including_wave );

bool cc_do_step( CcChessboard * const restrict cb,
                 CcMove const * const restrict move,
                 CcPly const * const restrict ply,
                 CcStep const * const restrict step );

bool cc_do_ply( CcChessboard * const restrict cb,
                CcMove const * const restrict move,
                CcPly const * const restrict ply );

bool cc_do_moves( CcChessboard * const restrict cb,
                  CcMove const * const restrict moves,
                  CcDoMoveEnum do_spec );


#endif /* __CC_DO_MOVES_H__ */
