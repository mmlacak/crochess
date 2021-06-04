// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_DO_MOVE_H__
#define __CC_DO_MOVE_H__

#include "cc_chessboard.h"
#include "cc_move.h"


CcPlyLinkEnum * cc_get_next_ply_link( CcPly const * const restrict ply );
bool cc_is_teleporting_next( CcPly const * const restrict ply, bool including_wave );

bool cc_do_ply( CcChessboard * const restrict cb, CcMove const * const restrict move, CcPly const * const restrict ply );

bool cc_do_move( CcChessboard * const restrict cb, CcMove const * const restrict move );


#endif /* __CC_DO_MOVE_H__ */
