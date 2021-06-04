// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __DO_MOVE_H__
#define __DO_MOVE_H__

#include "cc_chessboard.h"
#include "cc_move.h"


CcPlyLinkEnum * next_ply_link( CcPly const * const restrict ply );
bool is_teleporting_next( CcPly const * const restrict ply, bool including_wave );

bool do_ply( CcChessboard * const restrict cb, CcMove const * const restrict move, CcPly const * const restrict ply );

bool do_move( CcChessboard * const restrict cb, CcMove const * const restrict move );


#endif /* __DO_MOVE_H__ */
