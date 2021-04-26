// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __DO_MOVE_H__
#define __DO_MOVE_H__

#include "chessboard.h"
#include "move.h"


bool is_teleport( Chessboard const * const restrict cb, int i, int j, PieceType pt );

bool do_ply( Chessboard * const restrict cb, Move const * const restrict move, Ply const * const restrict ply );

bool do_move( Chessboard * const restrict cb, Move const * const restrict move );


#endif /* __DO_MOVE_H__ */
