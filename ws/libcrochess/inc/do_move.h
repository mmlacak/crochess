// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __DO_MOVE_H__
#define __DO_MOVE_H__

#include "chessboard.h"
#include "move.h"


// bool pt_is_conversion(PieceType const actor, PieceType const passive);
// bool pt_is_failed_conversion(PieceType const actor, PieceType const passive);
bool is_teleporting( Chessboard const * const restrict cb, int i, int j, PieceType pt );

PlyLink * next_ply_link( Ply const * const restrict ply );

bool do_ply( Chessboard * const restrict cb, Move const * const restrict move, Ply const * const restrict ply );

bool do_move( Chessboard * const restrict cb, Move const * const restrict move );


#endif /* __DO_MOVE_H__ */
