// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DO_MOVE_H__
#define __CC_DO_MOVE_H__

#include <stdbool.h>

#include "cc_chessboard.h"

/**
    @file cc_do_move.h
    @brief Functions applying a move (algebraic notation string) to a chessboard.
*/

/**
    Applies a move (algebraic notation string) to a chessboard.

    @param move_an_str A complete move, algebraic notation string.
    @param cb__io An _input/output_ parameter, chessboard to be altered.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_move( char const * restrict move_an_str,
                 CcChessboard * restrict cb__io );

#endif /* __CC_DO_MOVE_H__ */
