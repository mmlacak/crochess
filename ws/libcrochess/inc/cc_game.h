// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>

#include "cc_chessboard.h"

/**
    @file cc_game.h
    @brief Status of a game, and functionality to progress it.
*/

// TODO :: add status parameter
/**
    Applies a move (algebraic notation string) to a chessboard.

    @param move_an_str A complete move, algebraic notation string.
    @param cb__io An _input/output_ parameter, chessboard to be altered.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_move( char const * restrict move_an_str,
                 CcChessboard * restrict cb__io );
// TODO :: add status parameter

#endif /* __CC_GAME_H__ */
