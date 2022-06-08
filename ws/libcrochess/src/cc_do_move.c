// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_do_move.h"

/**
    @file cc_do_move.c
    @brief Functions applying a move (algebraic notation string) to a chessboard.
*/


bool cc_do_move( char const * restrict move_an_str,
                 CcChessboard * restrict cb__io )
{
    if ( !move_an_str ) return false;
    if ( !cb__io ) return false;

    // TODO

    return false;
}
