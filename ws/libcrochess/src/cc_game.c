// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_game.h"

/**
    @file cc_game.c
    @brief Status of a game, and functionality to progress it.
*/


// TODO :: add status parameter
bool cc_do_move( char const * restrict move_an_str,
                 CcChessboard * restrict cb__io )
{
    if ( !move_an_str ) return false;
    if ( !cb__io ) return false;

    // TODO

    return false;
}
// TODO :: add status parameter
