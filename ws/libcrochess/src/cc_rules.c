// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include "cc_rules.h"


bool cc_rules_do_moves( CcGame * const restrict gm,
                        CcMove ** const restrict moves__n,
                        CcDoMoveEnum dme )
{
    if ( !gm ) return false;
    if ( !moves__n ) return false;
    if ( !*moves__n ) return false;

// TODO :: status after each move

    if ( !cc_do_moves( gm->chessboard, *moves__n, dme ) )
        return false;

    if ( !cc_move_append_or_init( &( gm->moves ), moves__n ) )
        return false;

    return true;
}


bool cc_rules_make_move( CcGame * const restrict game,
                         char const * const restrict move_an )
{
    if ( !game ) return false;
    if ( !move_an ) return false;
    if ( !CC_GAME_STATUS_IS_TURN( game->status ) ) return false;


    return true;
}
