// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "piece_type.h"
#include "board_type.h"
#include "chessboard.h"

#include "step.h"
#include "ply.h"
#include "move.h"
#include "do_move.h"

#include "tests.h"


bool tst_single_ply()
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 5, 2, PT_LightPegasus );
    cb_set_piece( cb, 10, 12, PT_DarkPawn );
    cb_print( cb, true );

    //
    // steps

    Step * start = step_new_alx( SL_Start, 5, 2 );
    if ( !start )
    {
        free( cb );
        return false;
    }

    Step * step_1 = step_new_alx( SL_Next, 6, 4 );
    start->next = step_1;
    if ( !step_1 )
    {
        step_free_all_steps( &start );
        free( cb );
        return false;
    }

    Step * step_2 = step_new_alx( SL_Distant, 8, 8 );
    step_1->next = step_2;
    if ( !step_2 )
    {
        step_free_all_steps( &start );
        free( cb );
        return false;
    }

    Step * dest = step_new_alx( SL_Destination, 10, 12 );
    step_2->next = dest;
    if ( !dest )
    {
        step_free_all_steps( &start );
        free( cb );
        return false;
    }

    //
    // ply

    PlySideEffect pse = ply_side_effect_capture( PT_DarkPawn, true );
    Ply * ply = ply_new_ply_alx( PT_LightPegasus, start, pse );
    if ( !ply )
    {
        step_free_all_steps( &start );
        free( cb );
        return false;
    }

    //
    // move

    Move * move = mv_new_alx( ply, MS_None );
    if ( !move )
    {
        ply_free_all_plies( &ply );
        free( cb );
        return false;
    }

    do_move( cb, move );
    cb_print( cb, true );

    //
    // free, return

    mv_free_complete_move( &move );
    free( cb );

    return true;
}
