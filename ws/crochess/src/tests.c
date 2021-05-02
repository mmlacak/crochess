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
    // tests

    if ( cb_get_piece( cb, 5, 2 ) != PT_LightPegasus ) return false;
    if ( cb_get_piece( cb, 10, 12 ) != PT_DarkPawn ) return false;

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
    // tests

    if ( cb_get_piece( cb, 5, 2 ) != PT_None ) return false;
    if ( cb_get_piece( cb, 10, 12 ) != PT_LightPegasus ) return false;

    //
    // free, return

    mv_free_complete_move( &move );
    free( cb );

    return true;
}

bool tst_cascading_plies()
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 1, 5, PT_LightPegasus );
    cb_set_piece( cb, 7, 2, PT_LightWave );
    cb_set_piece( cb, 9, 1, PT_LightPawn );
    cb_set_piece( cb, 10, 3, PT_DarkPawn );
    cb_print( cb, true );

    //
    // tests

    if ( cb_get_piece( cb, 1, 5 ) != PT_LightPegasus ) return false;
    if ( cb_get_piece( cb, 7, 2 ) != PT_LightWave ) return false;
    if ( cb_get_piece( cb, 9, 1 ) != PT_LightPawn ) return false;
    if ( cb_get_piece( cb, 10, 3 ) != PT_DarkPawn ) return false;

    //
    // ply 0, G --> W

    Step * step_0_0 = step_new_alx( SL_Start, 1, 5 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 7, 2 );
    step_0_0->next = step_0_1;
    if ( !step_0_1 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * ply_0 = ply_new_ply_alx( PT_LightPegasus, step_0_0, pse_0 );
    if ( !ply_0 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    //
    // ply 1, W --> P

    Step * step_1_0 = step_new_alx( SL_Start, 7, 2 );
    if ( !step_1_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    Step * step_1_1 = step_new_alx( SL_Destination, 9, 1 );
    step_1_0->next = step_1_1;
    if ( !step_1_1 )
    {
        step_free_all_steps( &step_1_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    Ply * ply_1 = ply_new_ply_alx( PT_LightWave, step_1_0, pse_1 );
    ply_0->next = ply_1;
    if ( !ply_1 )
    {
        step_free_all_steps( &step_1_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    //
    // ply 2, P --> ...

    Step * step_2_0 = step_new_alx( SL_Start, 9, 1 );
    if ( !step_2_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    Step * step_2_1 = step_new_alx( SL_Destination, 9, 4 );
    step_2_0->next = step_2_1;
    if ( !step_2_1 )
    {
        step_free_all_steps( &step_2_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_none();
    Ply * ply_2 = ply_new_ply_alx( PT_LightPawn, step_2_0, pse_2 );
    ply_1->next = ply_2;
    if ( !ply_2 )
    {
        step_free_all_steps( &step_2_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    //
    // move 0, G --> W --> P --> ...

    Move * move_0 = mv_new_alx( ply_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );
    cb_print( cb, true );

    //
    // tests

    if ( cb_get_piece( cb, 1, 5 ) != PT_None ) return false;
    if ( cb_get_piece( cb, 7, 2 ) != PT_LightPegasus ) return false;
    if ( cb_get_piece( cb, 9, 1 ) != PT_LightWave ) return false;
    if ( cb_get_piece( cb, 9, 4 ) != PT_LightPawn ) return false;
    if ( cb_get_piece( cb, 10, 3 ) != PT_DarkPawn ) return false;

    //
    // move 1, p --> :P

    Step * step_3_0 = step_new_alx( SL_Start, 10, 3 );
    if ( !step_3_0 )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    Step * step_3_1 = step_new_alx( SL_Destination, 9, 2 );
    step_3_0->next = step_3_1;
    if ( !step_3_1 )
    {
        step_free_all_steps( &step_3_0 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_3 = ply_side_effect_en_passant( 9, 4 );
    Ply * ply_3 = ply_new_ply_alx( PT_DarkPawn, step_3_0, pse_3 );
    if ( !ply_3 )
    {
        step_free_all_steps( &step_3_0 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    Move * move_1 = mv_new_alx( ply_3, MS_None );
    if ( !move_1 )
    {
        ply_free_all_plies( &ply_3 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_1 );
    cb_print( cb, true );

    //
    // tests

    if ( cb_get_piece( cb, 1, 5 ) != PT_None ) return false;
    if ( cb_get_piece( cb, 7, 2 ) != PT_LightPegasus ) return false;
    if ( cb_get_piece( cb, 9, 1 ) != PT_LightWave ) return false;
    if ( cb_get_piece( cb, 9, 2 ) != PT_DarkPawn ) return false;

    //
    // free, return

    mv_free_complete_move( &move_0 );
    mv_free_complete_move( &move_1 );
    free( cb );

    return true;
}
