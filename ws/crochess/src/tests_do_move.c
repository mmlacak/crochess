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

#include "tests_do_move.h"


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

    bool result = true;

    result = result && ( cb_get_piece( cb, 5, 2 ) == PT_LightPegasus );
    result = result && ( cb_get_piece( cb, 10, 12 ) == PT_DarkPawn );

    if ( !result )
    {
        free( cb );
        return false;
    }

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

    result = result && ( cb_get_piece( cb, 5, 2 ) == PT_None );
    result = result && ( cb_get_piece( cb, 10, 12 ) == PT_LightPegasus );

    //
    // free, return

    mv_free_complete_move( &move );
    free( cb );

    return result;
}

bool tst_cascading_plies()
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 1, 5, PT_LightPegasus );
    cb_set_piece( cb, 7, 2, PT_LightWave );
    cb_set_piece_tag( cb, 9, 1, PT_LightPawn, TT_CanRush );
    cb_set_piece( cb, 10, 3, PT_DarkPawn );
    cb_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 1, 5 ) == PT_LightPegasus );
    result = result && ( cb_get_piece( cb, 7, 2 ) == PT_LightWave );
    result = result && ( cb_get_piece( cb, 9, 1 ) == PT_LightPawn );
    result = result && ( cb_get_tag( cb, 9, 1 ) == TT_CanRush );
    result = result && ( cb_get_piece( cb, 10, 3 ) == PT_DarkPawn );

    if ( !result )
    {
        free( cb );
        return false;
    }

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

    result = result && ( cb_get_piece( cb, 1, 5 ) == PT_None );
    result = result && ( cb_get_piece( cb, 7, 2 ) == PT_LightPegasus );
    result = result && ( cb_get_piece( cb, 9, 1 ) == PT_LightWave );
    result = result && ( cb_get_tag( cb, 9, 1 ) == TT_None );
    result = result && ( cb_get_piece( cb, 9, 4 ) == PT_LightPawn );
    result = result && ( cb_get_tag( cb, 9, 4 ) == TT_None );
    result = result && ( cb_get_piece( cb, 10, 3 ) == PT_DarkPawn );

    if ( !result )
    {
        mv_free_complete_move( &move_0 );
        free( cb );

        return false;
    }

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

    result = result && ( cb_get_piece( cb, 1, 5 ) == PT_None );
    result = result && ( cb_get_piece( cb, 7, 2 ) == PT_LightPegasus );
    result = result && ( cb_get_piece( cb, 9, 1 ) == PT_LightWave );
    result = result && ( cb_get_tag( cb, 9, 1 ) == TT_None );
    result = result && ( cb_get_piece( cb, 9, 2 ) == PT_DarkPawn );
    result = result && ( cb_get_tag( cb, 9, 4 ) == TT_None );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    mv_free_complete_move( &move_1 );
    free( cb );

    return result;
}

bool tst_castling()
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece_tag( cb, 1, 0, PT_LightRook, TT_CanCastle );
    cb_set_piece_tag( cb, 13, 0, PT_LightKing, TT_CanCastle );
    cb_set_piece_tag( cb, 24, 0, PT_LightRook, TT_CanCastle );

    cb_print( cb, true );
    cb_print( cb, false );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 1, 0 ) == PT_LightRook );
    result = result && ( cb_get_piece( cb, 13, 0 ) == PT_LightKing );
    result = result && ( cb_get_piece( cb, 24, 0 ) == PT_LightRook );

    result = result && ( cb_get_tag( cb, 1, 0 ) == TT_CanCastle );
    result = result && ( cb_get_tag( cb, 13, 0 ) == TT_CanCastle );
    result = result && ( cb_get_tag( cb, 24, 0 ) == TT_CanCastle );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // move Ku&t

    Step * step_0 = step_new_alx( SL_Start, 13, 0 );
    if ( !step_0 )
    {
        free( cb );
        return false;
    }

    Step * step_1 = step_new_alx( SL_Destination, 20, 0 );
    step_0->next = step_1;
    if ( !step_1 )
    {
        step_free_all_steps( &step_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse = ply_side_effect_castle( PT_LightRook, 24, 0, 19, 0 );
    Ply * ply = ply_new_ply_alx( PT_LightKing, step_0, pse );
    if ( !ply )
    {
        step_free_all_steps( &step_0 );
        free( cb );
        return false;
    }

    Move * move = mv_new_alx( ply, MS_None );
    if ( !move )
    {
        ply_free_all_plies( &ply );
        free( cb );
        return false;
    }

    do_move( cb, move );

    cb_print( cb, true );
    cb_print( cb, false );

    //
    // tests

    result = result && ( cb_get_piece( cb, 1, 0 ) == PT_LightRook );
    result = result && ( cb_get_piece( cb, 19, 0 ) == PT_LightRook );
    result = result && ( cb_get_piece( cb, 20, 0 ) == PT_LightKing );

    result = result && ( cb_get_tag( cb, 1, 0 ) == TT_CanCastle );
    result = result && ( cb_get_tag( cb, 19, 0 ) == TT_None );
    result = result && ( cb_get_tag( cb, 20, 0 ) == TT_None );

    //
    // free, return

    mv_free_complete_move( &move );
    free( cb );

    return result;
}

bool tst_tag_and_promotion()
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 11, 21, PT_LightPawn );
    cb_set_piece( cb, 15, 21, PT_LightPyramid );
    cb_set_piece( cb, 21, 15, PT_LightBishop );

    cb_print( cb, true );
    // cb_print( cb, false );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 11, 21 ) == PT_LightPawn );
    result = result && ( cb_get_piece( cb, 15, 21 ) == PT_LightPyramid );
    result = result && ( cb_get_piece( cb, 21, 15 ) == PT_LightBishop );

    result = result && ( cb_get_tag( cb, 11, 21 ) == TT_None );
    result = result && ( cb_get_tag( cb, 15, 21 ) == TT_None );
    result = result && ( cb_get_tag( cb, 21, 15 ) == TT_None );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bp22~

    Step * step_0_0 = step_new_alx( SL_Start, 21, 15 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 15, 21 );
    step_0_0->next = step_0_1;
    if ( !step_0_1 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * ply_0 = ply_new_ply_alx( PT_LightBishop, step_0_0, pse_0 );
    if ( !ply_0 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    //
    // ply Al22=

    Step * step_1_0 = step_new_alx( SL_Start, 15, 21 );
    if ( !step_1_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    Step * step_1_1 = step_new_alx( SL_Destination, 11, 21 );
    step_1_0->next = step_1_1;
    if ( !step_1_1 )
    {
        step_free_all_steps( &step_1_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_tag_for_promotion();
    Ply * ply_1 = ply_new_ply_alx( PT_LightPyramid, step_1_0, pse_1 );
    ply_0->next = ply_1;
    if ( !ply_1 )
    {
        step_free_all_steps( &step_1_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    //
    // move Bp22~Al22=

    Move * move_0 = mv_new_alx( ply_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    cb_print( cb, true );
    // cb_print( cb, false );

    //
    // tests

    result = result && ( cb_get_piece( cb, 11, 21 ) == PT_LightPawn );
    result = result && ( cb_get_piece( cb, 15, 21 ) == PT_LightBishop );

    result = result && ( cb_get_tag( cb, 11, 21 ) == TT_DelayedPromotion );
    result = result && ( cb_get_tag( cb, 15, 21 ) == TT_None );
    result = result && ( cb_get_tag( cb, 21, 15 ) == TT_None );

    if ( !result )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    //
    // ply l22Q

    Step * step_2_0 = step_new_alx( SL_Start, 11, 21 );
    if ( !step_2_0 )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    Step * step_2_1 = step_new_alx( SL_Destination, 11, 21 );
    step_2_0->next = step_2_1;
    if ( !step_2_1 )
    {
        step_free_all_steps( &step_2_0 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_promote( PT_LightQueen );
    Ply * ply_2 = ply_new_ply_alx( PT_LightPawn, step_2_0, pse_2 );
    if ( !ply_2 )
    {
        step_free_all_steps( &step_2_0 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    //
    // move l22Q

    Move * move_1 = mv_new_alx( ply_2, MS_None );
    if ( !move_1 )
    {
        ply_free_all_plies( &ply_2 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_1 );

    cb_print( cb, true );
    // cb_print( cb, false );

    //
    // tests

    result = result && ( cb_get_piece( cb, 11, 21 ) == PT_LightQueen );
    result = result && ( cb_get_piece( cb, 15, 21 ) == PT_LightBishop );

    result = result && ( cb_get_tag( cb, 11, 21 ) == TT_None );
    result = result && ( cb_get_tag( cb, 15, 21 ) == TT_None );
    result = result && ( cb_get_tag( cb, 21, 15 ) == TT_None );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    mv_free_complete_move( &move_1 );
    free( cb );

    return result;
}

bool tst_conversion( bool is_failed )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    if ( is_failed )
        cb_set_piece( cb, 11, 5, PT_DarkStarchild );
    else
        cb_set_piece( cb, 11, 5, PT_DarkShaman );

    cb_set_piece( cb, 15, 5, PT_LightPyramid );
    cb_set_piece( cb, 21, 11, PT_LightBishop );

    cb_print( cb, true );

    //
    // tests

    bool result = true;

    if ( is_failed )
        result = result && ( cb_get_piece( cb, 11, 5 ) == PT_DarkStarchild );
    else
        result = result && ( cb_get_piece( cb, 11, 5 ) == PT_DarkShaman );

    result = result && ( cb_get_piece( cb, 15, 5 ) == PT_LightPyramid );
    result = result && ( cb_get_piece( cb, 21, 11 ) == PT_LightBishop );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bp6~

    Step * step_0_0 = step_new_alx( SL_Start, 21, 11 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 15, 5 );
    step_0_0->next = step_0_1;
    if ( !step_0_1 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * ply_0 = ply_new_ply_alx( PT_LightBishop, step_0_0, pse_0 );
    if ( !ply_0 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    //
    // ply Al6%H

    Step * step_1_0 = step_new_alx( SL_Start, 15, 5 );
    if ( !step_1_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    Step * step_1_1 = step_new_alx( SL_Destination, 11, 5 );
    step_1_0->next = step_1_1;
    if ( !step_1_1 )
    {
        step_free_all_steps( &step_1_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1;
    if ( is_failed )
        pse_1 = ply_side_effect_failed_conversion();
    else
        pse_1 = ply_side_effect_convert( PT_LightShaman, false );

    Ply * ply_1 = ply_new_ply_alx( PT_LightPyramid, step_1_0, pse_1 );
    ply_0->next = ply_1;
    if ( !ply_1 )
    {
        step_free_all_steps( &step_1_0 );
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    //
    // move Bp6~Al6%H

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


    if ( is_failed )
        result = result && ( cb_get_piece( cb, 11, 5 ) == PT_DarkStarchild );
    else
        result = result && ( cb_get_piece( cb, 11, 5 ) == PT_LightShaman );

    result = result && ( cb_get_piece( cb, 15, 5 ) == PT_LightBishop );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}
