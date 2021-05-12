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


bool tst_single_ply( bool do_print )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 5, 2, PT_LightPegasus );
    cb_set_piece( cb, 10, 12, PT_DarkPawn );
    if ( do_print ) cb_print( cb, true );

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
    if ( do_print ) cb_print( cb, true );

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

bool tst_cascading_plies( bool do_print )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 1, 5, PT_LightPegasus );
    cb_set_piece( cb, 7, 2, PT_LightWave );
    cb_set_piece_tag( cb, 9, 1, PT_LightPawn, TT_CanRush );
    cb_set_piece( cb, 10, 3, PT_DarkPawn );
    if ( do_print ) cb_print( cb, true );

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
    if ( do_print ) cb_print( cb, true );

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
    if ( do_print ) cb_print( cb, true );

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

bool tst_castling( bool do_print )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece_tag( cb, 1, 0, PT_LightRook, TT_CanCastle );
    cb_set_piece_tag( cb, 13, 0, PT_LightKing, TT_CanCastle );
    cb_set_piece_tag( cb, 24, 0, PT_LightRook, TT_CanCastle );

    if ( do_print )
    {
        cb_print( cb, true );
        cb_print( cb, false );
    }

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

    if ( do_print )
    {
        cb_print( cb, true );
        cb_print( cb, false );
    }

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

bool tst_tag_and_promotion( bool do_print )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 11, 21, PT_LightPawn );
    cb_set_piece( cb, 15, 21, PT_LightPyramid );
    cb_set_piece( cb, 21, 15, PT_LightBishop );

    if ( do_print )
    {
        cb_print( cb, true );
        // cb_print( cb, false );
    }

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

    if ( do_print )
    {
        cb_print( cb, true );
        // cb_print( cb, false );
    }

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

    if ( do_print )
    {
        cb_print( cb, true );
        // cb_print( cb, false );
    }

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

bool tst_conversion( bool do_print, bool is_failed )
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

    if ( do_print ) cb_print( cb, true );

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

    if ( do_print ) cb_print( cb, true );

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

bool tst_demotion( bool do_print )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 0, 0, PT_BrightStar );
    cb_set_piece( cb, 25, 25, PT_BrightStar );
    cb_set_piece( cb, 11, 11, PT_LightBishop );
    cb_set_piece( cb, 23, 15, PT_Monolith );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 0, 0 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 25, 25 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 11, 11 ) == PT_LightBishop );
    result = result && ( cb_get_piece( cb, 23, 15 ) == PT_Monolith );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Mw23>Bl12

    Step * step_0_0 = step_new_alx( SL_Start, 23, 15 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 22, 22 );
    step_0_0->next = step_0_1;
    if ( !step_0_1 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_demote( PT_LightPawn, 11, 11 );
    Ply * ply_0 = ply_new_ply_alx( PT_Monolith, step_0_0, pse_0 );
    if ( !ply_0 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    //
    // move Mw23>Bl12

    Move * move_0 = mv_new_alx( ply_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    result = result && ( cb_get_piece( cb, 0, 0 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 25, 25 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 11, 11 ) == PT_LightPawn );
    result = result && ( cb_get_piece( cb, 22, 22 ) == PT_Monolith );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}

bool tst_resurrection( bool do_print, bool is_failed, bool is_oblationing )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 25, 0, PT_DimStar );
    cb_set_piece( cb, 0, 25, PT_DimStar );
    cb_set_piece( cb, 23, 15, PT_LightStarchild );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 25, 0 ) == PT_DimStar );
    result = result && ( cb_get_piece( cb, 0, 25 ) == PT_DimStar );
    result = result && ( cb_get_piece( cb, 23, 15 ) == PT_LightStarchild );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Ip11$B, Ip11$$

    Step * step_0_0 = step_new_alx( SL_Start, 23, 15 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 15, 10 );
    step_0_0->next = step_0_1;
    if ( !step_0_1 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0;
    if ( is_failed )
        pse_0 = ply_side_effect_failed_resurrection();
    else
    {
        if ( is_oblationing )
            pse_0 = ply_side_effect_resurrect( PT_LightBishop, 15, 10 );
        else
            pse_0 = ply_side_effect_resurrect( PT_LightWave, 16, 11 );
    }

    Ply * ply_0 = ply_new_ply_alx( PT_LightStarchild, step_0_0, pse_0 );
    if ( !ply_0 )
    {
        step_free_all_steps( &step_0_0 );
        free( cb );
        return false;
    }

    //
    // move Ip11$B, Ip11$$

    Move * move_0 = mv_new_alx( ply_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    result = result && ( cb_get_piece( cb, 25, 0 ) == PT_DimStar );
    result = result && ( cb_get_piece( cb, 0, 25 ) == PT_DimStar );

    if ( is_failed )
    {
        result = result && ( cb_get_piece( cb, 15, 10 ) == PT_LightStarchild );
        result = result && ( cb_get_piece( cb, 16, 11 ) == PT_None );
    }
    else
    {
        if ( is_oblationing )
        {
            result = result && ( cb_get_piece( cb, 15, 10 ) == PT_LightBishop );
            result = result && ( cb_get_piece( cb, 16, 11 ) == PT_None );
        }
        else
        {
            result = result && ( cb_get_piece( cb, 15, 10 ) == PT_LightStarchild );
            result = result && ( cb_get_piece( cb, 16, 11 ) == PT_LightWave );
        }
    }

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}

bool tst_teleportation( bool do_print, bool is_failed )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 0, 0, PT_BrightStar );
    cb_set_piece( cb, 25, 25, PT_BrightStar );
    cb_set_piece( cb, 25, 0, PT_DimStar );
    cb_set_piece( cb, 0, 25, PT_DimStar );

    cb_set_piece( cb, 3, 22, PT_LightBishop );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 0, 0 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 25, 25 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 25, 0 ) == PT_DimStar );
    result = result && ( cb_get_piece( cb, 0, 25 ) == PT_DimStar );

    result = result && ( cb_get_piece( cb, 3, 22 ) == PT_LightBishop );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Ba26

    Step * step_0_0 = step_new_alx( SL_Start, 3, 22 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 0, 25 );
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
    // ply |By25

    PlySideEffect pse_1 = ply_side_effect_none();
    Ply * ply_1;
    if ( is_failed )
        ply_1 = ply_new_failed_teleport_alx( PT_LightBishop, 0, 24, pse_1 );
    else
        ply_1 = ply_new_teleport_alx( PT_LightBishop, 24, 24, pse_1 );
    ply_0->next = ply_1;
    if ( !ply_1 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    //
    // move Ba26|By25

    Move * move_0 = mv_new_alx( ply_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    result = result && ( cb_get_piece( cb, 0, 0 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 25, 25 ) == PT_BrightStar );
    result = result && ( cb_get_piece( cb, 25, 0 ) == PT_DimStar );
    result = result && ( cb_get_piece( cb, 0, 25 ) == PT_DimStar );

    result = result && ( cb_get_piece( cb, 3, 22 ) == PT_None );

    if ( is_failed )
    {
        result = result && ( cb_get_piece( cb, 0, 24 ) == PT_LightBishop );
        result = result && ( cb_get_piece( cb, 24, 24 ) == PT_None );
    }
    else
    {
        result = result && ( cb_get_piece( cb, 0, 24 ) == PT_None );
        result = result && ( cb_get_piece( cb, 24, 24 ) == PT_LightBishop );
    }

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}

bool tst_teleportation_wave( bool do_print, bool is_oblationing )
{
    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 5, 11, PT_Monolith );
    cb_set_piece( cb, 19, 9, PT_Monolith );

    cb_set_piece( cb, 10, 12, PT_LightBishop );
    cb_set_piece( cb, 8, 14, PT_LightWave );
    cb_set_piece( cb, 17, 7, PT_LightKnight );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 5, 11 ) == PT_Monolith );
    result = result && ( cb_get_piece( cb, 19, 9 ) == PT_Monolith );

    result = result && ( cb_get_piece( cb, 10, 12 ) == PT_LightBishop );
    result = result && ( cb_get_piece( cb, 8, 14 ) == PT_LightWave );
    result = result && ( cb_get_piece( cb, 17, 7 ) == PT_LightKnight );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bi15

    Step * step_0_0 = step_new_alx( SL_Start, 10, 12 );
    if ( !step_0_0 )
    {
        free( cb );
        return false;
    }

    Step * step_0_1 = step_new_alx( SL_Destination, 8, 14 );
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
    // ply ~Wf12

    Step * step_1_0 = step_new_alx( SL_Start, 8, 14 );
    if ( !step_1_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    Step * step_1_1 = step_new_alx( SL_Destination, 5, 11 );
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
    // ply |Wr8

    PlySideEffect pse_2 = ply_side_effect_none();
    Ply * ply_2;

    if ( is_oblationing )
        ply_2 = ply_new_failed_teleport_oblation_alx( PT_LightWave, pse_2 );
    else
    {
        Step * step_2_0 = step_new_alx( SL_Start, 19, 9 );
        if ( !step_2_0 )
        {
            ply_free_all_plies( &ply_0 );
            free( cb );
            return false;
        }

        Step * step_2_1 = step_new_alx( SL_Destination, 17, 7 );
        step_2_0->next = step_2_1;
        if ( !step_2_1 )
        {
            step_free_all_steps( &step_2_0 );
            ply_free_all_plies( &ply_0 );
            free( cb );
            return false;
        }

        ply_2 = ply_new_teleport_wave_alx( PT_LightWave, step_2_0, pse_2 );
    }

    ply_1->next = ply_2;
    if ( !ply_2 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Np9

    if ( !is_oblationing )
    {
        Step * step_3_0 = step_new_alx( SL_Start, 17, 7 );
        if ( !step_3_0 )
        {
            ply_free_all_plies( &ply_0 );
            free( cb );
            return false;
        }

        Step * step_3_1 = step_new_alx( SL_Destination, 15, 8 );
        step_3_0->next = step_3_1;
        if ( !step_3_1 )
        {
            step_free_all_steps( &step_3_0 );
            ply_free_all_plies( &ply_0 );
            free( cb );
            return false;
        }

        PlySideEffect pse_3 = ply_side_effect_none();
        Ply * ply_3 = ply_new_ply_alx( PT_LightKnight, step_3_0, pse_3 );
        ply_2->next = ply_3;
        if ( !ply_3 )
        {
            step_free_all_steps( &step_3_0 );
            ply_free_all_plies( &ply_0 );
            free( cb );
            return false;
        }
    }

    //
    // move Bi15~Wf12|Wr8~Np9

    Move * move_0 = mv_new_alx( ply_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &ply_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    result = result && ( cb_get_piece( cb, 5, 11 ) == PT_Monolith );
    result = result && ( cb_get_piece( cb, 19, 9 ) == PT_Monolith );

    result = result && ( cb_get_piece( cb, 10, 12 ) == PT_None );
    result = result && ( cb_get_piece( cb, 8, 14 ) == PT_LightBishop );

    if ( is_oblationing )
    {
        result = result && ( cb_get_piece( cb, 17, 7 ) == PT_LightKnight );
    }
    else
    {
        result = result && ( cb_get_piece( cb, 17, 7 ) == PT_LightWave );
        result = result && ( cb_get_piece( cb, 15, 8 ) == PT_LightKnight );
    }

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}
