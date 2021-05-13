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

    Step * start = step_append_alx( NULL, SL_Start, 5, 2 );
    if ( !start )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( start, SL_Next, 6, 4 ) )
    {
        step_free_all_steps( &start );
        free( cb );
        return false;
    }

    if ( !step_append_alx( start, SL_Distant, 8, 8 ) )
    {
        step_free_all_steps( &start );
        free( cb );
        return false;
    }

    if ( !step_append_alx( start, SL_Destination, 10, 12 ) )
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

    Step * steps_0 = step_new_alx( SL_Start, 1, 5 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 7, 2 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( PT_LightPegasus, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply 1, W --> P

    Step * steps_1 = step_new_alx( SL_Start, 7, 2 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_1, SL_Destination, 9, 1 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, PT_LightWave, steps_1, pse_1 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply 2, P --> ...

    Step * steps_2 = step_new_alx( SL_Start, 9, 1 );
    if ( !steps_2 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_2, SL_Destination, 9, 4 ) )
    {
        step_free_all_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, PT_LightPawn, steps_2, pse_2 ) )
    {
        step_free_all_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move 0, G --> W --> P --> ...

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

    Step * steps_3 = step_new_alx( SL_Start, 10, 3 );
    if ( !steps_3 )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_3, SL_Destination, 9, 2 ) )
    {
        step_free_all_steps( &steps_3 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_3 = ply_side_effect_en_passant( 9, 4 );
    Ply * plies_3 = ply_new_ply_alx( PT_DarkPawn, steps_3, pse_3 );
    if ( !plies_3 )
    {
        step_free_all_steps( &steps_3 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    Move * move_1 = mv_new_alx( plies_3, MS_None );
    if ( !move_1 )
    {
        ply_free_all_plies( &plies_3 );
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

    Step * steps_0 = step_new_alx( SL_Start, 13, 0 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 20, 0 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse = ply_side_effect_castle( PT_LightRook, 24, 0, 19, 0 );
    Ply * ply = ply_new_ply_alx( PT_LightKing, steps_0, pse );
    if ( !ply )
    {
        step_free_all_steps( &steps_0 );
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

    Step * steps_0 = step_new_alx( SL_Start, 21, 15 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 15, 21 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( PT_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply Al22=

    Step * steps_1 = step_new_alx( SL_Start, 15, 21 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_1, SL_Destination, 11, 21 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_tag_for_promotion();
    if ( !ply_append_ply_alx( plies_0, PT_LightPyramid, steps_1, pse_1 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move Bp22~Al22=

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

    Step * steps_2 = step_new_alx( SL_Start, 11, 21 );
    if ( !steps_2 )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_2, SL_Destination, 11, 21 ) )
    {
        step_free_all_steps( &steps_2 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_promote( PT_LightQueen );
    Ply * plies_2 = ply_new_ply_alx( PT_LightPawn, steps_2, pse_2 );
    if ( !plies_2 )
    {
        step_free_all_steps( &steps_2 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    //
    // move l22Q

    Move * move_1 = mv_new_alx( plies_2, MS_None );
    if ( !move_1 )
    {
        ply_free_all_plies( &plies_2 );
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

    Step * steps_0 = step_new_alx( SL_Start, 21, 11 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 15, 5 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( PT_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply Al6%H

    Step * steps_1 = step_new_alx( SL_Start, 15, 5 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_1, SL_Destination, 11, 5 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1;
    if ( is_failed )
        pse_1 = ply_side_effect_failed_conversion();
    else
        pse_1 = ply_side_effect_convert( PT_LightShaman, false );

    if ( !ply_append_ply_alx( plies_0, PT_LightPyramid, steps_1, pse_1 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move Bp6~Al6%H

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

    Step * steps_0 = step_new_alx( SL_Start, 23, 15 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 22, 22 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_demote( PT_LightPawn, 11, 11 );
    Ply * plies_0 = ply_new_ply_alx( PT_Monolith, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // move Mw23>Bl12

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

    Step * steps_0 = step_new_alx( SL_Start, 23, 15 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 15, 10 ) )
    {
        step_free_all_steps( &steps_0 );
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

    Ply * plies_0 = ply_new_ply_alx( PT_LightStarchild, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // move Ip11$B, Ip11$$

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

    Step * steps_0 = step_new_alx( SL_Start, 3, 22 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 0, 25 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( PT_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply |By25

    PlySideEffect pse_1 = ply_side_effect_none();

    Ply * ply_1;
    if ( is_failed )
        ply_1 = ply_append_failed_teleport_alx( plies_0, PT_LightBishop, 0, 24, pse_1 );
    else
        ply_1 = ply_append_teleport_alx( plies_0, PT_LightBishop, 24, 24, pse_1 );

    if ( !ply_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move Ba26|By25

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

    Step * steps_0 = step_new_alx( SL_Start, 10, 12 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 8, 14 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( PT_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Wf12

    Step * steps_1 = step_new_alx( SL_Start, 8, 14 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_1, SL_Destination, 5, 11 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, PT_LightWave, steps_1, pse_1 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply |Wr8

    PlySideEffect pse_2 = ply_side_effect_none();
    Ply * ply_2;

    if ( is_oblationing )
        ply_2 = ply_append_failed_teleport_oblation_alx( plies_0, PT_LightWave, pse_2 );
    else
    {
        Step * steps_2 = step_new_alx( SL_Start, 19, 9 );
        if ( !steps_2 )
        {
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !step_append_alx( steps_2, SL_Destination, 17, 7 ) )
        {
            step_free_all_steps( &steps_2 );
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        ply_2 = ply_append_teleport_wave_alx( plies_0, PT_LightWave, steps_2, pse_2 );
    }

    if ( !ply_2 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Np9

    if ( !is_oblationing )
    {
        Step * steps_3 = step_new_alx( SL_Start, 17, 7 );
        if ( !steps_3 )
        {
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !step_append_alx( steps_3, SL_Destination, 15, 8 ) )
        {
            step_free_all_steps( &steps_3 );
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        PlySideEffect pse_3 = ply_side_effect_none();
        if ( !ply_append_ply_alx( plies_0, PT_LightKnight, steps_3, pse_3 ) )
        {
            step_free_all_steps( &steps_3 );
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }
    }

    //
    // move Bi15~Wf12|Wr8~Np9

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
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

bool tst_trance_journey( bool do_print, bool is_capturing )
{
    PieceType shaman = is_capturing ? PT_DarkShaman : PT_LightShaman;

    // chessboard

    Chessboard * cb = cb_new_alx( BT_One, false );
    if ( !cb ) return false;

    cb_set_piece( cb, 4, 8, shaman ); // entrancing
    cb_set_piece( cb, 6, 9, PT_LightWave );
    cb_set_piece( cb, 7, 7, shaman ); // entranced

    cb_set_piece( cb, 7, 12, PT_LightBishop ); // 2
    cb_set_piece( cb, 5, 1, PT_DarkKnight ); // 4
    cb_set_piece( cb, 21, 4, PT_DarkPawn ); // 9

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cb_get_piece( cb, 4, 8 ) == shaman );
    result = result && ( cb_get_piece( cb, 6, 9 ) == PT_LightWave );
    result = result && ( cb_get_piece( cb, 7, 7 ) == shaman );

    result = result && ( cb_get_piece( cb, 7, 12 ) == PT_LightBishop );
    result = result && ( cb_get_piece( cb, 5, 1 ) == PT_DarkKnight );
    result = result && ( cb_get_piece( cb, 21, 4 ) == PT_DarkPawn );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Hg10

    Step * steps_0 = step_new_alx( SL_Start, 4, 8 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_0, SL_Destination, 6, 9 ) )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( shaman, steps_0, pse_0 );
    if ( !plies_0 )
    {
        step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Wh8

    Step * steps_1 = step_new_alx( SL_Start, 6, 9 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !step_append_alx( steps_1, SL_Destination, 7, 7 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, PT_LightWave, steps_1, pse_1 ) )
    {
        step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply @H..h13<Bj19..f2<Nb6..p7..j19<Bl25..v5<Pp7

    StepSideEffect sse_2_0 = step_side_effect_none();
    SideEffectStep * steps_2 = step_new_side_effect_alx( SL_Start, 7, 7, sse_2_0 );
    if ( !steps_2 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    StepSideEffect sse_2_1 = step_side_effect_displacement( PT_LightBishop, false, 9, 18 );
    if ( !step_append_side_effect_alx( steps_2, SL_Distant, 7, 12, sse_2_1 ) )
    {
        step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    StepSideEffect sse_2_2 = step_side_effect_displacement( PT_DarkKnight, false, 1, 5 );
    if ( !step_append_side_effect_alx( steps_2, SL_Distant, 5, 1, sse_2_2 ) )
    {
        step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    StepSideEffect sse_2_3 = step_side_effect_none();
    if ( !step_append_side_effect_alx( steps_2, SL_Distant, 15, 6, sse_2_3 ) )
    {
        step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    StepSideEffect sse_2_4 = step_side_effect_displacement( PT_LightBishop, false, 11, 24 );
    if ( !step_append_side_effect_alx( steps_2, SL_Distant, 9, 18, sse_2_4 ) )
    {
        step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    StepSideEffect sse_2_5 = step_side_effect_displacement( PT_DarkPawn, false, 15, 6 );
    if ( !step_append_side_effect_alx( steps_2, SL_Destination, 21, 4, sse_2_5 ) )
    {
        step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_none();
    if ( !ply_append_trance_journey_alx( plies_0, shaman, 7, 7, steps_2, pse_2 ) )
    {
        step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move Hg10~Wh8@H..h13<Bj19..f2<Nb6..p7..j19<Bl25..v5<Pp7

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    if ( do_print ) cb_print( cb, true );

    //
    // tests

    result = result && ( cb_get_piece( cb, 4, 8 ) == PT_None );
    result = result && ( cb_get_piece( cb, 6, 9 ) == shaman );
    result = result && ( cb_get_piece( cb, 7, 7 ) == PT_LightWave );
    result = result && ( cb_get_piece( cb, 21, 4 ) == shaman );

    result = result && ( cb_get_piece( cb, 7, 12 ) == PT_None );
    result = result && ( cb_get_piece( cb, 5, 1 ) == PT_None );
    result = result && ( cb_get_piece( cb, 9, 18 ) == PT_None );

    result = result && ( cb_get_piece( cb, 11, 24 ) == PT_LightBishop );
    result = result && ( cb_get_piece( cb, 1, 5 ) == PT_DarkKnight );
    result = result && ( cb_get_piece( cb, 15, 6 ) == PT_DarkPawn );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}
