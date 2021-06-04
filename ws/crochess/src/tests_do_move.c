// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>

#include "cc_piece.h"
#include "cc_variant.h"
#include "cc_chessboard.h"

#include "cc_step.h"
#include "ply.h"
#include "move.h"
#include "do_move.h"

#include "tests_do_move.h"


bool tst_single_ply( bool do_print )
{
    // chessboard

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 5, 2, CC_PE_LightPegasus );
    cc_chessboard_set_piece( cb, 10, 12, CC_PE_DarkPawn );
    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 5, 2 ) == CC_PE_LightPegasus );
    result = result && ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_DarkPawn );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // steps

    CcStep * start = cc_step_append_new( NULL, CC_SLE_Start, 5, 2 );
    if ( !start )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( start, CC_SLE_Next, 6, 4 ) )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( start, CC_SLE_Distant, 8, 8 ) )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( start, CC_SLE_Destination, 10, 12 ) )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    //
    // ply

    PlySideEffect pse = ply_side_effect_capture( CC_PE_DarkPawn, true );
    Ply * ply = ply_new_ply_alx( CC_PE_LightPegasus, start, pse );
    if ( !ply )
    {
        cc_step_free_all_steps( &start );
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
    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 5, 2 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_LightPegasus );

    //
    // free, return

    mv_free_complete_move( &move );
    free( cb );

    return result;
}

bool tst_cascading_plies( bool do_print )
{
    // chessboard

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 1, 5, CC_PE_LightPegasus );
    cc_chessboard_set_piece( cb, 7, 2, CC_PE_LightWave );
    cc_chessboard_set_piece_tag( cb, 9, 1, CC_PE_LightPawn, CC_TE_CanRush );
    cc_chessboard_set_piece( cb, 10, 3, CC_PE_DarkPawn );
    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_LightPegasus );
    result = result && ( cc_chessboard_get_piece( cb, 7, 2 ) == CC_PE_LightWave );
    result = result && ( cc_chessboard_get_piece( cb, 9, 1 ) == CC_PE_LightPawn );
    result = result && ( cc_chessboard_get_tag( cb, 9, 1 ) == CC_TE_CanRush );
    result = result && ( cc_chessboard_get_piece( cb, 10, 3 ) == CC_PE_DarkPawn );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply 0, G --> W

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 1, 5 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 7, 2 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( CC_PE_LightPegasus, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply 1, W --> P

    CcStep * steps_1 = cc_step_new( CC_SLE_Start, 7, 2 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_1, CC_SLE_Destination, 9, 1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, CC_PE_LightWave, steps_1, pse_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply 2, P --> ...

    CcStep * steps_2 = cc_step_new( CC_SLE_Start, 9, 1 );
    if ( !steps_2 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_2, CC_SLE_Destination, 9, 4 ) )
    {
        cc_step_free_all_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, CC_PE_LightPawn, steps_2, pse_2 ) )
    {
        cc_step_free_all_steps( &steps_2 );
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
    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 7, 2 ) == CC_PE_LightPegasus );
    result = result && ( cc_chessboard_get_piece( cb, 9, 1 ) == CC_PE_LightWave );
    result = result && ( cc_chessboard_get_tag( cb, 9, 1 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_piece( cb, 9, 4 ) == CC_PE_LightPawn );
    result = result && ( cc_chessboard_get_tag( cb, 9, 4 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_piece( cb, 10, 3 ) == CC_PE_DarkPawn );

    if ( !result )
    {
        mv_free_complete_move( &move_0 );
        free( cb );

        return false;
    }

    //
    // move 1, p --> :P

    CcStep * steps_3 = cc_step_new( CC_SLE_Start, 10, 3 );
    if ( !steps_3 )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_3, CC_SLE_Destination, 9, 2 ) )
    {
        cc_step_free_all_steps( &steps_3 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_3 = ply_side_effect_en_passant( 9, 4 );
    Ply * plies_3 = ply_new_ply_alx( CC_PE_DarkPawn, steps_3, pse_3 );
    if ( !plies_3 )
    {
        cc_step_free_all_steps( &steps_3 );
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
    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 7, 2 ) == CC_PE_LightPegasus );
    result = result && ( cc_chessboard_get_piece( cb, 9, 1 ) == CC_PE_LightWave );
    result = result && ( cc_chessboard_get_tag( cb, 9, 1 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_piece( cb, 9, 2 ) == CC_PE_DarkPawn );
    result = result && ( cc_chessboard_get_tag( cb, 9, 4 ) == CC_TE_None );

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

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece_tag( cb, 1, 0, CC_PE_LightRook, CC_TE_CanCastle );
    cc_chessboard_set_piece_tag( cb, 13, 0, CC_PE_LightKing, CC_TE_CanCastle );
    cc_chessboard_set_piece_tag( cb, 24, 0, CC_PE_LightRook, CC_TE_CanCastle );

    if ( do_print )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 1, 0 ) == CC_PE_LightRook );
    result = result && ( cc_chessboard_get_piece( cb, 13, 0 ) == CC_PE_LightKing );
    result = result && ( cc_chessboard_get_piece( cb, 24, 0 ) == CC_PE_LightRook );

    result = result && ( cc_chessboard_get_tag( cb, 1, 0 ) == CC_TE_CanCastle );
    result = result && ( cc_chessboard_get_tag( cb, 13, 0 ) == CC_TE_CanCastle );
    result = result && ( cc_chessboard_get_tag( cb, 24, 0 ) == CC_TE_CanCastle );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // move Ku&t

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 13, 0 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 20, 0 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse = ply_side_effect_castle( CC_PE_LightRook, 24, 0, 19, 0 );
    Ply * ply = ply_new_ply_alx( CC_PE_LightKing, steps_0, pse );
    if ( !ply )
    {
        cc_step_free_all_steps( &steps_0 );
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
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 1, 0 ) == CC_PE_LightRook );
    result = result && ( cc_chessboard_get_piece( cb, 19, 0 ) == CC_PE_LightRook );
    result = result && ( cc_chessboard_get_piece( cb, 20, 0 ) == CC_PE_LightKing );

    result = result && ( cc_chessboard_get_tag( cb, 1, 0 ) == CC_TE_CanCastle );
    result = result && ( cc_chessboard_get_tag( cb, 19, 0 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_tag( cb, 20, 0 ) == CC_TE_None );

    //
    // free, return

    mv_free_complete_move( &move );
    free( cb );

    return result;
}

bool tst_tag_and_promotion( bool do_print )
{
    // chessboard

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 11, 21, CC_PE_LightPawn );
    cc_chessboard_set_piece( cb, 15, 21, CC_PE_LightPyramid );
    cc_chessboard_set_piece( cb, 21, 15, CC_PE_LightBishop );

    if ( do_print )
    {
        cc_chessboard_print( cb, true );
        // cc_chessboard_print( cb, false );
    }

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 11, 21 ) == CC_PE_LightPawn );
    result = result && ( cc_chessboard_get_piece( cb, 15, 21 ) == CC_PE_LightPyramid );
    result = result && ( cc_chessboard_get_piece( cb, 21, 15 ) == CC_PE_LightBishop );

    result = result && ( cc_chessboard_get_tag( cb, 11, 21 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_tag( cb, 15, 21 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_tag( cb, 21, 15 ) == CC_TE_None );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bp22~

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 21, 15 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 15, 21 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( CC_PE_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply Al22=

    CcStep * steps_1 = cc_step_new( CC_SLE_Start, 15, 21 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_1, CC_SLE_Destination, 11, 21 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_tag_for_promotion();
    if ( !ply_append_ply_alx( plies_0, CC_PE_LightPyramid, steps_1, pse_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
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
        cc_chessboard_print( cb, true );
        // cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 11, 21 ) == CC_PE_LightPawn );
    result = result && ( cc_chessboard_get_piece( cb, 15, 21 ) == CC_PE_LightBishop );

    result = result && ( cc_chessboard_get_tag( cb, 11, 21 ) == CC_TE_DelayedPromotion );
    result = result && ( cc_chessboard_get_tag( cb, 15, 21 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_tag( cb, 21, 15 ) == CC_TE_None );

    if ( !result )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    //
    // ply l22Q

    CcStep * steps_2 = cc_step_new( CC_SLE_Start, 11, 21 );
    if ( !steps_2 )
    {
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_2, CC_SLE_Destination, 11, 21 ) )
    {
        cc_step_free_all_steps( &steps_2 );
        mv_free_complete_move( &move_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_promote( CC_PE_LightQueen );
    Ply * plies_2 = ply_new_ply_alx( CC_PE_LightPawn, steps_2, pse_2 );
    if ( !plies_2 )
    {
        cc_step_free_all_steps( &steps_2 );
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
        cc_chessboard_print( cb, true );
        // cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 11, 21 ) == CC_PE_LightQueen );
    result = result && ( cc_chessboard_get_piece( cb, 15, 21 ) == CC_PE_LightBishop );

    result = result && ( cc_chessboard_get_tag( cb, 11, 21 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_tag( cb, 15, 21 ) == CC_TE_None );
    result = result && ( cc_chessboard_get_tag( cb, 21, 15 ) == CC_TE_None );

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

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    if ( is_failed )
        cc_chessboard_set_piece( cb, 11, 5, CC_PE_DarkStarchild );
    else
        cc_chessboard_set_piece( cb, 11, 5, CC_PE_DarkShaman );

    cc_chessboard_set_piece( cb, 15, 5, CC_PE_LightPyramid );
    cc_chessboard_set_piece( cb, 21, 11, CC_PE_LightBishop );

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    if ( is_failed )
        result = result && ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_DarkStarchild );
    else
        result = result && ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_DarkShaman );

    result = result && ( cc_chessboard_get_piece( cb, 15, 5 ) == CC_PE_LightPyramid );
    result = result && ( cc_chessboard_get_piece( cb, 21, 11 ) == CC_PE_LightBishop );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bp6~

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 21, 11 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 15, 5 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( CC_PE_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply Al6%H

    CcStep * steps_1 = cc_step_new( CC_SLE_Start, 15, 5 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_1, CC_SLE_Destination, 11, 5 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1;
    if ( is_failed )
        pse_1 = ply_side_effect_failed_conversion();
    else
        pse_1 = ply_side_effect_convert( CC_PE_LightShaman, false );

    if ( !ply_append_ply_alx( plies_0, CC_PE_LightPyramid, steps_1, pse_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
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

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    if ( is_failed )
        result = result && ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_DarkStarchild );
    else
        result = result && ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_LightShaman );

    result = result && ( cc_chessboard_get_piece( cb, 15, 5 ) == CC_PE_LightBishop );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}

bool tst_demotion( bool do_print )
{
    // chessboard

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 0, 0, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 25, 25, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 11, 11, CC_PE_LightBishop );
    cc_chessboard_set_piece( cb, 23, 15, CC_PE_Monolith );

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 11, 11 ) == CC_PE_LightBishop );
    result = result && ( cc_chessboard_get_piece( cb, 23, 15 ) == CC_PE_Monolith );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Mw23>Bl12

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 23, 15 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 22, 22 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_demote( CC_PE_LightPawn, 11, 11 );
    Ply * plies_0 = ply_new_ply_alx( CC_PE_Monolith, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
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

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 11, 11 ) == CC_PE_LightPawn );
    result = result && ( cc_chessboard_get_piece( cb, 22, 22 ) == CC_PE_Monolith );

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}

bool tst_resurrection( bool do_print, bool is_failed, bool is_oblationing )
{
    // chessboard

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 25, 0, CC_PE_DimStar );
    cc_chessboard_set_piece( cb, 0, 25, CC_PE_DimStar );
    cc_chessboard_set_piece( cb, 23, 15, CC_PE_LightStarchild );

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar );
    result = result && ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar );
    result = result && ( cc_chessboard_get_piece( cb, 23, 15 ) == CC_PE_LightStarchild );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Ip11$B, Ip11$$

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 23, 15 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 15, 10 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0;
    if ( is_failed )
        pse_0 = ply_side_effect_failed_resurrection();
    else
    {
        if ( is_oblationing )
            pse_0 = ply_side_effect_resurrect( CC_PE_LightBishop, 15, 10 );
        else
            pse_0 = ply_side_effect_resurrect( CC_PE_LightWave, 16, 11 );
    }

    Ply * plies_0 = ply_new_ply_alx( CC_PE_LightStarchild, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
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

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar );
    result = result && ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar );

    if ( is_failed )
    {
        result = result && ( cc_chessboard_get_piece( cb, 15, 10 ) == CC_PE_LightStarchild );
        result = result && ( cc_chessboard_get_piece( cb, 16, 11 ) == CC_PE_None );
    }
    else
    {
        if ( is_oblationing )
        {
            result = result && ( cc_chessboard_get_piece( cb, 15, 10 ) == CC_PE_LightBishop );
            result = result && ( cc_chessboard_get_piece( cb, 16, 11 ) == CC_PE_None );
        }
        else
        {
            result = result && ( cc_chessboard_get_piece( cb, 15, 10 ) == CC_PE_LightStarchild );
            result = result && ( cc_chessboard_get_piece( cb, 16, 11 ) == CC_PE_LightWave );
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

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 0, 0, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 25, 25, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 25, 0, CC_PE_DimStar );
    cc_chessboard_set_piece( cb, 0, 25, CC_PE_DimStar );

    cc_chessboard_set_piece( cb, 3, 22, CC_PE_LightBishop );

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar );
    result = result && ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar );

    result = result && ( cc_chessboard_get_piece( cb, 3, 22 ) == CC_PE_LightBishop );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Ba26

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 3, 22 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 0, 25 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( CC_PE_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply |By25

    PlySideEffect pse_1 = ply_side_effect_none();

    Ply * ply_1;
    if ( is_failed )
        ply_1 = ply_append_failed_teleport_alx( plies_0, CC_PE_LightBishop, 0, 24, pse_1 );
    else
        ply_1 = ply_append_teleport_alx( plies_0, CC_PE_LightBishop, 24, 24, pse_1 );

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

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar );
    result = result && ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar );
    result = result && ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar );

    result = result && ( cc_chessboard_get_piece( cb, 3, 22 ) == CC_PE_None );

    if ( is_failed )
    {
        result = result && ( cc_chessboard_get_piece( cb, 0, 24 ) == CC_PE_LightBishop );
        result = result && ( cc_chessboard_get_piece( cb, 24, 24 ) == CC_PE_None );
    }
    else
    {
        result = result && ( cc_chessboard_get_piece( cb, 0, 24 ) == CC_PE_None );
        result = result && ( cc_chessboard_get_piece( cb, 24, 24 ) == CC_PE_LightBishop );
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

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 5, 11, CC_PE_Monolith );
    cc_chessboard_set_piece( cb, 19, 9, CC_PE_Monolith );

    cc_chessboard_set_piece( cb, 10, 12, CC_PE_LightBishop );
    cc_chessboard_set_piece( cb, 8, 14, CC_PE_LightWave );
    cc_chessboard_set_piece( cb, 17, 7, CC_PE_LightKnight );

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 5, 11 ) == CC_PE_Monolith );
    result = result && ( cc_chessboard_get_piece( cb, 19, 9 ) == CC_PE_Monolith );

    result = result && ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_LightBishop );
    result = result && ( cc_chessboard_get_piece( cb, 8, 14 ) == CC_PE_LightWave );
    result = result && ( cc_chessboard_get_piece( cb, 17, 7 ) == CC_PE_LightKnight );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bi15

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 10, 12 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 8, 14 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( CC_PE_LightBishop, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Wf12

    CcStep * steps_1 = cc_step_new( CC_SLE_Start, 8, 14 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_1, CC_SLE_Destination, 5, 11 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, CC_PE_LightWave, steps_1, pse_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply |Wr8

    PlySideEffect pse_2 = ply_side_effect_none();
    Ply * ply_2;

    if ( is_oblationing )
        ply_2 = ply_append_failed_teleport_oblation_alx( plies_0, CC_PE_LightWave, pse_2 );
    else
    {
        CcStep * steps_2 = cc_step_new( CC_SLE_Start, 19, 9 );
        if ( !steps_2 )
        {
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !cc_step_append_new( steps_2, CC_SLE_Destination, 17, 7 ) )
        {
            cc_step_free_all_steps( &steps_2 );
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        ply_2 = ply_append_teleport_wave_alx( plies_0, CC_PE_LightWave, steps_2, pse_2 );
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
        CcStep * steps_3 = cc_step_new( CC_SLE_Start, 17, 7 );
        if ( !steps_3 )
        {
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !cc_step_append_new( steps_3, CC_SLE_Destination, 15, 8 ) )
        {
            cc_step_free_all_steps( &steps_3 );
            ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        PlySideEffect pse_3 = ply_side_effect_none();
        if ( !ply_append_ply_alx( plies_0, CC_PE_LightKnight, steps_3, pse_3 ) )
        {
            cc_step_free_all_steps( &steps_3 );
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

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 5, 11 ) == CC_PE_Monolith );
    result = result && ( cc_chessboard_get_piece( cb, 19, 9 ) == CC_PE_Monolith );

    result = result && ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 8, 14 ) == CC_PE_LightBishop );

    if ( is_oblationing )
    {
        result = result && ( cc_chessboard_get_piece( cb, 17, 7 ) == CC_PE_LightKnight );
    }
    else
    {
        result = result && ( cc_chessboard_get_piece( cb, 17, 7 ) == CC_PE_LightWave );
        result = result && ( cc_chessboard_get_piece( cb, 15, 8 ) == CC_PE_LightKnight );
    }

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}

bool tst_trance_journey( bool do_print, bool is_capturing )
{
    CcPieceEnum shaman = is_capturing ? CC_PE_DarkShaman : CC_PE_LightShaman;

    // chessboard

    CcChessboard * cb = cc_chessboard_new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 4, 8, shaman ); // entrancing
    cc_chessboard_set_piece( cb, 6, 9, CC_PE_LightWave );
    cc_chessboard_set_piece( cb, 7, 7, shaman ); // entranced

    cc_chessboard_set_piece( cb, 7, 12, CC_PE_LightBishop ); // 2
    cc_chessboard_set_piece( cb, 5, 1, CC_PE_DarkKnight ); // 4
    cc_chessboard_set_piece_tag( cb, 21, 4, CC_PE_DarkPawn, CC_TE_DelayedPromotion ); // 9

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = result && ( cc_chessboard_get_piece( cb, 4, 8 ) == shaman );
    result = result && ( cc_chessboard_get_piece( cb, 6, 9 ) == CC_PE_LightWave );
    result = result && ( cc_chessboard_get_piece( cb, 7, 7 ) == shaman );

    result = result && ( cc_chessboard_get_piece( cb, 7, 12 ) == CC_PE_LightBishop );
    result = result && ( cc_chessboard_get_piece( cb, 5, 1 ) == CC_PE_DarkKnight );
    result = result && ( cc_chessboard_get_piece( cb, 21, 4 ) == CC_PE_DarkPawn );
    result = result && ( cc_chessboard_get_tag( cb, 21, 4 ) == CC_TE_DelayedPromotion );

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Hg10

    CcStep * steps_0 = cc_step_new( CC_SLE_Start, 4, 8 );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_0, CC_SLE_Destination, 6, 9 ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_0 = ply_side_effect_none();
    Ply * plies_0 = ply_new_ply_alx( shaman, steps_0, pse_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Wh8

    CcStep * steps_1 = cc_step_new( CC_SLE_Start, 6, 9 );
    if ( !steps_1 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_append_new( steps_1, CC_SLE_Destination, 7, 7 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_1 = ply_side_effect_none();
    if ( !ply_append_ply_alx( plies_0, CC_PE_LightWave, steps_1, pse_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply @H..h13<Bj19..f2<Nb6..p7..j19<Bl25..v5<P==p7
    // ply @H..h13*B..f2*N..p7..j19..v5*P==

    CcStepSideEffect sse_2_0 = cc_step_side_effect_none();
    CcSideEffectStep * steps_2 = cc_step_side_effect_step_new( CC_SLE_Start, 7, 7, sse_2_0 );
    if ( !steps_2 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcStepSideEffect sse_2_1 = is_capturing ?
                             cc_step_side_effect_capture( CC_PE_LightBishop, false ) :
                             cc_step_side_effect_displacement( CC_PE_LightBishop, false, 9, 18 );
    if ( !cc_step_append_side_effect_step_new( steps_2, CC_SLE_Distant, 7, 12, sse_2_1 ) )
    {
        cc_step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcStepSideEffect sse_2_2 = is_capturing ?
                             cc_step_side_effect_capture( CC_PE_DarkKnight, false ) :
                             cc_step_side_effect_displacement( CC_PE_DarkKnight, false, 1, 5 );
    if ( !cc_step_append_side_effect_step_new( steps_2, CC_SLE_Distant, 5, 1, sse_2_2 ) )
    {
        cc_step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcStepSideEffect sse_2_3 = cc_step_side_effect_none();
    if ( !cc_step_append_side_effect_step_new( steps_2, CC_SLE_Distant, 15, 6, sse_2_3 ) )
    {
        cc_step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcStepSideEffect sse_2_4 = is_capturing ?
                             cc_step_side_effect_none() :
                             cc_step_side_effect_displacement( CC_PE_LightBishop, false, 11, 24 );
    if ( !cc_step_append_side_effect_step_new( steps_2, CC_SLE_Distant, 9, 18, sse_2_4 ) )
    {
        cc_step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcStepSideEffect sse_2_5 = is_capturing ?
                             cc_step_side_effect_capture( CC_PE_DarkPawn, true ) :
                             cc_step_side_effect_displacement( CC_PE_DarkPawn, true, 15, 6 );
    if ( !cc_step_append_side_effect_step_new( steps_2, CC_SLE_Destination, 21, 4, sse_2_5 ) )
    {
        cc_step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    PlySideEffect pse_2 = ply_side_effect_none();
    if ( !ply_append_trance_journey_alx( plies_0, shaman, 7, 7, steps_2, pse_2 ) )
    {
        cc_step_free_all_side_effect_steps( &steps_2 );
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move Hg10~Wh8@H..h13<Bj19..f2<Nb6..p7..j19<Bl25..v5<P==p7
    // move Hg10~Wh8@H..h13*B..f2*N..p7..j19..v5*P==

    Move * move_0 = mv_new_alx( plies_0, MS_None );
    if ( !move_0 )
    {
        ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    do_move( cb, move_0 );

    if ( do_print ) cc_chessboard_print( cb, true );

    //
    // tests

    result = result && ( cc_chessboard_get_piece( cb, 4, 8 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 6, 9 ) == shaman );
    result = result && ( cc_chessboard_get_piece( cb, 7, 7 ) == CC_PE_LightWave );
    result = result && ( cc_chessboard_get_piece( cb, 21, 4 ) == shaman );
    result = result && ( cc_chessboard_get_tag( cb, 21, 4 ) == CC_TE_None );

    result = result && ( cc_chessboard_get_piece( cb, 7, 12 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 5, 1 ) == CC_PE_None );
    result = result && ( cc_chessboard_get_piece( cb, 9, 18 ) == CC_PE_None );

    if ( is_capturing )
    {
        result = result && ( cc_chessboard_get_piece( cb, 11, 24 ) == CC_PE_None );
        result = result && ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_None );
        result = result && ( cc_chessboard_get_piece( cb, 15, 6 ) == CC_PE_None );
        result = result && ( cc_chessboard_get_tag( cb, 15, 6 ) == CC_TE_None );
    }
    else
    {
        result = result && ( cc_chessboard_get_piece( cb, 11, 24 ) == CC_PE_LightBishop );
        result = result && ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_DarkKnight );
        result = result && ( cc_chessboard_get_piece( cb, 15, 6 ) == CC_PE_DarkPawn );
        result = result && ( cc_chessboard_get_tag( cb, 15, 6 ) == CC_TE_None );
    }

    //
    // free, return

    mv_free_complete_move( &move_0 );
    free( cb );

    return result;
}
