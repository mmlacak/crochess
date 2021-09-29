// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <stdio.h>

#include "cc_piece.h"
#include "cc_variant.h"
#include "cc_chessboard.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"
#include "cc_do_moves.h"
#include "cc_format_moves.h"
#include "cc_game.h"
#include "cc_rules.h"

#include "test_utils.h"
#include "test_msgs.h"
#include "tests_do_move.h"


bool test_do_move_single_ply( int const index, TestPrints const tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_single_ply\n", index );

    // chessboard

    CcGame * game__o = cc_game_new( CC_GSE_Turn_Light, CC_VE_One, false );
    if ( !game__o ) return false;

    CcPieceEnum piece_G = CC_PE_LightPegasus;
    CcPieceEnum piece_p = CC_PE_DarkPawn;

    cc_chessboard_set_piece( game__o->chessboard, 5, 2, piece_G );
    cc_chessboard_set_piece_tag( game__o->chessboard, 10, 12, piece_p, CC_TE_DelayedPromotion );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 5, 2 ) == piece_G ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 10, 12 ) == piece_p ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 10, 12 ) == CC_TE_DelayedPromotion ),
                                   TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    //
    // Steps

    CcStep * start__t = cc_step_none_new( CC_SLE_Start, 5, 2, CC_FSUE_Clarification_NoOutput );
    if ( !start__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( start__t, CC_SLE_Next, 6, 4, CC_FSUE_Clarification_NoOutput ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    if ( !cc_step_none_append( start__t, CC_SLE_Distant, 8, 8, CC_FSUE_Addition ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    if ( !cc_step_capture_append( start__t, CC_SLE_Distant, 10, 12, CC_PE_DarkPawn, CC_TE_DelayedPromotion, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    //
    // Ply

    CcPly * ply__t = cc_ply_new( CC_PLE_Ply, piece_G, &start__t );
    if ( !ply__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    //
    // Move [Gf3.g5..i9..k13*p==]

    CcMove * move__o = cc_move_new( "[Gf3.g5..i9..k13*p==]", &ply__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &ply__t, &start__t, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move(s) not done", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( CC_GAME_STATUS_IS_DARK_TURN( game__o->status ),
                                 TME_Error, "dark player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 5, 2 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 10, 12 ) == piece_G ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 10, 12 ) == CC_TE_None ),
                                   TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, NULL, result );
}

bool test_do_move_cascading_plies( int const index, TestPrints const tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_cascading_plies\n", index );

    // chessboard

    CcGame * game__o = cc_game_new( CC_GSE_Turn_Light, CC_VE_One, false );
    if ( !game__o ) return false;

    cc_chessboard_set_piece( game__o->chessboard, 1, 5, CC_PE_LightPegasus );
    cc_chessboard_set_piece( game__o->chessboard, 7, 2, CC_PE_LightWave );
    cc_chessboard_set_piece_tag( game__o->chessboard, 9, 1, CC_PE_LightPawn, CC_TE_CanRush );
    cc_chessboard_set_piece( game__o->chessboard, 10, 3, CC_PE_DarkPawn );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 1, 5 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 7, 2 ) == CC_PE_LightWave ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 9, 1 ) == CC_PE_LightPawn ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 9, 1 ) == CC_TE_CanRush ),
                                   TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 10, 3 ) == CC_PE_DarkPawn ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    //
    // ply 0, G --> W

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 1, 5, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( steps_0__t, CC_SLE_Destination, 7, 2, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightPegasus, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_0__t, false );

    //
    // ply 1, W --> P

    CcStep * steps_1__t = cc_step_none_new( CC_SLE_Start, 7, 2, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, NULL, false );

    if ( !cc_step_none_append( steps_1__t, CC_SLE_Destination, 9, 1, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, &steps_1__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightWave, &steps_1__t ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, &steps_1__t, false );

    //
    // ply 2, P --> ...

    CcStep * steps_2__t = cc_step_none_new( CC_SLE_Start, 9, 1, CC_FSUE_Clarification_NoOutput );
    if ( !steps_2__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, NULL, false );

    if ( !cc_step_none_append( steps_2__t, CC_SLE_Destination, 9, 4, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, &steps_2__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightPawn, &steps_2__t ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, &steps_2__t, false );

    //
    // move 0, [Gb6-h3]~[Wh3-j2]~[Pj2-j5]

    CcMove * move__o = cc_move_new( "[Gb6-h3]~[Wh3-j2]~[Pj2-j5]", &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, NULL, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move(s) not done", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( CC_GAME_STATUS_IS_DARK_TURN( game__o->status ),
                                 TME_Error, "dark player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 1, 5 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 7, 2 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 9, 1 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 9, 1 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 9, 4 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 9, 4 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 10, 3 ) == CC_PE_DarkPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    //
    // move 1, p --> :P

    CcStep * steps_3__t = cc_step_none_new( CC_SLE_Start, 10, 3, CC_FSUE_Clarification_NoOutput );
    if ( !steps_3__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    if ( !cc_step_en_passant_append( steps_3__t, CC_SLE_Destination, 9, 2, CC_PE_LightPawn, 9, 4, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_3__t, false );

    CcPly * plies_3__t = cc_ply_new( CC_PLE_Ply, CC_PE_DarkPawn, &steps_3__t );
    if ( !plies_3__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_3__t, false );

    //
    // move 1, [pk4-j3:Pj5]

    CcMove * move_1__o = cc_move_new( "[pk4-j3:Pj5]", &plies_3__t, CC_MSE_None );
    if ( !move_1__o ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_3__t, NULL, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move_1__o, tp.format_move );
        printf( "%s\n", move_1__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_DARK_TURN( game__o->status ),
                                 TME_Error, "dark player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move_1__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move(s) not done", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 1, 5 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 7, 2 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 9, 1 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 9, 1 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 9, 2 ) == CC_PE_DarkPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 9, 4 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, result );
}

bool test_do_move_castling( int const index, TestPrints const tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_castling\n", index );

    // chessboard

    CcGame * game__o = cc_game_new( CC_GSE_Turn_Light, CC_VE_One, false );
    if ( !game__o ) return false;

    cc_chessboard_set_piece_tag( game__o->chessboard, 1, 0, CC_PE_LightRook, CC_TE_CanCastle );
    cc_chessboard_set_piece_tag( game__o->chessboard, 13, 0, CC_PE_LightKing, CC_TE_CanCastle );
    cc_chessboard_set_piece_tag( game__o->chessboard, 24, 0, CC_PE_LightRook, CC_TE_CanCastle );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 1, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 13, 0 ) == CC_PE_LightKing ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 24, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 1, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 13, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 24, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    //
    // move Ku&t

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 13, 0, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    if ( !cc_step_castle_append( steps_0__t, CC_SLE_Destination, 20, 0, CC_PE_LightRook, 24, 0, 19, 0, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_0__t, false );

    CcPly * ply__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightKing, &steps_0__t );
    if ( !ply__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_0__t, false );

    //
    // move, [Kn1-u1&Ry1-t1]

    CcMove * move__o = cc_move_new( "[Kn1-u1&Ry1-t1]", &ply__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &ply__t, NULL, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move(s) not done", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( CC_GAME_STATUS_IS_DARK_TURN( game__o->status ),
                                 TME_Error, "dark player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 1, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 19, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 20, 0 ) == CC_PE_LightKing ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 1, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 19, 0 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 20, 0 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, NULL, result );
}

bool test_do_move_tag_and_promotion( int const index, TestPrints const tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_tag_and_promotion\n", index );

    // chessboard

    CcGame * game__o = cc_game_new( CC_GSE_Turn_Light, CC_VE_One, false );
    if ( !game__o ) return false;

    cc_chessboard_set_piece( game__o->chessboard, 11, 21, CC_PE_LightPawn );
    cc_chessboard_set_piece( game__o->chessboard, 15, 21, CC_PE_LightPyramid );
    cc_chessboard_set_piece( game__o->chessboard, 21, 15, CC_PE_LightBishop );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 11, 21 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 15, 21 ) == CC_PE_LightPyramid ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 21, 15 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 11, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 15, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 21, 15 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    //
    // ply Bp22~

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 21, 15, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( steps_0__t, CC_SLE_Destination, 15, 21, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightBishop, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &steps_0__t, false );

    //
    // ply Al22=

    CcStep * steps_1__t = cc_step_none_new( CC_SLE_Start, 15, 21, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, NULL, false );

    if ( !cc_step_tag_for_promotion_append( steps_1__t, CC_SLE_Destination, 11, 21, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, &steps_1__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightPyramid, &steps_1__t ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, &steps_1__t, false );

    //
    // move [Bv16-p22]~[Ap22-l22=]

    CcMove * move__o = cc_move_new( "[Bv16-p22]~[Ap22-l22=]", &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &plies_0__t, NULL, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move(s) not done", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( CC_GAME_STATUS_IS_DARK_TURN( game__o->status ),
                                 TME_Error, "dark player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 11, 21 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 15, 21 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 11, 21 ) == CC_TE_DelayedPromotion ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 15, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 21, 15 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, NULL, false );

    //
    // ply l22Q

    CcStep * steps_2__t = cc_step_none_new( CC_SLE_Start, 11, 21, CC_FSUE_Clarification_NoOutput );
    if ( !steps_2__t ) return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, NULL, false );

    if ( !cc_step_promote_append( steps_2__t, CC_SLE_Destination, 11, 21, CC_PE_LightQueen, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, &steps_2__t, false );

    CcPly * plies_2__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightPawn, &steps_2__t );
    if ( !plies_2__t ) return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, &steps_2__t, false );

    //
    // move [Pl22-l22=Q]

    CcMove * move_1__o = cc_move_new( "[Pl22-l22=Q]", &plies_2__t, CC_MSE_None );
    if ( !move_1__o ) return cc_game_move_data_free_all( &game__o, NULL, &move__o, &plies_2__t, NULL, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move_1__o, tp.format_move );
        printf( "%s\n", move_1__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_DARK_TURN( game__o->status ),
                                 TME_Error, "dark player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move_1__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move(s) not done", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 11, 21 ) == CC_PE_LightQueen ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 15, 21 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 11, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 15, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( game__o->chessboard, 21, 15 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( &game__o, NULL, &move__o, NULL, NULL, result );
}

bool test_do_move_conversion( int const index, TestPrints const tp, bool const is_failed )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_conversion\n", index );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    if ( is_failed )
        cc_chessboard_set_piece( cb__o, 11, 5, CC_PE_DarkStarchild );
    else
        cc_chessboard_set_piece( cb__o, 11, 5, CC_PE_DarkShaman );

    cc_chessboard_set_piece( cb__o, 15, 5, CC_PE_LightPyramid );
    cc_chessboard_set_piece( cb__o, 21, 11, CC_PE_LightBishop );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    if ( is_failed )
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 5 ) == CC_PE_DarkStarchild ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;
    else
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 5 ) == CC_PE_DarkShaman ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 5 ) == CC_PE_LightPyramid ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 21, 11 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // ply Bp6~

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 21, 11, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( steps_0__t, CC_SLE_Destination, 15, 5, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightBishop, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    //
    // ply Al6%H

    CcStep * steps_1__t = cc_step_none_new( CC_SLE_Start, 15, 5, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    CcSideEffect se_1;
    if ( is_failed )
        se_1 = cc_side_effect_failed_conversion();
    else
        se_1 = cc_side_effect_convert( CC_PE_LightShaman, false );

    if ( !cc_step_append( steps_1__t, CC_SLE_Destination, 11, 5, se_1, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_1__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightPyramid, &steps_1__t ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_1__t, false );

    //
    // move [Bv12-p6]~[Ap6-l6%H]
    //      [Bv12-p6]~[Ap6-l6%%]

    char * alg_not =  ( is_failed ) ? "[Bv12-p6]~[Ap6-l6%%]" : "[Bv12-p6]~[Ap6-l6%H]";

    CcMove * move__o = cc_move_new( alg_not, &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    result = test_print_failure( cc_do_moves( cb__o, move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    if ( is_failed )
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 5 ) == CC_PE_DarkStarchild ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;
    else
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 5 ) == CC_PE_LightShaman ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 5 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}

bool test_do_move_demotion( int const index, TestPrints const tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_demotion\n", index );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    cc_chessboard_set_piece( cb__o, 0, 0, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb__o, 25, 25, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb__o, 11, 11, CC_PE_LightBishop );
    cc_chessboard_set_piece( cb__o, 23, 15, CC_PE_Monolith );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 11 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 23, 15 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // ply Mw23>Bl12

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 23, 15, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    if ( !cc_step_demote_append( steps_0__t, CC_SLE_Destination, 22, 22, CC_PE_LightBishop, 11, 11, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_Monolith, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    //
    // move [Mx16-w23>Bl12]

    CcMove * move__o = cc_move_new( "[Mx16-w23>Bl12]", &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    result = test_print_failure( cc_do_moves( cb__o, move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 11 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 22, 22 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}

bool test_do_move_resurrection( int const index,
                                TestPrints const tp,
                                bool const is_failed,
                                bool const is_oblationing )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_resurrection\n", index );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    cc_chessboard_set_piece( cb__o, 25, 0, CC_PE_DimStar );
    cc_chessboard_set_piece( cb__o, 0, 25, CC_PE_DimStar );
    cc_chessboard_set_piece( cb__o, 23, 15, CC_PE_LightStarchild );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 23, 15 ) == CC_PE_LightStarchild ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // ply Ip11$B, Ip11$$

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 23, 15, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    CcSideEffect se_0;
    if ( is_failed )
        se_0 = cc_side_effect_failed_resurrection();
    else
    {
        if ( is_oblationing )
            se_0 = cc_side_effect_resurrect( CC_PE_LightBishop, 15, 10 );
        else
            se_0 = cc_side_effect_resurrect( CC_PE_LightWave, 16, 11 );
    }

    if ( !cc_step_append( steps_0__t, CC_SLE_Destination, 15, 10, se_0, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightStarchild, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    //
    // move [Ix16-p11$Wq12]
    //      [Ix16-p11$Bp11]
    //      [Ix16-p11$$]
    //      [Ix16-p11$$]

    char * alg_not = ( is_failed ) ? "[Ix16-p11$$]"
                                   : ( is_oblationing ) ? "[Ix16-p11$Bp11]"
                                                        : "[Ix16-p11$Wq12]";

    CcMove * move__o = cc_move_new( alg_not, &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    result = test_print_failure( cc_do_moves( cb__o, move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_failed )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 10 ) == CC_PE_LightStarchild ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 16, 11 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                 && result;
    }
    else
    {
        if ( is_oblationing )
        {
            result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 10 ) == CC_PE_LightBishop ),
                                        TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                     && result;

            result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 16, 11 ) == CC_PE_None ),
                                        TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                     && result;
        }
        else
        {
            result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 10 ) == CC_PE_LightStarchild ),
                                        TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                     && result;

            result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 16, 11 ) == CC_PE_LightWave ),
                                        TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                     && result;
        }
    }

    //
    // free, return

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}

bool test_do_move_teleportation( int const index, TestPrints const tp, bool const is_failed )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_teleportation\n", index );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    cc_chessboard_set_piece( cb__o, 0, 0, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb__o, 25, 25, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb__o, 25, 0, CC_PE_DimStar );
    cc_chessboard_set_piece( cb__o, 0, 25, CC_PE_DimStar );

    cc_chessboard_set_piece( cb__o, 3, 22, CC_PE_LightBishop );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 3, 22 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // ply Ba26

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 3, 22, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( steps_0__t, CC_SLE_Destination, 0, 25, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightBishop, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    //
    // ply |By25
    // ply ||Ba25

    int i = ( is_failed ) ? 0 : 24;

    CcStep * steps_1__t = cc_step_none_new( CC_SLE_Destination, i, 24, CC_FSUE_User );
    if ( !steps_1__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    CcPlyLinkEnum ple = ( is_failed ) ? CC_PLE_FailedTeleportation : CC_PLE_Teleportation;

    CcPly * ply_1__w = cc_ply_append( plies_0__t, ple, CC_PE_LightBishop, &steps_1__t );
    if ( !ply_1__w ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    //
    // move [Bd23-a26]|[By25]
    //      [Bd23-a26]||[Ba25]

    char * alg_not = ( is_failed ) ? "[Bd23-a26]||[Ba25]" : "[Bd23-a26]|[By25]";

    CcMove * move__o = cc_move_new( alg_not, &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    result = test_print_failure( cc_do_moves( cb__o, move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 3, 22 ) == CC_PE_None ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_failed )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 24 ) == CC_PE_LightBishop ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 24, 24 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                 && result;
    }
    else
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 0, 24 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 24, 24 ) == CC_PE_LightBishop ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;
    }

    //
    // free, return

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}

bool test_do_move_teleportation_wave( int const index, TestPrints const tp, bool const is_oblationing )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_teleportation_wave\n", index );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    cc_chessboard_set_piece( cb__o, 5, 11, CC_PE_Monolith );
    cc_chessboard_set_piece( cb__o, 19, 9, CC_PE_Monolith );

    cc_chessboard_set_piece( cb__o, 10, 12, CC_PE_LightBishop );
    cc_chessboard_set_piece( cb__o, 8, 14, CC_PE_LightWave );
    cc_chessboard_set_piece( cb__o, 17, 7, CC_PE_LightKnight );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 5, 11 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 19, 9 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 10, 12 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 8, 14 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 17, 7 ) == CC_PE_LightKnight ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // ply Bi15

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 10, 12, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( steps_0__t, CC_SLE_Destination, 8, 14, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, CC_PE_LightBishop, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    //
    // ply ~Wf12

    CcStep * steps_1__t = cc_step_none_new( CC_SLE_Start, 8, 14, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    if ( !cc_step_none_append( steps_1__t, CC_SLE_Destination, 5, 11, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_1__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightWave, &steps_1__t ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_1__t, false );

    //
    // ply |Wr8

    CcPly * ply_2__w;

    if ( is_oblationing )
        ply_2__w = cc_ply_append( plies_0__t, CC_PLE_FailedTeleportation, CC_PE_LightWave, NULL );
    else
    {
        CcStep * steps_2__t = cc_step_none_new( CC_SLE_Start, 19, 9, CC_FSUE_Clarification_NoOutput );
        if ( !steps_2__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

        if ( !cc_step_none_append( steps_2__t, CC_SLE_Destination, 17, 7, CC_FSUE_User ) )
            return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

        ply_2__w = cc_ply_append( plies_0__t, CC_PLE_Teleportation, CC_PE_LightWave, &steps_2__t );
    }

    if ( !ply_2__w ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    //
    // ply ~Np9

    if ( !is_oblationing )
    {
        CcStep * steps_3__t = cc_step_none_new( CC_SLE_Start, 17, 7, CC_FSUE_Clarification_NoOutput );
        if ( !steps_3__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

        if ( !cc_step_none_append( steps_3__t, CC_SLE_Destination, 15, 8, CC_FSUE_User ) )
            return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_3__t, false );

        if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightKnight, &steps_3__t ) )
            return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_3__t, false );
    }

    //
    // move [Bk13-i15]~[Wi15-f12]|[Wt10-r8]~[Nr8-p9]
    //      [Bk13-i15]~[Wi15-f12]||[W]

    char * alg_not = ( is_oblationing ) ? "[Bk13-i15]~[Wi15-f12]||[W]"
                                        : "[Bk13-i15]~[Wi15-f12]|[Wt10-r8]~[Nr8-p9]";

    CcMove * move__o = cc_move_new( alg_not, &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    result = test_print_failure( cc_do_moves( cb__o, move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 5, 11 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 19, 9 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 10, 12 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 8, 14 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_oblationing )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 17, 7 ) == CC_PE_LightKnight ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;
    }
    else
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 17, 7 ) == CC_PE_LightWave ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 8 ) == CC_PE_LightKnight ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;
    }

    //
    // free, return

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}

bool test_do_move_trance_journey( int const index, TestPrints const tp, bool const is_capturing )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d test_do_move_trance_journey\n", index );

    CcPieceEnum shaman = is_capturing ? CC_PE_DarkShaman : CC_PE_LightShaman;

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    cc_chessboard_set_piece( cb__o, 4, 8, shaman ); // entrancing
    cc_chessboard_set_piece( cb__o, 6, 9, CC_PE_LightWave );
    cc_chessboard_set_piece( cb__o, 7, 7, shaman ); // entranced

    cc_chessboard_set_piece( cb__o, 7, 12, CC_PE_LightBishop ); // 2
    cc_chessboard_set_piece( cb__o, 5, 1, CC_PE_DarkKnight ); // 4
    cc_chessboard_set_piece_tag( cb__o, 21, 4, CC_PE_DarkPawn, CC_TE_DelayedPromotion ); // 9

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 4, 8 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 6, 9 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 7, 7 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 7, 12 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 5, 1 ) == CC_PE_DarkKnight ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 21, 4 ) == CC_PE_DarkPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb__o, 21, 4 ) == CC_TE_DelayedPromotion ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // ply Hg10

    CcStep * steps_0__t = cc_step_none_new( CC_SLE_Start, 4, 8, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( steps_0__t, CC_SLE_Destination, 6, 9, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    CcPly * plies_0__t = cc_ply_new( CC_PLE_Ply, shaman, &steps_0__t );
    if ( !plies_0__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, NULL, &steps_0__t, false );

    //
    // ply ~Wh8

    CcStep * steps_1__t = cc_step_none_new( CC_SLE_Start, 6, 9, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    if ( !cc_step_none_append( steps_1__t, CC_SLE_Destination, 7, 7, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_1__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_Ply, CC_PE_LightWave, &steps_1__t ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_1__t, false );

    //
    // ply @H..h13<Bj19..f2<Nb6..p7..j19<Bl25..v5<P==p7
    // ply @hh8,j9..h13*B..f2*n..p7..j19-v5*p==

    CcStep * steps_2__t = cc_step_none_new( CC_SLE_Start, 7, 7, CC_FSUE_Clarification_NoOutput );
    if ( !steps_2__t ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    if ( is_capturing )
        if ( !cc_step_none_append( steps_2__t, CC_SLE_Reposition, 9, 8, CC_FSUE_Clarification ) )
            return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    CcSideEffect sse_2_1 = is_capturing ?
                           cc_side_effect_capture( CC_PE_LightBishop, CC_TE_None ) :
                           cc_side_effect_displacement( CC_PE_LightBishop, CC_TE_None, 9, 18 );
    if ( !cc_step_append( steps_2__t, CC_SLE_Distant, 7, 12, sse_2_1, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    CcSideEffect sse_2_2 = is_capturing ?
                           cc_side_effect_capture( CC_PE_DarkKnight, CC_TE_None ) :
                           cc_side_effect_displacement( CC_PE_DarkKnight, CC_TE_None, 1, 5 );
    if ( !cc_step_append( steps_2__t, CC_SLE_Distant, 5, 1, sse_2_2, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    if ( !cc_step_none_append( steps_2__t, CC_SLE_Distant, 15, 6, CC_FSUE_Addition ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    CcSideEffect sse_2_4 = is_capturing ?
                           cc_side_effect_none() :
                           cc_side_effect_displacement( CC_PE_LightBishop, false, 11, 24 );
    if ( !cc_step_append( steps_2__t, CC_SLE_Distant, 9, 18, sse_2_4, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    CcSideEffect sse_2_5 = is_capturing ?
                           cc_side_effect_capture( CC_PE_DarkPawn, CC_TE_DelayedPromotion ) :
                           cc_side_effect_displacement( CC_PE_DarkPawn, CC_TE_DelayedPromotion, 15, 6 );
    if ( !cc_step_append( steps_2__t, CC_SLE_Destination, 21, 4, sse_2_5, CC_FSUE_User ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    if ( !cc_ply_append( plies_0__t, CC_PLE_TranceJourney, shaman, &steps_2__t ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, &steps_2__t, false );

    //
    // move [He9-g10]~[Wg10-h8]@[Hh8..h13<Bj19..f2<nb6..p7..j19<Bl25-v5<p==p7]
    //      [he9-g10]~[Wg10-h8]@[hh8,j9..h13*B..f2*n..p7..j19-v5*p==]

    char * alg_not = ( is_capturing ) ? "[he9-g10]~[Wg10-h8]@[hh8,j9..h13*B..f2*n..p7..j19-v5*p==]"
                                      : "[He9-g10]~[Wg10-h8]@[Hh8..h13<Bj19..f2<nb6..p7..j19<Bl25-v5<p==p7]";

    CcMove * move__o = cc_move_new( alg_not, &plies_0__t, CC_MSE_None );
    if ( !move__o ) return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0__t, NULL, false );

    result = test_print_failure( cc_do_moves( cb__o, move__o, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__o, tp.format_move );
        printf( "%s\n", move__o->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, false );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 4, 8 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 6, 9 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 7, 7 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 21, 4 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb__o, 21, 4 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 7, 12 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 5, 1 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 9, 18 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_capturing )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 24 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 1, 5 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 6 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_tag( cb__o, 15, 6 ) == CC_TE_None ),
                                    TME_Error, "tag found", __FILE__, __LINE__, __func__ )
                 && result;
    }
    else
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 11, 24 ) == CC_PE_LightBishop ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 1, 5 ) == CC_PE_DarkKnight ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 15, 6 ) == CC_PE_DarkPawn ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                 && result;

        result = test_print_failure( ( cc_chessboard_get_tag( cb__o, 15, 6 ) == CC_TE_None ),
                                    TME_Error, "tag found", __FILE__, __LINE__, __func__ )
                 && result;
    }

    //
    // free, return

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}
