// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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

#include "test_msgs.h"
#include "tests_book_move.h"


// bool test_book_move_scn_ct_03_define_step_ply( TestPrints tp )
bool test_book_move_scn_ct_03_define_step_ply( int index, TestPrints tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "%d scn_ct_03_define_step_ply\n", index );

    // game

    CcGame * game__o = cc_game_new( CC_GSE_Turn_Light, CC_VE_CroatianTies, false );
    if ( !game__o ) return false;

    CcPieceEnum piece_G = CC_PE_LightPegasus;

    cc_chessboard_set_piece( game__o->chessboard, 2, 1, piece_G );
    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( game__o->chessboard, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 2, 1 ) == piece_G ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    //
    // steps

    CcStep * start__t = cc_step_none_new( CC_SLE_Start, 2, 1, CC_FSUE_Clarification_NoOutput );
    if ( !start__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, false );

    if ( !cc_step_none_append( start__t, CC_SLE_Next, 3, 3, CC_FSUE_Clarification_NoOutput ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    if ( !cc_step_none_append( start__t, CC_SLE_Next, 4, 5, CC_FSUE_Clarification_NoOutput ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    if ( !cc_step_none_append( start__t, CC_SLE_Next, 5, 7, CC_FSUE_User ) )
        return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    //
    // ply

    CcPly * ply__t = cc_ply_new( CC_PLE_Ply, piece_G, &start__t );
    if ( !ply__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, &start__t, false );

    //
    // move [Gc2.d4.e6.f8]

    CcMove * move__t = cc_move_new( "[Gc2.d4.e6.f8]", &ply__t, CC_MSE_None );
    if ( !move__t ) return cc_game_move_data_free_all( &game__o, NULL, NULL, &ply__t, &start__t, false );

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        char * alg_not = cc_format_move_new( move__t, tp.format_move );
        printf( "%s\n", move__t->notation );
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    result = test_print_failure( CC_GAME_STATUS_IS_LIGHT_TURN( game__o->status ),
                                 TME_Error, "light player should be on turn", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( cc_rules_do_moves( &game__o, &move__t, CC_DME_DoAllMoves ),
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
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 2, 1 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( game__o->chessboard, 5, 7 ) == piece_G ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_game_move_data_free_all( &game__o, NULL, NULL, NULL, NULL, result );
}
