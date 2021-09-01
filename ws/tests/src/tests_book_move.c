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
#include "cc_game.h"
#include "cc_format_moves.h"

#include "test_msgs.h"
#include "tests_book_move.h"


bool test_book_move_scn_ct_03_define_step_ply( TestPrints tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "scn_ct_03_define_step_ply\n" );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_CroatianTies, false );
    if ( !cb__o ) return false;

    CcPieceEnum pe = CC_PE_LightPegasus;

    cc_chessboard_set_piece( cb__o, 2, 1, pe );
    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 2, 1 ) == pe ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result ) return cc_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    //
    // steps

    CcStep * start = cc_step_none_new( CC_SLE_Start, 2, 1, CC_FSUE_Clarification_NoOutput );
    if ( !start ) return cc_move_data_free_all( NULL, &cb__o, NULL, NULL, NULL, false );

    if ( !cc_step_none_append_new( start, CC_SLE_Next, 3, 3, CC_FSUE_Clarification_NoOutput ) )
        return cc_move_data_free_all( NULL, &cb__o, NULL, NULL, &start, false );

    if ( !cc_step_none_append_new( start, CC_SLE_Next, 4, 5, CC_FSUE_Clarification_NoOutput ) )
        return cc_move_data_free_all( NULL, &cb__o, NULL, NULL, &start, false );

    if ( !cc_step_none_append_new( start, CC_SLE_Next, 5, 7, CC_FSUE_User ) )
        return cc_move_data_free_all( NULL, &cb__o, NULL, NULL, &start, false );

    //
    // ply

    CcPly * ply = cc_ply_new( CC_PLE_Ply, pe, &start );
    if ( !ply ) return cc_move_data_free_all( NULL, &cb__o, NULL, NULL, &start, false );

    //
    // move [Gc2.d4.e6.f8]

    CcMove * move__o = cc_move_new( "[Gc2.d4.e6.f8]", &ply, CC_MSE_None );
    if ( !move__o ) return cc_move_data_free_all( NULL, &cb__o, NULL, &ply, &start, false );

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

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 2, 1 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb__o, 5, 7 ) == CC_PE_LightPegasus ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result );
}
