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

#include "test_msgs.h"
#include "tests_do_move.h"


TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move )
{
    TestPrints tp = { .do_print_chessboard = do_print_chessboard,
                      .do_print_move = do_print_move,
                      .format_move = format_move };
    return tp;
}


bool test_do_move_single_ply( TestPrints tp )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 5, 2, CC_PE_LightPegasus );
    cc_chessboard_set_piece( cb, 10, 12, CC_PE_DarkPawn );
    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 5, 2 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_DarkPawn ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // steps

    CcStep * start = cc_step_none_append__new( NULL, CC_SLE_Start, 5, 2, CC_FSUE_Clarification_NoOutput );
    if ( !start )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( start, CC_SLE_Next, 6, 4, CC_FSUE_Clarification_NoOutput ) )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( start, CC_SLE_Distant, 8, 8, CC_FSUE_Addition ) )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    if ( !cc_step_capture_append__new( start, CC_SLE_Distant, 10, 12, CC_PE_DarkPawn, true, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    //
    // ply

    CcPly * ply = cc_ply_cascade__new( CC_PE_LightPegasus, &start );
    if ( !ply )
    {
        cc_step_free_all_steps( &start );
        free( cb );
        return false;
    }

    //
    // move [Gf3.g5..i9..k13*p==]

    CcMove * move = cc_move__new( "[Gf3.g5..i9..k13*p==]", &ply, CC_MSE_None );
    if ( !move )
    {
        cc_ply_free_all_plies( &ply );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 5, 2 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_LightPegasus ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    cc_move_free_all_moves( &move );
    free( cb );

    return result;
}

bool test_do_move_cascading_plies( TestPrints tp )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 1, 5, CC_PE_LightPegasus );
    cc_chessboard_set_piece( cb, 7, 2, CC_PE_LightWave );
    cc_chessboard_set_piece_tag( cb, 9, 1, CC_PE_LightPawn, CC_TE_CanRush );
    cc_chessboard_set_piece( cb, 10, 3, CC_PE_DarkPawn );
    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 2 ) == CC_PE_LightWave ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 9, 1 ) == CC_PE_LightPawn ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 9, 1 ) == CC_TE_CanRush ),
                                   TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 10, 3 ) == CC_PE_DarkPawn ),
                                   TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply 0, G --> W

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 1, 5, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_0, CC_SLE_Destination, 7, 2, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_LightPegasus, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply 1, W --> P

    CcStep * steps_1 = cc_step_none__new( CC_SLE_Start, 7, 2, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_1, CC_SLE_Destination, 9, 1, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightWave, &steps_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply 2, P --> ...

    CcStep * steps_2 = cc_step_none__new( CC_SLE_Start, 9, 1, CC_FSUE_Clarification_NoOutput );
    if ( !steps_2 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_2, CC_SLE_Destination, 9, 4, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightPawn, &steps_2 ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move 0, [Gb6-h3]~[Wh3-j2]~[j2-j5]

    CcMove * move_0 = cc_move__new( "[Gb6-h3]~[Wh3-j2]~[j2-j5]", &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 2 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 9, 1 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 9, 1 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 9, 4 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 9, 4 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 10, 3 ) == CC_PE_DarkPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        cc_move_free_all_moves( &move_0 );
        free( cb );

        return false;
    }

    //
    // move 1, p --> :P

    CcStep * steps_3 = cc_step_none__new( CC_SLE_Start, 10, 3, CC_FSUE_Clarification_NoOutput );
    if ( !steps_3 )
    {
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_en_passant_append__new( steps_3, CC_SLE_Destination, 9, 2, CC_PE_LightPawn, 9, 4, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_3 );
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    CcPly * plies_3 = cc_ply_cascade__new( CC_PE_DarkPawn, &steps_3 );
    if ( !plies_3 )
    {
        cc_step_free_all_steps( &steps_3 );
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    //
    // move 1, [pk4-j3:Pj5]

    CcMove * move_1 = cc_move__new( "[pk4-j3:Pj5]", &plies_3, CC_MSE_None );
    if ( !move_1 )
    {
        cc_ply_free_all_plies( &plies_3 );
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_1, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_1, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 2 ) == CC_PE_LightPegasus ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 9, 1 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 9, 1 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 9, 2 ) == CC_PE_DarkPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 9, 4 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    cc_move_free_all_moves( &move_1 );
    free( cb );

    return result;
}

bool test_do_move_castling( TestPrints tp )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece_tag( cb, 1, 0, CC_PE_LightRook, CC_TE_CanCastle );
    cc_chessboard_set_piece_tag( cb, 13, 0, CC_PE_LightKing, CC_TE_CanCastle );
    cc_chessboard_set_piece_tag( cb, 24, 0, CC_PE_LightRook, CC_TE_CanCastle );

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 13, 0 ) == CC_PE_LightKing ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 24, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 1, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 13, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 24, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // move Ku&t

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 13, 0, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_castle_append__new( steps_0, CC_SLE_Destination, 20, 0, CC_PE_LightRook, 24, 0, 19, 0, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * ply = cc_ply_cascade__new( CC_PE_LightKing, &steps_0 );
    if ( !ply )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // move, [Kn1-u1&Ry1-t1]

    CcMove * move = cc_move__new( "[Kn1-u1&Ry1-t1]", &ply, CC_MSE_None );
    if ( !move )
    {
        cc_ply_free_all_plies( &ply );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 19, 0 ) == CC_PE_LightRook ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 20, 0 ) == CC_PE_LightKing ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 1, 0 ) == CC_TE_CanCastle ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 19, 0 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 20, 0 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    cc_move_free_all_moves( &move );
    free( cb );

    return result;
}

bool test_do_move_tag_and_promotion( TestPrints tp )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 11, 21, CC_PE_LightPawn );
    cc_chessboard_set_piece( cb, 15, 21, CC_PE_LightPyramid );
    cc_chessboard_set_piece( cb, 21, 15, CC_PE_LightBishop );

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 21 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 21 ) == CC_PE_LightPyramid ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 21, 15 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 11, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 15, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 21, 15 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bp22~

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 21, 15, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_0, CC_SLE_Destination, 15, 21, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_LightBishop, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply Al22=

    CcStep * steps_1 = cc_step_none__new( CC_SLE_Start, 15, 21, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_tag_for_promotion_append__new( steps_1, CC_SLE_Destination, 11, 21, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightPyramid, &steps_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move [Bv16-p22]~[Ap22-l22=]

    CcMove * move_0 = cc_move__new( "[Bv16-p22]~[Ap22-l22=]", &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 21 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 21 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 11, 21 ) == CC_TE_DelayedPromotion ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 15, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 21, 15 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    //
    // ply l22Q

    CcStep * steps_2 = cc_step_none__new( CC_SLE_Start, 11, 21, CC_FSUE_Clarification_NoOutput );
    if ( !steps_2 )
    {
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_promote_append__new( steps_2, CC_SLE_Destination, 11, 21, CC_PE_LightQueen, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    CcPly * plies_2 = cc_ply_cascade__new( CC_PE_LightPawn, &steps_2 );
    if ( !plies_2 )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    //
    // move [Pl22-l22=Q]

    CcMove * move_1 = cc_move__new( "[Pl22-l22=Q]", &plies_2, CC_MSE_None );
    if ( !move_1 )
    {
        cc_ply_free_all_plies( &plies_2 );
        cc_move_free_all_moves( &move_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_1, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_1, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 21 ) == CC_PE_LightQueen ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 21 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 11, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 15, 21 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 21, 15 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    cc_move_free_all_moves( &move_1 );
    free( cb );

    return result;
}

bool test_do_move_conversion( TestPrints tp, bool is_failed )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    if ( is_failed )
        cc_chessboard_set_piece( cb, 11, 5, CC_PE_DarkStarchild );
    else
        cc_chessboard_set_piece( cb, 11, 5, CC_PE_DarkShaman );

    cc_chessboard_set_piece( cb, 15, 5, CC_PE_LightPyramid );
    cc_chessboard_set_piece( cb, 21, 11, CC_PE_LightBishop );

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    if ( is_failed )
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_DarkStarchild ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;
    else
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_DarkShaman ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 5 ) == CC_PE_LightPyramid ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 21, 11 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bp6~

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 21, 11, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_0, CC_SLE_Destination, 15, 5, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_LightBishop, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply Al6%H

    CcStep * steps_1 = cc_step_none__new( CC_SLE_Start, 15, 5, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcSideEffect se_1;
    if ( is_failed )
        se_1 = cc_side_effect_failed_conversion();
    else
        se_1 = cc_side_effect_convert( CC_PE_LightShaman, false );

    if ( !cc_step_append__new( steps_1, CC_SLE_Destination, 11, 5, se_1, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightPyramid, &steps_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move [Bv12-p6]~[Ap6-l6%H]
    //      [Bv12-p6]~[Ap6-l6%%]

    char * alg_not =  ( is_failed ) ? "[Bv12-p6]~[Ap6-l6%%]" : "[Bv12-p6]~[Ap6-l6%H]";

    CcMove * move_0 = cc_move__new( alg_not, &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    if ( is_failed )
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_DarkStarchild ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;
    else
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 5 ) == CC_PE_LightShaman ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 5 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    free( cb );

    return result;
}

bool test_do_move_demotion( TestPrints tp )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 0, 0, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 25, 25, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 11, 11, CC_PE_LightBishop );
    cc_chessboard_set_piece( cb, 23, 15, CC_PE_Monolith );

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 11 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 23, 15 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Mw23>Bl12

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 23, 15, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_demote_append__new( steps_0, CC_SLE_Destination, 22, 22, CC_PE_LightBishop, 11, 11, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_Monolith, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // move [Mx16-w23>Bl12]

    CcMove * move_0 = cc_move__new( "[Mx16-w23>Bl12]", &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 11 ) == CC_PE_LightPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 22, 22 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    free( cb );

    return result;
}

bool test_do_move_resurrection( TestPrints tp, bool is_failed, bool is_oblationing )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 25, 0, CC_PE_DimStar );
    cc_chessboard_set_piece( cb, 0, 25, CC_PE_DimStar );
    cc_chessboard_set_piece( cb, 23, 15, CC_PE_LightStarchild );

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 23, 15 ) == CC_PE_LightStarchild ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Ip11$B, Ip11$$

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 23, 15, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

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

    if ( !cc_step_append__new( steps_0, CC_SLE_Destination, 15, 10, se_0, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_LightStarchild, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // move [Ix16-p11$Wq12]
    //      [Ix16-p11$Bp11]
    //      [Ix16-p11$$]
    //      [Ix16-p11$$]

    char * alg_not = ( is_failed ) ? "[Ix16-p11$$]"
                                   : ( is_oblationing ) ? "[Ix16-p11$Bp11]"
                                                        : "[Ix16-p11$Wq12]";

    CcMove * move_0 = cc_move__new( alg_not, &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_failed )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 10 ) == CC_PE_LightStarchild ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 16, 11 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                && result;
    }
    else
    {
        if ( is_oblationing )
        {
            result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 10 ) == CC_PE_LightBishop ),
                                        TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                    && result;

            result = test_print_failure( ( cc_chessboard_get_piece( cb, 16, 11 ) == CC_PE_None ),
                                        TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                    && result;
        }
        else
        {
            result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 10 ) == CC_PE_LightStarchild ),
                                        TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                    && result;

            result = test_print_failure( ( cc_chessboard_get_piece( cb, 16, 11 ) == CC_PE_LightWave ),
                                        TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                    && result;
        }
    }

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    free( cb );

    return result;
}

bool test_do_move_teleportation( TestPrints tp, bool is_failed )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 0, 0, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 25, 25, CC_PE_BrightStar );
    cc_chessboard_set_piece( cb, 25, 0, CC_PE_DimStar );
    cc_chessboard_set_piece( cb, 0, 25, CC_PE_DimStar );

    cc_chessboard_set_piece( cb, 3, 22, CC_PE_LightBishop );

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 3, 22 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Ba26

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 3, 22, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_0, CC_SLE_Destination, 0, 25, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_LightBishop, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply |By25
    // ply ||By25

    CcPly * ply_1;
    if ( is_failed )
        ply_1 = cc_ply_failed_teleport_append__new( plies_0, CC_PE_LightBishop, 0, 24 );
    else
        ply_1 = cc_ply_teleport_append__new( plies_0, CC_PE_LightBishop, 24, 24 );

    if ( !ply_1 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move [Bd23-a26]|[By25]
    //      [Bd23-a26]||[Ba25]

    char * alg_not = ( is_failed ) ? "[Bd23-a26]||[Ba25]" : "[Bd23-a26]|[By25]";

    CcMove * move_0 = cc_move__new( alg_not, &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 0 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 25 ) == CC_PE_BrightStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 25, 0 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 25 ) == CC_PE_DimStar ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 3, 22 ) == CC_PE_None ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_failed )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 24 ) == CC_PE_LightBishop ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 24, 24 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                && result;
    }
    else
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 0, 24 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 24, 24 ) == CC_PE_LightBishop ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;
    }

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    free( cb );

    return result;
}

bool test_do_move_teleportation_wave( TestPrints tp, bool is_oblationing )
{
    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 5, 11, CC_PE_Monolith );
    cc_chessboard_set_piece( cb, 19, 9, CC_PE_Monolith );

    cc_chessboard_set_piece( cb, 10, 12, CC_PE_LightBishop );
    cc_chessboard_set_piece( cb, 8, 14, CC_PE_LightWave );
    cc_chessboard_set_piece( cb, 17, 7, CC_PE_LightKnight );

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 5, 11 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 19, 9 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 8, 14 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 17, 7 ) == CC_PE_LightKnight ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Bi15

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 10, 12, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_0, CC_SLE_Destination, 8, 14, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( CC_PE_LightBishop, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Wf12

    CcStep * steps_1 = cc_step_none__new( CC_SLE_Start, 8, 14, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_1, CC_SLE_Destination, 5, 11, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightWave, &steps_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply |Wr8

    CcPly * ply_2;

    if ( is_oblationing )
        ply_2 = cc_ply_failed_teleport_oblation_append__new( plies_0, CC_PE_LightWave );
    else
    {
        CcStep * steps_2 = cc_step_none__new( CC_SLE_Start, 19, 9, CC_FSUE_Clarification_NoOutput );
        if ( !steps_2 )
        {
            cc_ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !cc_step_none_append__new( steps_2, CC_SLE_Destination, 17, 7, CC_FSUE_User ) )
        {
            cc_step_free_all_steps( &steps_2 );
            cc_ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        ply_2 = cc_ply_teleport_wave_append__new( plies_0, CC_PE_LightWave, &steps_2 );
    }

    if ( !ply_2 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Np9

    if ( !is_oblationing )
    {
        CcStep * steps_3 = cc_step_none__new( CC_SLE_Start, 17, 7, CC_FSUE_Clarification_NoOutput );
        if ( !steps_3 )
        {
            cc_ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !cc_step_none_append__new( steps_3, CC_SLE_Destination, 15, 8, CC_FSUE_User ) )
        {
            cc_step_free_all_steps( &steps_3 );
            cc_ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }

        if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightKnight, &steps_3 ) )
        {
            cc_step_free_all_steps( &steps_3 );
            cc_ply_free_all_plies( &plies_0 );
            free( cb );
            return false;
        }
    }

    //
    // move [Bk13-i15]~[Wi15-f12]|[Wt10-r8]~[Nr8-p9]
    //      [Bk13-i15]~[Wi15-f12]||[W]

    char * alg_not = ( is_oblationing ) ? "[Bk13-i15]~[Wi15-f12]||[W]"
                                        : "[Bk13-i15]~[Wi15-f12]|[Wt10-r8]~[Nr8-p9]";

    CcMove * move_0 = cc_move__new( alg_not, &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard ) cc_chessboard_print( cb, true );

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 5, 11 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 19, 9 ) == CC_PE_Monolith ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 10, 12 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 8, 14 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_oblationing )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 17, 7 ) == CC_PE_LightKnight ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;
    }
    else
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 17, 7 ) == CC_PE_LightWave ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 8 ) == CC_PE_LightKnight ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;
    }

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    free( cb );

    return result;
}

bool test_do_move_trance_journey( TestPrints tp, bool is_capturing )
{
    CcPieceEnum shaman = is_capturing ? CC_PE_DarkShaman : CC_PE_LightShaman;

    // chessboard

    CcChessboard * cb = cc_chessboard__new( CC_VE_One, false );
    if ( !cb ) return false;

    cc_chessboard_set_piece( cb, 4, 8, shaman ); // entrancing
    cc_chessboard_set_piece( cb, 6, 9, CC_PE_LightWave );
    cc_chessboard_set_piece( cb, 7, 7, shaman ); // entranced

    cc_chessboard_set_piece( cb, 7, 12, CC_PE_LightBishop ); // 2
    cc_chessboard_set_piece( cb, 5, 1, CC_PE_DarkKnight ); // 4
    cc_chessboard_set_piece_tag( cb, 21, 4, CC_PE_DarkPawn, CC_TE_DelayedPromotion ); // 9

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    bool result = true;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 4, 8 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 6, 9 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 7 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 12 ) == CC_PE_LightBishop ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 5, 1 ) == CC_PE_DarkKnight ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 21, 4 ) == CC_PE_DarkPawn ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 21, 4 ) == CC_TE_DelayedPromotion ),
                                 TME_Error, "tag not found", __FILE__, __LINE__, __func__ )
             && result;

    if ( !result )
    {
        free( cb );
        return false;
    }

    //
    // ply Hg10

    CcStep * steps_0 = cc_step_none__new( CC_SLE_Start, 4, 8, CC_FSUE_Clarification_NoOutput );
    if ( !steps_0 )
    {
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_0, CC_SLE_Destination, 6, 9, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    CcPly * plies_0 = cc_ply_cascade__new( shaman, &steps_0 );
    if ( !plies_0 )
    {
        cc_step_free_all_steps( &steps_0 );
        free( cb );
        return false;
    }

    //
    // ply ~Wh8

    CcStep * steps_1 = cc_step_none__new( CC_SLE_Start, 6, 9, CC_FSUE_Clarification_NoOutput );
    if ( !steps_1 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_1, CC_SLE_Destination, 7, 7, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_cascade_append__new( plies_0, CC_PE_LightWave, &steps_1 ) )
    {
        cc_step_free_all_steps( &steps_1 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // ply @H..h13<Bj19..f2<Nb6..p7..j19<Bl25..v5<P==p7
    // ply @H..h13*B..f2*N..p7..j19..v5*P==

    CcStep * steps_2 = cc_step_none__new( CC_SLE_Start, 7, 7, CC_FSUE_Clarification_NoOutput );
    if ( !steps_2 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcSideEffect sse_2_1 = is_capturing ?
                           cc_side_effect_capture( CC_PE_LightBishop, false ) :
                           cc_side_effect_displacement( CC_PE_LightBishop, false, 9, 18 );
    if ( !cc_step_append__new( steps_2, CC_SLE_Distant, 7, 12, sse_2_1, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcSideEffect sse_2_2 = is_capturing ?
                           cc_side_effect_capture( CC_PE_DarkKnight, false ) :
                           cc_side_effect_displacement( CC_PE_DarkKnight, false, 1, 5 );
    if ( !cc_step_append__new( steps_2, CC_SLE_Distant, 5, 1, sse_2_2, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_step_none_append__new( steps_2, CC_SLE_Distant, 15, 6, CC_FSUE_Addition ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcSideEffect sse_2_4 = is_capturing ?
                           cc_side_effect_none() :
                           cc_side_effect_displacement( CC_PE_LightBishop, false, 11, 24 );
    if ( !cc_step_append__new( steps_2, CC_SLE_Distant, 9, 18, sse_2_4, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    CcSideEffect sse_2_5 = is_capturing ?
                           cc_side_effect_capture( CC_PE_DarkPawn, true ) :
                           cc_side_effect_displacement( CC_PE_DarkPawn, true, 15, 6 );
    if ( !cc_step_append__new( steps_2, CC_SLE_Destination, 21, 4, sse_2_5, CC_FSUE_User ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    if ( !cc_ply_trance_journey_append__new( plies_0, shaman, &steps_2, 7, 7 ) )
    {
        cc_step_free_all_steps( &steps_2 );
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    //
    // move [He9-g10]~[Wg10-h8]@[Hh8..h13<Bj19..f2<nb6..p7..j19<Bl25-v5<p==p7]
    //      [he9-g10]~[Wg10-h8]@[hh8..h13*B..f2*n..p7..j19-v5*p==]

    char * alg_not = ( is_capturing ) ? "[he9-g10]~[Wg10-h8]@[hh8..h13*B..f2*n..p7..j19-v5*p==]"
                                      : "[He9-g10]~[Wg10-h8]@[Hh8..h13<Bj19..f2<nb6..p7..j19<Bl25-v5<p==p7]";

    CcMove * move_0 = cc_move__new( alg_not, &plies_0, CC_MSE_None );
    if ( !move_0 )
    {
        cc_ply_free_all_plies( &plies_0 );
        free( cb );
        return false;
    }

    result = test_print_failure( cc_do_moves( cb, move_0, CC_DME_DoAllMoves ),
                                 TME_Error, "move not done", __FILE__, __LINE__, __func__ )
             && result;

    if ( tp.do_print_move )
    {
        char * alg_not = cc_format_move__new( cb, move_0, tp.format_move );
        printf( "%s\n", alg_not );
        free( alg_not );
    }

    if ( tp.do_print_chessboard )
    {
        cc_chessboard_print( cb, true );
        cc_chessboard_print( cb, false );
    }

    //
    // tests

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 4, 8 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 6, 9 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 7 ) == CC_PE_LightWave ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 21, 4 ) == shaman ),
                                 TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_tag( cb, 21, 4 ) == CC_TE_None ),
                                 TME_Error, "tag found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 7, 12 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 5, 1 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    result = test_print_failure( ( cc_chessboard_get_piece( cb, 9, 18 ) == CC_PE_None ),
                                 TME_Error, "piece found", __FILE__, __LINE__, __func__ )
             && result;

    if ( is_capturing )
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 24 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 6 ) == CC_PE_None ),
                                    TME_Error, "piece found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_tag( cb, 15, 6 ) == CC_TE_None ),
                                    TME_Error, "tag found", __FILE__, __LINE__, __func__ )
                && result;
    }
    else
    {
        result = test_print_failure( ( cc_chessboard_get_piece( cb, 11, 24 ) == CC_PE_LightBishop ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 1, 5 ) == CC_PE_DarkKnight ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_piece( cb, 15, 6 ) == CC_PE_DarkPawn ),
                                    TME_Error, "piece not found", __FILE__, __LINE__, __func__ )
                && result;

        result = test_print_failure( ( cc_chessboard_get_tag( cb, 15, 6 ) == CC_TE_None ),
                                    TME_Error, "tag found", __FILE__, __LINE__, __func__ )
                && result;
    }

    //
    // free, return

    cc_move_free_all_moves( &move_0 );
    free( cb );

    return result;
}
