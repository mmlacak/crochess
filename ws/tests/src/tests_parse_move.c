// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


// #include <stdlib.h>
// #include <stdio.h>
// #include <string.h>

// #include "cc_defines.h"
// #include "cc_str_utils.h"

// #include "cc_version.h"
// #include "cc_tokenizer.h"
// #include "cc_piece.h"
// #include "cc_chessboard.h"

// #include "cc_step.h"
// #include "cc_ply.h"
// #include "cc_move.h"
// #include "cc_parse_msg.h"
// #include "cc_parse_utils.h"
// #include "cc_parse_move.h"

// #include "hlp_msgs.h"
// #include "test_msgs.h"
// #include "tests_do_move.h"
// #include "tests_book_move.h"
// #include "tests.h"


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
#include "cc_parse_utils.h"
#include "cc_parse_move.h"

#include "test_utils.h"
#include "test_msgs.h"
#include "tests_parse_move.h"


bool test_parser( CcChessboard const * const restrict cb,
                  char const * const restrict move_str,
                  TestPrints const tp )
{
    if ( !cb ) return false;
    if ( !move_str ) return false;

    printf( "%s\n", move_str );

    char * ply_an__o = cc_parse_utils_next_ply_str_new( move_str );
    if ( !ply_an__o ) return false;

    CcPlyLinkEnum ple = CC_PLE_Ply;
    bool result_1 = true;

    CcPieceEnum pe = CC_PE_None;
    bool result_2 = true;

    bool is_piece_light = true;

    char const * steps_str = NULL;

    do
    {
        printf( "%s ", ply_an__o );

        result_1 = cc_parse_utils_get_ply_link( ply_an__o, &ple );
        if ( result_1 )
            printf( " [%d %s]", ple, cc_ply_link_symbol( ple ) );
        else
            printf( " [---]" );

        result_2 = cc_parse_utils_get_ply_piece( ply_an__o, is_piece_light, &pe );
        if ( result_2 )
            printf( " {%d %c}", pe, cc_piece_as_char( pe ) );
        else
            printf( " {---}" );

        steps_str = cc_parse_utils_get_steps_str( ply_an__o );
        if ( steps_str )
        {
            printf( " (%s)", steps_str );

            char * step_an__o = cc_parse_utils_next_step_str_new( steps_str );
            CcStepLinkEnum sle = CC_SLE_Destination;

            if ( step_an__o )
            {
                printf( "\n" );

                do
                {
                    printf( "    %s", step_an__o );

                    bool result_3 = cc_parse_utils_get_step_link( ply_an__o, step_an__o, &sle );
                    if ( result_3 )
                        printf( " [%d %s]", sle, cc_step_link_symbol( sle ) );
                    else
                        printf( " [---]" );

                    char * fields_an__o = cc_parse_utils_step_fields_str_new( step_an__o );
                    if ( fields_an__o )
                        printf( " {%s}", fields_an__o );
                    else
                        printf( " {---}" );

                    int disamb_step_i;
                    int disamb_step_j;
                    int step_i;
                    int step_j;

                    bool result_4 = cc_parse_utils_get_fields( fields_an__o,
                                                               cb,
                                                               &disamb_step_i,
                                                               &disamb_step_j,
                                                               &step_i,
                                                               &step_j );

                    if ( result_4 )
                        printf( " {%d, %d --> %d, %d}", disamb_step_i, disamb_step_j, step_i, step_j );
                    else
                        printf( " { --> }" );

                    free( fields_an__o );
                    fields_an__o = NULL;

                    char const * side_effects = cc_parse_utils_side_effect_str( step_an__o );
                    if ( side_effects )
                        printf( " (%s)", side_effects );
                    else
                        printf( " (---)" );

                    CcSideEffect se = cc_side_effect_none();
                    bool result_5 = cc_parse_utils_get_side_effect( step_an__o,
                                                                    cb,
                                                                    pe,
                                                                    step_i,
                                                                    step_j,
                                                                    &se );
                    if ( result_5 )
                    {
                        char * se__o = cc_format_side_effect_new( &se, tp.format_move );
                        if ( se__o )
                        {
                            printf( " >%s<", se__o );
                            free( se__o );
                            se__o = NULL;
                        }
                        else
                            printf( " >===<" );
                    }
                    else
                        printf( " >---<" );

                    printf( "\n" );
                    step_an__o = cc_parse_utils_next_step_str_new( NULL );
                }
                while ( step_an__o );
            }

            free( step_an__o );
            step_an__o = NULL;
        }

        if ( ( !result_1 ) && ( !result_2 ) && ( !steps_str ) )
            printf( " ---" );

        printf( "\n" );

        is_piece_light = !is_piece_light;

        free( ply_an__o );
        ply_an__o = cc_parse_utils_next_ply_str_new( NULL );
    }
    while ( ply_an__o );

    return true;
}


bool test_parse_move_single_ply( TestPrints tp )
{
    printf( TESTS_MOVE_TEST_SEPARATOR );
    printf( "test_do_move_single_ply\n" );

    // chessboard

    CcChessboard * cb__o = cc_chessboard_new( CC_VE_One, false );
    if ( !cb__o ) return false;

    cc_chessboard_set_piece( cb__o, 5, 2, CC_PE_LightPegasus );
    cc_chessboard_set_piece( cb__o, 10, 12, CC_PE_DarkPawn );

    if ( tp.do_print_chessboard )
    {
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
        cc_chessboard_print( cb__o, true );
        printf( TESTS_MOVE_CHESSBOARD_SEPARATOR );
    }

    //
    // [Gf3.g5..i9..k13*p==]
    char const * const move_str = "[Gf3.g5..i9..k13*P==]";

    if ( tp.do_print_move )
    {
        printf( TESTS_MOVE_NOTATION_SEPARATOR );
        printf( "%s\n", move_str );
    }


    //
    // tests

    bool result = true;

    result = test_print_failure( test_parser( cb__o, move_str, tp ),
                                 TME_Error, "parse failed", __FILE__, __LINE__, __func__ )
             && result;

    //
    // free, return

    return cc_move_data_free_all( &cb__o, NULL, NULL, NULL, result );
}
