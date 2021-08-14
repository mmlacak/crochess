// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_version.h"
#include "cc_tokenizer.h"
#include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"
#include "cc_parse_msg.h"
#include "cc_parse_utils.h"
#include "cc_parse_move.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests_do_move.h"
#include "tests_book_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.2.2:201+20210814.152812"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


TestMsg * test()
{
    TestMsg * test_msgs = NULL;

    test_msg_init_or_append_new( &test_msgs, TME_Debug, "just debugging", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append_new( &test_msgs, TME_Info, "just informing", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append_new( &test_msgs, TME_Warning, "just warning", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append_new( &test_msgs, TME_Error, "stepped into a turd", __FILE__, __LINE__, __func__ );

    test_msg_init_or_append_new( &test_msgs, TME_Fatal, "it's a serious shit", __FILE__, __LINE__, __func__ );

    return test_msgs;
}


bool get_print_chessboard_from_cli_arg()
{
    bool do_print_chesboard = false;

    char * dpcb = cc_next_token_new( NULL, NULL );
    if ( dpcb )
        do_print_chesboard = ( ( !strncmp( dpcb, "1", 1 ) ) || ( !strncmp( dpcb, "true", 4 ) ) ) ? true : false;

    free( dpcb );
    dpcb = NULL;

    return do_print_chesboard;
}

bool get_print_move_from_cli_arg()
{
    bool do_print_move = true;

    char * dpm = cc_next_token_new( NULL, NULL );
    if ( dpm )
        do_print_move = ( ( !strncmp( dpm, "0", 1 ) ) || ( !strncmp( dpm, "false", 5 ) ) ) ? false : true;

    free( dpm );
    dpm = NULL;

    return do_print_move;
}

CcFormatMove get_format_move_from_cli_arg()
{
    CcFormatMove fm_user = cc_format_move_user( CC_FMSE_FormatOnlyCurrentMove );
    CcFormatMove fm_output = cc_format_move_output( CC_FMSE_FormatOnlyCurrentMove );
    CcFormatMove fm_debug = cc_format_move_debug( CC_FMSE_FormatOnlyCurrentMove );

    CcFormatMove format_move = fm_output;

    char * fm = cc_next_token_new( NULL, NULL );
    if ( fm )
        format_move = ( ( !strncmp( fm, "u", 1 ) ) || ( !strncmp( fm, "user", 4 ) ) ) ? fm_user
                    : ( ( !strncmp( fm, "d", 1 ) ) || ( !strncmp( fm, "debug", 5 ) ) ) ? fm_debug
                    : fm_output;

    free( fm );
    fm = NULL;

    return format_move;
}

int get_test_number_from_cli_arg()
{
    int test_number = 0; // all tests

    char * tn = cc_next_token_new( NULL, NULL );
    if ( tn )
        test_number = atoi( tn );

    free( tn );
    tn = NULL;

    return test_number;
}


int main( void )
{
    print_app_intro( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );

// // TODO :: DEBUG
//     // /* static */ char * foo = "xxx";
//     char * foo = malloc( 4 );
//     strncpy( foo, "xxx", 4 );
//     printf( "%s.\n", foo );

//     // foo = (char *)((int)foo + 1);
//     // printf( "%s.\n", foo );
//     printf( "%s.\n", ++foo );

//     free( foo-1 );
//     // free( foo );

//     printf( "%s.\n", foo );
// // TODO :: DEBUG


// // TODO :: DEBUG
//     printf("1: %d.\n", CC_MAX( 5, 11 ));
//     printf("2: %d.\n", CC_MAX( 11, 5 ));

//     printf("3: %d.\n", CC_MIN( 5, 11 ));
//     printf("4: %d.\n", CC_MIN( 11, 5 ));

//     printf("5: %f.\n", CC_MAX( 5.1, 11.2 ));
//     printf("6: %f.\n", CC_MAX( 11.3, 5.4 ));

//     printf("7: %f.\n", CC_MIN( 5.5, 11.6 ));
//     printf("8: %f.\n", CC_MIN( 11.7, 5.8 ));
// // TODO :: DEBUG


// // TODO :: DEBUG
//     char * con_1 = cc_str_concatenate_new( "Hello", "World!" );
//     printf( "1: %s.\n", con_1 );
//     free( con_1 );

//     char * con_2 = cc_str_concatenate_len_new( "Hello", "World!", BUFSIZ );
//     printf( "2: %s.\n", con_2 );
//     free( con_2 );

//     char * con_3 = cc_str_concatenate_len_new( "Hello", "World!", 7 );
//     printf( "3: %s.\n", con_3 );
//     free( con_3 );

//     char * dup_4 = cc_str_duplicate_len_new( "Hello World!", BUFSIZ );
//     printf( "4: %s.\n", dup_4 );
//     free( dup_4 );

//     char * dup_5 = cc_str_duplicate_len_new( "Hello World!", 9 );
//     printf( "5: %s.\n", dup_5 );
//     free( dup_5 );

//     char * con_6 = cc_str_concatenate_new( NULL, "Hello World!" );
//     printf( "6: %s.\n", con_6 );
//     free( con_6 );

//     char * dup_7 = cc_str_concatenate_len_new( "Hello World!", NULL, BUFSIZ );
//     printf( "7: %s.\n", dup_7 );
//     free( dup_7 );

//     char * dup_8 = cc_str_concatenate_len_new( NULL, "Hello World!", 9 );
//     printf( "8: %s.\n", dup_8 );
//     free( dup_8 );
// // TODO :: DEBUG


    char * ret = NULL;
    char buffer[ BUFSIZ ];

    while ( true )
    {
        memset( buffer, 0, BUFSIZ );

        printf( "> " );
        // fflush( stdout ); // Run directly from terminal works ok, even without fflush().

        ret = fgets( buffer, BUFSIZ, stdin );
        if ( !ret )
        {
            printf( "Input error.\n" );
            continue;
        }

        char * cmd = cc_next_token_new( buffer, CC_TOKEN_SEPARATORS_WHITESPACE );
        if ( !cmd ) continue;

        if ( ( !strcmp( "q", cmd ) ) || ( !strcmp( "quit", cmd ) ) )
        {
            free( cmd );
            break;
        }
        else if ( ( !strcmp( "v", cmd ) ) || ( !strcmp( "version", cmd ) ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );
        }
        else if ( ( !strcmp( "a", cmd ) ) || ( !strcmp( "about", cmd ) ) )
        {
            print_about_info();
        }
        else if ( ( !strcmp( "b", cmd ) ) || ( !strcmp( "book", cmd ) ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg();
            bool do_print_move = get_print_move_from_cli_arg();
            CcFormatMove format_move = get_format_move_from_cli_arg();
            int test_number = get_test_number_from_cli_arg();

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_book_move_scn_ct_03_define_step_ply( tp ) )
                    printf( "Test test_book_move_scn_ct_03_define_step_ply() failed.\n" );
        }
        else if ( ( !strcmp( "t", cmd ) ) || ( !strcmp( "test", cmd ) ) )
        {
            bool do_print_chesboard = get_print_chessboard_from_cli_arg();
            bool do_print_move = get_print_move_from_cli_arg();
            CcFormatMove format_move = get_format_move_from_cli_arg();
            int test_number = get_test_number_from_cli_arg();

            TestPrints tp = test_prints( do_print_chesboard, do_print_move, format_move );

            if ( ( test_number == 1 ) || ( test_number == 0 ) )
                if ( !test_do_move_single_ply( tp ) )
                    printf( "Test test_do_move_single_ply() failed.\n" );

            if ( ( test_number == 2 ) || ( test_number == 0 ) )
                if ( !test_do_move_cascading_plies( tp ) )
                    printf( "Test test_do_move_cascading_plies() failed.\n" );

            if ( ( test_number == 3 ) || ( test_number == 0 ) )
                if ( !test_do_move_castling( tp ) )
                    printf( "Test test_do_move_castling() failed.\n" );

            if ( ( test_number == 4 ) || ( test_number == 0 ) )
                if ( !test_do_move_tag_and_promotion( tp ) )
                    printf( "Test test_do_move_tag_and_promotion() failed.\n" );

            if ( ( test_number == 5 ) || ( test_number == 0 ) )
                if ( !test_do_move_conversion( tp, false ) )
                    printf( "Test test_do_move_conversion( _, false ) failed.\n" );

            if ( ( test_number == 6 ) || ( test_number == 0 ) )
                if ( !test_do_move_conversion( tp, true ) )
                    printf( "Test test_do_move_conversion( _, true ) failed.\n" );

            if ( ( test_number == 7 ) || ( test_number == 0 ) )
                if ( !test_do_move_demotion( tp ) )
                    printf( "Test test_do_move_demotion() failed.\n" );


            if ( ( test_number == 8 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( tp, false, false ) )
                    printf( "Test test_do_move_resurrection( _, false, false ) failed.\n" );

            if ( ( test_number == 9 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( tp, false, true ) )
                    printf( "Test test_do_move_resurrection( _, false, true ) failed.\n" );

            if ( ( test_number == 10 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( tp, true, false ) )
                    printf( "Test test_do_move_resurrection( _, true, false ) failed.\n" );

            if ( ( test_number == 11 ) || ( test_number == 0 ) )
                if ( !test_do_move_resurrection( tp, true, true ) )
                    printf( "Test test_do_move_resurrection( _, true, true ) failed.\n" );


            if ( ( test_number == 12 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation( tp, false ) )
                    printf( "Test test_do_move_teleportation( _, false ) failed.\n" );

            if ( ( test_number == 13 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation( tp, true ) )
                    printf( "Test test_do_move_teleportation( _, true ) failed.\n" );


            if ( ( test_number == 14 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation_wave( tp, false ) )
                    printf( "Test test_do_move_teleportation_wave( _, false ) failed.\n" );

            if ( ( test_number == 15 ) || ( test_number == 0 ) )
                if ( !test_do_move_teleportation_wave( tp, true ) )
                    printf( "Test test_do_move_teleportation_wave( _, true ) failed.\n" );


            if ( ( test_number == 16 ) || ( test_number == 0 ) )
                if ( !test_do_move_trance_journey( tp, false ) )
                    printf( "Test test_do_move_trance_journey( _, false ) failed.\n" );

            if ( ( test_number == 17 ) || ( test_number == 0 ) )
                if ( !test_do_move_trance_journey( tp, true ) )
                    printf( "Test test_do_move_trance_journey( _, true ) failed.\n" );

            printf( "Tests finished.\n" );
        }
        else if ( !strcmp( "y", cmd ) )
        {
            // char const * const user_an = "[Ng6]~[We5]~[Re8]";
            // char const * const user_an = "Ng6~[We5]~Re8";
            // char const * const user_an = "Ne6-a3";
            // char const * const user_an = "Hb14~We12@@P,B,R,R,N,B,N";

            // char const * const user_an = "Bi15~Wf12|Wr8~Np9";
            // char const * const user_an = "Bi15~Wf12||W";
            char const * const user_an = "Hg10~Wh8@[H..h13<Bj19..f2<Nb6..j19<Bl25-v5<P==p7]";
            // char const * const user_an = "Hg10~Wh8@[H,j9..h13*B..f2*N..j19-v5*P==]";

            // char const * const user_an = "Bi15~Wf12|Wr8|Na3@Np9||Ba3||K@@P,B,R,R,N,B,N@@@M::Sx7||";

            // TODO :: Uncomment free(), if this is active!
            // char * user_an = cc_next_token_new( NULL, NULL );

            if ( user_an )
            {
                printf( "%s\n", user_an );

                char * an = cc_parse_utils_next_ply_str_new( user_an );
                if ( !an ) continue;

                CcPlyLinkEnum ple = CC_PLE_Ply;
                bool result_1 = true;

                CcPieceEnum pe = CC_PE_None;
                bool result_2 = true;

                bool is_piece_light = true;

                char const * steps_str = NULL;

                do
                {
                    result_1 = cc_parse_utils_get_ply_link( an, &ple );
                    result_2 = cc_parse_ply_get_piece( an, is_piece_light, &pe );
                    steps_str = cc_parse_utils_get_steps_str( an );

                    printf( "%s ", an );

                    if ( result_1 )
                        printf( " - %d %s", ple, cc_ply_link_symbol(ple ) );

                    if ( result_2 )
                        printf( " - %d %c", pe, cc_piece_as_char( pe ) );

                    if ( steps_str )
                        printf( " - %s", steps_str );

                    if ( ( !result_1 ) && ( !result_2 ) )
                        printf( " ---" );

                    printf( "\n" );

                    is_piece_light = !is_piece_light;

                    free( an );
                    // an = NULL;

                    an = cc_parse_utils_next_ply_str_new( NULL );
                }
                while ( an );

                // TODO :: Uncomment, if cc_next_token_new() is active!
                // free( user_an );
                // user_an = NULL;
            }
        }
        else if ( !strcmp( "z", cmd ) )
        {
            TestMsg * test_msgs = test();

            test_msg_print_all( test_msgs, TME_Warning );

            test_msg_free_all( &test_msgs );
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }

        free( cmd );
    }

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
