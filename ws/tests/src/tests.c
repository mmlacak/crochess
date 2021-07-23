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
#include "cc_format_moves.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests_do_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.60:164+20210723.191952"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


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
        else if ( !strcmp( "y", cmd ) )
        {
            // CcFormatMove fm = cc_format_move_user( CC_FMSE_FormatOnlyCurrentMove );
            CcFormatMove fm = cc_format_move_output( CC_FMSE_FormatOnlyCurrentMove );
            // CcFormatMove fm = cc_format_move_debug( CC_FMSE_FormatOnlyCurrentMove );

            TestPrints tp = test_prints( false, true, fm );

            if ( !test_do_move_single_ply( tp ) ) printf( "Test test_do_move_single_ply() failed.\n" );
            if ( !test_do_move_cascading_plies( tp ) ) printf( "Test test_do_move_cascading_plies() failed.\n" );
            if ( !test_do_move_castling( tp ) ) printf( "Test test_do_move_castling() failed.\n" );
            if ( !test_do_move_tag_and_promotion( tp ) ) printf( "Test test_do_move_tag_and_promotion() failed.\n" );
            if ( !test_do_move_conversion( tp, false ) ) printf( "Test test_do_move_conversion( _, false ) failed.\n" );
            if ( !test_do_move_conversion( tp, true ) ) printf( "Test test_do_move_conversion( _, true ) failed.\n" );
            if ( !test_do_move_demotion( tp ) ) printf( "Test test_do_move_demotion() failed.\n" );

            if ( !test_do_move_resurrection( tp, false, false ) ) printf( "Test test_do_move_resurrection( _, false, false ) failed.\n" );
            if ( !test_do_move_resurrection( tp, false, true ) ) printf( "Test test_do_move_resurrection( _, false, true ) failed.\n" );
            if ( !test_do_move_resurrection( tp, true, false ) ) printf( "Test test_do_move_resurrection( _, true, false ) failed.\n" );
            if ( !test_do_move_resurrection( tp, true, true ) ) printf( "Test test_do_move_resurrection( _, true, true ) failed.\n" );

            if ( !test_do_move_teleportation( tp, false ) ) printf( "Test test_do_move_teleportation( _, false ) failed.\n" );
            if ( !test_do_move_teleportation( tp, true ) ) printf( "Test test_do_move_teleportation( _, true ) failed.\n" );

            if ( !test_do_move_teleportation_wave( tp, false ) ) printf( "Test test_do_move_teleportation_wave( _, false ) failed.\n" );
            if ( !test_do_move_teleportation_wave( tp, true ) ) printf( "Test test_do_move_teleportation_wave( _, true ) failed.\n" );

            if ( !test_do_move_trance_journey( tp, false ) ) printf( "Test test_do_move_trance_journey( _, false ) failed.\n" );
            if ( !test_do_move_trance_journey( tp, true ) ) printf( "Test test_do_move_trance_journey( _, true ) failed.\n" );

            printf( "Tests finished.\n" );
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
