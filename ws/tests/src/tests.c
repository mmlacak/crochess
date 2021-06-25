// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_version.h"
#include "cc_tokenizer.h"
#include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_step.h"
#include "cc_ply.h"
#include "cc_move.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests_do_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.20:124+20210625.151241"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


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

        char * cmd = cc_next_token_new( buffer, CC_TOKEN_SEPARATORS_WHITEPSACE );
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
            if ( !test_do_move_single_ply( false ) ) printf( "Test test_do_move_single_ply() failed.\n" );
            if ( !test_do_move_cascading_plies( false ) ) printf( "Test test_do_move_cascading_plies() failed.\n" );
            if ( !test_do_move_castling( false ) ) printf( "Test test_do_move_castling() failed.\n" );
            if ( !test_do_move_tag_and_promotion( false ) ) printf( "Test test_do_move_tag_and_promotion() failed.\n" );
            if ( !test_do_move_conversion( false, false ) ) printf( "Test test_do_move_conversion( _, false ) failed.\n" );
            if ( !test_do_move_conversion( false, true ) ) printf( "Test test_do_move_conversion( _, true ) failed.\n" );
            if ( !test_do_move_demotion( false ) ) printf( "Test test_do_move_demotion() failed.\n" );

            if ( !test_do_move_resurrection( false, false, false ) ) printf( "Test test_do_move_resurrection( _, false, false ) failed.\n" );
            if ( !test_do_move_resurrection( false, false, true ) ) printf( "Test test_do_move_resurrection( _, false, true ) failed.\n" );
            if ( !test_do_move_resurrection( false, true, false ) ) printf( "Test test_do_move_resurrection( _, true, false ) failed.\n" );
            if ( !test_do_move_resurrection( false, true, true ) ) printf( "Test test_do_move_resurrection( _, true, true ) failed.\n" );

            if ( !test_do_move_teleportation( false, false ) ) printf( "Test test_do_move_teleportation( _, false ) failed.\n" );
            if ( !test_do_move_teleportation( false, true ) ) printf( "Test test_do_move_teleportation( _, true ) failed.\n" );

            if ( !test_do_move_teleportation_wave( false, false ) ) printf( "Test test_do_move_teleportation_wave( _, false ) failed.\n" );
            if ( !test_do_move_teleportation_wave( false, true ) ) printf( "Test test_do_move_teleportation_wave( _, true ) failed.\n" );

            if ( !test_do_move_trance_journey( false, false ) ) printf( "Test test_do_move_trance_journey( _, false ) failed.\n" );
            if ( !test_do_move_trance_journey( false, true ) ) printf( "Test test_do_move_trance_journey( _, true ) failed.\n" );

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
