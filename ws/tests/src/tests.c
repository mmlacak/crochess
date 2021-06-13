// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#include <stdbool.h>
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
#include "tests_do_move.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.7:111+20210613.141736"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


int main( void )
{
    print_app_intro( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );

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
            if ( !tst_single_ply( false ) ) printf( "Test tst_single_ply() failed.\n" );
            if ( !tst_cascading_plies( false ) ) printf( "Test tst_cascading_plies() failed.\n" );
            if ( !tst_castling( false ) ) printf( "Test tst_castling() failed.\n" );
            if ( !tst_tag_and_promotion( false ) ) printf( "Test tst_tag_and_promotion() failed.\n" );
            if ( !tst_conversion( false, false ) ) printf( "Test tst_conversion( _, false ) failed.\n" );
            if ( !tst_conversion( false, true ) ) printf( "Test tst_conversion( _, true ) failed.\n" );
            if ( !tst_demotion( false ) ) printf( "Test tst_demotion() failed.\n" );
            if ( !tst_resurrection( false, false, false ) ) printf( "Test tst_resurrection( _, false, false ) failed.\n" );
            if ( !tst_resurrection( false, false, true ) ) printf( "Test tst_resurrection( _, false, true ) failed.\n" );
            if ( !tst_resurrection( false, true, false ) ) printf( "Test tst_resurrection( _, true, false ) failed.\n" );
            if ( !tst_resurrection( false, true, true ) ) printf( "Test tst_resurrection( _, true, true ) failed.\n" );

            if ( !tst_teleportation( false, false ) ) printf( "Test tst_teleportation( _, false ) failed.\n" );
            if ( !tst_teleportation( false, true ) ) printf( "Test tst_teleportation( _, true ) failed.\n" );
            if ( !tst_teleportation_wave( false, false ) ) printf( "Test tst_teleportation_wave( _, false ) failed.\n" );
            if ( !tst_teleportation_wave( false, true ) ) printf( "Test tst_teleportation_wave( _, true ) failed.\n" );
            if ( !tst_trance_journey( false, false ) ) printf( "Test tst_trance_journey( _, false ) failed.\n" );
            if ( !tst_trance_journey( false, true ) ) printf( "Test tst_trance_journey( _, true ) failed.\n" );

            printf( "Tests finished.\n" );
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
