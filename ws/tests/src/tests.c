// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_version.h"
#include "cc_token.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

#include "cc_parse_msg.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.49:481+20220626.020646"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


int main( void )
{
    print_app_intro( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );

// // DEBUG
//     // /* static */ char * foo = "xxx";
//     char * foo = malloc( 4 );
//     strncpy( foo, "xxx", 4 );
//     printf( "%s.\n", foo );

//     // foo = (char *)((int)foo + 1);
//     // printf( "%s.\n", foo );
//     printf( "%s.\n", ++foo );

//     CC_FREE( foo-1 );
//     // CC_FREE( foo );

//     printf( "%s.\n", foo );
// // DEBUG


// // DEBUG
//     printf("1: %d.\n", CC_MAX( 5, 11 ));
//     printf("2: %d.\n", CC_MAX( 11, 5 ));

//     printf("3: %d.\n", CC_MIN( 5, 11 ));
//     printf("4: %d.\n", CC_MIN( 11, 5 ));

//     printf("5: %f.\n", CC_MAX( 5.1, 11.2 ));
//     printf("6: %f.\n", CC_MAX( 11.3, 5.4 ));

//     printf("7: %f.\n", CC_MIN( 5.5, 11.6 ));
//     printf("8: %f.\n", CC_MIN( 11.7, 5.8 ));
// // DEBUG


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

        char const * first__w = NULL;
        char const * end__w = NULL;
        if ( !cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &first__w, &end__w ) )
            continue;

        if ( cc_str_is_equal( first__w, end__w, "q", NULL, BUFSIZ ) ||
             cc_str_is_equal( first__w, end__w, "quit", NULL, BUFSIZ ) )
        {
            break;
        }
        else if ( cc_str_is_equal( first__w, end__w, "v", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "version", NULL, BUFSIZ ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );
        }
        else if ( cc_str_is_equal( first__w, end__w, "a", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "about", NULL, BUFSIZ ) )
        {
            print_about_info();
        }
        else if ( cc_str_is_equal( first__w, end__w, "b", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "book", NULL, BUFSIZ ) )
        {
        }
        else if ( cc_str_is_equal( first__w, end__w, "t", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "test", NULL, BUFSIZ ) )
        {
        }
        else if ( cc_str_is_equal( first__w, end__w, "p", NULL, BUFSIZ ) ||
                  cc_str_is_equal( first__w, end__w, "parse", NULL, BUFSIZ ) )
        {
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }
    }

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
