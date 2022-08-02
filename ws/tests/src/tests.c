// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc_defines.h"
#include "cc_str_utils.h"
#include "cc_math.h"

#include "cc_version.h"
#include "cc_token.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"
#include "cc_game.h"
#include "cc_rules.h"

#include "cc_parse_msgs.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.101:533+20220802.181742"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


void test_gcd( int x, int y )
{
    printf( "%d ~ %d --> %d\n", x, y, cc_gcd( x, y ) );
}

void test_pos_step( int i1, int j1, int i2, int j2 )
{
    CcPos step = cc_pos_step( cc_pos( i1, j1 ), cc_pos( i2, j2 ) );
    printf( "(%d, %d) ~ (%d, %d) --> (%d, %d)\n", i1, j1, i2, j2, step.i, step.j );
}


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

        char const * token_start = NULL;
        char const * token_end = NULL;
        if ( !cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            continue;

        if ( cc_str_is_equal( token_start, token_end, "q", NULL, BUFSIZ ) ||
             cc_str_is_equal( token_start, token_end, "quit", NULL, BUFSIZ ) )
        {
            break;
        }
        else if ( cc_str_is_equal( token_start, token_end, "v", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "version", NULL, BUFSIZ ) )
        {
            print_version_info( CC_LIB_VERSION, CROCHESS_TESTS_VERSION );
        }
        else if ( cc_str_is_equal( token_start, token_end, "a", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "about", NULL, BUFSIZ ) )
        {
            print_about_info();
        }
        else if ( cc_str_is_equal( token_start, token_end, "b", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "book", NULL, BUFSIZ ) )
        {
        }
        else if ( cc_str_is_equal( token_start, token_end, "t", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "test", NULL, BUFSIZ ) )
        {
        }
        else if ( cc_str_is_equal( token_start, token_end, "p", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "parse", NULL, BUFSIZ ) )
        {
        }
        else if ( cc_str_is_equal( token_start, token_end, "z", NULL, BUFSIZ ) )
        {
            char_8 foo = { 'a', '1', 'b', '2', 'c', '3', 'd', '4' };

            // cc_str_print( foo, NULL, CC_MAX_LEN_CHAR_8, "Before: '%s'.\n", NULL );
            printf( "Before: { %c, %c, %c, %c, %c, %c, %c, %c }.\n",
                    foo[ 0 ],
                    foo[ 1 ],
                    foo[ 2 ],
                    foo[ 3 ],
                    foo[ 4 ],
                    foo[ 5 ],
                    foo[ 6 ],
                    foo[ 7 ] );

            printf( "Before: { %.2x, %.2x, %.2x, %.2x, %.2x, %.2x, %.2x, %.2x }.\n",
                    foo[ 0 ],
                    foo[ 1 ],
                    foo[ 2 ],
                    foo[ 3 ],
                    foo[ 4 ],
                    foo[ 5 ],
                    foo[ 6 ],
                    foo[ 7 ] );

            cc_str_clear( foo, CC_MAX_LEN_CHAR_8 );

            // cc_str_print( foo, NULL, CC_MAX_LEN_CHAR_8, "After: '%s'.\n", NULL );

            printf( "After: { %.2x, %.2x, %.2x, %.2x, %.2x, %.2x, %.2x, %.2x }.\n",
                    foo[ 0 ],
                    foo[ 1 ],
                    foo[ 2 ],
                    foo[ 3 ],
                    foo[ 4 ],
                    foo[ 5 ],
                    foo[ 6 ],
                    foo[ 7 ] );
        }
        else if ( cc_str_is_equal( token_start, token_end, "z0", NULL, BUFSIZ ) )
        {
            test_gcd(  54,  24 );
            test_gcd( -54,  24 );
            test_gcd(  54, -24 );
            test_gcd( -54, -24 );
            printf( "---------------------\n" );

            test_gcd(  24,  54 );
            test_gcd( -24,  54 );
            test_gcd(  24, -54 );
            test_gcd( -24, -54 );
            printf( "---------------------\n" );

            test_gcd(  48,  18 );
            test_gcd( -48,  18 );
            test_gcd(  48, -18 );
            test_gcd( -48, -18 );
            printf( "---------------------\n" );

            test_gcd(  18,  48 );
            test_gcd( -18,  48 );
            test_gcd(  18, -48 );
            test_gcd( -18, -48 );
            printf( "---------------------\n" );

            test_gcd(  48,  0 );
            test_gcd( -48,  0 );
            test_gcd(  48, -0 );
            test_gcd( -48, -0 );
            printf( "---------------------\n" );

            test_gcd( 48, 180 );
            test_gcd( 180, 48 );
            printf( "---------------------\n" );

            test_gcd( 6, 3 );
            test_gcd( 9, 2 );
            test_gcd( 9, 6 );
            test_gcd( 19, 6 );
            test_gcd( 24, 11 );
            printf( "---------------------\n" );
        }
        else if ( cc_str_is_equal( token_start, token_end, "z1", NULL, BUFSIZ ) )
        {
            test_pos_step( 2, 3, 14, 15 );
            test_pos_step( 14, 15, 2, 3 );
            printf( "---------------------\n" );

            test_pos_step( 17, 11, 19, 9 );
            test_pos_step( 19, 9, 17, 11 );
            printf( "---------------------\n" );

            test_pos_step( 22, 3, 10, 11 );
            test_pos_step( 10, 11, 22, 3 );
            printf( "---------------------\n" );

            test_pos_step( 2, 3, 17, 11 );
            test_pos_step( 17, 11, 2, 3 );
            printf( "---------------------\n" );
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
