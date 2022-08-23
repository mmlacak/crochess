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

#include "cc_parse_defs.h"
#include "cc_parse.h"
#include "cc_parse_msgs.h"
#include "cc_rules.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests.h"


char const CROCHESS_TESTS_VERSION[] = "0.0.1.133:565+20220823.100357"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker


void test_gcd( int x, int y )
{
    printf( "%d ~ %d --> %d\n", x, y, cc_gcd( x, y ) );
}

void test_pos_step( int i1, int j1, int i2, int j2 )
{
    CcPos step = cc_pos_step( cc_pos( i1, j1 ), cc_pos( i2, j2 ) );
    printf( "(%d, %d) ~ (%d, %d) --> (%d, %d)\n", i1, j1, i2, j2, step.i, step.j );
}

char * test_str_append_into( char const * restrict buffer,
                             char * restrict str__io,
                             size_t size_dest__d,
                             char const * restrict str,
                             size_t max_len__d )
{
    printf( "Before: %s\n", buffer );
    char * io = cc_str_append_into( str__io, size_dest__d, str, max_len__d );
    printf( "After: %s\n", buffer );
    return io;
}

bool test_move( char const * restrict an_str,
                CcGame * restrict game__io )
{
    if ( !an_str ) return false;

    bool is_game_allocated = false;

    if ( !game__io )
    {
        game__io = cc_game__new( CC_GSE_Turn_Light, CC_VE_One, true );
        if ( !game__io ) return false;

        is_game_allocated = true;
    }

    bool result = false;
    CcParseMsgs * pms__a = NULL;

    if ( ( result = cc_make_move( an_str, (CcGame **)( &game__io ), &pms__a ) ) )
    {
        // TODO :: TEMP :: uncomment (?)
        // cc_chessboard_print( game__io->chessboard, true );
    }
    else
    {
        CcParseMsgs * p = pms__a;
        while ( p )
        {
            printf( "%s\n", p->msg );
            p = p->next;
        }
    }

    cc_parse_msgs_free_all( &pms__a );

    if ( is_game_allocated )
        cc_game_free_all( (CcGame **)( &game__io ) );

    return result;
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

    CcGame * game__a = cc_game__new( CC_GSE_Turn_Light, CC_VE_One, true );

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
        else if ( cc_str_is_equal( token_start, token_end, "d", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "display", NULL, BUFSIZ ) )
        {
            cc_chessboard_print( game__a->chessboard, true );
        }
        else if ( cc_str_is_equal( token_start, token_end, "t", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "tags", NULL, BUFSIZ ) )
        {
            cc_chessboard_print( game__a->chessboard, false );
        }
        else if ( cc_str_is_equal( token_start, token_end, "l", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "list", NULL, BUFSIZ ) )
        {
            cc_moves_print( game__a->moves );
        }
        else if ( cc_str_is_equal( token_start, token_end, "m", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "move", NULL, BUFSIZ ) )
        {
            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            {
                char * an_str = cc_str_copy__new( token_start, token_end, CC_MAX_LEN_ZERO_TERMINATED );
                if ( !an_str )
                    continue;

                CcParseMsgs * pms__a = NULL;

                if ( cc_make_move( an_str, &game__a, &pms__a ) )
                {
                    // TODO :: TEMP :: uncomment (?)
                    // cc_chessboard_print( game__a->chessboard, true );
                }
                else
                {
                    CcParseMsgs * p = pms__a;
                    while ( p )
                    {
                        printf( "%s\n", p->msg );
                        p = p->next;
                    }
                }

                cc_parse_msgs_free_all( &pms__a );
            }
        }
        else if ( cc_str_is_equal( token_start, token_end, "n", NULL, BUFSIZ ) ||
                  cc_str_is_equal( token_start, token_end, "new", NULL, BUFSIZ ) )
        {
            bool is_code = false;
            char_8 code;

            if ( !cc_str_clear( code, CC_SIZE_CHAR_8 ) )
                continue;

            if ( cc_token_iter( buffer, CC_TOKEN_SEPARATORS_WHITESPACE, &token_start, &token_end ) )
            {
                size_t len = cc_str_copy( token_start, token_end, CC_MAX_LEN_VARIANT_SYMBOL + 1, code, CC_SIZE_CHAR_8 );
                if ( len < 1 )
                    continue;

                CcVariantEnum ve = CC_VE_One;
                is_code = cc_variant_from_symbol( code, CC_MAX_LEN_VARIANT_SYMBOL + 1, &ve );

                if ( is_code )
                {
                    if ( !cc_game_free_all( &game__a ) )
                        continue;

                    game__a = cc_game__new( CC_GSE_Turn_Light, ve, true );
                }
                else
                    print_new_code_invalid( code, CC_MAX_LEN_VARIANT_SYMBOL + 1 );
            }

            bool is_empty = cc_str_is_empty( code );
            if ( is_empty || ( !is_empty && is_code ) )
            {
                cc_chessboard_setup( game__a->chessboard );
                cc_chessboard_print( game__a->chessboard, true );
            }
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
        else if ( cc_str_is_equal( token_start, token_end, "z2", NULL, BUFSIZ ) )
        {
            char x[ BUFSIZ ];

            char * p = x;
            *p = '\0';

            printf( "---------------------\n" );
            p = test_str_append_into( x, p, BUFSIZ, "foo", 11 );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, BUFSIZ, " Hello, World!", 12 );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, BUFSIZ, " bar", CC_MAX_LEN_ZERO_TERMINATED );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, CC_SIZE_IGNORE, " Goodbye, World!", CC_MAX_LEN_ZERO_TERMINATED );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, CC_SIZE_IGNORE, " baz", 11 );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, 99, " zaz", 11 );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, 10, " Hello, again!", 12 );
            printf( "---------------------\n" );
            p = test_str_append_into( x, p, 12, " Goodbye, again!", CC_MAX_LEN_ZERO_TERMINATED );
            printf( "---------------------\n" );

        }
        else if ( cc_str_is_equal( token_start, token_end, "x0", NULL, BUFSIZ ) )
        {
            bool result = test_move( "n5", NULL );
            result = test_move( "mn5", NULL ) && result;

            result = test_move( "7n5", NULL ) && result;
            result = test_move( "m7n5", NULL ) && result;

            result = test_move( "::n5", NULL ) && result;
            result = test_move( "::mn5", NULL ) && result;

            result = test_move( "::7n5", NULL ) && result;
            result = test_move( "::m7n5", NULL ) && result;

            printf( "x0: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "x1", NULL, BUFSIZ ) )
        {
            bool result = test_move( "n5*", NULL );
            result = test_move( "mn5*", NULL ) && result;

            result = test_move( "7n5*", NULL ) && result;
            result = test_move( "m7n5*", NULL ) && result;

            result = test_move( "::n5*", NULL ) && result;
            result = test_move( "::mn5*", NULL ) && result;

            result = test_move( "::7n5*", NULL ) && result;
            result = test_move( "::m7n5*", NULL ) && result;

            printf( "x1: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "x2", NULL, BUFSIZ ) )
        {
            bool result = test_move( "::n5*", NULL );
            result = test_move( "B&&n5*N", NULL ) && result;

            result = test_move( "::mn5*", NULL ) && result;
            result = test_move( "B&&mn5*N", NULL ) && result;

            result = test_move( "[::mn5*]", NULL ) && result;
            result = test_move( "[B&&mn5*N]", NULL ) && result;

            printf( "x2: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "x3", NULL, BUFSIZ ) )
        {
            bool result = test_move( "::m..n5*", NULL );
            result = test_move( "B&&m..n5*N", NULL ) && result;

            result = test_move( "::m11..n15*", NULL ) && result;
            result = test_move( "B&&m11..n15*N", NULL ) && result;

            result = test_move( "[::m..n5*]", NULL ) && result;
            result = test_move( "[B&&m..n5*N]", NULL ) && result;

            printf( "x3: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "x4", NULL, BUFSIZ ) )
        {
            bool result = test_move( "::7g11*", NULL );
            result = test_move( "B&&7g11*N", NULL ) && result;

            result = test_move( "::e7g11*", NULL ) && result;
            result = test_move( "B&&e7g11*N", NULL ) && result;

            result = test_move( "[::7g11*]", NULL ) && result;
            result = test_move( "[B&&7g11*N]", NULL ) && result;

            printf( "x4: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "x5", NULL, BUFSIZ ) )
        {
            bool result = test_move( "::3..n5*", NULL );
            result = test_move( "B&&3..n5*N", NULL ) && result;

            result = test_move( "::m11..n15*", NULL ) && result;
            result = test_move( "B&&m11..n15*N", NULL ) && result;

            result = test_move( "[::3..n5*]", NULL ) && result;
            result = test_move( "[B&&3..n5*N]", NULL ) && result;

            printf( "x5: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "x6", NULL, BUFSIZ ) )
        {
            bool result = test_move( "::3.m4<Rx11..n5*H-o7:", NULL );
            result = test_move( "B&&3..m4<Rx11.n5*H-o7>a11", NULL ) && result;

            result = test_move( "::m11.o12<Rx11..n15*H-o17:", NULL ) && result;
            result = test_move( "B&&m11..o12<Rx11.n15*H-o17>a11", NULL ) && result;

            result = test_move( "[::3.m4<Rx11..n5*H-o7:]", NULL ) && result;
            result = test_move( "[B&&3..m4<Rx11.n5*H-o7>a11]", NULL ) && result;

            printf( "x6: %d.\n", result );
        }
        else if ( cc_str_is_equal( token_start, token_end, "y", NULL, BUFSIZ ) )
        {
            bool result = test_move( "::a5~Ab5", NULL );
            result = test_move( "[B&&a5]~[Ab5]", NULL ) && result;

            result = test_move( "B&&5.b9<Rx11..d11-h14", NULL ) && result;
            result = test_move( "[B&&5.b9<Rx11..d11-h14]", NULL ) && result;

            result = test_move( "==a.b9<Rx11..d11-h14~A..g15*H.h16&h..j18<Rx11-k19%%R", NULL ) && result;
            result = test_move( "[B==a.b9<Rx11..d11-h14]~[A..g15*H.h16&h..j18<Rx11-k19%%R]", NULL ) && result;

            // result = test_move( "Ba5~[Wc7]||Nd9", NULL ) && result;
            // result = test_move( "[Ba5]~Wc7@@[Nd9]", NULL ) && result;

            printf( "y: %d.\n", result );
        }
        else
        {
            printf( "Unknown: '%s'.\n", buffer );
            // fflush( stdout );
        }
    }

    cc_game_free_all( &game__a );

    printf( "Bye, have a nice day!\n" );
    // fflush( stdout );

    return 0;
}
