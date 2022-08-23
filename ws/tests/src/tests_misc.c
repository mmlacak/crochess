// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
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
#include "tests_misc.h"


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


bool tests_gcds()
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

    return true;
}

bool tests_pos_steps()
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

    return true;
}

bool tests_str_append_into()
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

    return (bool)( p );
}


bool tests_misc( int test_number )
{
    bool do_all_tests = ( test_number == 0 );
    bool result = true;

    if ( ( test_number == 1 ) || do_all_tests )
        result = tests_gcds() && result;

    if ( ( test_number == 2 ) || do_all_tests )
        result = tests_pos_steps() && result;

    if ( ( test_number == 3 ) || do_all_tests )
        result = tests_str_append_into() && result;

    printf( "Finished: '%d'.\n", result );
    return result;
}
