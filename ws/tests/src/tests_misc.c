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
#include "cc_typed_step_defs.h"
#include "cc_pos_utils.h"
#include "cc_game.h"

#include "cc_parse_utils.h"
#include "cc_parse_msg.h"
#include "cc_rules.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "test_args.h"
#include "tests_misc.h"


void test_gcd( int x, int y ) {
    printf( "%d ~ %d --> %d\n", x, y, cc_gcd( x, y ) );
}

void test_pos_step( int i1, int j1, int i2, int j2 ) {
    CcPos step = cc_pos_calc_step( CC_POS_CAST( i1, j1 ), CC_POS_CAST( i2, j2 ) );
    printf( "(%d, %d) ~ (%d, %d) --> (%d, %d)\n", i1, j1, i2, j2, step.i, step.j );
}

char * test_str_append_into( char const * buffer,
                             char * str__io,
                             size_t size_dest__d,
                             char const * str,
                             size_t max_len__d ) {
    printf( "Before: %s\n", buffer );
    char * io = cc_str_append_into( str__io, size_dest__d, str, max_len__d );
    printf( "After: %s\n", buffer );
    return io;
}


bool tests_gcds( void ) {
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

bool tests_pos_steps( void ) {
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

bool tests_str_append_into( void ) {
    char x[ BUFSIZ ];

    char * p = x;
    *p = '\0';

    printf( "---------------------\n" );
    p = test_str_append_into( x, p, BUFSIZ, "foo", 11 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, BUFSIZ, " Hello, World!", 12 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, BUFSIZ, " bar", CC_MAX_LEN_BUFFER );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, CC_SIZE_BUFFER, " Goodbye, World!", CC_MAX_LEN_BUFFER );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, CC_SIZE_BUFFER, " baz", 11 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, 99, " zaz", 11 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, 10, " Hello, again!", 12 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, 12, " Goodbye, again!", CC_MAX_LEN_BUFFER );
    printf( "---------------------\n" );

    return (bool)( p );
}

bool tests_str_len( void ) {
    char const * hello = "Hello, buggy world!";
    char const * start = hello + 7;
    char const * end = start + 5;
    bool result = true;

    printf( "---------------------\n" );
    result = ( strlen( hello ) == cc_str_len( hello, NULL, CC_MAX_LEN_BUFFER ) ) && result;
    printf( "Length hello: %lu == %zu --> %d.\n", strlen( hello ), cc_str_len( hello, NULL, CC_MAX_LEN_BUFFER ), result );

    printf( "---------------------\n" );
    result = ( strlen( start ) == cc_str_len( start, NULL, CC_MAX_LEN_BUFFER ) ) && result;
    printf( "Length start: %lu == %zu --> %d.\n", strlen( start ), cc_str_len( start, NULL, CC_MAX_LEN_BUFFER ), result );

    printf( "---------------------\n" );
    result = ( strlen( end ) == cc_str_len( end, NULL, CC_MAX_LEN_BUFFER ) ) && result;
    printf( "Length end: %lu == %zu --> %d.\n", strlen( end ), cc_str_len( end, NULL, CC_MAX_LEN_BUFFER ), result );

    printf( "---------------------\n" );
    result = ( cc_str_len( start, NULL, CC_MAX_LEN_BUFFER ) == 12 ) && result;
    printf( "Len start: %zu --> %d.\n", cc_str_len( start, NULL, CC_MAX_LEN_BUFFER ), result );
    result = ( cc_str_len( start, end, CC_MAX_LEN_BUFFER ) == 5 ) && result;
    printf( "Len start, end: %zu --> %d.\n", cc_str_len( start, end, CC_MAX_LEN_BUFFER ), result );

    printf( "---------------------\n" );
    result = ( cc_str_len( start, NULL, 99 ) == 12 ) && result;
    printf( "Len start, , 99: %zu --> %d.\n", cc_str_len( start, NULL, 99 ), result );
    result = ( cc_str_len( start, end, 99 ) == 5 ) && result;
    printf( "Len start, end, 99: %zu --> %d.\n", cc_str_len( start, end, 99 ), result );

    printf( "---------------------\n" );
    result = ( cc_str_len( start, NULL, 3 ) == 3 ) && result;
    printf( "Len start, , 3: %zu --> %d.\n", cc_str_len( start, NULL, 3 ), result );
    result = ( cc_str_len( start, end, 3 ) == 3 ) && result;
    printf( "Len start, end, 3: %zu --> %d.\n", cc_str_len( start, end, 3 ), result );

    printf( "---------------------\n" );

    return result;
}

bool tests_iter_monolith_steps( void ) {
    bool result = true;
    CcTypedStep step = CC_TYPED_STEP_CAST_INVALID;
    cc_uint_t step_index = 3;

    printf( "---------------------\n" );
    while ( cc_iter_monolith_steps( step_index, &step ) ) {
        printf( "Step: (%d, %d).\n", step.step.i, step.step.j );
    }
    printf( "---------------------\n" );

    return result;
}

bool tests_iter_piece_steps( void ) {
    bool result = true;
    CcPieceType piece = CC_PE_LightCentaur;
    bool sideways_pawns = true;
    bool short_step = false;
    CcSerpentDiagonalEnum serpent_diagonal = CC_SDE_BothDiagonals;
    CcStepTypeEnum filter = CC_STE_None;
    CcTypedStep const * step = NULL;

    printf( "---------------------\n" );
    while ( cc_iter_piece_steps( piece, sideways_pawns, short_step, serpent_diagonal, filter, &step ) ) {
        printf( "Step: (%d, %d; %d).\n", step->step.i, step->step.j, step->type );
    }
    printf( "---------------------\n" );

    return result;
}


bool tests_misc( int test_number ) {
    if ( ( test_number < 0 ) || ( 6 < test_number ) ) {
        printf( "No such a misc test: '%d'.\n", test_number );
        return false; }

    bool do_all_tests = ( test_number == TEST_ALL_MOVES );
    bool result = true;

    if ( ( test_number == 1 ) || do_all_tests )
        result = tests_gcds() && result;

    if ( ( test_number == 2 ) || do_all_tests )
        result = tests_pos_steps() && result;

    if ( ( test_number == 3 ) || do_all_tests )
        result = tests_str_append_into() && result;

    if ( ( test_number == 4 ) || do_all_tests )
        result = tests_str_len() && result;

    if ( ( test_number == 5 ) || do_all_tests )
        result = tests_iter_monolith_steps() && result;

    if ( ( test_number == 6 ) || do_all_tests )
        result = tests_iter_piece_steps() && result;

    printf( "Finished: '%d'.\n", result );
    return result;
}
