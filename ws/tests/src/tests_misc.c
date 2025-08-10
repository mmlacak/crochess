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

#include "cc_checks.h"

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
                             char * start_str__io,
                             char const * end_str__d,
                             size_t size_dest__d,
                             char const * start_sub_str,
                             char const * end_sub_str__d,
                             size_t max_len__d ) {
    printf( "Before: %s\n", buffer );
    char * io = cc_str_append_into( start_str__io, end_str__d, size_dest__d, start_sub_str, end_sub_str__d, max_len__d );
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
    p = test_str_append_into( x, p, NULL, BUFSIZ, "foo", NULL, 11 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, BUFSIZ, " Hello, World!", NULL, 12 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, BUFSIZ, " bar", NULL, CC_MAX_LEN_BUFFER );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, CC_SIZE_BUFFER, " Goodbye, World!", NULL, CC_MAX_LEN_BUFFER );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, CC_SIZE_BUFFER, " baz", NULL, 11 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, 99, " zaz", NULL, 11 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, 10, " Hello, again!", NULL, 12 );
    printf( "---------------------\n" );
    p = test_str_append_into( x, p, NULL, 12, " Goodbye, again!", NULL, CC_MAX_LEN_BUFFER );
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
    CcPieceTagType piece = CC_PTE_LightCentaur;
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

bool tests_pos_desc_link( void ) {
    CcPosDescLink * pdl = NULL;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_LightBishop, .pos = (CcPos){ .i = 1, .j = 2 } } ) )
        return false;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_LightGrenadier, .pos = (CcPos){ .i = 11, .j = 7 } } ) )
        return false;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_DarkRook_CanCastle, .pos = (CcPos){ .i = 5, .j = 21 } } ) )
        return false;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_DarkPawn_CanRush, .pos = (CcPos){ .i = 17, .j = 22 } } ) )
        return false;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_LightKnight, .pos = (CcPos){ .i = 23, .j = 3 } } ) )
        return false;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_DarkPawn_DelayedPromotion, .pos = (CcPos){ .i = 19, .j = 5 } } ) )
        return false;

    if ( !cc_pos_desc_link_append( &pdl, (CcPosDesc){ .piece = CC_PTE_LightScout, .pos = (CcPos){ .i = 3, .j = 11 } } ) )
        return false;

    printf( "---------------------\n" );
    char * str = cc_pos_desc_link_to_string__new( pdl );
    if ( str ) {
        printf( "%s\n", str ); // Bb3 ,Gl8 ,rf22C,pr23R,Nx4 ,pt6P,Od12
    } else {
        printf( "NULL\n" );
    }
    printf( "---------------------\n" );

    return (bool)( str );
}

bool tests_transparencies( void ) {
    #define PIECES_SIZE (4)

    CcPieceTagType const PIECES[ PIECES_SIZE ] = { CC_PTE_LightBishop, CC_PTE_LightWave, CC_PTE_Monolith, CC_PTE_LightStarchild };

    bool result = true;

    printf( "---------------------\n" );
    printf( "moving -> encounter: block 0, 1, step over 0, 1 --> result.\n" );
    for ( int i = 0; i < PIECES_SIZE; ++i ) {
        printf( ".....................\n" );

        for ( int j = 0; j < PIECES_SIZE; ++j ) {
            CcPieceTagType moving = PIECES[ i ];
            CcPieceTagType encounter = PIECES[ j ];

            bool is_blocking_0 = cc_check_piece_is_blocked( moving, encounter, 0 );
            bool is_blocking_1 = cc_check_piece_is_blocked( moving, encounter, 1 );

            bool is_step_over_0 = cc_check_piece_can_step_over( moving, encounter, 0 );
            bool is_step_over_1 = cc_check_piece_can_step_over( moving, encounter, 1 );

            char moving_chr = cc_piece_as_char( moving );
            char encounter_chr = cc_piece_as_char( encounter );
            bool r = true;

            if ( moving == CC_PTE_LightBishop && encounter == CC_PTE_LightBishop )
                r = is_blocking_0 && is_blocking_1 && !is_step_over_0 && !is_step_over_1;
            else if ( moving == CC_PTE_LightBishop && encounter == CC_PTE_LightWave )
                r = is_blocking_0 && !is_blocking_1 && !is_step_over_0 && is_step_over_1;
            else if ( moving == CC_PTE_LightBishop && encounter == CC_PTE_Monolith )
                r = is_blocking_0 && is_blocking_1 && !is_step_over_0 && !is_step_over_1;
            else if ( moving == CC_PTE_LightBishop && encounter == CC_PTE_LightStarchild )
                r = is_blocking_0 && !is_blocking_1 && !is_step_over_0 && is_step_over_1;

            else if ( moving == CC_PTE_LightWave && encounter == CC_PTE_LightBishop )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;
            else if ( moving == CC_PTE_LightWave && encounter == CC_PTE_LightWave )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;
            else if ( moving == CC_PTE_LightWave && encounter == CC_PTE_Monolith )
                r = is_blocking_0 && is_blocking_1 && !is_step_over_0 && !is_step_over_1;
            else if ( moving == CC_PTE_LightWave && encounter == CC_PTE_LightStarchild )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;

            else if ( moving == CC_PTE_Monolith && encounter == CC_PTE_LightBishop )
                r = is_blocking_0 && is_blocking_1 && !is_step_over_0 && !is_step_over_1;
            else if ( moving == CC_PTE_Monolith && encounter == CC_PTE_LightWave )
                r = is_blocking_0 && is_blocking_1 && !is_step_over_0 && !is_step_over_1;
            else if ( moving == CC_PTE_Monolith && encounter == CC_PTE_Monolith )
                r = is_blocking_0 && is_blocking_1 && !is_step_over_0 && !is_step_over_1;
            else if ( moving == CC_PTE_Monolith && encounter == CC_PTE_LightStarchild )
                r = is_blocking_0 && !is_blocking_1 && !is_step_over_0 && is_step_over_1;

            else if ( moving == CC_PTE_LightStarchild && encounter == CC_PTE_LightBishop )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;
            else if ( moving == CC_PTE_LightStarchild && encounter == CC_PTE_LightWave )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;
            else if ( moving == CC_PTE_LightStarchild && encounter == CC_PTE_Monolith )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;
            else if ( moving == CC_PTE_LightStarchild && encounter == CC_PTE_LightStarchild )
                r = !is_blocking_0 && !is_blocking_1 && is_step_over_0 && is_step_over_1;
            else {
                printf( "Unhandled test case: %c --> %c.\n", moving_chr, encounter_chr );
                result = false;
                break;
            }

            printf( "%c -> %c: %d, %d, %d, %d --> %d.\n", moving_chr, encounter_chr, is_blocking_0, is_blocking_1, is_step_over_0, is_step_over_1, r );

            result = r && result;
        }
    }
    printf( "---------------------\n" );

    // TODO :: FIX
    // ---------------------
    // moving -> encounter: block 0, 1, step over 0, 1 --> result.
    // .....................
    // B -> B: 1, 1, 0, 0 --> 1.
    // B -> W: 1, 0, 0, 0 --> 0. // TODO :: 1, 0, 0, 1
    // B -> M: 1, 1, 0, 0 --> 1.
    // B -> I: 1, 0, 0, 0 --> 0. // TODO :: 1, 0, 0, 1
    // .....................
    // W -> B: 0, 0, 1, 1 --> 1.
    // W -> W: 0, 0, 1, 1 --> 1.
    // W -> M: 1, 1, 0, 0 --> 1.
    // W -> I: 0, 0, 1, 1 --> 1.
    // .....................
    // M -> B: 1, 1, 0, 0 --> 1.
    // M -> W: 1, 1, 0, 0 --> 1.
    // M -> M: 1, 1, 0, 0 --> 1.
    // M -> I: 1, 0, 0, 0 --> 0. // TODO :: 1, 0, 0, 1
    // .....................
    // I -> B: 0, 0, 0, 0 --> 0. // TODO :: 0, 0, 1, 1
    // I -> W: 0, 0, 0, 0 --> 0. // TODO :: 0, 0, 1, 1
    // I -> M: 0, 0, 1, 1 --> 1.
    // I -> I: 0, 0, 1, 1 --> 1.
    // ---------------------

    return result;
}


bool tests_misc( int test_number ) {
    if ( ( test_number < TEST_ALL_MOVES ) || ( 8 < test_number ) ) {
        printf( "No such a misc test: '%d'.\n", test_number );
        return false;
    }

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

    if ( ( test_number == 7 ) || do_all_tests )
        result = tests_pos_desc_link() && result;

    if ( ( test_number == 8 ) || do_all_tests )
        result = tests_transparencies() && result;

    printf( "Finished: '%d'.\n", result );
    return result;
}
