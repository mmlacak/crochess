// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
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

#include "cc_parse_utils.h"
#include "cc_parse_msg.h"
#include "cc_parse_move.h"
#include "cc_rules.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "test_defs.h"
#include "tests_parse.h"


bool test_parse( char const * an_str,
                 char const * setup__d,
                 char const * check_setup__d,
                 char const * check_end__d,
                 CcGame ** game__iodr ) {
    if ( !an_str ) return false;
    if ( !setup__d && ( !game__iodr || !*game__iodr ) ) return false;
    // if ( game__iodr && !*game__iodr ) return false;

    CcGame * before_setup = game__iodr ? *game__iodr : NULL;
    CcGame * game__a = cc_game_setup_from_string__new( setup__d, before_setup );
    if ( !game__a ) return false;

    bool result = true;
    uint result_at = 0x0;

    // if ( check_setup__d ) {
    //     CcGame * setup__a = cc_game_setup_from_string__new( check_setup__d, before_setup );
    //     if ( !setup__a ) return false;
    //
    //     result = cc_chessboard_is_equal( game__a->chessboard, setup__a->chessboard ) && result;
    //     if ( !result ) result_at |= 0x1;
    //
    //     cc_game_free_all( &setup__a );
    // }

    CcParsedMove * parsed__a = NULL;
    CcParseMsg * pm__a = NULL;

    if ( ( result = cc_parse_move( an_str, game__a, &parsed__a, &pm__a ) && result ) ) {
        // TODO :: add stringify move notation to libcrochess

        // TODO :: add stringify move properties to libcrochess

        // print_all_moves( parsed__a );
        printf( "Parsed: %s.\n", parsed__a->notation );
    } else {
        result_at |= 0x2;

        CcParseMsg * p = pm__a;
        while ( p ) {
            printf( "%s\n", p->msg );
            p = p->next;
        }
    }

    // if ( check_end__d ) {
    //     CcGame * end__a = cc_game_setup_from_string__new( check_end__d, before_setup );
    //     if ( !end__a ) return false;
    //
    //     result = cc_chessboard_is_equal( game__a->chessboard, end__a->chessboard ) && result;
    //     if ( !result ) result_at |= 0x4;
    //
    //     cc_game_free_all( &end__a );
    // }

    // if ( game__a->moves ) {
    //     CcParsedMove * m = game__a->moves;
    //     CC_FASTFORWARD( m );
    //
    //     result = cc_str_is_equal( an_str, NULL, m->notation, NULL, CC_MAX_LEN_ZERO_TERMINATED ) && result;
    //     if ( !result ) result_at |= 0x8;
    // } else {
    //     result = false;
    //     result_at |= 0x10;
    // }

    if ( !game__iodr ) {
        cc_game_free_all( &game__a );
    } else {
        if ( *game__iodr )
            cc_game_free_all( game__iodr );

        *game__iodr = game__a;
    }

    if ( !result ) {
        printf( "Parse '%s' failed, error(s) 0x%x.\n", an_str, result_at );

        if ( pm__a ) {
            printf( "-----------------------------------------------------------------------\n" );

            CcParseMsg * pm = pm__a;
            while ( pm ) {
                printf( "%s\n", pm->msg );

                pm = pm->next;
            }
        }

        printf( "-----------------------------------------------------------------------\n" );
    }

    cc_parsed_move_free_all( &parsed__a );

    cc_parse_msg_free_all( &pm__a );

    return result;
}

bool tests_parse( int test_number ) {
    if ( ( test_number < TEST_ALL_MOVES ) || ( 48 < test_number ) ) {
        printf( "No such a move test: '%d'.\n", test_number );
        return false;
    }

    bool do_all_tests = ( test_number == TEST_ALL_MOVES );
    bool result = true;

    TestMoveArgs * tma = NULL;

    if ( do_all_tests ) {
        while ( test_move_args_iter( &tma ) ) {
            result = test_parse( tma->an_str, tma->setup__d, tma->check_setup__d, tma->check_end__d, NULL ) && result;
        }
    } else {
        tma = test_move_args_fetch( test_number );
        result = test_parse( tma->an_str, tma->setup__d, tma->check_setup__d, tma->check_end__d, NULL ) && result;
    }

    printf( "Finished: '%d'.\n", result );
    return result;
}

bool tests_skip_disambiguation( int test_number ) {
    bool do_all_tests = ( test_number == TEST_ALL_MOVES );

    char const * ans[] = { "..b0",
                           "z..b1",
                           "10..b2",
                           "z11..b3",
                           "Bz12..b4",
                           "-c0",
                           "y-c1",
                           "10-c2",
                           "y11-c3",
                           "By12-c4",
                           "d0",
                           "xd1",
                           "15d2",
                           "x16d3",
                           "Bx17d4",
                           NULL };

    size_t const ans_size = (size_t)( ( sizeof ans ) / ( sizeof ans[ 0 ] ) ); // Currently: 16.
    size_t index = do_all_tests ? 0 : test_number;

    if ( ( index < 0 ) || ( ans_size <= index ) ) {
        printf( "Test index %zu out of bounds [0, %zu].\n", index, ans_size-1 );
        return false;
    }

    char const * an = ans[ index ];

    if ( do_all_tests ) {
        while ( an ) {
            char const * no_dis = cc_skip_disambiguation( an );
            printf( "%s.\n", no_dis );
            an = ans[ ++index ];
        };
    } else {
        char const * no_dis = cc_skip_disambiguation( an );
        printf( "%s.\n", no_dis );
    }

    return true;
}
