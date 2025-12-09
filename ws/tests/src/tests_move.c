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

#include "cc_parse_utils.h"
#include "cc_parse_msg.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "test_args.h"
#include "tests_move.h"


cc_ull_t test_move( char const * an_str,
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

    cc_ull_t result = 0x0;
    CcParseMsg * pm__a = NULL;

    // TEMP :: DEBUG :: un/comment (?)
    cc_chessboard_print( game__a->chessboard, true );
    cc_chessboard_print( game__a->chessboard, false );

    if ( check_setup__d ) {
        CcGame * setup__a = cc_game_setup_from_string__new( check_setup__d, before_setup );
        if ( !setup__a ) return false;

        if ( !cc_chessboard_is_equal( game__a->chessboard, setup__a->chessboard ) )
            result |= 0x1;

        cc_game_free_all( &setup__a );
    }

// todo
    // if ( ( result = cc_apply_move( an_str, game__a, &pm__a ) && result ) )
    // {
    //     // TEMP :: DEBUG :: un/comment (?)
    //     cc_chessboard_print( game__a->chessboard, true );
    //     cc_chessboard_print( game__a->chessboard, false );
    // }
    // else
    // {
    //     result_at |= 0x2;

    //     CcParseMsg * p = pm__a;
    //     while ( p )
    //     {
    //         printf( "%s\n", p->msg );
    //         p = p->next;
    //     }
    // }
// todo

    if ( check_end__d ) {
        CcGame * end__a = cc_game_setup_from_string__new( check_end__d, before_setup );
        if ( !end__a ) return false;

        if ( !cc_chessboard_is_equal( game__a->chessboard, end__a->chessboard ) )
            result |= 0x4;

        cc_game_free_all( &end__a );
    }

    if ( game__a->moves ) {
        CcMove * m = game__a->moves;
        CC_FASTFORWARD( m );

        if ( !cc_str_is_equal( an_str, NULL, m->notation, NULL, CC_MAX_LEN_BUFFER ) )
            result |= 0x8;
    } else
        result |= 0x10;

    if ( !game__iodr )
        cc_game_free_all( &game__a );
    else
    {
        if ( *game__iodr )
            cc_game_free_all( game__iodr );

        *game__iodr = game__a;
    }

    if ( result != 0x0 ) {
        printf( "Move '%s' failed, error(s) 0x%llx.\n", an_str, result );
    }

    printf( "-----------------------------------------------------------------------\n" );

    cc_parse_msg_free_all( &pm__a );

    return result;
}


bool tests_move( int test_number ) {
    bool do_all_tests = ( test_number == TEST_ALL_MOVES );

    if ( !do_all_tests ) {
        if ( ( test_number < TEST_ALL_MOVES ) || ( TEST_ARGS_ARRAY_SIZE <= (size_t)test_number ) ) {
            printf( "No such a move test: '%d'.\n", test_number );
            return false;
        }
    }

    bool result = true;
    TestArgs * tma = NULL;

    printf( "=======================================================================\n" );

    if ( do_all_tests ) {
        while ( test_args_iter( &tma ) ) {
            result = ( test_move( tma->an_str, tma->setup__d, tma->check_setup__d, tma->check_end__d, NULL ) == tma->error_code ) && result;
        }
    } else {
        tma = test_args_fetch( test_number );
        result = ( test_move( tma->an_str, tma->setup__d, tma->check_setup__d, tma->check_end__d, NULL ) == tma->error_code ) && result;
    }

    size_t count = do_all_tests ? TEST_ARGS_ARRAY_SIZE - 1 : 1;
    printf( "Finished %zu test(s): '%d'.\n", count, result );
    printf( "=======================================================================\n" );

    return result;
}
