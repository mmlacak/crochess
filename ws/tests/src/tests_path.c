// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
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

#include "cc_typed_step_defs.h"
#include "cc_path.h"
#include "cc_path_utils.h"
#include "cc_path_tree.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "test_args.h"
#include "tests_move.h"


bool test_path( CcSideEffect side_effect,
                CcPosDesc move_from,
                CcPosDesc ply_from,
                CcTypedStep step,
                char const * setup ) {
    if ( !setup ) return false;

    CcGame * game__a = cc_game_setup_from_string__new( setup, NULL );
    if ( !game__a ) return false;

    // TEMP :: DEBUG :: un/comment (?)
    cc_chessboard_print( game__a->chessboard, true );
    // cc_chessboard_print( game__a->chessboard, false );

    CcPathContext * path_ctx__a = cc_path_context__new( game__a );
    if ( !path_ctx__a ) {
        cc_game_free_all( &game__a );
        return false;
    }

    bool is_first_ply = CC_POS_DESC_IS_EQUAL( move_from, ply_from );

    if ( !cc_path_context_init( path_ctx__a, move_from, is_first_ply ) ) {
        cc_path_context_free_all( &path_ctx__a );
        cc_game_free_all( &game__a );
        return false;
    }

    bool result = true;
    CcPathLink * pl__a = NULL;
    CcPathSideEffectLink * sel__a = NULL;

    if ( !cc_path_segment__new( side_effect, ply_from, step, CC_TYPED_STEP_CAST_INVALID, path_ctx__a, &pl__a, &sel__a ) ) {
        cc_path_side_effect_link_free_all( &sel__a );
        cc_path_link_free_all( &pl__a );
        cc_path_context_free_all( &path_ctx__a );
        cc_game_free_all( &game__a );
        return false;
    };

    if ( pl__a ) {
        char * pl_str__a = cc_path_link_node_to_string__new( 0, pl__a );
        printf( "Path link '%s' test ok.\n", pl_str__a );
        CC_FREE( pl_str__a );
    } else {
        result = false;
        cc_char_16 moving_from_str = CC_CHAR_16_EMPTY;

        if ( cc_pos_desc_to_string( ply_from, &moving_from_str ) ) {
            printf( "Path '%s' failed.\n", moving_from_str ); // TODO :: add typed step
        }
    }

    if ( sel__a ) {
        char * sel_str__a = cc_path_side_effect_link_to_string__new( sel__a );
        printf( "Side-effects: %s.\n", sel_str__a );
        CC_FREE( sel_str__a );
    } else {
        printf( "Side-effects: none.\n" );
    }

    printf( "-----------------------------------------------------------------------\n" );

    cc_path_side_effect_link_free_all( &sel__a );
    cc_path_link_free_all( &pl__a );
    cc_path_context_free_all( &path_ctx__a );
    cc_game_free_all( &game__a );

    return result;
}

bool test_bishop_simple( char const * setup ) {
    CcSideEffect se = cc_side_effect_none();
    CcPosDesc move_from = CC_POS_DESC_COORDS_CAST( 3, 5, CC_PTE_LightBishop );
    CcPosDesc ply_from = CC_POS_DESC_COORDS_CAST( 3, 5, CC_PTE_LightBishop );
    CcTypedStep step = CC_TYPED_STEP_CAST( 1, -1, CC_STE_CaptureOrMovement );
    // char const * setup = "O Bd6";

    return test_path( se, move_from, ply_from, step, setup );
}

bool tests_path( int test_number ) {
    if ( ( test_number < TEST_ALL_MOVES ) || ( 3 < test_number ) ) {
        printf( "No such a path test: '%d'.\n", test_number );
        return false;
    }

    bool do_all_tests = ( test_number == TEST_ALL_MOVES );
    bool result = true;

    if ( ( test_number == 1 ) || do_all_tests )
        result = test_bishop_simple( "O Bd6" ) && result; // Bd6.e5.f4.g3.h2.i1

    if ( ( test_number == 2 ) || do_all_tests )
        result = test_bishop_simple( "O Bd6,Ng3" ) && result;

    if ( ( test_number == 3 ) || do_all_tests )
        result = test_bishop_simple( "O Bd6,ng3" ) && result;

    printf( "Finished: '%d'.\n", result );
    return result;
}
