// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include <stdlib.h>
// #include <stdio.h>
// #include <string.h>
//
// #include "cc_defines.h"
// #include "cc_math.h"
//
// #include "cc_version.h"
// #include "cc_token.h"
// #include "cc_piece.h"
// #include "cc_chessboard.h"
// #include "cc_pos.h"
// #include "cc_game.h"
//
// #include "cc_parse_utils.h"
// #include "cc_parse_msg.h"
// #include "cc_rules.h"
//
// #include "hlp_msgs.h"
// #include "test_msgs.h"

#include "cc_str_utils.h"

#include "test_defs.h"


TestMoveArgs test_move_args( char const * an_str,
                             char const * setup__d,
                             char const * check_setup__d,
                             char const * check_end__d,
                             ull error_code ) {
    TestMoveArgs tma = { .an_str = an_str,
                         .setup__d = setup__d,
                         .check_setup__d = check_setup__d,
                         .check_end__d = check_end__d,
                         .error_code = error_code };
    return tma;
}

static char const * const setup_simple = "O Bd1,Bl1,bd9";
static char const * const end_simple = "o Bh5,Bl1,bd9";

static char const * const setup_cascading = "O Bd1,Bd9,Wh5,Rk2";
static char const * const end_cascading = "o Bh5,Bd9,Wk2,Ro2";

static char const * const setup_tags = "O Ra1C,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11E";
// static char const * const end_tags = "O Bh5,Bd9,Wk2,Ro2";

static char const * const setup_shaman = "O Hc11,pg10,pk9,po8,As7,Wk15";
static char const * const end_shaman = "o Hs7,As9,Wk15";

TestMoveArgs const TEST_MOVE_ARGS_ARRAY[ ] = {
    // simple movement, disambiguation
    TEST_MOVE_ARGS( "Bh5", "O Bd1,Bk1,bd9", NULL, "o Bh5,Bk1,bd9", TEST_OK ),
    TEST_MOVE_ARGS( "Bdh5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "Blh5", setup_simple, NULL, "o Bd1,Bh5,bd9", TEST_OK ),

    // simple movement, steps
    TEST_MOVE_ARGS( "Bd1.e2.f3.g4.h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "Bd..h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "Bd1..h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "Bd1..f3..h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "B..f3..h5", setup_simple, NULL, end_simple, TEST_OK ),

    // simple movement, destination
    TEST_MOVE_ARGS( "Bd-h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "Bd1-h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "Bd1..f3-h5", setup_simple, NULL, end_simple, TEST_OK ),
    TEST_MOVE_ARGS( "B..f3-h5", setup_simple, NULL, end_simple, TEST_OK ),

    // simple movement, failures
    TEST_MOVE_ARGS( "Bh5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Both light Bishops can reach destination field.
    TEST_MOVE_ARGS( "B-h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Both light Bishops can reach destination field.
    TEST_MOVE_ARGS( "B1h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Both light Bishops are on the same rank.
    TEST_MOVE_ARGS( "B1..h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Both light Bishops are on the same rank.
    TEST_MOVE_ARGS( "B1-h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Both light Bishops are on the same rank.
    TEST_MOVE_ARGS( "B..e3..h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Intermediate field not part of any legal path.
    TEST_MOVE_ARGS( "Bbh5", setup_simple, NULL, setup_simple, TEST_FAIL ), // There is no light Bishop at 'b' file at all.
    TEST_MOVE_ARGS( "Bdh5", "O bd1,Bl1,bd9", NULL, "O bd1,Bl1,bd9", TEST_FAIL ), // Light player on the move, but dark Bishop is on a starting field.
    TEST_MOVE_ARGS( "Bd.h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Single dot, instead of double.
    TEST_MOVE_ARGS( "B..e2.h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Double dot, instead of single. Single dot, instead of double.
    TEST_MOVE_ARGS( "B.g4..h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Single dot, instead of double. Double dot, instead of single.
    TEST_MOVE_ARGS( "B.d1..h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Single dot in front of a starting field.
    TEST_MOVE_ARGS( "B..d1..h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Double dot in front of a starting field.
    TEST_MOVE_ARGS( "B-d1..h5", setup_simple, NULL, setup_simple, TEST_FAIL ), // Ending dash in front of a starting field.
    TEST_MOVE_ARGS( "B-f3..h5", setup_simple, NULL, end_simple, TEST_FAIL ), // Ending dash in the middle of a path.

    // simple cascading
    TEST_MOVE_ARGS( "Bh5~Wk2~Ro2", "O Bd1,Bl1,Wh5,Rk2", NULL, "o Bh5,Bl1,Wk2,Ro2", TEST_OK ),
    TEST_MOVE_ARGS( "B1h5~Wk2~Ro2", setup_cascading, NULL, end_cascading, TEST_OK ),
    TEST_MOVE_ARGS( "Bd1.e2.f3.g4.h5~[Wh5.i4.j3.k2]~Rk2.l2.m2.n2.o2", setup_cascading, NULL, end_cascading, TEST_OK ),
    TEST_MOVE_ARGS( "B..f3..h5~[W.i4..k2]~R..m2..o2", setup_cascading, NULL, end_cascading, TEST_OK ),
    TEST_MOVE_ARGS( "B1..h5~[Wh..k2]~Rk..o2", setup_cascading, NULL, end_cascading, TEST_OK ),
    TEST_MOVE_ARGS( "B1-h5~[Wh-k2]~Rk-o2", setup_cascading, NULL, end_cascading, TEST_OK ),

    // simple cascading, failures
    TEST_MOVE_ARGS( "Bdh5~Wk2~Ro2", setup_cascading, NULL, setup_cascading, TEST_FAIL ), // Both light Bishops are on the same file.
    TEST_MOVE_ARGS( "Bh5~Wk2~Ro2", setup_cascading, NULL, setup_cascading, TEST_FAIL ), // Both light Bishops are on the same file.
    TEST_MOVE_ARGS( "B1h5~Wk2~Rr2", setup_cascading, NULL, setup_cascading, TEST_FAIL ), // Rook moved for more than received momentum.

    // simple losing tags
    TEST_MOVE_ARGS( "Rl1", setup_tags, NULL, "o Rl1,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_MOVE_ARGS( "R&&l1", setup_tags, NULL, "o Rl1,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_MOVE_ARGS( "a9", setup_tags, NULL, "o Ra1C,Pa9,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_MOVE_ARGS( "::a9", setup_tags, NULL, "o Ra1C,Pa9,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_MOVE_ARGS( "c23", setup_tags, NULL, "o Ra1C,Pa2R,Pc23,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_MOVE_ARGS( "==c23", setup_tags, NULL, "o Ra1C,Pa2R,Pc23,bc24,Pc7,pd8,Pf11", TEST_OK ),

    // simple losing tags, failures
    TEST_MOVE_ARGS( "R::l1", setup_tags, NULL, setup_tags, TEST_FAIL ), // Wrong losing tag.
    TEST_MOVE_ARGS( "R==l1", setup_tags, NULL, setup_tags, TEST_FAIL ), // Wrong losing tag.

    // simple Shaman
    TEST_MOVE_ARGS( "Hs7~As9", setup_shaman, NULL, end_shaman, TEST_OK ),
    TEST_MOVE_ARGS( "Hk15~Wg23", setup_shaman, NULL, "o Hk15,pg10,pk9,po8,As7,Wg23", TEST_OK ),
    TEST_MOVE_ARGS( "H.g10*P.k9*P.o8*P~As9", setup_shaman, NULL, end_shaman, TEST_OK ),
    TEST_MOVE_ARGS( "H.g10*.k9*.o8*~As9", setup_shaman, NULL, end_shaman, TEST_OK ),

    // TEST_MOVE_ARGS(  ),

    TEST_MOVE_ARGS_INVALID,
};

size_t const TEST_MOVE_ARGS_ARRAY_SIZE = (size_t)( ( sizeof TEST_MOVE_ARGS_ARRAY ) / ( sizeof TEST_MOVE_ARGS_ARRAY[ 0 ] ) ); // Currently: 49.


bool test_move_args_are_equal( TestMoveArgs tma_1, TestMoveArgs tma_2 ) {
    if ( tma_1.an_str != tma_2.an_str ) return false;
    if ( tma_1.setup__d != tma_2.setup__d ) return false;
    if ( tma_1.check_setup__d != tma_2.check_setup__d ) return false;
    if ( tma_1.check_end__d != tma_2.check_end__d ) return false;
    if ( tma_1.error_code != tma_2.error_code ) return false;
    return true;
}

bool test_move_args_are_invalid( TestMoveArgs tma ) {
    return test_move_args_are_equal( tma, TEST_MOVE_ARGS_INVALID_CAST );
}

bool test_move_args_iter( TestMoveArgs ** tma__iod ) {
    if ( !tma__iod ) return false;

    if ( !*tma__iod ) {
        *tma__iod = (TestMoveArgs *)( TEST_MOVE_ARGS_ARRAY );
        return true;
    }

    *tma__iod += 1;
    bool do_reset = false;

    if ( test_move_args_are_invalid( **tma__iod ) )
        do_reset = true;
    else if ( *tma__iod < TEST_MOVE_ARGS_ARRAY )
        do_reset = true;
    else if ( TEST_MOVE_ARGS_ARRAY + TEST_MOVE_ARGS_ARRAY_SIZE <= *tma__iod )
        do_reset = true;

    if ( do_reset ) {
        *tma__iod = NULL;
        return false;
    }

    return true;
}

TestMoveArgs * test_move_args_fetch( size_t index ) {
    if ( TEST_MOVE_ARGS_ARRAY_SIZE < index )
        return NULL;

    TestMoveArgs * tma = (TestMoveArgs *)( TEST_MOVE_ARGS_ARRAY + index );
    return tma;
}
