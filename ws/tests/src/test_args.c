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

#include "test_args.h"


TestArgs test_args( char const * an_str,
                    char const * setup__d,
                    char const * check_setup__d,
                    char const * check_end__d,
                    cc_ull_t error_code ) {
    TestArgs tma = { .an_str = an_str,
                     .setup__d = setup__d,
                     .check_setup__d = check_setup__d,
                     .check_end__d = check_end__d,
                     .error_code = error_code };
    return tma;
}

static char const * const _setup_simple = "O Bd1,Bl1,bd9";
static char const * const _end_simple = "o Bh5,Bl1,bd9";

static char const * const _setup_cascading = "O Bd1,Bd9,Wh5,Rk2";
static char const * const _end_cascading = "o Bh5,Bd9,Wk2,Ro2";

static char const * const _setup_tags = "O Ra1C,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11E";
// static char const * const _end_tags = "O Bh5,Bd9,Wk2,Ro2";

static char const * const _setup_shaman = "O Hc11,pg10,pk9,po8,As7,Wk15";
static char const * const _end_shaman = "o Hs7,As9,Wk15";

static char const * const _setup_castling = "O Rb1C,Wk1,Kn1C,Nr1,Ry1C,rb26C,wi26,kn26C,nt26,ry26C";

TestArgs const TEST_ARGS_ARRAY[ ] = {
    // simple movement, disambiguation
    TEST_ARGS( "Bh5", "O Bd1,Bk1,bd9", NULL, "o Bh5,Bk1,bd9", TEST_OK ),
    TEST_ARGS( "Bdh5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "Blh5", _setup_simple, NULL, "o Bd1,Bh5,bd9", TEST_OK ),

    // simple movement, steps
    TEST_ARGS( "Bd1.e2.f3.g4.h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "Bd..h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "Bd1..h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "Bd1..f3..h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "B..f3..h5", _setup_simple, NULL, _end_simple, TEST_OK ),

    // simple movement, destination
    TEST_ARGS( "Bd-h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "Bd1-h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "Bd1..f3-h5", _setup_simple, NULL, _end_simple, TEST_OK ),
    TEST_ARGS( "B..f3-h5", _setup_simple, NULL, _end_simple, TEST_OK ),

    // simple movement, failures
    TEST_ARGS( "Bh5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Both light Bishops can reach destination field.
    TEST_ARGS( "B-h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Both light Bishops can reach destination field.
    TEST_ARGS( "B1h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Both light Bishops are on the same rank.
    TEST_ARGS( "B1..h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Both light Bishops are on the same rank.
    TEST_ARGS( "B1-h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Both light Bishops are on the same rank.
    TEST_ARGS( "B..e3..h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Intermediate field not part of any legal path.
    TEST_ARGS( "Bbh5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // There is no light Bishop at 'b' file at all.
    TEST_ARGS( "Bdh5", "O bd1,Bl1,bd9", NULL, "O bd1,Bl1,bd9", TEST_FAIL ), // Light player on the move, but dark Bishop is on a starting field.
    TEST_ARGS( "Bd.h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Single dot, instead of double.
    TEST_ARGS( "B..e2.h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Double dot, instead of single. Single dot, instead of double.
    TEST_ARGS( "B.g4..h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Single dot, instead of double. Double dot, instead of single.
    TEST_ARGS( "B.d1..h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Single dot in front of a starting field.
    TEST_ARGS( "B..d1..h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Double dot in front of a starting field.
    TEST_ARGS( "B-d1..h5", _setup_simple, NULL, _setup_simple, TEST_FAIL ), // Ending dash in front of a starting field.
    TEST_ARGS( "B-f3..h5", _setup_simple, NULL, _end_simple, TEST_FAIL ), // Ending dash in the middle of a path.

    // simple cascading
    TEST_ARGS( "Bh5~Wk2~Ro2", "O Bd1,Bl1,Wh5,Rk2", NULL, "o Bh5,Bl1,Wk2,Ro2", TEST_OK ),
    TEST_ARGS( "B1h5~Wk2~Ro2", _setup_cascading, NULL, _end_cascading, TEST_OK ),
    TEST_ARGS( "Bd1.e2.f3.g4.h5~[Wh5.i4.j3.k2]~Rk2.l2.m2.n2.o2", _setup_cascading, NULL, _end_cascading, TEST_OK ),
    TEST_ARGS( "B..f3..h5~[W.i4..k2]~R..m2..o2", _setup_cascading, NULL, _end_cascading, TEST_OK ),
    TEST_ARGS( "B1..h5~[Wh..k2]~Rk..o2", _setup_cascading, NULL, _end_cascading, TEST_OK ),
    TEST_ARGS( "B1-h5~[Wh-k2]~Rk-o2", _setup_cascading, NULL, _end_cascading, TEST_OK ),

    // simple cascading, failures
    TEST_ARGS( "Bdh5~Wk2~Ro2", _setup_cascading, NULL, _setup_cascading, TEST_FAIL ), // Both light Bishops are on the same file.
    TEST_ARGS( "Bh5~Wk2~Ro2", _setup_cascading, NULL, _setup_cascading, TEST_FAIL ), // Both light Bishops are on the same file.
    TEST_ARGS( "B1h5~Wk2~Rr2", _setup_cascading, NULL, _setup_cascading, TEST_FAIL ), // Rook moved for more than received momentum.

    // simple losing tags
    TEST_ARGS( "Rl1", _setup_tags, NULL, "o Rl1,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_ARGS( "R&&l1", _setup_tags, NULL, "o Rl1,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_ARGS( "a9", _setup_tags, NULL, "o Ra1C,Pa9,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_ARGS( "::a9", _setup_tags, NULL, "o Ra1C,Pa9,Pb23P,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_ARGS( "c23", _setup_tags, NULL, "o Ra1C,Pa2R,Pc23,bc24,Pc7,pd8,Pf11", TEST_OK ),
    TEST_ARGS( "==c23", _setup_tags, NULL, "o Ra1C,Pa2R,Pc23,bc24,Pc7,pd8,Pf11", TEST_OK ),

    // simple losing tags, failures
    TEST_ARGS( "R::l1", _setup_tags, NULL, _setup_tags, TEST_FAIL ), // Wrong losing tag.
    TEST_ARGS( "R==l1", _setup_tags, NULL, _setup_tags, TEST_FAIL ), // Wrong losing tag.

    // simple Shaman
    TEST_ARGS( "Hs7~As9", _setup_shaman, NULL, _end_shaman, TEST_OK ),
    TEST_ARGS( "Hk15~Wg23", _setup_shaman, NULL, "o Hk15,pg10,pk9,po8,As7,Wg23", TEST_OK ),

    TEST_ARGS( "H.g10*P.k9*P.o8*P~As9", _setup_shaman, NULL, _end_shaman, TEST_FAIL ), // Shaman at destination captures Pawn, and activates Pyramid.
    TEST_ARGS( "H.g10*.k9*.o8*~As9", _setup_shaman, NULL, _end_shaman, TEST_FAIL ), // Shaman at destination captures Pawn, and activates Pyramid.
    TEST_ARGS( "H.g10*P.k9*P.o8*P.s7~As9", _setup_shaman, NULL, _end_shaman, TEST_OK ),
    TEST_ARGS( "H.g10*.k9*.o8*-s7~As9", _setup_shaman, NULL, _end_shaman, TEST_OK ),

    TEST_ARGS( "H.g10*P.k9*P.o8*P.u6~As9", _setup_shaman, NULL, _end_shaman, TEST_OK ), // TODO :: should fail, but currenty does not check pathing.
    TEST_ARGS( "H.g10*.k9*.o8*-u6~As9", _setup_shaman, NULL, _end_shaman, TEST_OK ), // TODO :: should fail, but currenty does not check pathing.

    // castling
    TEST_ARGS( "Ke1&f", _setup_castling, NULL, "o Ke1,Rf1,Wk1,Nr1,Ry1C,rb26C,wi26,kn26C,nt26,ry26C", TEST_OK ),
    TEST_ARGS( "Ke&f", _setup_castling, NULL, "o Ke1,Rf1,Wk1,Nr1,Ry1C,rb26C,wi26,kn26C,nt26,ry26C", TEST_OK ),
    TEST_ARGS( "Ke1", _setup_castling, NULL, "o Ke1,Rf1,Wk1,Nr1,Ry1C,rb26C,wi26,kn26C,nt26,ry26C", TEST_OK ), // TODO :: default interactions :: if King is moving horizontally, for 2+ fields, it's castling
    TEST_ARGS( "Ke", _setup_castling, NULL, "o Ke1,Rf1,Wk1,Nr1,Ry1C,rb26C,wi26,kn26C,nt26,ry26C", TEST_OK ), // TODO :: default interactions :: if King is moving horizontally, for 2+ fields, it's castling
    TEST_ARGS( "Kj&k", _setup_castling, NULL, _setup_castling, TEST_FAIL ), // Queen-side light Rook would end on position already occupied by light Wave.
    TEST_ARGS( "Ks&t", _setup_castling, NULL, _setup_castling, TEST_FAIL ), // King moving over position already occupied by light Knight.

    // TEST_ARGS(  ),

    TEST_ARGS_INVALID,
};

size_t const TEST_ARGS_ARRAY_SIZE = CC_ARRAY_SIZE( TEST_ARGS_ARRAY ); // Currently: 57.


bool test_args_are_equal( TestArgs tma_1, TestArgs tma_2 ) {
    if ( tma_1.an_str != tma_2.an_str ) return false;
    if ( tma_1.setup__d != tma_2.setup__d ) return false;
    if ( tma_1.check_setup__d != tma_2.check_setup__d ) return false;
    if ( tma_1.check_end__d != tma_2.check_end__d ) return false;
    if ( tma_1.error_code != tma_2.error_code ) return false;
    return true;
}

bool test_args_are_invalid( TestArgs tma ) {
    return test_args_are_equal( tma, TEST_ARGS_INVALID_CAST );
}

bool test_args_iter( TestArgs ** tma__iod ) {
    if ( !tma__iod ) return false;

    if ( !*tma__iod ) {
        *tma__iod = (TestArgs *)( TEST_ARGS_ARRAY );
        return true;
    }

    *tma__iod += 1;
    bool do_reset = false;

    if ( test_args_are_invalid( **tma__iod ) )
        do_reset = true;
    else if ( *tma__iod < TEST_ARGS_ARRAY )
        do_reset = true;
    else if ( TEST_ARGS_ARRAY + TEST_ARGS_ARRAY_SIZE <= *tma__iod )
        do_reset = true;

    if ( do_reset ) {
        *tma__iod = NULL;
        return false;
    }

    return true;
}

TestArgs * test_args_fetch( size_t index ) {
    if ( TEST_ARGS_ARRAY_SIZE <= index ) return NULL;

    TestArgs * tma = (TestArgs *)( TEST_ARGS_ARRAY + index );
    return tma;
}
