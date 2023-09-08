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
#include "cc_parse_utils.h"
#include "cc_parse_msg.h"
#include "cc_rules.h"

#include "hlp_msgs.h"
#include "test_msgs.h"
#include "tests_move.h"


bool test_move( char const * restrict an_str,
                char const * restrict setup__d,
                char const * restrict check_setup__d,
                char const * restrict check_end__d,
                CcGame ** restrict game__iodr ) {
    if ( !an_str ) return false;
    if ( !setup__d && ( !game__iodr || !*game__iodr ) ) return false;
    // if ( game__iodr && !*game__iodr ) return false;

    CcGame * before_setup = game__iodr ? *game__iodr : NULL;
    CcGame * game__a = cc_game_setup_from_string__new( setup__d, before_setup );
    if ( !game__a ) return false;

    bool result = true;
    unsigned int result_at = 0x0;
    CcParseMsg * pm__a = NULL;

    // TODO :: TEMP :: un/comment (?)
    cc_chessboard_print( game__a->chessboard, true );
    cc_chessboard_print( game__a->chessboard, false );

    if ( check_setup__d ) {
        CcGame * setup__a = cc_game_setup_from_string__new( check_setup__d, before_setup );
        if ( !setup__a ) return false;

        result = cc_chessboard_is_equal( game__a->chessboard, setup__a->chessboard ) && result;
        if ( !result ) result_at |= 0x1;

        cc_game_free_all( &setup__a );
    }

// TODO
    // if ( ( result = cc_apply_move( an_str, game__a, &pm__a ) && result ) )
    // {
    //     // TODO :: TEMP :: un/comment (?)
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
// TODO

    if ( check_end__d ) {
        CcGame * end__a = cc_game_setup_from_string__new( check_end__d, before_setup );
        if ( !end__a ) return false;

        result = cc_chessboard_is_equal( game__a->chessboard, end__a->chessboard ) && result;
        if ( !result ) result_at |= 0x4;

        cc_game_free_all( &end__a );
    }

    if ( game__a->moves ) {
        CcMove * m = game__a->moves;
        while ( m->next ) m = m->next;

        result = cc_str_is_equal( an_str, NULL, m->notation, NULL, CC_MAX_LEN_ZERO_TERMINATED ) && result;
        if ( !result ) result_at |= 0x8; }
    else {
        result = false;
        result_at |= 0x10;
    }

    cc_parse_msg_free_all( &pm__a );

    if ( !game__iodr )
        cc_game_free_all( &game__a );
    else
    {
        if ( *game__iodr )
            cc_game_free_all( game__iodr );

        *game__iodr = game__a;
    }

    if ( !result ) {
        printf( "Move '%s' failed, error(s) 0x%x.\n", an_str, result_at );
        printf( "-----------------------------------------------------------------------\n" );
    }

    return result;
}


bool tests_move( int test_number ) {
    if ( ( test_number < 0 ) || ( 48 < test_number ) ) {
        printf( "No such a move test: '%d'.\n", test_number );
        return false;
    }

    bool do_all_tests = ( test_number == 0 );
    bool result = true;

    //
    // simple movement, disambiguation

    char const * const setup_simple = "O Bd1,Bl1,bd9";
    char const * const end_simple = "o Bh5,Bl1,bd9";

    if ( ( test_number == 1 ) || do_all_tests )
        result = test_move( "Bh5", "O Bd1,Bk1,bd9", NULL, "o Bh5,Bk1,bd9", NULL ) && result;

    if ( ( test_number == 2 ) || do_all_tests )
        result = test_move( "Bdh5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 3 ) || do_all_tests )
        result = test_move( "Blh5", setup_simple, NULL, "o Bd1,Bh5,bd9", NULL ) && result;

    //
    // simple movement, steps

    if ( ( test_number == 4 ) || do_all_tests )
        result = test_move( "Bd1.e2.f3.g4.h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 5 ) || do_all_tests )
        result = test_move( "Bd..h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 6 ) || do_all_tests )
        result = test_move( "Bd1..h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 7 ) || do_all_tests )
        result = test_move( "Bd1..f3..h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 8 ) || do_all_tests )
        result = test_move( "B..f3..h5", setup_simple, NULL, end_simple, NULL ) && result;

    //
    // simple movement, destination

    if ( ( test_number == 9 ) || do_all_tests )
        result = test_move( "Bd-h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 10 ) || do_all_tests )
        result = test_move( "Bd1-h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 11 ) || do_all_tests )
        result = test_move( "Bd1..f3-h5", setup_simple, NULL, end_simple, NULL ) && result;

    if ( ( test_number == 12 ) || do_all_tests )
        result = test_move( "B..f3-h5", setup_simple, NULL, end_simple, NULL ) && result;

    //
    // simple movement, failures

    if ( ( test_number == 13 ) || do_all_tests )
        // Both light Bishops can reach destination field.
        result = !test_move( "Bh5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 14 ) || do_all_tests )
        // Both light Bishops can reach destination field.
        result = !test_move( "B-h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 15 ) || do_all_tests )
        // Both light Bishops are on the same rank.
        result = !test_move( "B1h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 16 ) || do_all_tests )
        // Both light Bishops are on the same rank.
        result = !test_move( "B1..h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 17 ) || do_all_tests )
        // Both light Bishops are on the same rank.
        result = !test_move( "B1-h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 18 ) || do_all_tests )
        // Intermediate field not part of any legal path.
        result = !test_move( "B..e3..h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 19 ) || do_all_tests )
        // There is no light Bishop at 'b' file at all.
        result = !test_move( "Bbh5", setup_simple, NULL, setup_simple, NULL ) && result; // TODO :: wrong error message

    if ( ( test_number == 20 ) || do_all_tests )
        // Light player on the move, but dark Bishop is on a starting field.
        result = !test_move( "Bdh5", "O bd1,Bl1,bd9", NULL, "O bd1,Bl1,bd9", NULL ) && result; // TODO :: wrong error message

    if ( ( test_number == 21 ) || do_all_tests )
        // Single dot, instead of double.
        result = !test_move( "Bd.h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 22 ) || do_all_tests )
        // Double dot, instead of single. Single dot, instead of double.
        result = !test_move( "B..e2.h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 23 ) || do_all_tests )
        // Single dot, instead of double. Double dot, instead of single.
        result = !test_move( "B.g4..h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 24 ) || do_all_tests )
        // Single dot in front of a starting field.
        result = !test_move( "B.d1..h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 25 ) || do_all_tests )
        // Double dot in front of a starting field.
        result = !test_move( "B..d1..h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 26 ) || do_all_tests )
        // Ending dash in front of a starting field.
        result = !test_move( "B-d1..h5", setup_simple, NULL, setup_simple, NULL ) && result;

    if ( ( test_number == 27 ) || do_all_tests )
        // Ending dash in the middle of a path.
        result = !test_move( "B-f3..h5", setup_simple, NULL, end_simple, NULL ) && result;

    //
    // simple cascading

    char const * const setup_cascading = "O Bd1,Bd9,Wh5,Rk2";
    char const * const end_cascading = "o Bh5,Bd9,Wk2,Ro2";

    if ( ( test_number == 28 ) || do_all_tests )
        result = test_move( "Bh5~Wk2~Ro2", "O Bd1,Bl1,Wh5,Rk2", NULL, "o Bh5,Bl1,Wk2,Ro2", NULL ) && result;

    if ( ( test_number == 29 ) || do_all_tests )
        result = test_move( "B1h5~Wk2~Ro2", setup_cascading, NULL, end_cascading, NULL ) && result;

    if ( ( test_number == 30 ) || do_all_tests )
        result = test_move( "Bd1.e2.f3.g4.h5~[Wh5.i4.j3.k2]~Rk2.l2.m2.n2.o2", setup_cascading, NULL, end_cascading, NULL ) && result;

    if ( ( test_number == 31 ) || do_all_tests )
        result = test_move( "B..f3..h5~[W.i4..k2]~R..m2..o2", setup_cascading, NULL, end_cascading, NULL ) && result;

    if ( ( test_number == 32 ) || do_all_tests )
        result = test_move( "B1..h5~[Wh..k2]~Rk..o2", setup_cascading, NULL, end_cascading, NULL ) && result;

    if ( ( test_number == 33 ) || do_all_tests )
        result = test_move( "B1-h5~[Wh-k2]~Rk-o2", setup_cascading, NULL, end_cascading, NULL ) && result;

    //
    // simple cascading, failures

    if ( ( test_number == 34 ) || do_all_tests )
        // Both light Bishops are on the same file.
        result = !test_move( "Bdh5~Wk2~Ro2", setup_cascading, NULL, setup_cascading, NULL ) && result;

    if ( ( test_number == 35 ) || do_all_tests )
        // Both light Bishops are on the same file.
        result = !test_move( "Bh5~Wk2~Ro2", setup_cascading, NULL, setup_cascading, NULL ) && result;

    if ( ( test_number == 36 ) || do_all_tests )
        // Rook moved for more than received momentum.
        result = !test_move( "B1h5~Wk2~Rr2", setup_cascading, NULL, setup_cascading, NULL ) && result;

    //
    // simple losing tags

    char const * const setup_tags = "O Ra1C,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11E";
    // char const * const end_tags = "O Bh5,Bd9,Wk2,Ro2";

    if ( ( test_number == 37 ) || do_all_tests )
        result = test_move( "Rl1", setup_tags, NULL, "o Rl1,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11", NULL ) && result;

    if ( ( test_number == 38 ) || do_all_tests )
        result = test_move( "R&&l1", setup_tags, NULL, "o Rl1,Pa2R,Pb23P,bc24,Pc7,pd8,Pf11", NULL ) && result;

    if ( ( test_number == 39 ) || do_all_tests )
        result = test_move( "a9", setup_tags, NULL, "o Ra1C,Pa9,Pb23P,bc24,Pc7,pd8,Pf11", NULL ) && result;

    if ( ( test_number == 40 ) || do_all_tests )
        result = test_move( "::a9", setup_tags, NULL, "o Ra1C,Pa9,Pb23P,bc24,Pc7,pd8,Pf11", NULL ) && result;

    if ( ( test_number == 41 ) || do_all_tests )
        result = test_move( "c23", setup_tags, NULL, "o Ra1C,Pa2R,Pc23,bc24,Pc7,pd8,Pf11", NULL ) && result;

    if ( ( test_number == 42 ) || do_all_tests )
        result = test_move( "==c23", setup_tags, NULL, "o Ra1C,Pa2R,Pc23,bc24,Pc7,pd8,Pf11", NULL ) && result;

    //
    // simple losing tags, failures

    if ( ( test_number == 43 ) || do_all_tests )
        // Wrong losing tag.
        result = !test_move( "R::l1", setup_tags, NULL, setup_tags, NULL ) && result;

    if ( ( test_number == 44 ) || do_all_tests )
        // Wrong losing tag.
        result = !test_move( "R==l1", setup_tags, NULL, setup_tags, NULL ) && result;

    //
    // simple Shaman

    char const * const setup_shaman = "O Hc11,pg10,pk9,po8,As7,Wk15";
    char const * const end_shaman = "o Hs7,As9,Wk15";

    if ( ( test_number == 45 ) || do_all_tests )
        result = test_move( "Hs7~As9", setup_shaman, NULL, end_shaman, NULL ) && result;

    if ( ( test_number == 46 ) || do_all_tests )
        result = test_move( "Hk15~Wg23", setup_shaman, NULL, "o Hk15,pg10,pk9,po8,As7,Wg23", NULL ) && result;

    // Not supported yet.
    //
    if ( ( test_number == 47 ) || do_all_tests )
        result = test_move( "H.g10*P.k9*P.o8*P~As9", setup_shaman, NULL, end_shaman, NULL ) && result;

    if ( ( test_number == 48 ) || do_all_tests )
        result = test_move( "H.g10*.k9*.o8*~As9", setup_shaman, NULL, end_shaman, NULL ) && result;



    // if ( ( test_number == 2 ) || do_all_tests )
    //     result = test_move( "mn5", "O Pm4R,Pn4R,pn5", NULL, "o Pm5,Pn4R", NULL ) && result;

    // if ( ( test_number == 3 ) || do_all_tests )
    //     result = test_move( "7n5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 4 ) || do_all_tests )
    //     result = test_move( "m7n5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 5 ) || do_all_tests )
    //     result = test_move( "::n5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 6 ) || do_all_tests )
    //     result = test_move( "::mn5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 7 ) || do_all_tests )
    //     result = test_move( "::7n5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 8 ) || do_all_tests )
    //     result = test_move( "::m7n5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 9 ) || do_all_tests )
    //     result = test_move( "n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 10 ) || do_all_tests )
    //     result = test_move( "mn5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 11 ) || do_all_tests )
    //     result = test_move( "7n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 12 ) || do_all_tests )
    //     result = test_move( "m7n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 13 ) || do_all_tests )
    //     result = test_move( "::n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 14 ) || do_all_tests )
    //     result = test_move( "::mn5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 15 ) || do_all_tests )
    //     result = test_move( "::7n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 16 ) || do_all_tests )
    //     result = test_move( "::m7n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 17 ) || do_all_tests )
    //     result = test_move( "B&&n5*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 18 ) || do_all_tests )
    //     result = test_move( "B&&mn5*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 19 ) || do_all_tests )
    //     result = test_move( "[::mn5*]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 20 ) || do_all_tests )
    //     result = test_move( "[B&&mn5*N]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 21 ) || do_all_tests )
    //     result = test_move( "::m..n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 22 ) || do_all_tests )
    //     result = test_move( "B&&m..n5*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 23 ) || do_all_tests )
    //     result = test_move( "::m11..n15*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 24 ) || do_all_tests )
    //     result = test_move( "B&&m11..n15*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 25 ) || do_all_tests )
    //     result = test_move( "[::m..n5*]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 26 ) || do_all_tests )
    //     result = test_move( "[B&&m..n5*N]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 27 ) || do_all_tests )
    //     result = test_move( "::7g11*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 28 ) || do_all_tests )
    //     result = test_move( "B&&7g11*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 29 ) || do_all_tests )
    //     result = test_move( "::e7g11*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 30 ) || do_all_tests )
    //     result = test_move( "B&&e7g11*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 31 ) || do_all_tests )
    //     result = test_move( "[::7g11*]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 32 ) || do_all_tests )
    //     result = test_move( "[B&&7g11*N]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 33 ) || do_all_tests )
    //     result = test_move( "::3..n5*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 34 ) || do_all_tests )
    //     result = test_move( "B&&3..n5*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 35 ) || do_all_tests )
    //     result = test_move( "::m11..n15*", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 36 ) || do_all_tests )
    //     result = test_move( "B&&m11..n15*N", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 37 ) || do_all_tests )
    //     result = test_move( "[::3..n5*]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 38 ) || do_all_tests )
    //     result = test_move( "[B&&3..n5*N]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 39 ) || do_all_tests )
    //     result = test_move( "::3.m4<Rx11..n5*H-o7:", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 40 ) || do_all_tests )
    //     result = test_move( "B&&3..m4<Rx11.n5*H-o7>a11", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 41 ) || do_all_tests )
    //     result = test_move( "::m11.o12<Rx11..n15*H-o17:", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 42 ) || do_all_tests )
    //     result = test_move( "B&&m11..o12<Rx11.n15*H-o17>a11", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 43 ) || do_all_tests )
    //     result = test_move( "[::3.m4<Rx11..n5*H-o7:]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 44 ) || do_all_tests )
    //     result = test_move( "[B&&3..m4<Rx11.n5*H-o7>a11]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 45 ) || do_all_tests )
    //     result = test_move( "::a5~Ab5", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 46 ) || do_all_tests )
    //     result = test_move( "[B&&a5]~[Ab5]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 47 ) || do_all_tests )
    //     result = test_move( "B&&5.b9<Rx11..d11-h14", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 48 ) || do_all_tests )
    //     result = test_move( "[B&&5.b9<Rx11..d11-h14]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 49 ) || do_all_tests )
    //     result = test_move( "==a.b9<Rx11..d11-h14~A..g15*H.h16&h..j18<Rx11-k19%%R", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 50 ) || do_all_tests )
    //     result = test_move( "[B==a.b9<Rx11..d11-h14]~[A..g15*H.h16&h..j18<Rx11-k19%%R]", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 51 ) || do_all_tests )
    //     result = test_move( "Ba5~[Wc7]||Nd9", NULL, NULL, NULL, NULL ) && result;

    // if ( ( test_number == 52 ) || do_all_tests )
    //     result = test_move( "[Ba5]~Wc7@@[Nd9]", NULL, NULL, NULL, NULL ) && result;

    printf( "Finished: '%d'.\n", result );
    return result;
}
