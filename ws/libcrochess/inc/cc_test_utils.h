// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_TEST_UTILS_H__
#define __CC_TEST_UTILS_H__

#include "cc_chessboard.h"
#include "cc_move.h"

#include "cc_format_moves.h"

/**
    @file cc_test_utils.h
    @brief Convenience test functions.
*/


// TODO :: DOCS
typedef struct TestPrints
{
    bool do_print_chessboard;
    bool do_print_move;
    CcFormatMove format_move;
} TestPrints;

// TODO :: DOCS
TestPrints test_prints( bool do_print_chessboard, bool do_print_move, CcFormatMove format_move );


/**
    Frees everything from all of given chessboard, moves, plies, steps.

    Function is meant as a convenience, to allow for one-liner to free
    allocated resources (even in the middle of building whole move data
    structure), before returning from a calling test function.

    @param cb_f A chessboard, can be `NULL`.
    @param moves_f Linked list of moves, can be `NULL`.
    @param plies_f Linked list of plies, can be `NULL`.
    @param steps_f Linked list of steps, can be `NULL`.
    @param cumulative_result A cumulative result so far.

    @note
    Any of ponters can be `NULL`, in which case they are skipped,
    and don't affect result returned.

    Argument `cumulative_result` is meant for functions returning
    `bool` result, so tests could be writen as:
    @code{.c}
    bool result = true; // cumulative result of tests

    // Assuming test_1 returns bool too.
    // Putting it to first place is mandatory,
    // to ensure it's actually run, even if result is already `false`.
    result = test_1( ... ) && result;

    result = test_2( ... ) && result; // Ditto for test 2.

    return cc_test_util_free_all( &cb__o, &move__o, NULL, NULL, result ); // return cumulative result
    @endcode
    .

    Failure can be enforced (e.g. when allocation fails),
    simply by passing `false` as an argument, e.g.:
    @code{.c}
    if ( !cc_ply_cascade_append_new( ... ) )
        return cc_test_util_free_all( &cb__o, NULL, &plies_0, &steps_2, false ); // false == enforced failure
    @endcode
    .

    @return `true` if cumulatively successful, `false` otherwise.
*/
bool cc_test_util_free_all( CcChessboard ** const cb_f,
                            CcMove ** const moves_f,
                            CcPly ** const plies_f,
                            CcStep ** const steps_f,
                            bool cumulative_result );


#endif /* __CC_TEST_UTILS_H__ */
