// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_DO_MOVES_H__
#define __CC_DO_MOVES_H__

#include "cc_chessboard.h"
#include "cc_move.h"

/**
    @file cc_do_moves.h
    @brief Enumeration, and related functions applying transformations to chessboard.
*/


/**
    Apply move enumeration.
*/
typedef enum CcDoMoveEnum
{
    CC_DME_DoOnlyCurrentMove,
    CC_DME_DoOnlyLastMove,
    CC_DME_DoAllMoves,
} CcDoMoveEnum;


/**
    Function returning linkage of a next ply in a cascade.

    @param ply A ply.

    @return Linkage if successful (and if there is next ply in a cascade), `NULL` otherwise.
*/
CcPlyLinkEnum * cc_get_next_ply_link( CcPly const * const restrict ply );

/**
    Checks if linkage of a next ply in a cascade is teleportation.

    @param ply A ply.

    @note
    Currently, teleporting linkages are:
    - `CC_PLE_Teleportation`
    - `CC_PLE_FailedTeleportation`

    @return `true` if next ply is teleporting, `false` otherwise.
*/
bool cc_is_teleporting_next( CcPly const * const restrict ply );


/**
    Applies step to chessboard.

    @param cb A chessboard to be altered.
    @param move Grand-parent move which owns this step.
    @param ply Parent ply which owns this step.
    @param step A step being applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_step( CcChessboard * const restrict cb,
                 CcMove const * const restrict move,
                 CcPly const * const restrict ply,
                 CcStep const * const restrict step );

/**
    Applies ply to chessboard.

    @param cb A chessboard to be altered.
    @param move Parent move which owns this step.
    @param ply A ply being applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_ply( CcChessboard * const restrict cb,
                CcMove const * const restrict move,
                CcPly const * const restrict ply );

/**
    Applies move(s) to chessboard.

    @param cb A chessboard to be altered.
    @param moves A move(s) being applied.
    @param do_spec Flag, which move(s) are to be applied.

    @return `true` if successful, `false` otherwise.
*/
bool cc_do_moves( CcChessboard * const restrict cb,
                  CcMove const * const restrict moves,
                  CcDoMoveEnum const do_spec );


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

    return cc_move_data_free_all( &cb__o, &move__o, NULL, NULL, result ); // return cumulative result
    @endcode

    Failure can be enforced (e.g. when allocation fails),
    simply by passing `false` as an argument, e.g.:
    @code{.c}
    if ( !cc_ply_cascade_append_new( ... ) )
        return cc_move_data_free_all( &cb__o, NULL, &plies_0, &steps_2, false ); // false == enforced failure
    @endcode

    @return `true` if cumulatively successful, `false` otherwise.
*/
bool cc_move_data_free_all( CcChessboard ** const restrict cb_f,
                            CcMove ** const restrict moves_f,
                            CcPly ** const restrict plies_f,
                            CcStep ** const restrict steps_f,
                            bool const cumulative_result );


#endif /* __CC_DO_MOVES_H__ */
