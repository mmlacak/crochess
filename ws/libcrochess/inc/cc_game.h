// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>

#include "cc_chessboard.h"
#include "cc_move.h"
#include "cc_do_moves.h"

// DOCS
#define CC_GAME_STATUS_IS_LIGHT_TURN(gse) ( (gse) == CC_GSE_Turn_Light )

// DOCS
#define CC_GAME_STATUS_IS_DARK_TURN(gse) ( (gse) == CC_GSE_Turn_Dark )

// DOCS
#define CC_GAME_STATUS_IS_TURN(gse) ( ( (gse) == CC_GSE_Turn_Light ) || ( (gse) == CC_GSE_Turn_Dark ) )


// DOCS
typedef enum CcGameStatusEnum
{
    CC_GSE_None,
    CC_GSE_Turn_Light,
    CC_GSE_Turn_Dark,
    CC_GSE_Win_Light,
    CC_GSE_Win_Dark,
    CC_GSE_Draw,
} CcGameStatusEnum;

// DOCS
CcGameStatusEnum cc_game_status_next( CcGameStatusEnum const gse,
                                      bool const is_resign,
                                      bool const is_end,
                                      bool const is_won );


// DOCS
typedef struct CcGame
{
    CcGameStatusEnum status;
    CcChessboard * chessboard;
    CcMove * moves;
} CcGame;

// DOCS
CcGame * cc_game_new( CcGameStatusEnum status,
                      CcVariantEnum ve,
                      bool const do_setup );

/**
    Duplicates a given game into a newly allocated one.

    @param game Game to duplicate.

    @return
    A newly allocated game, is successful, `NULL` otherwise.
*/
CcGame * cc_game_duplicate_all_new( CcGame const * const restrict game );

// DOCS
bool cc_game_free_all( CcGame ** const restrict game__f );


/**
    Frees everything from all of given chessboard, moves, plies, steps.

    Function is meant as a convenience, to allow for one-liner to free
    allocated resources (even in the middle of building whole move data
    structure), before returning from a calling test function.

    @param gm__f A game, can be `NULL`.
    @param cb__f A chessboard, can be `NULL`.
    @param moves__f Linked list of moves, can be `NULL`.
    @param plies__f Linked list of plies, can be `NULL`.
    @param steps__f Linked list of steps, can be `NULL`.
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

    return cc_game_move_data_free_all( NULL, &cb__o, &move__o, NULL, NULL, result ); // return cumulative result
    @endcode

    Failure can be enforced (e.g. when allocation fails),
    simply by passing `false` as an argument, e.g.:
    @code{.c}
    if ( !cc_ply_cascade_append_new( ... ) )
        return cc_game_move_data_free_all( NULL, &cb__o, NULL, &plies_0, &steps_2, false ); // false == enforced failure
    @endcode

    @return `true` if cumulatively successful, `false` otherwise.
*/
bool cc_game_move_data_free_all( CcGame ** const restrict gm__f,
                                 CcChessboard ** const restrict cb__f,
                                 CcMove ** const restrict moves__f,
                                 CcPly ** const restrict plies__f,
                                 CcStep ** const restrict steps__f,
                                 bool const cumulative_result );


#endif /* __CC_GAME_H__ */
