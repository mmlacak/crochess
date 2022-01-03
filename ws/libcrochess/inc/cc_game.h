// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GAME_H__
#define __CC_GAME_H__

#include <stdbool.h>

#include "cc_chessboard.h"
#include "cc_move.h"
#include "cc_do_moves.h"

/**
    @file cc_game.h
    @brief Storage for game status, current chessboard and all performed moves so far.
*/


/**
    Macro to inline check if it's light player's turn.

    @param gse A game status enumeration.

    @return `1` if it's light player's turn, `0` otherwise.
*/
#define CC_GAME_STATUS_IS_LIGHT_TURN(gse) ( (gse) == CC_GSE_Turn_Light )

/**
    Macro to inline check if it's dark player's turn.

    @param gse A game status enumeration.

    @return `1` if it's dark player's turn, `0` otherwise.
*/
#define CC_GAME_STATUS_IS_DARK_TURN(gse) ( (gse) == CC_GSE_Turn_Dark )

/**
    Macro to inline check if game is on-going, i.e. it's either light or dark players turn.

    @param gse A game status enumeration.

    @return `1` if it's either light or dark player's turn, `0` otherwise.
*/
#define CC_GAME_STATUS_IS_TURN(gse) ( ( (gse) == CC_GSE_Turn_Light ) || ( (gse) == CC_GSE_Turn_Dark ) )


/**
    Game status enumeration.
*/
typedef enum CcGameStatusEnum
{
    CC_GSE_None, /**< Uninitialized game. */
    CC_GSE_Turn_Light, /**< Light player is on turn. */
    CC_GSE_Turn_Dark, /**< Dark player is on turn. */
    CC_GSE_Win_Light, /**< Light player has won. */
    CC_GSE_Win_Dark, /**< Dark player has won. */
    CC_GSE_Draw, /**< Game was drawn. */
} CcGameStatusEnum;

/**
    Function returns next game status, based on current one, and additional flags
    from user or position on chessboard.

    @param gse Current game status.
    @param is_end Flag, if game has ended due to rules.
    @param is_won Flag, if current player has won the game.

    @return Next game status.
*/
CcGameStatusEnum cc_game_status_next( CcGameStatusEnum gse,
                                      bool is_end,
                                      bool is_won );

/**
    Function returns next game status, based on current player resignation.

    @param gse Current game status.

    @return Next game status.
*/
CcGameStatusEnum cc_game_resign( CcGameStatusEnum gse );


/**
    Game structure.
*/
typedef struct CcGame
{
    CcGameStatusEnum status; /**< Current game status. */
    CcChessboard * chessboard; /**< Current position on a chessboard. */
    CcMove * moves; /**< Linked list of moves played so far. */
} CcGame;

/**
    Returns a newly allocated game.

    @param status Initial game status.
    @param ve Variant to play.
    @param do_setup Flag, if start from initial setup (`true`), or from manually set-up position (`false`).

    @note
    Linked list of performed moves will be empty. If neccessary, it can be populated once newly allocated game is returned.

    @return
    A newly allocated game, is successful, `NULL` otherwise.
*/
CcGame * cc_game_new( CcGameStatusEnum status,
                      CcVariantEnum ve,
                      bool do_setup );

/**
    Duplicates a given game into a newly allocated one.

    @param game Game to duplicate.

    @return
    A newly allocated game, is successful, `NULL` otherwise.
*/
CcGame * cc_game_duplicate_all_new( CcGame * restrict game );

/**
    Frees game, and all owned resources (chessboard, moves).

    @param game__f A game to free.

    @return `true` if successful, `false` otherwise.
*/
bool cc_game_free_all( CcGame ** restrict game__f );


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
bool cc_game_move_data_free_all( CcGame ** restrict gm__f,
                                 CcChessboard ** restrict cb__f,
                                 CcMove ** restrict moves__f,
                                 CcPly ** restrict plies__f,
                                 CcStep ** restrict steps__f,
                                 bool cumulative_result );


#endif /* __CC_GAME_H__ */
