// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_MOVE_H__
#define __CC_MOVE_H__

#include "cc_ply.h"

/**
    @file cc_move.h
    @brief Move linked list; storage for plies, status, user algebraic notation.
*/


/**
    Move status enumeration, after a valid movement.
*/
typedef enum CcMoveStatusEnum
{
    CC_MSE_None, /**< No status. */
    CC_MSE_DrawOffer, /**< Player offered a draw. */
    CC_MSE_DrawOffer_Revoked, /**< Player took back draw offer. */

    CC_MSE_Check, /**< Checking opponent. */
    CC_MSE_Check_DrawOffer, /**< Checking opponent, player offered a draw. */
    CC_MSE_Check_DrawOffer_Revoked, /**< Checking opponent, player took back draw offer. */

    CC_MSE_Checkmate, /**< Opponent checkmated. */
    CC_MSE_SelfCheckmate, /**< Opponent checkmated self, game ended. */

    CC_MSE_Resign, /**< Player resigned, game ended. */
    CC_MSE_DrawAccepted, /**< Player accepted draw offer, game ended. */
    CC_MSE_DrawByRules, /**< Game was drawn by rules, game ended. */
} CcMoveStatusEnum;


/**
    Move structure, linked list.
*/
typedef struct CcMove
{
    char * notation; /**< Original notation, before parsing. Usually, from user input. */
    CcPly * plies; /**< Plies. */
    // CcChessboard * cb; // TODO :: if adding, pieces and tags enums **should** be based on char, not int
    CcMoveStatusEnum status; /**< Status. */

    struct CcMove * prev; /**< Previous move, in a queue. */
    struct CcMove * next; /**< Next move. */
} CcMove;


/**
    Returns newly allocated move.

    @param notation Original notation, as received from a user.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param plies__n Plies linked list, can be `NULL`.
    @param status Move status.

    @warning
    Takes ownership of plies, inner pointer will be set to `NULL`, if valid move is produced.

    @warning
    If no valid `plies__n` is given, move is still returned, with `plies` member set to `NULL`.

    @warning
    If no valid move is produced, `plies__n` are still valid, and accessible.

    @return
    A newly allocated move, is successful, `NULL` otherwise.
*/
CcMove * cc_move__new( char const * restrict notation,
                       size_t max_len__d,
                       CcPly ** restrict plies__n,
                       CcMoveStatusEnum status );

/**
    Appends a newly allocated move to a linked list.

    @param moves__io _Input/output_ parameter, linked list of moves, to which a new move is appended.
    @param notation Original notation, as received from a user.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param plies__n Plies, should be valid pointer.
    @param status Move status.

    @see cc_move__new()

    @return
    Weak pointer to a newly allocated move, is successful, `NULL` otherwise.
*/
CcMove * cc_move_append( CcMove * restrict moves__io,
                         char const * restrict notation,
                         size_t max_len__d,
                         CcPly ** restrict plies__n,
                         CcMoveStatusEnum status );

/**
    Allocates a new move, appends it to a linked list.

    @param moves__io _Input/output_ parameter, linked list of moves, to which a new move is appended.
    @param notation Original notation, as received from a user.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param plies__n Plies, should be valid pointer.
    @param status Move status.

    @note
    Linked list `*moves__io` can be `NULL`, a move will still be allocated, and returned.

    @note
    If linked list `*moves__io` is `NULL`, it will be initialized,
    with a newly allocated move as its first element.

    @see cc_move__new()

    @return
    Weak pointer to a newly allocated move, is successful, `NULL` otherwise.
*/
CcMove * cc_move_append_if( CcMove ** restrict moves__io,
                            char const * restrict notation,
                            size_t max_len__d,
                            CcPly ** restrict plies__n,
                            CcMoveStatusEnum status );

/**
    Duplicates a given moves into a newly allocated linked list.

    @param moves Linked list to duplicate.

    @return
    A newly allocated moves, is successful, `NULL` otherwise.
*/
CcMove * cc_move_duplicate_all__new( CcMove * restrict moves );

/**
    Frees all moves in a linked list, and all associated entities.

    @param moves__f Linked list of moves.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_move_free_all( CcMove ** restrict moves__f );


/**
    Function returns count of plies owned by a given move.

    @param move Move.

    @return Count of plies if successful, `0` otherwise.
*/
size_t cc_move_plies_count( CcMove * restrict move );


#endif /* __CC_MOVE_H__ */
