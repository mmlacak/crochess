// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_MOVE_H__
#define __CC_MOVE_H__

#include "cc_ply.h"

/**
    @file cc_move.h
    @brief Move queue; storage for plies, status, user algebraic notation.
*/


/**
    Move status enumeration, after a valid movement.
*/
typedef enum CcParsedMoveStatusEnum {
    CC_PMSE_None, /**< No status. */
    CC_PMSE_DrawOffer, /**< Player offered a draw. */
    CC_PMSE_DrawOffer_Revoked, /**< Player took back draw offer. */

    CC_PMSE_Check, /**< Checking opponent. */
    CC_PMSE_Check_DrawOffer, /**< Checking opponent, player offered a draw. */
    CC_PMSE_Check_DrawOffer_Revoked, /**< Checking opponent, player took back draw offer. */

    CC_PMSE_Checkmate, /**< Opponent checkmated. */
    CC_PMSE_SelfCheckmate, /**< Opponent checkmated self, game ended. */

    CC_PMSE_Resign, /**< Player resigned, game ended. */
    CC_PMSE_DrawAccepted, /**< Player accepted draw offer, game ended. */
    CC_PMSE_DrawByRules, /**< Game was drawn by rules, game ended. */
} CcParsedMoveStatusEnum;


/**
    Move structure, queue.
*/
typedef struct CcParsedMove {
    char * notation; /**< Original notation, before parsing. Usually, from user input. */
    CcParsedPly * plies; /**< Plies. */
    // CcChessboard * cb; // TODO :: if adding, pieces and tags enums **should** be based on char, not int
    CcParsedMoveStatusEnum status; /**< Status. */

    struct CcParsedMove * prev; /**< Previous move, in a queue. */
    struct CcParsedMove * next; /**< Next move. */
} CcParsedMove;


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
    A newly allocated move if successful, `NULL` otherwise.
*/
CcParsedMove * cc_parsed_move__new( char const * notation,
                                    size_t max_len__d,
                                    CcParsedPly ** plies__n,
                                    CcParsedMoveStatusEnum status );

/**
    Appends a newly allocated move to a queue.

    @param moves__iod_a **Ownership**, _optional_ _input/output_ parameter, queue of moves, to which a new move is appended.
    @param notation Original notation, as received from a user.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param plies__n Plies; linked list.
    @param status Move status.

    @note
    Queue `*moves__iod_a` can be `NULL`, a move will still be allocated,
    and weak pointer to it returned.

    @note
    If queue `*moves__iod_a` is `NULL`, it will be initialized
    with a newly allocated move as its only element.

    @note
    Pointer `moves__iod_a` has ownership over given queue, takes ownership
    over newly allocated move, and retains ownership after function returns.

    @see cc_parsed_move__new()

    @return
    Weak pointer to a newly allocated move if successful, `NULL` otherwise.
*/
CcParsedMove * cc_parsed_move_append( CcParsedMove ** moves__iod_a,
                                      char const * notation,
                                      size_t max_len__d,
                                      CcParsedPly ** plies__n,
                                      CcParsedMoveStatusEnum status );

/**
    Duplicates a given moves into a newly allocated queue.

    @param moves Queue to duplicate.

    @return
    A newly allocated moves if successful, `NULL` otherwise.
*/
CcParsedMove * cc_parsed_move_duplicate_all__new( CcParsedMove * moves );

/**
    Frees all moves in a queue, and all associated entities.

    @param moves__f Queue of moves.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parsed_move_free_all( CcParsedMove ** moves__f );


/**
    Function returns count of plies owned by a given move.

    @param move Move.

    @return Count of plies if successful, `0` otherwise.
*/
size_t cc_parsed_move_plies_count( CcParsedMove * move );


#endif /* __CC_MOVE_H__ */
