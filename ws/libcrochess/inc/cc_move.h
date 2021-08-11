// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_MOVE_H__
#define __CC_MOVE_H__

#include "cc_ply.h"

/**
    @file cc_move.h
    @brief Move enumeration, structure, and related functions.
*/

/**
    Move status enumeration.
*/
typedef enum CcMoveStatusEnum
{
    CC_MSE_None,
    CC_MSE_Check,
    CC_MSE_Checkmate,
} CcMoveStatusEnum;


/**
    Move structure, linked list.
*/
typedef struct CcMove
{
    char const * notation; /**< Original notation, before parsing. Usually, from user input. */
    CcPly * plies; /**< Plies. */
    CcMoveStatusEnum status; /**< Status. */

    struct CcMove * next; /**< Next move. */
} CcMove;


/**
    Returns newly allocated move.

    @param notation Original notation, as received from a user.
    @param plies_n Plies, should be valid pointer.
    @param status Move status.

    @warning
    Takes ownership of plies, inner pointer will be set to `NULL`, if valid move is produced.

    @warning
    If no valid move is produced, plies are still valid, and accessible.

    @return
    A newly allocated move, is successful, `NULL` otherwise.
*/
// TODO :: DOCS
CcMove * cc_move_new( char const * const restrict notation,
                      CcPly ** restrict plies_n,
                      CcMoveStatusEnum status );

/**
    Allocates a new move, appends it to a linked list.

    @param moves Linked list of moves, to which a new move is appended.
    @param notation Original notation, as received from a user.
    @param plies_n Plies, should be valid pointer.
    @param status Move status.

    @note
    A new move is appended to `moves`, if it's a valid pointer.

    @note
    If not, appending is not done, but a new move is still returned.

    @warning
    Takes ownership of plies, inner pointer will be set to `NULL`, if valid move is produced.

    @warning
    If no valid move is produced, plies are still valid, and accessible.

    @see cc_move_new()

    @return
    A newly allocated move, is successful, `NULL` otherwise.
*/
// TODO :: DOCS
CcMove * cc_move_append_new( CcMove * const restrict moves,
                             char const * const restrict notation,
                             CcPly ** restrict plies_n,
                             CcMoveStatusEnum status );

/**
    Frees all moves in a linked list, and all associated entities.

    @param moves_f Linked list of moves.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_move_free_all_moves( CcMove ** const moves_f );

/**
    Function returns count of plies owned by a given move.

    @param move Move.

    @return Count of plies if successful, `0` otherwise.
*/
size_t cc_move_ply_count( CcMove const * const restrict move );


#endif /* __CC_MOVE_H__ */
