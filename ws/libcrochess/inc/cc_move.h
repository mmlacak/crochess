// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_MOVE_H__
#define __CC_MOVE_H__

#include "cc_ply.h"

/**
    @file cc_move.h
    @brief Move linked list; storage for plies, status, user algebraic notation.
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
    char * notation; /**< Original notation, before parsing. Usually, from user input. */
    CcPly * plies; /**< Plies. */
    CcMoveStatusEnum status; /**< Status. */

    struct CcMove * next; /**< Next move. */
} CcMove;


/**
    Returns newly allocated move.

    @param notation Original notation, as received from a user.
    @param plies__n Plies linked list, can be `NULL`.
    @param status Move status.

    @warning
    Takes ownership of plies, inner pointer will be set to `NULL`, if valid move is produced.

    @warning
    If no valid `plies__n` is given, move is still returned, with `plies` member set to `NULL`.

    @warning
    If no valid move is produced, plies are still valid, and accessible.

    @return
    A newly allocated move, is successful, `NULL` otherwise.
*/
CcMove * cc_move_new( char const * restrict notation,
                      CcPly ** restrict plies__n,
                      CcMoveStatusEnum status );

/**
    Appends a newly allocated move to a linked list.

    @param moves__io _Input/output_ parameter, linked list of moves, to which a new move is appended.
    @param notation Original notation, as received from a user.
    @param plies__n Plies, should be valid pointer.
    @param status Move status.

    @see cc_move_new()

    @return
    Weak pointer to a newly allocated move, is successful, `NULL` otherwise.
*/
CcMove * cc_move_append( CcMove * restrict moves__io,
                         char const * restrict notation,
                         CcPly ** restrict plies__n,
                         CcMoveStatusEnum status );

/**
    Allocates a new move, appends it to a linked list.

    @param moves__io _Input/output_ parameter, linked list of moves, to which a new move is appended.
    @param notation Original notation, as received from a user.
    @param plies__n Plies, should be valid pointer.
    @param status Move status.

    @note
    Linked list `*moves__io` can be `NULL`, a move will still be allocated, and returned.

    @note
    If linked list `*moves__io` is `NULL`, it will be initialized,
    with a newly allocated move as its first element.

    @return
    Weak pointer to a newly allocated move, is successful, `NULL` otherwise.
*/
CcMove * cc_move_append_or_init( CcMove ** restrict moves__io,
                                 char const * restrict notation,
                                 CcPly ** restrict plies__n,
                                 CcMoveStatusEnum status );

/**
    Extends moves linked list with another move(s).

    @param moves__io Linked list of moves, to which a move is appended, can be `NULL`.
    @param move__n A move(s) to append to a list.

    @note
    If linked list `*moves__io` is `NULL`, it will be initialized,
    with a move as its first element.

    @return
    `true` is successful, `false` otherwise.
*/
bool cc_move_extend_or_init( CcMove ** restrict moves__io,
                             CcMove ** restrict move__n );

/**
    Duplicates a given moves into a newly allocated linked list.

    @param moves Linked list to duplicate.

    @return
    A newly allocated moves, is successful, `NULL` otherwise.
*/
CcMove * cc_move_duplicate_all_new( CcMove * restrict moves );

/**
    Frees all moves in a linked list, and all associated entities.

    @param moves__f Linked list of moves.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_move_free_all_moves( CcMove ** restrict moves__f );

/**
    Function returns count of plies owned by a given move.

    @param move Move.

    @return Count of plies if successful, `0` otherwise.
*/
size_t cc_move_ply_count( CcMove * restrict move );


#endif /* __CC_MOVE_H__ */
