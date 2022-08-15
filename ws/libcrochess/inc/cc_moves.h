// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_MOVES_H__
#define __CC_MOVES_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_moves.h
    @brief Moves struct (queue), and related functions.
*/


/**
    Move structure, queue.
*/
typedef struct CcMoves
{
    char * an; /**< A move, algebraic notation. */

    struct CcMoves * prev; /**< Previous move, in a queue. */
    struct CcMoves * next; /**< Next move, in a queue. */
} CcMoves;

/**
    Returns a newly allocated move.

    @param an Move (algebraic notation) to copy.
    @param max_len__d _Optional_, maximum length to copy.

    @return A newly allocated move if successful, `NULL` otherwise.
*/
CcMoves * cc_moves__new( char const * restrict an,
                         size_t max_len__d );

/**
    Appends a newly allocated move to a queue.

    @param moves__io Queue of moves.
    @param an Move to copy.
    @param max_len__d _Optional_, maximum length to copy.

    @return
    Weak pointer to a newly allocated move, is successful, `NULL` otherwise.
*/
CcMoves * cc_moves_append( CcMoves * restrict moves__io,
                           char const * restrict an,
                           size_t max_len__d );

/**
    Allocates a new move, appends it to a queue, which might not be allocated yet.

    @param moves__io Queue of moves.
    @param an Move.
    @param max_len__d _Optional_, maximum length to copy.

    @note
    Parameter `*moves__io` is a queue of moves to which a newly allocated
    move is appended.

    @note
    Queue `*moves__io` can be `NULL`, a newly allocated move will
    still be allocated, and returned.

    @note
    If queue `*moves__io` is `NULL`, it will be initialized,
    with a newly allocated move as its first element.

    @return
    Weak pointer to a newly allocated move, is successful, `NULL` otherwise.
*/
CcMoves * cc_moves_append_if( CcMoves ** restrict moves__io,
                              char const * restrict an,
                              size_t max_len__d );

/**
    Duplicates all moves in a queue.

    @param moves Queue of moves.

    @return Pointer to a newly allocated queue if successful, `NULL` otherwise.
*/
CcMoves * cc_moves_duplicate_all__new( CcMoves * restrict moves );

/**
    Frees all moves, and associated resources, in a queue.

    @param moves__f Queue of moves.

    @return `true` if successful, `false` otherwise.
*/
bool cc_moves_free_all( CcMoves ** restrict moves__f );

/**
    Prints all moves in a queue.

    @param moves Queue of moves.

    @return `true` if successful, `false` otherwise.
*/
bool cc_moves_print( CcMoves * restrict moves );


#endif /* __CC_MOVES_H__ */
