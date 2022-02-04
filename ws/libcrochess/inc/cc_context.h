// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CONTEXT_H__
#define __CC_CONTEXT_H__

#include <stddef.h>

#include "cc_chessboard.h"
#include "cc_game.h"

/**
    @file cc_context.h
    @brief Ply context linked list, and context structure; used while parsing AN notation.
*/


/**
    Ply context, used while parsing ply AN.
*/
typedef struct CcContextPly
{
    CcChessboard * chessboard; /**< Chessboard, position at the beginning of a ply being parsed. */
    char const * ply_start__w; /**< Start of a ply AN substring, i.e. first `char` that belongs to it. Points to a `char` within `move_an__w` in a parent context. */
    char const * ply_end__w; /**< End of a ply AN substring, i.e. first `char` that does not belongs to it. Points to a `char` within `move_an__w` in a parent context. */
    struct CcContextPly * next; /**< Next ply context. */
} CcContextPly;

/**
    Returns newly allocated ply context.

    @param ply_start__w Weak pointer to a substring start within `move_an__w`, in a parent context.
    @param ply_end__w Weak pointer to a substring end within `move_an__w`, in a parent context.

    @note
    Start of a (sub)string is a first `char` which belongs to that (sub)string.

    @note
    End of a (sub)string is a first `char` which does not belong to that (sub)string.

    @return
    A newly allocated ply context, is successful, `NULL` otherwise.
*/
CcContextPly * cc_context_ply__new( char const * restrict ply_start__w,
                                   char const * restrict ply_end__w );

/**
    Appends a newly allocated ply context to a linked list.

    @param context_ply__io _Input/output_ parameter, linked list of ply contexts, to which a new ply context is appended.
    @param ply_start__w Weak pointer to a substring start within `move_an__w`, in a parent context.
    @param ply_end__w Weak pointer to a substring end within `move_an__w`, in a parent context.

    @see cc_context_ply__new()

    @return
    Weak pointer to a newly allocated ply context, is successful, `NULL` otherwise.
*/
CcContextPly * cc_context_ply_append( CcContextPly * restrict context_ply__io,
                                      char const * restrict ply_start__w,
                                      char const * restrict ply_end__w );

/**
    Allocates a new ply context, appends it to a linked list.

    @param context_ply__io _Input/output_ parameter, linked list of ply contexts, to which a new ply context is appended.
    @param ply_start__w Weak pointer to a substring start within `move_an__w`, in a parent context.
    @param ply_end__w Weak pointer to a substring end within `move_an__w`, in a parent context.

    @note
    Linked list `*context_ply__io` can be `NULL`, a ply context will still be allocated, and returned.

    @note
    If linked list `*context_ply__io` is `NULL`, it will be initialized,
    with a newly allocated ply context as its first element.

    @return
    Weak pointer to a newly allocated ply context, is successful, `NULL` otherwise.
*/
CcContextPly * cc_context_ply_append_or_init( CcContextPly ** restrict context_ply__io,
                                              char const * restrict ply_start__w,
                                              char const * restrict ply_end__w );

/**
    Frees all ply contexts in a linked list, and all associated entities.

    @param context_ply__f Linked list of ply contexts.

    @return `true` if successful, `false` otherwise.
*/
bool cc_context_ply_free_all( CcContextPly ** restrict context_ply__f );


/**
    Context, used while parsing move AN.
*/
typedef struct CcContext
{
    CcGame * game__w; /**< Weak pointer to a game, status and positions are as they were before parsing move AN started. */
    char * user_move_an; /**< _Owned_, user input, i.e. move algebraic notation. */
    char * converted_an; /**< _Owned_, converted user AN, if input was made for Classical Chess, as a compatibility AN. */
    char const * move_an__w; /**< _Weak_, convenience pointer to either `converted_an` if not `NULL`, or to `user_move_an`. */
    CcContextPly * context_ply; /**< _Owned_, linked list of ply contexts. */
} CcContext;


/**
    Returns newly allocated context.

    @param game__w Weak pointer to a game.
    @param user_move_an Pointer to a user input, i.e. move algebraic notation.

    @return
    A newly allocated context, is successful, `NULL` otherwise.
*/
CcContext * cc_context__new( CcGame * restrict game__w,
                            char const * restrict user_move_an );

/**
    Frees user linked list of ply contexts, and all owned resources (move notation, converted notation).

    @param context__f A context to free.

    @return `true` if successful, `false` otherwise.
*/
bool cc_context_free_all( CcContext ** restrict context__f );



#endif /* __CC_CONTEXT_H__ */
