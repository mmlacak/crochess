// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_H__
#define __CC_PATH_H__

#include <stddef.h>

#include "cc_pos.h"


/**
    @file cc_path.h
    @brief Path struct, functions.
*/


//
// Linked list of linked list of positions.

/**
    A linked list of linked list of positions.
*/
typedef struct CcPathLink {
    CcPosLink * pos_ll; /**< A linked list of positions. */
    struct CcPathLink * next; /**< Link to a next linke list. */
} CcPathLink;

/**
    Function allocates a new linked path.

    @param pos__n A linked list of positions.

    @note
    Argument `pos__n` will have its ownership taken, positions assigned to newly allocated linked path item,
    pointer itself will be `NULL`-ed, to prevent any future use.

    @return Pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link__new( CcPosLink ** restrict pos__n );

/**
    Function appends a newly allocated linked path to a given linked list.

    @param path_link__io _Input/output_ parameter, linked list of paths.
    @param pos__n A linked list of positions; ownership will be taken, and pointer `NULL`-ed.

    @return A weak pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link_append( CcPathLink * restrict path_link__io,
                                  CcPosLink ** restrict pos__n );

/**
    Allocates a new linked path, appends it to a linked list.

    @param path_link__io _Input/output_ parameter, linked list of paths.
    @param pos__n A linked list of positions; ownership will be taken, and pointer `NULL`-ed.

    @note
    Linked list `*path_link__io` can be `NULL`, a linked path will still be
    allocated, and returned.

    @note
    If linked list `*path_link__io` is `NULL`, it will be initialized,
    with a newly allocated linked path as its first element.

    @return A weak pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link_expand( CcPathLink ** restrict path_link__io,
                                  CcPosLink ** restrict pos__n );

/**
    Frees all paths in a linked list.

    @param path_link__f Linked list of paths.

    @return `true` if successful, `false` otherwise.
*/
bool cc_path_link_free_all( CcPathLink ** restrict path_link__f );

/**
    Function returns length of a linked list.

    @param path_link A linked list of paths.

    @return Length of a linked list if successful, `0` otherwise.
*/
size_t cc_path_link_len( CcPathLink * restrict path_link );



#endif /* __CC_PATH_H__ */
