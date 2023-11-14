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
// Linked list of paths.

/**
    Tree of routes. Each route follows paths from start to destination.
*/
typedef struct CcPathLink {
    struct CcPathLink * alt_path; /**< Link to an alternative path.
                                       All paths start from the same location.
                                       After divergence, all paths continue from the same location (but does not contain it). */

    CcPptLink * path; /**< A linked list of positions, with pieces and tags on them. */

    struct CcPathLink * divergence; /**< Link to a set of paths, all continuing this route. */
} CcPathLink;

/**
    Function allocates a new linked path.

    @param ppt__n A linked list of positions.

    @note
    Argument `ppt__n` will have its ownership taken, positions assigned to newly allocated linked path item,
    pointer itself will be `NULL`-ed, to prevent any future use.

    @return Pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link__new( CcPptLink ** restrict ppt__n );

/**
    Function appends a newly allocated linked path to a given linked list.

    @param path_link__iod _Optional_, _input/output_ parameter; linked list of paths.
    @param ppt__n A linked list of positions + pieces + tags; ownership will be taken, and pointer `NULL`-ed.

    @note
    Linked list `*path_link__iod` can be `NULL`, a linked path will still be allocated,
    and weak pointer to it returned.

    @note
    If linked list `*path_link__iod` is `NULL`, it will be initialized,
    with a newly allocated linked path as its only element.

    @return A weak pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link_append( CcPathLink ** restrict path_link__iod,
                                  CcPptLink ** restrict ppt__n );

/**
    Frees all paths in a linked list.

    @param path_link__f Linked list of paths.

    @return `true` if successful, `false` otherwise.
*/
bool cc_path_link_free_all( CcPathLink ** restrict path_link__f );

// /**
//     Function returns length of a linked list.

//     @param path_link A linked list of paths.

//     @return Length of a linked list if successful, `0` otherwise.
// */
// size_t cc_path_link_len( CcPathLink * restrict path_link );



#endif /* __CC_PATH_H__ */
