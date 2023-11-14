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
    Function appends a newly allocated path as an alternative to a given path segment.

    @param path_link__iod _Optional_, _input/output_ parameter; path segment.
    @param ppt__n A path; linked list of positions + pieces + tags. Ownership will be taken, and pointer `NULL`-ed.

    @note
    Tree `*path_link__iod` can be `NULL`, a path will still be allocated,
    and weak pointer to it returned.

    @note
    If tree `*path_link__iod` is `NULL`, it will be initialized,
    with a newly allocated path as its only element.

    @return A weak pointer to a newly allocated path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link_append_alternative( CcPathLink ** restrict path_link__iod,
                                              CcPptLink ** restrict ppt__n );

/**
    Function appends a newly allocated path as an divergence to a given path segment.

    @param path_link__io _Input/output_ parameter; path segment.
    @param ppt__n A path; linked list of positions + pieces + tags. Ownership will be taken, and pointer `NULL`-ed.

    @note
    Tree `path_link__io->divergence` can be `NULL`, a path will still be allocated,
    and weak pointer to it returned.

    @note
    If tree `path_link__io->divergence` is `NULL`, it will be initialized,
    with a newly allocated path as its only element.

    @return A weak pointer to a newly allocated path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link_append_divergent( CcPathLink * restrict path_link__io,
                                            CcPptLink ** restrict ppt__n );

/**
    Frees all paths in a tree.

    @param path_link__f A tree of paths.

    @return `true` if successful, `false` otherwise.
*/
bool cc_path_link_free_all( CcPathLink ** restrict path_link__f );

/**
    Function returns count of alternative paths to a given path segment.

    @param path_link A path segment.

    @note
    Given path segment is included in a count.

    @return Count of a alternative paths if successful, `0` otherwise.
*/
size_t cc_path_link_count_alt( CcPathLink * restrict path_link );



#endif /* __CC_PATH_H__ */
