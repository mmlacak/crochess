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


// TODO :: DELETE ::
//

//
// Linked list of paths.

// /**
//     Linked list of paths, comprising one route. A route follows paths from starting field to destination.
// */
// typedef struct CcPathLink {
//     CcPptLink * path; /**< A linked list of positions + pieces + tags, with pieces and tags on them. */
//     struct CcPathLink * next; /**< Link to a next path. */
// } CcPathLink;

// /**
//     Function allocates a new linked path.

//     @param ppt__n A linked list of positions + pieces + tags.

//     @note
//     Argument `ppt__n` will have its ownership taken, positions + pieces + tags assigned to newly allocated linked path item,
//     pointer itself will be `NULL`-ed, to prevent any future use.

//     @return Pointer to a newly allocated linked path if successful, `NULL` otherwise.
// */
// CcPathLink * cc_path_link__new( CcPptLink ** restrict ppt__n );

// /**
//     Function appends a newly allocated path to a given path segment.

//     @param path_link__iod_a **Ownership**, _optional_ _input/output_ parameter; path segment.
//     @param ppt__n A path; linked list of positions + pieces + tags. Ownership will be taken, and pointer `NULL`-ed.

//     @note
//     Linked list `*path_link__iod_a` can be `NULL`, a path will still be allocated,
//     and weak pointer to it returned.

//     @note
//     If linked list `*path_link__iod_a` is `NULL`, it will be initialized,
//     with a newly allocated path as its only element.

//     @note
//     Pointer `path_link__iod_a` has ownership over given linked list, takes ownership
//     over newly allocated path, and retains ownership after function returns.

//     @return A weak pointer to a newly allocated path if successful, `NULL` otherwise.
// */
// CcPathLink * cc_path_link_append( CcPathLink ** restrict path_link__iod_a,
//                                   CcPptLink ** restrict ppt__n );

// /**
//     Frees all paths in a given linked list.

//     @param path_link__f A linked list of paths.

//     @return `true` if successful, `false` otherwise.
// */
// bool cc_path_link_free_all( CcPathLink ** restrict path_link__f );

// /**
//     Function returns length of a linked list.

//     @param path_link A linked list of paths.

//     @return Length of a linked list if successful, `0` otherwise.
// */
// size_t cc_path_link_len( CcPathLink * restrict path_link );

//
// TODO :: DELETE ::


//
// Tree of paths.

/**
    Tree of routes. Each route follows paths from start to destination.
*/
typedef struct CcPathNode {
    struct CcPathNode * alt_path; /**< Link to an alternative path.
                                       All paths start from the same location.
                                       After divergence, all paths continue from the same location (but does not contain it). */

    CcPptLink * path; /**< A linked list of positions, with pieces and tags on them. */

    struct CcPathNode * divergence; /**< Link to a set of paths, all continuing this route. */
} CcPathNode;

/**
    Function allocates a new linked path.

    @param ppt__n A path, linked list of positions + pieces + tags.

    @note
    Argument `ppt__n` will have its ownership taken, positions + pieces + tags assigned to newly allocated linked path item,
    pointer itself will be `NULL`-ed, to prevent any future use.

    @return Pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathNode * cc_path_node__new( CcPptLink ** restrict ppt__n );

/**
    Function appends a newly allocated path as an alternative to a given path segment.

    @param path_node__iod_a **Ownership**, _optional_ _input/output_ parameter; path segment.
    @param ppt__n A path; linked list of positions + pieces + tags. Ownership will be taken, and pointer `NULL`-ed.

    @note
    Tree `*path_node__iod_a` can be `NULL`, a path will still be allocated,
    and weak pointer to it returned.

    @note
    If tree `*path_node__iod_a` is `NULL`, it will be initialized,
    with a newly allocated path as its only element.

    @note
    Pointer `path_node__iod_a` has ownership over given path segment, takes ownership
    over newly allocated path, and retains ownership after function returns.

    @return A weak pointer to a newly allocated path if successful, `NULL` otherwise.
*/
CcPathNode * cc_path_node_append_alternative( CcPathNode ** restrict path_node__iod_a,
                                              CcPptLink ** restrict ppt__n );

/**
    Function appends a newly allocated path as an divergence to a given path segment.

    @param path_node__io _Input/output_ parameter; path segment.
    @param ppt__n A path; linked list of positions + pieces + tags. Ownership will be taken, and pointer `NULL`-ed.

    @note
    Sub-tree `path_node__io->divergence` can be `NULL`, a path will still be allocated,
    and weak pointer to it returned.

    @note
    If sub-tree `path_node__io->divergence` is `NULL`, it will be initialized,
    with a newly allocated path as its only element.

    @return A weak pointer to a newly allocated path if successful, `NULL` otherwise.
*/
CcPathNode * cc_path_node_append_divergent( CcPathNode * restrict path_node__io,
                                            CcPptLink ** restrict ppt__n );

/**
    Frees all paths in a tree.

    @param path_node__f A tree of paths.

    @return `true` if successful, `false` otherwise.
*/
bool cc_path_node_free_all( CcPathNode ** restrict path_node__f );

/**
    Function returns count of alternative paths to a given path segment.

    @param path_node A path segment.

    @note
    Given path segment is included in a count.

    @return Count of a alternative paths if successful, `0` otherwise.
*/
size_t cc_path_node_count_alt( CcPathNode * restrict path_node );


//
// Pinned route, i.e. queue of weak pointers to path node.

/**
    Pinned route, i.e. queue of weak pointers to path node.
    A route follows paths from starting field to destination.
*/
typedef struct CcRoutePin {
    CcPathNode * node__w;

    struct CcRoutePin * prev;
    struct CcRoutePin * next;
} CcRoutePin;

/**
    Function allocates a new route pin.

    @param node A node in path tree.

    @return Pointer to a newly allocated route pin if successful, `NULL` otherwise.
*/
CcRoutePin * cc_route_pin__new( CcPathNode * restrict node );

/**
    Function appends a newly allocated node to a given queue.

    @param route_pin__iod_a **Ownership**, _optional_ _input/output_ parameter; pinned route.
    @param node A node in path tree.

    @note
    Linked list `*route_pin__iod_a` can be `NULL`, a path will still be allocated,
    and weak pointer to it returned.

    @note
    If queue `*route_pin__iod_a` is `NULL`, it will be initialized,
    with a newly allocated path as its only element.

    @note
    Pointer `route_pin__iod_a` has ownership over given queue, takes ownership
    over newly allocated ply, and retains ownership after function returns.

    @see cc_route_pin__new()

    @return A weak pointer to a newly allocated node if successful, `NULL` otherwise.
*/
CcRoutePin * cc_route_pin_append( CcRoutePin ** restrict route_pin__iod_a,
                                  CcPathNode * restrict node );

/**
    Frees all pins in a given route.

    @param route_pin__f A queue of route pins.

    @return `true` if successful, `false` otherwise.
*/
bool cc_route_pin_free_all( CcRoutePin ** restrict route_pin__f );

/**
    Creates a new, shallow copy of a given route.

    @param route_pin Pinned route, a queue.

    @note
    Shallow copy means that in each pin `node__w` member points to the same
    path segment as in originating node.

    @return Pointer to newly allocated queue if successful, `NULL` otherwise.
*/
CcRoutePin * cc_route_pin_copy_shallow__new( CcRoutePin * restrict route_pin );

/**
    Function returns length of a queue.

    @param route_pin Pinned route, a queue.

    @return Length of a queue if successful, `0` otherwise.
*/
size_t cc_route_pin_len( CcRoutePin * restrict route_pin );

/**
    Function checks if pinned route is valid.

    @param route_pin Pinned route, a queue.

    @note
    Function checks that route is exhaustive, i.e. valid from starting to
    destination field; this is done by checking that:
    - all `node__w` pin members are valid (not `NULL`)
    - next pinned segment is divergence from previous path segment
    - `divergence` member of last pointed to path segment is `NULL`.

    @return `true` if successful, `false` otherwise.
*/
bool cc_route_pin_check_if_valid( CcRoutePin * restrict route_pin );

/**
    Function appends a newly allocated path segments to a given pinned route.

    @param path_node Path segment, a node in path tree.
    @param route_pin__iod_a **Ownership**, _optional_ _input/output_ parameter; pinned route.

    @note
    Similar function `cc_route_pin_append()` appends only a given path segment
    to a given pinned route.

    @note
    This function follows all first items for both alternative paths, and divergences,
    until it appends all path segments down to a leaf node.

    @see cc_route_pin_append()

    @return `true` if successful, `false` otherwise.
*/
bool cc_route_pin_append_route( CcPathNode * restrict path_node,
                                CcRoutePin ** restrict route_pin__iod_a );

// TODO :: DOCS
bool cc_route_pin_get_next_route( CcPathNode * restrict path_node,
                                  CcRoutePin ** restrict route_pin__iod_a_n );

// TODO :: DOCS
size_t cc_route_pin_count_of_steps( CcRoutePin * restrict route_pin );


//
// Auxilary functions.

// TODO :: DOCS
CcRoutePin * cc_path_find_route__new( CcPathNode * restrict path_node,
                                      bool is_shortest );

// TODO :: DOCS
CcPptLink * cc_path_assemble_route__new( CcRoutePin * restrict route );

// TODO :: find shortest route
CcPptLink * cc_path_find_shortest_route__new( CcPathNode * restrict path_node );

// TODO :: find longest route
CcPptLink * cc_path_find_longest_route__new( CcPathNode * restrict path_node );


#endif /* __CC_PATH_H__ */
