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
    Tree of path segments. Each route follows path segments from starting field to destination.
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
    Pinned route, i.e. queue of weak pointers to path segments.
    A route follows paths from starting field to destination.
*/
typedef struct CcRoutePin {
    CcPathNode * node__w; /**< Path segment, a weak pointer. */

    struct CcRoutePin * prev; /**< Previous node in a queue. */
    struct CcRoutePin * next; /**< Next node in a queue. */
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
    Frees all nodes in a given route.

    @param route_pin__f A queue of route pins.

    @return `true` if successful, `false` otherwise.
*/
bool cc_route_pin_free_all( CcRoutePin ** restrict route_pin__f );

/**
    Frees a given node in a route.

    @param route_pin__f A node to free.

    @return Pointer to node in a route if successful, `NULL` otherwise.

    @return
    Function will return node previous to the one freed, if it exists;
    otherwise, it will return next node in a given sequence.
*/
CcRoutePin * cc_route_pin_free_node( CcRoutePin * restrict route_pin__f );

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

    @param path_node Path segment, a node in path tree.
    @param route_pin Pinned route, a queue.

    @note
    Function checks that route is exhaustive, i.e. valid from starting to
    destination field; this is done by checking that:
    - first pinned route node points to root path segment (or one of its alternatives)
    - all `node__w` pin members are valid (not `NULL`)
    - next pinned segment is divergence from previous path segment
    - `divergence` member of last pointed to path segment is `NULL`.

    @return `true` if successful, `false` otherwise.
*/
bool cc_route_pin_check_if_valid( CcPathNode * restrict path_node,
                                  CcRoutePin * restrict route_pin );

/**
    Function appends a newly allocated path segments to a given pinned route.

    @param path_node Path segment, a node in path tree.
    @param route_pin__iod_a **Ownership**, _optional_ _input/output_ parameter; pinned route.

    @note
    Similar function `cc_route_pin_append()` appends only a given path segment
    to a given pinned route.

    @note
    This function follows through all divergences, until it appends all path
    segments down to a leaf node.

    @see cc_route_pin_append()

    @return `true` if successful, `false` otherwise.
*/
bool cc_route_pin_append_route( CcPathNode * restrict path_node,
                                CcRoutePin ** restrict route_pin__iod_a );

/**
    Function iterates over path tree, returning next route via _input/output_ argument.

    @param path_node A path tree to traverse.
    @param route_pin__io_a_F **Ownership**, _conditionally freed_, _input / output_ parameter; pinned route.

    @note
    Argument `*route_pin__io_a_F` contains currently pinned route when iterator is called. <br />
    If function returns `true`, it contains next route found in a given path tree.

    @note
    Argument `*route_pin__io_a_F` can be `NULL` at the beginning of iteration, <br />
    it will be initialized with first route found in a given path tree.

    @warning
    When iterator founds no more routes to traverse, it will **free** last pinned route, <br />
    and set `*route_pin__io_a_F` pointer to `NULL`, to prepare for next iterations. <br />
    The same will happen in case of any error, to prevent infinite loops.

    @warning
    Path tree **must** always be the same with which route was initialized. <br />
    To change path tree before it's traversed completely, one has to free route, reset its pointer to `NULL`, <br />
    then first call to iterator with new path tree will initialize route with it.

    @note
    Argument `route_pin__io_a_F` retains ownership of pinned route between iterator calls; <br />
    if loop is exited prematurely (or iterator is called outside of loop), one has to free
    pinned route explicitly.

    Usage example:
    @code{.c}
    CcPathNode pn; // Path tree to traverse; just a (forward) declaration.
    CcRoutePin * rp__a = NULL; // Pinned route; must be set to NULL before start iterating.

    while ( cc_route_pin_iter( pn, &rp__a ) ) {
        // Do some stuff here ...
    }
    // After loop is done (or if some error happens), route is free-ed,
    // and pointer to it set to NULL.

    // After first loop, one can immediately roll another one,
    // no free + NULL necessary, as it's already done.
    while ( cc_route_pin_iter( pn, &rp__a ) ) {
        // Do other stuff here ...
    }

    // Iterator can be initialized outside a loop, and then called as needed.
    if ( cc_route_pin_iter( pn, &rp__a ) ) {
        // Do someting to first route found here ...
    }

    // All calls to iterator must be done with the same path tree which was used to initialize route.
    if ( cc_route_pin_iter( pn, &rp__a ) ) {
        // ... and to second
    }

    CcPathNode pn_2; // The other path tree; just a (forward) declaration.

    // When changing path tree before it was exhausted in iterator,
    // route must be free-ed, and pointer NULL-ed.
    cc_route_pin_free_all( &rp__a );

    // Next call initializes route with a new path tree ...
    if ( cc_route_pin_iter( pn_2, &rp__a ) ) {
        // Do someting to first route found in the other tree here ...
    }

    // Pinned route pointer is owner of allocated data; if it's not free()-ed
    // (if loop hasn't finished, or iterator was called outside of loop),
    // one has to free pinned route explicitly.
    cc_route_pin_free_all( &rp__a );
    @endcode

    @see
    cc_route_pin_check_if_valid(), cc_route_pin_append_route(), cc_route_pin_free_all()

    @return `true` if successful, `false` otherwise. <br />

    @return
    Next route is returned via `route_pin__io_a_F` argument, if iterator returned `true`. <br />
    If iterator returned `false` and both arguments were supplied, pinned route was `free()`-ed,
    and argument `*route_pin__io_a_F` was reset to `NULL`.
*/
bool cc_route_pin_iter( CcPathNode * restrict path_node,
                        CcRoutePin ** restrict route_pin__io_a_F );

/**
    Function returns count of steps in all path segments in a given pinned route.

    @param route_pin Pinned route, a queue.

    @note
    Similar function `cc_route_pin_len()` returns simple count of nodes in a given queue.

    @note
    This function counts steps in all path segments (pointed by `node__w` member).

    @see cc_route_pin_len()

    @return Count of steps if successful, `0` otherwise.
*/
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
