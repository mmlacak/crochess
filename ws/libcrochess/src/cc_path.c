// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path.h"

/**
    @file cc_path.c
    @brief Path struct, functions.
*/


// TODO :: DELETE ::
//

//
// Linked list of paths.

// CcPathLink * cc_path_link__new( CcPptLink ** restrict ppt__n ) {
//     if ( !ppt__n ) return NULL;
//     if ( !*ppt__n ) return NULL;

//     CcPathLink * pl__a = malloc( sizeof( CcPathLink ) );
//     if ( !pl__a ) return NULL;

//     pl__a->path = *ppt__n; // Transfering ownership.
//     *ppt__n = NULL; // Preventing usage from old pointer holding ownership.

//     pl__a->next = NULL;

//     return pl__a;
// }

// CcPathLink * cc_path_link_append( CcPathLink ** restrict path_link__iod,
//                                   CcPptLink ** restrict ppt__n ) {
//     if ( !path_link__iod ) return NULL;

//     CcPathLink * pl__t = cc_path_link__new( ppt__n );
//     if ( !pl__t ) return NULL;

//     if ( !*path_link__iod ) {
//         *path_link__iod = pl__t; // Ownership transfer.
//     } else {
//         CcPathLink * pl = *path_link__iod;
//         CC_FASTFORWARD( pl );
//         pl->next = pl__t; // Append + ownership transfer.
//     }

//     return pl__t; // Weak pointer.
// }

// bool cc_path_link_free_all( CcPathLink ** restrict path_link__f ) {
//     if ( !path_link__f ) return false;
//     if ( !*path_link__f ) return true;

//     bool result = true;
//     CcPathLink * pl = *path_link__f;
//     CcPathLink * tmp = NULL;

//     while ( pl ) {
//         tmp = pl->next;

//         result = cc_ppt_link_free_all( &(pl->path) ) && result;

//         CC_FREE( pl );
//         pl = tmp;
//     }

//     *path_link__f = NULL;
//     return result;
// }

// size_t cc_path_link_len( CcPathLink * restrict path_link ) {
//     if ( !path_link ) return 0;

//     size_t len = 0;
//     CcPathLink * pl = path_link;

//     while ( pl ) {
//         ++len;
//         pl = pl->next;
//     }

//     return len;
// }

//
// TODO :: DELETE ::


//
// Tree of paths.

CcPathNode * cc_path_node__new( CcPptLink ** restrict ppt__n ) {
    if ( !ppt__n ) return NULL;
    if ( !*ppt__n ) return NULL;

    CcPathNode * pl__a = malloc( sizeof( CcPathNode ) );
    if ( !pl__a ) return NULL;

    pl__a->path = *ppt__n; // Transfering ownership.
    *ppt__n = NULL; // Preventing usage from old pointer holding ownership.

    pl__a->alt_path = NULL;
    pl__a->divergence = NULL;

    return pl__a;
}

CcPathNode * cc_path_node_append_alternative( CcPathNode ** restrict path_node__iod_a,
                                              CcPptLink ** restrict ppt__n ) {
    if ( !path_node__iod_a ) return NULL;

    CcPathNode * pl__t = cc_path_node__new( ppt__n );
    if ( !pl__t ) return NULL;

    if ( !*path_node__iod_a ) {
        *path_node__iod_a = pl__t; // Ownership transfer.
    } else {
        CcPathNode * pn = *path_node__iod_a;
        CC_REWIND_BY( pn, pn->alt_path );
        pn->alt_path = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPathNode * cc_path_node_append_divergent( CcPathNode * restrict path_node__io,
                                            CcPptLink ** restrict ppt__n ) {
    if ( !path_node__io ) return NULL;

    CcPathNode * div = path_node__io->divergence;

    CcPathNode * pl__w = cc_path_node_append_alternative( &div, ppt__n );

    return pl__w;
}

bool cc_path_node_free_all( CcPathNode ** restrict path_node__f ) {
    if ( !path_node__f ) return false;
    if ( !*path_node__f ) return true;

    bool result = true;
    CcPathNode * pn = *path_node__f;
    CcPathNode * ap = NULL;
    CcPathNode * div = NULL;

    while ( pn ) {
        ap = pn->alt_path;
        div = pn->divergence;

        result = cc_ppt_link_free_all( &(pn->path) ) && result;

        if ( div )
            result = cc_path_node_free_all( &div ) && result;

        CC_FREE( pn );
        pn = ap;
    }

    *path_node__f = NULL;
    return result;
}

size_t cc_path_node_count_alt( CcPathNode * restrict path_node ) {
    if ( !path_node ) return 0;

    size_t len = 0;
    CcPathNode * pn = path_node;

    while ( pn ) {
        ++len;
        pn = pn->alt_path;
    }

    return len;
}


//
// Pinned route, i.e. queue of weak pointers to path node.

CcRoutePin * cc_route_pin__new( CcPathNode * restrict node ) {
    if ( !node ) return NULL;

    CcRoutePin * rp__a = malloc( sizeof( CcRoutePin ) );
    if ( !rp__a ) return NULL;

    rp__a->node__w = node;
    rp__a->prev = NULL;
    rp__a->next = NULL;

    return rp__a;
}

CcRoutePin * cc_route_pin_append( CcRoutePin ** restrict route_pin__iod_a,
                                  CcPathNode * restrict node ) {
    if ( !route_pin__iod_a ) return NULL;
    if ( !node ) return NULL;

    CcRoutePin * pw__t = cc_route_pin__new( node );
    if ( !pw__t ) return NULL;

    if ( !*route_pin__iod_a ) {
        *route_pin__iod_a = pw__t; // Ownership transfer.
    } else {
        CcRoutePin * rp = *route_pin__iod_a;
        CC_FASTFORWARD( rp );

        // Append + ownership transfer.
        rp->next = pw__t;
        pw__t->prev = rp;
    }

    return pw__t; // Weak pointer.
}

bool cc_route_pin_free_all( CcRoutePin ** restrict route_pin__f ) {
    if ( !route_pin__f ) return false;
    if ( !*route_pin__f ) return true;

    CcRoutePin * rp = *route_pin__f;
    CcRoutePin * tmp = NULL;

    CC_REWIND( rp );

    while ( rp ) {
        tmp = rp->next;
        CC_FREE( rp );
        rp = tmp;
    }

    *route_pin__f = NULL;
    return true;
}

CcRoutePin * cc_route_pin_free_node( CcRoutePin * restrict route_pin__f ) {
    if ( !route_pin__f ) return NULL;

    CcRoutePin * prev = route_pin__f->prev;
    CcRoutePin * rp = route_pin__f;
    CcRoutePin * next = route_pin__f->next;

    CC_FREE( rp );

    if ( prev )
        prev->next = next;

    if ( next )
        next->prev = prev;

    return prev ? prev : next;
}

CcRoutePin * cc_route_pin_copy_shallow__new( CcRoutePin * restrict route_pin ) {
    if ( !route_pin ) return NULL;

    CcRoutePin * rp = route_pin;
    CcRoutePin * rp__a = NULL;

    CC_REWIND( rp );

    while ( rp ) {
        if ( !cc_route_pin_append( &rp__a, rp->node__w ) ) {
            cc_route_pin_free_all( &rp__a );
            return NULL;
        }

        rp = rp->next;
    }

    return rp__a;
}

size_t cc_route_pin_len( CcRoutePin * restrict route_pin ) {
    if ( !route_pin ) return 0;

    size_t len = 0;
    CcRoutePin * rp = route_pin;

    CC_REWIND( rp );

    while ( rp ) {
        ++len;
        rp = rp->next;
    }

    return len;
}

static CcPathNode * cc_route_pin_check_if_node_valid( CcPathNode * restrict path_node,
                                                      CcRoutePin * restrict route_pin ) {
    if ( !path_node ) return NULL;
    if ( !route_pin ) return NULL;
    if ( !route_pin->node__w ) return NULL;

    CcPathNode * pn = path_node;
    CcPathNode * found = NULL;

    while ( pn ) {
        if ( route_pin->node__w == pn ) {
            if ( !found )
                found = route_pin->node__w;
            else
                return NULL; // Just sanity fail, should not happen.
        }

        pn = pn->alt_path;
    }

    return found;
}

bool cc_route_pin_check_if_valid( CcPathNode * restrict path_node,
                                  CcRoutePin * restrict route_pin ) {
    if ( !path_node ) return false;
    if ( !route_pin ) return false;

    CcRoutePin * rp = route_pin;
    CcPathNode * pn = path_node;

    CC_REWIND( rp );

    while ( rp ) {
        if ( !( pn = cc_route_pin_check_if_node_valid( pn, rp ) ) )
            return false;

        rp = rp->next;
        pn = pn->divergence;
    }

    if ( rp->node__w->divergence || pn->divergence )
        return false; // Not leaf node, didn't follow complete route.

    return true;
}

bool cc_route_pin_append_route( CcPathNode * restrict path_node,
                                CcRoutePin ** restrict route_pin__iod_a ) {
    if ( !path_node ) return false;
    if ( !route_pin__iod_a ) return false;

    CcPathNode * pn = path_node;

    while ( pn ) {
        CcRoutePin * pw__w = cc_route_pin_append( route_pin__iod_a, pn );
        if ( !pw__w ) { // Append failed ...
            cc_route_pin_free_all( route_pin__iod_a );
            return false;
        }

        pn = pn->divergence;
    }

    return true;
}

bool cc_route_pin_iter( CcPathNode * restrict path_node,
                        CcRoutePin ** restrict route_pin__io_a_F ) {
    if ( !path_node ) return false;
    if ( !route_pin__io_a_F ) return false;

    if ( !*route_pin__io_a_F ) {
        if ( cc_route_pin_append_route( path_node, route_pin__io_a_F ) ) {
            return true;
        } else {
            cc_route_pin_free_all( route_pin__io_a_F );
            return false;
        }
    } else {
        if ( !cc_route_pin_check_if_valid( path_node, *route_pin__io_a_F ) ) {
            cc_route_pin_free_all( route_pin__io_a_F );
            return false;
        }
    }

    CcRoutePin * rp = *route_pin__io_a_F;

    CC_FASTFORWARD( rp );

    while ( rp ) {
        // cc_route_pin_check_if_valid() ensured that ->node__w is valid for all nodes.
        if ( rp->node__w->alt_path ) {
            rp->node__w = rp->node__w->alt_path;

            CcPathNode * d = rp->node__w->divergence;
            if ( d ) {
                if ( cc_route_pin_append_route( d, &rp ) ) {
                    return true;
                } else {
                    cc_route_pin_free_all( route_pin__io_a_F );
                    return false;
                }
            } else
                return true;
        }

        if ( rp->next ) { // Should always be at leaf node.
            cc_route_pin_free_all( route_pin__io_a_F );
            return false;
        }

        // cc_route_pin_free_node() returns previous node, if valid;
        // otherwise next one which, we checked earlier, does not exists.
        // So, this essentially frees rp, then assigns previous node to rp.
        rp = cc_route_pin_free_node( rp );
    }

    // Traversed all nodes, reached past tree root, lets reset this.
    cc_route_pin_free_all( route_pin__io_a_F );
    return false;
}

size_t cc_route_pin_count_of_steps( CcRoutePin * restrict route_pin ) {
    if ( !route_pin ) return 0;

    size_t count = 0;
    CcRoutePin * rp = route_pin;

    CC_REWIND( rp );

    while ( rp ) {
        if ( rp->node__w && rp->node__w->path )
            count += cc_ppt_link_len( rp->node__w->path );

        rp = rp->next;
    }

    return count;
}


//
// Auxilary functions.

CcRoutePin * cc_path_find_route__new( CcPathNode * restrict path_node,
                                      bool is_shortest ) {
    if ( !path_node ) return NULL;

    size_t count = 0;
    CcRoutePin * route__a = NULL;
    CcRoutePin * pw__t = NULL;

    while ( cc_route_pin_iter( path_node, &pw__t ) ) {
        if ( !route__a ) { // Not initialized search yet.
            count = cc_route_pin_count_of_steps( pw__t );

            route__a = cc_route_pin_copy_shallow__new( pw__t );
            if ( !route__a ) {
                cc_route_pin_free_all( &pw__t );
                return NULL;
            }
        } else {
            size_t c = cc_route_pin_count_of_steps( pw__t );
            bool found = is_shortest ? ( c < count ) : ( c > count );

            if ( found ) {
                cc_route_pin_free_all( &route__a );

                route__a = cc_route_pin_copy_shallow__new( pw__t );
                if ( !route__a ) {
                    cc_route_pin_free_all( &pw__t );
                    return NULL;
                }

                count = c;
            }
        }
    }

    cc_route_pin_free_all( &pw__t );

    return route__a;
}

CcPptLink * cc_path_assemble_route__new( CcRoutePin * restrict route ) {
    if ( !route ) return NULL;

    CcRoutePin * rp = route;
    CcPptLink * pl__a = NULL;

    CC_REWIND( rp );

    while ( rp ) {
        if ( rp->node__w && rp->node__w->path ) {
            CcPptLink * ppt__t = cc_ppt_link_duplicate_all__new( rp->node__w->path );
            if ( !ppt__t ) {
                cc_ppt_link_free_all( &pl__a );
                return NULL;
            }

            if ( !cc_ppt_link_extend( &pl__a, &ppt__t ) ) {
                cc_ppt_link_free_all( &pl__a );
                cc_ppt_link_free_all( &ppt__t );
                return NULL;
            }
        } else {
            cc_ppt_link_free_all( &pl__a );
            return NULL;
        }

        rp = rp->next;
    }

    return pl__a;
}

CcPptLink * cc_path_find_shortest_route__new( CcPathNode * restrict path_node ) {
    if ( !path_node ) return NULL;

    CcRoutePin * shortest__a = cc_path_find_route__new( path_node, true );

    CcPptLink * pl__a = cc_path_assemble_route__new( shortest__a );

    cc_route_pin_free_all( &shortest__a );

    return pl__a;
}

CcPptLink * cc_path_find_longest_route__new( CcPathNode * restrict path_node ) {
    if ( !path_node ) return NULL;

    CcRoutePin * longest__a = cc_path_find_route__new( path_node, false );

    CcPptLink * pl__a = cc_path_assemble_route__new( longest__a );

    cc_route_pin_free_all( &longest__a );

    return pl__a;
}
