// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path.h"

/**
    @file cc_path.c
    @brief Path struct, functions.
*/


//
// Linked list of paths.

CcPathLink * cc_path_link__new( CcPptLink ** restrict ppt__n ) {
    if ( !ppt__n ) return NULL;
    if ( !*ppt__n ) return NULL;

    CcPathLink * pl__a = malloc( sizeof( CcPathLink ) );
    if ( !pl__a ) return NULL;

    pl__a->path = *ppt__n; // Transfering ownership.
    *ppt__n = NULL; // Preventing usage from old pointer holding ownership.

    pl__a->next = NULL;

    return pl__a;
}

CcPathLink * cc_path_link_append( CcPathLink ** restrict path_link__iod,
                                  CcPptLink ** restrict ppt__n ) {
    if ( !path_link__iod ) return NULL;

    CcPathLink * pl__t = cc_path_link__new( ppt__n );
    if ( !pl__t ) return NULL;

    if ( !*path_link__iod ) {
        *path_link__iod = pl__t; // Ownership transfer.
    } else {
        CcPathLink * pl = *path_link__iod;
        CC_FASTFORWARD( pl );
        pl->next = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

bool cc_path_link_free_all( CcPathLink ** restrict path_link__f ) {
    if ( !path_link__f ) return false;
    if ( !*path_link__f ) return true;

    bool result = true;
    CcPathLink * pl = *path_link__f;
    CcPathLink * tmp = NULL;

    while ( pl ) {
        tmp = pl->next;

        result = cc_ppt_link_free_all( &(pl->path) ) && result;

        CC_FREE( pl );
        pl = tmp;
    }

    *path_link__f = NULL;
    return result;
}

size_t cc_path_link_len( CcPathLink * restrict path_link ) {
    if ( !path_link ) return 0;

    size_t len = 0;
    CcPathLink * pl = path_link;

    while ( pl ) {
        ++len;
        pl = pl->next;
    }

    return len;
}


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
// Queue of weak pointers to path node.

typedef struct CcPathWeak {
    CcPathNode * node__w;

    struct CcPathWeak * prev;
    struct CcPathWeak * next;
} CcPathWeak;

static CcPathWeak * cc_path_weak__new( CcPathNode * restrict node ) {
    if ( !node ) return NULL;

    CcPathWeak * pw__a = malloc( sizeof( CcPathWeak ) );
    if ( !pw__a ) return NULL;

    pw__a->node__w = node;
    pw__a->prev = NULL;
    pw__a->next = NULL;

    return pw__a;
}

static CcPathWeak * cc_path_weak_append( CcPathWeak ** restrict path_weak__iod,
                                         CcPathNode * restrict node ) {
    if ( !path_weak__iod ) return NULL;
    if ( !node ) return NULL;

    CcPathWeak * pw__t = cc_path_weak__new( node );
    if ( !pw__t ) return NULL;

    if ( !*path_weak__iod ) {
        *path_weak__iod = pw__t; // Ownership transfer.
    } else {
        CcPathWeak * pw = *path_weak__iod;
        CC_FASTFORWARD( pw );

        // Append + ownership transfer.
        pw->next = pw__t;
        pw__t->prev = pw;
    }

    return pw__t; // Weak pointer.
}

static bool cc_path_weak_free_all( CcPathWeak ** restrict path_weak__f ) {
    if ( !path_weak__f ) return false;
    if ( !*path_weak__f ) return true;

    CcPathWeak * pw = *path_weak__f;
    CcPathWeak * tmp = NULL;

    CC_REWIND( pw );

    while ( pw ) {
        tmp = pw->next;
        CC_FREE( pw );
        pw = tmp;
    }

    *path_weak__f = NULL;
    return true;
}

static CcPathWeak * cc_path_weak_copy_shallow__new( CcPathWeak * restrict path_weak ) {
    if ( !path_weak ) return NULL;

    CcPathWeak * pw = path_weak;
    CcPathWeak * pw__a = NULL;

    CC_REWIND( pw );

    while ( pw ) {
        if ( !cc_path_weak_append( &pw__a, pw->node__w ) ) {
            cc_path_weak_free_all( &pw__a );
            return NULL;
        }

        pw = pw->next;
    }

    return pw__a;
}

static size_t cc_path_weak_len( CcPathWeak * restrict path_weak ) {
    if ( !path_weak ) return 0;

    size_t len = 0;
    CcPathWeak * pw = path_weak;

    CC_REWIND( pw );

    while ( pw ) {
        ++len;
        pw = pw->next;
    }

    return len;
}

static bool cc_path_weak_check_if_valid( CcPathWeak * restrict path_weak ) {
    if ( !path_weak ) return false;

    CcPathWeak * pw = path_weak;

    CC_REWIND( pw );

    while ( pw ) {
        if ( !pw->node__w ) return false;

        CcPathNode * d = pw->node__w->divergence;
        pw = pw->next;

        if ( pw->node__w != d ) return false; // Not next node in divergent sequence.
    }

    if ( pw->node__w->divergence )
        return false; // Not terminal node.

    return true;
}

static bool cc_path_weak_append_route( CcPathNode * restrict path_node,
                                       CcPathWeak ** restrict path_weak__iod ) {
    if ( !path_node ) return false;
    if ( !path_weak__iod ) return false;

    CcPathNode * pn = path_node;

    while ( pn ) {
        CcPathWeak * pw__w = cc_path_weak_append( path_weak__iod, pn );
        if ( !pw__w ) { // Append failed ...
            cc_path_weak_free_all( path_weak__iod );
            return false;
        }

        pn = pn->divergence;
    }

    return true;
}

static bool cc_path_weak_get_next_route( CcPathNode * restrict path_node,
                                         CcPathWeak ** restrict path_weak__iod ) {
    if ( !path_node ) return false;
    if ( !path_weak__iod ) return false;

    if ( !*path_weak__iod )
        return cc_path_weak_append_route( path_node, path_weak__iod );
    else {
        if ( !cc_path_weak_check_if_valid( *path_weak__iod ) ) {
            cc_path_weak_free_all( path_weak__iod );
            return false;
        }
    }

    CcPathWeak * pw = *path_weak__iod;
    CC_FASTFORWARD( pw );

    while ( pw ) {
        // cc_path_weak_check_if_valid() ensured that ->node__w is valid for all nodes.
        if ( pw->node__w->alt_path ) {
            pw->node__w = pw->node__w->alt_path;

            CcPathNode * d = pw->node__w->divergence;
            if ( d )
                return cc_path_weak_append_route( d, &pw );
            else
                return true;
        }

        pw = pw->prev;
    }

    // Traversed all nodes, reached past tree root, lets reset this.
    cc_path_weak_free_all( path_weak__iod );
    return false;
}

static size_t cc_path_weak_count_of_steps( CcPathWeak * restrict path_weak ) {
    if ( !path_weak ) return 0;

    size_t count = 0;
    CcPathWeak * pw = path_weak;

    CC_REWIND( pw );

    while ( pw ) {
        if ( pw->node__w && pw->node__w->path )
            count += cc_ppt_link_len( pw->node__w->path );

        pw = pw->next;
    }

    return count;
}


//
// Auxilary functions.

static CcPathWeak * cc_path_find_route__new( CcPathNode * restrict path_node,
                                             bool is_shortest ) {
    if ( !path_node ) return NULL;

    size_t count = 0;
    CcPathWeak * route__a = NULL;
    CcPathWeak * pw__t = NULL;

    while ( cc_path_weak_get_next_route( path_node, &pw__t ) ) {
        if ( !route__a ) { // Not initialized search yet.
            count = cc_path_weak_count_of_steps( pw__t );

            route__a = cc_path_weak_copy_shallow__new( pw__t );
            if ( !route__a ) {
                cc_path_weak_free_all( &pw__t );
                return NULL;
            }
        } else {
            size_t c = cc_path_weak_count_of_steps( pw__t );
            bool found = is_shortest ? ( c < count ) : ( c > count );

            if ( found ) {
                cc_path_weak_free_all( &route__a );

                route__a = cc_path_weak_copy_shallow__new( pw__t );
                if ( !route__a ) {
                    cc_path_weak_free_all( &pw__t );
                    return NULL;
                }

                count = c;
            }
        }
    }

    cc_path_weak_free_all( &pw__t );

    return route__a;
}

static CcPptLink * cc_path_assemble_route__new( CcPathWeak * restrict route ) {
    if ( !route ) return NULL;

    CcPathWeak * pw = route;
    CcPptLink * pl__a = NULL;

    CC_REWIND( pw );

    while ( pw ) {
        if ( pw->node__w && pw->node__w->path ) {
            CcPptLink * ppt__t = cc_ppt_link_duplicate_all__new( pw->node__w->path );
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

        pw = pw->next;
    }

    return pl__a;
}

CcPptLink * cc_path_find_shortest_route__new( CcPathNode * restrict path_node ) {
    if ( !path_node ) return NULL;

    CcPathWeak * shortest__a = cc_path_find_route__new( path_node, true );

    CcPptLink * pl__a = cc_path_assemble_route__new( shortest__a );

    cc_path_weak_free_all( &shortest__a );

    return pl__a;
}

CcPptLink * cc_path_find_longest_route__new( CcPathNode * restrict path_node ) {
    if ( !path_node ) return NULL;

    CcPathWeak * longest__a = cc_path_find_route__new( path_node, false );

    CcPptLink * pl__a = cc_path_assemble_route__new( longest__a );

    cc_path_weak_free_all( &longest__a );

    return pl__a;
}
