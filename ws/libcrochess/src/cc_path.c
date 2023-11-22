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

CcPathNode * cc_path_node_append_alternative( CcPathNode ** restrict path_node__iod,
                                              CcPptLink ** restrict ppt__n ) {
    if ( !path_node__iod ) return NULL;

    CcPathNode * pl__t = cc_path_node__new( ppt__n );
    if ( !pl__t ) return NULL;

    if ( !*path_node__iod ) {
        *path_node__iod = pl__t; // Ownership transfer.
    } else {
        CcPathNode * pn = *path_node__iod;
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
