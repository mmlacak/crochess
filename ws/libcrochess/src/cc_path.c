// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path.h"

/**
    @file cc_path.c
    @brief Path struct, functions.
*/


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

CcPathNode * cc_path_node_append_alternative( CcPathNode ** restrict path_link__iod,
                                              CcPptLink ** restrict ppt__n ) {
    if ( !path_link__iod ) return NULL;

    CcPathNode * pl__t = cc_path_node__new( ppt__n );
    if ( !pl__t ) return NULL;

    if ( !*path_link__iod ) {
        *path_link__iod = pl__t; // Ownership transfer.
    } else {
        CcPathNode * pl = *path_link__iod;
        CC_REWIND_BY( pl, pl->alt_path );
        pl->alt_path = pl__t; // Append + ownership transfer.
    }

    return pl__t; // Weak pointer.
}

CcPathNode * cc_path_node_append_divergent( CcPathNode * restrict path_link__io,
                                            CcPptLink ** restrict ppt__n ) {
    if ( !path_link__io ) return NULL;

    CcPathNode * div = path_link__io->divergence;

    CcPathNode * pl__w = cc_path_node_append_alternative( &div, ppt__n );

    return pl__w;
}

bool cc_path_node_free_all( CcPathNode ** restrict path_link__f ) {
    if ( !path_link__f ) return false;
    if ( !*path_link__f ) return true;

    bool result = true;
    CcPathNode * pl = *path_link__f;
    CcPathNode * ap = NULL;
    CcPathNode * div = NULL;

    while ( pl ) {
        ap = pl->alt_path;
        div = pl->divergence;

        result = cc_ppt_link_free_all( &(pl->path) ) && result;

        if ( div )
            result = cc_path_node_free_all( &div ) && result;

        CC_FREE( pl );
        pl = ap;
    }

    *path_link__f = NULL;
    return result;
}

size_t cc_path_node_count_alt( CcPathNode * restrict path_link ) {
    if ( !path_link ) return 0;

    size_t len = 0;
    CcPathNode * pl = path_link;

    while ( pl ) {
        ++len;
        pl = pl->alt_path;
    }

    return len;
}
