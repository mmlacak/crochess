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
