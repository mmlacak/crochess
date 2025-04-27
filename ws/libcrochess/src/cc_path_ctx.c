// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_ctx.h"


CcPathContext * cc_path_context__new( CcGameStatusEnum status,
                                      CcVariantEnum ve,
                                      bool do_setup ) {
    if ( !CC_GAME_STATUS_IS_VALID( status ) ) return NULL;
    if ( !CC_VARIANT_IS_VALID( ve) ) return NULL;

    CcPathContext * px__a = malloc( sizeof( CcPathContext ) );
    if ( !px__a ) return NULL;

    px__a->game = cc_game__new( status, ve, do_setup );
    if ( !px__a->game ) {
        CC_FREE( px__a );
        return NULL;
    }

    // px__a->cb_old = NULL;
    px__a->cb_current = NULL;

    px__a->move_ctx = CC_MOVE_CONTEXT_CAST_INVALID;
    px__a->ply_ctx = CC_PLY_CONTEXT_CAST_INVALID;

    return px__a;
}
