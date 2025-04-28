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

bool cc_path_context_free_all( CcPathContext ** path_ctx__f ) {
    if ( !path_ctx__f ) return false;
    if ( !*path_ctx__f ) return true;

    bool result = true;

    result = cc_game_free_all( &((*path_ctx__f)->game) ) && result;
    // result = cc_chessboard_free_all( &((*path_ctx__f)->cb_old) ) && result;
    result = cc_chessboard_free_all( &((*path_ctx__f)->cb_current) ) && result;

    CC_FREE_AND_NULL( path_ctx__f );

    return result;
}

CcPathContext * cc_path_context_duplicate_all__new( CcPathContext * from,
                                                    bool copy_history ) {
    if ( !from ) return NULL;
    if ( !from->game ) return NULL;
    if ( !from->game->chessboard ) return NULL;

    CcGameStatusEnum status = from->game->status;
    CcVariantEnum ve = from->game->chessboard->type;

    CcPathContext * px__a = cc_path_context__new( status, ve, false );
    if ( !px__a ) return NULL;

    px__a->game = cc_game_duplicate_all__new( from->game, copy_history );
    if ( !px__a->game ) {
        cc_path_context_free_all( &px__a );
        return NULL;
    }

    if ( from->cb_current ) {
        px__a->cb_current = cc_chessboard_duplicate__new( from->cb_current );
        if ( !px__a->cb_current ) {
            cc_path_context_free_all( &px__a );
            return NULL;
        }
    } else
        px__a->cb_current = NULL;

    px__a->move_ctx = from->move_ctx;
    px__a->ply_ctx = from->ply_ctx;

    return px__a;
}

CcMaybeBoolEnum cc_path_context_is_legal( CcPathContext * path_ctx ) {
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !path_ctx->game ) return CC_MBE_Void;
    if ( !path_ctx->game->chessboard ) return CC_MBE_Void;

    if ( path_ctx->cb_current ) {
        if ( path_ctx->cb_current->type != path_ctx->game->chessboard->type ) return CC_MBE_False;
    }

    cc_uint_t board_size = cc_chessboard_get_size( path_ctx->game->chessboard );
    bool has_moves_played = (bool)(path_ctx->game->moves);

    if ( has_moves_played ) {
        if ( !CC_MOVE_CONTEXT_IS_LEGAL( path_ctx->move_ctx, board_size ) ) return CC_MBE_False;
    }

    bool has_plies_played = (bool)(path_ctx->cb_current);

    if ( has_plies_played ) {
        if ( !CC_PLY_CONTEXT_IS_LEGAL( path_ctx->ply_ctx, board_size ) ) return CC_MBE_False;
    }

    if ( !has_moves_played && has_plies_played )
        return CC_MBE_False;

    return CC_MBE_True;
}
