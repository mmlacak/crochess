// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_ctx.h"

//
// Path context.

CcPathContext * cc_path_context__new( CcGame * game ) {
    if ( !game ) return NULL;
    if ( !game->chessboard ) return NULL;

    CcPathContext * px__a = malloc( sizeof( CcPathContext ) );
    if ( !px__a ) return NULL;

    px__a->game__w = game; // Weak pointer, no ownership transferred.
    // px__a->cb_old = NULL;
    px__a->cb_current = cc_chessboard_duplicate__new( game->chessboard );

    px__a->move_ctx = CC_MOVE_CONTEXT_CAST_INVALID;
    px__a->ply_ctx = CC_PLY_CONTEXT_CAST_INVALID;

    return px__a;
}

bool cc_path_context_free_all( CcPathContext ** path_ctx__f ) {
    if ( !path_ctx__f ) return false;
    if ( !*path_ctx__f ) return true;

    bool result = true;

    // result = cc_game_free_all( &((*path_ctx__f)->game__w) ) && result; // <!> Weak pointer is not to be free()-ed.
    // result = cc_chessboard_free_all( &((*path_ctx__f)->cb_old) ) && result;
    result = cc_chessboard_free_all( &((*path_ctx__f)->cb_current) ) && result;

    CC_FREE_AND_NULL( path_ctx__f );

    return result;
}

CcPathContext * cc_path_context_duplicate_all__new( CcPathContext * from ) {
    if ( !from ) return NULL;
    if ( !from->game__w ) return NULL;
    if ( !from->game__w->chessboard ) return NULL;

    CcPathContext * px__a = cc_path_context__new( from->game__w ); // Game ownership not transferred here.
    if ( !px__a ) return NULL;

    if ( from->cb_current ) {
        px__a->cb_current = cc_chessboard_duplicate__new( from->cb_current );
        if ( !px__a->cb_current ) {
            cc_path_context_free_all( &px__a );
            return NULL;
        }
    }

    px__a->move_ctx = from->move_ctx;
    px__a->ply_ctx = from->ply_ctx;

    return px__a;
}

bool cc_path_context_is_legal( CcPathContext * path_ctx,
                               bool do_check_move_ctx,
                               bool do_check_ply_ctx ) {
    if ( !path_ctx ) return false;
    if ( !path_ctx->game__w ) return false;
    if ( !path_ctx->game__w->chessboard ) return false;
    if ( !path_ctx->cb_current ) return false;

    if ( path_ctx->cb_current->type != path_ctx->game__w->chessboard->type )
        return false;

    cc_uint_t board_size = cc_chessboard_get_size( path_ctx->game__w->chessboard );
    bool has_moves_played = (bool)(path_ctx->game__w->moves);

    if ( do_check_move_ctx ) {
        if ( has_moves_played ) {
            if ( !CC_MOVE_CONTEXT_IS_LEGAL( path_ctx->move_ctx, board_size ) )
                return false;
        }
    }

    bool has_plies_played = ( !path_ctx->ply_ctx.is_first );

    if ( do_check_ply_ctx ) {
        if ( has_plies_played ) {
            if ( !CC_PLY_CONTEXT_IS_LEGAL( path_ctx->ply_ctx, board_size ) )
                return false;
        }
    }

    if ( do_check_move_ctx &&
            do_check_ply_ctx &&
            ( !has_moves_played && has_plies_played ) )
        return false;

    return true;
}

static bool _cc_path_context_init_move( CcPathContext * path_ctx__io,
                                        CcPosDesc move_init ) {
    if ( !CC_PIECE_IS_VALID( move_init.piece ) ) return false; // [1]
    if ( !cc_path_context_is_legal( path_ctx__io, false, false ) ) return false;

    CcChessboard * cb = path_ctx__io->game__w->chessboard;

    // <!> Not needed, get_piece() (here, at [2]) returns None for positions outside
    //     chessboard; here, piece is certainly not None [1], and check is done at [3].
    // cc_uint_t board_size = cc_chessboard_get_size( cb );
    // if ( !CC_POS_DESC_IS_LEGAL( move_init, board_size ) ) return false;

    // Checking piece and its tag are still on declared position generaly does not hold, except at the very beginning of a move.
    CcPieceTagType piece = cc_chessboard_get_piece( cb, move_init.pos.i, move_init.pos.j ); // [2]
    if ( piece != move_init.piece ) return false; // [3]

    if ( !cc_chessboard_is_equal( path_ctx__io->cb_current, cb ) ) {
        cc_chessboard_free_all( &(path_ctx__io->cb_current) );
        path_ctx__io->cb_current = cc_chessboard_duplicate__new( cb );
        if ( !path_ctx__io->cb_current ) return false;
    }

    path_ctx__io->move_ctx = (CcMoveContext){ .initial = move_init,
                                              .current = move_init.pos,
                                              .multi_stage = CC_MSPTE_None,
                                              .pawn_sacrifice_serpent = CC_POS_DESC_CAST_INVALID };

    path_ctx__io->ply_ctx = (CcPlyContext){ .initial = move_init,
                                            .starting = move_init.pos,
                                            .act_desc = CC_ACTIVATION_DESC_CAST_INITIAL,
                                            .is_first = true };

    return true;
}

static bool _cc_path_context_init_ply( CcPathContext * path_ctx__io,
                                       CcPosDesc ply_init ) {
    if ( !CC_PIECE_IS_VALID( ply_init.piece ) ) return false;
    if ( !cc_path_context_is_legal( path_ctx__io, true, false ) ) return false; // true --> even before the very 1st ply of a cascade, 1st move context has to be initialized.

    CcChessboard * cb = path_ctx__io->game__w->chessboard;

    // <!> Do not remove; piece and its tag are not at declared position, at the start of a ply; see _cc_path_context_init_move().
    cc_uint_t board_size = cc_chessboard_get_size( cb );
    if ( !CC_POS_DESC_IS_LEGAL( ply_init, board_size ) ) return false;

    if ( !path_ctx__io->cb_current ) {
        path_ctx__io->cb_current = cc_chessboard_duplicate__new( cb );
        if ( !path_ctx__io->cb_current ) return false;
    }

    CcPlyContext * _ctx = &(path_ctx__io->ply_ctx);

    if ( CC_PIECE_IS_ACTIVATOR( _ctx->initial.piece ) ) {
        _ctx->act_desc.activator = _ctx->initial.piece;
    }

    _ctx->initial = ply_init;
    _ctx->starting = ply_init.pos;

    if ( CC_PIECE_IS_WEIGHTLESS( ply_init.piece ) )
        _ctx->act_desc.usage = CC_MUE_NotUsing;
    else
        _ctx->act_desc.usage = CC_MUE_Spending;

    _ctx->is_first = false;

    return true;
}

bool cc_path_context_init( CcPathContext * path_ctx__io,
                           CcPosDesc init_pos,
                           bool init_move_or_ply ) {
    if ( !path_ctx__io ) return false;

    return init_move_or_ply ? _cc_path_context_init_move( path_ctx__io, init_pos )
                            : _cc_path_context_init_ply( path_ctx__io, init_pos );
}
