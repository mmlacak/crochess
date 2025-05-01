// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_path_ctx.h"

CcPathContext * cc_path_context__new( CcGame * game ) {
    if ( !game ) return NULL;

    CcPathContext * px__a = malloc( sizeof( CcPathContext ) );
    if ( !px__a ) return NULL;

    // No ownership transferred.
    px__a->game__w = game;

    // px__a->cb_old = NULL;
    px__a->cb_current = NULL;

    px__a->move_ctx = CC_MOVE_CONTEXT_CAST_INVALID;
    px__a->ply_ctx = CC_PLY_CONTEXT_CAST_INVALID;

    return px__a;
}

// TODO :: DELETE
// CcPathContext * cc_path_context_init_game__new( CcGameStatusEnum status,
//                                                 CcVariantEnum ve,
//                                                 bool do_setup ) {
//     if ( !CC_GAME_STATUS_IS_VALID( status ) ) return NULL;
//     if ( !CC_VARIANT_IS_VALID( ve ) ) return NULL;
//
//     CcGame * game__t = cc_game__new( status, ve, do_setup );
//     if ( !game__t ) return NULL;
//
//     return cc_path_context__new( game__t );
// }
// TODO :: DELETE

bool cc_path_context_free_all( CcPathContext ** path_ctx__f ) {
    if ( !path_ctx__f ) return false;
    if ( !*path_ctx__f ) return true;

    bool result = true;

    // result = cc_game_free_all( &((*path_ctx__f)->game__w) ) && result; // Weak pointer is not to be free()-ed.
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

CcMaybeBoolEnum cc_path_context_is_legal( CcPathContext * path_ctx,
                                          bool do_check_move_ctx,
                                          bool do_check_ply_ctx ) {
    if ( !path_ctx ) return CC_MBE_Void;
    if ( !path_ctx->game__w ) return CC_MBE_Void;
    if ( !path_ctx->game__w->chessboard ) return CC_MBE_Void;

    if ( path_ctx->cb_current ) {
        if ( path_ctx->cb_current->type != path_ctx->game__w->chessboard->type ) return CC_MBE_False;
    }

    cc_uint_t board_size = cc_chessboard_get_size( path_ctx->game__w->chessboard );
    bool has_moves_played = (bool)(path_ctx->game__w->moves);

    if ( do_check_move_ctx || has_moves_played ) {
        if ( !CC_MOVE_CONTEXT_IS_LEGAL( path_ctx->move_ctx, board_size ) ) return CC_MBE_False;
    }

    bool has_plies_played = (bool)(path_ctx->cb_current);

    if ( do_check_ply_ctx || has_plies_played ) {
        if ( !CC_PLY_CONTEXT_IS_LEGAL( path_ctx->ply_ctx, board_size ) ) return CC_MBE_False;
    }

    if ( !has_moves_played && has_plies_played )
        return CC_MBE_False;

    return CC_MBE_True;
}

bool cc_path_context_init_move( CcPathContext * path_ctx__io,
                                CcPosDesc move_init ) {
    if ( !CC_PIECE_IS_VALID( move_init.piece ) ) return false; // [1]
    if ( !cc_path_context_is_legal( path_ctx__io, false, false ) ) return false;

    CcChessboard * cb = path_ctx__io->game__w->chessboard;

    // <?> Not needed, get_piece() returns None for positions outside chessboard; here, piece is certainly not None [1], and check is done at [2].
    // cc_uint_t board_size = cc_chessboard_get_size( cb );
    // if ( !CC_POS_DESC_IS_LEGAL( move_init, board_size ) ) return false;

    // Checking piece and its tag are still on declared position generaly does not hold, except at the very beginning of a move.
    CcPieceType piece = cc_chessboard_get_piece( cb, move_init.pos.i, move_init.pos.j );
    if ( piece != move_init.piece ) return false; // [2]

    CcTagType tag = cc_chessboard_get_tag( cb, move_init.pos.i, move_init.pos.j );
    if ( tag != move_init.tag ) return false;


    if ( path_ctx__io->cb_current )
        cc_chessboard_free_all( &(path_ctx__io->cb_current) );

    path_ctx__io->cb_current = cc_chessboard_duplicate__new( cb );
    if ( !path_ctx__io->cb_current ) return false;

    path_ctx__io->move_ctx = (CcMoveContext){ .initial = move_init,
                                              .starting = move_init.pos,
                                              .current = move_init.pos,
                                              .pawn_sacrifice_serpent = CC_POS_DESC_CAST_INVALID };

    path_ctx__io->ply_ctx = (CcPlyContext){ .initial = move_init,
                                            .starting = move_init.pos,
                                            .activation = CC_ACTIVATION_DESC_CAST_INITIAL,
                                            .step_1 = CC_TYPED_STEP_CAST_INVALID,
                                            .step_2 = CC_TYPED_STEP_CAST_INVALID,
                                            .is_first = true };

    return true;
}

bool cc_path_context_init_ply( CcPathContext * path_ctx__io,
                               CcPosDesc ply_init,
                               CcTypedStep step_1,
                               CcTypedStep step_2 ) {
    if ( !CC_PIECE_IS_VALID( ply_init.piece ) ) return false;
    if ( !cc_path_context_is_legal( path_ctx__io, true, false ) ) return false;

    CcChessboard * cb = path_ctx__io->game__w->chessboard;

    // <!> Do not remove; piece and its tag are not at declared position, at the start of a ply; see cc_path_context_init_move().
    cc_uint_t board_size = cc_chessboard_get_size( cb );
    if ( !CC_POS_DESC_IS_LEGAL( ply_init, board_size ) ) return false;

    if ( !path_ctx__io->cb_current ) {
        path_ctx__io->cb_current = cc_chessboard_duplicate__new( cb );
        if ( !path_ctx__io->cb_current ) return false;
    }

    // Move-initiating piece cannot return to its origin, so this comparison always holds true.
    bool is_first = CC_POS_DESC_IS_EQUAL( path_ctx__io->move_ctx.initial, ply_init );

    CcPlyContext * _ctx = &(path_ctx__io->ply_ctx);

    if ( !is_first ) {
        if ( CC_PIECE_IS_ACTIVATOR( _ctx->initial.piece ) ) {
            _ctx->activation.activator = _ctx->initial.piece;
        }
    }

    _ctx->initial = ply_init;
    _ctx->starting = ply_init.pos;

    // _ctx->activation = CC_ACTIVATION_DESC_CAST_INITIAL;
    if ( CC_PIECE_IS_WEIGHTLESS( ply_init.piece ) )
        _ctx->activation.usage = CC_MUE_NotUsing;
    else
        _ctx->activation.usage = is_first ? CC_MUE_Accumulating : CC_MUE_Spending;

    _ctx->step_1 = step_1;
    _ctx->step_2 = step_2;
    _ctx->is_first = is_first;

    return true;
}
