// Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_DEFS_H__
#define __CC_PATH_DEFS_H__

#include <stdbool.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"

#include "cc_pos.h"
#include "cc_typed_step.h"
#include "cc_chessboard.h"
#include "cc_game.h"

// todo :: DOCS

//
// Move context.

typedef struct CcMoveContext {
    CcPosDesc initial; // A piece starting current move, its initial position and tag.
    CcPos current; // Current position of a piece which started current move.
    CcPosDesc pawn_sacrifice_serpent; // Serpent and its position where it initiated Pawn-sacrifice.
} CcMoveContext;

#define CC_MOVE_CONTEXT_INVALID { .initial = CC_POS_DESC_CAST_INVALID,                  \
                                  .current = CC_POS_CAST_INVALID,                       \
                                  .pawn_sacrifice_serpent = CC_POS_DESC_CAST_INVALID }

#define CC_MOVE_CONTEXT_CAST_INVALID ( (CcMoveContext)CC_MOVE_CONTEXT_INVALID )

#define CC_MOVE_CONTEXT_IS_VALID(move_ctx)                                      \
    ( CC_POS_DESC_IS_VALID( (move_ctx).initial ) &&                             \
      CC_POS_IS_VALID( (move_ctx).current ) &&                                  \
      CC_POS_DESC_IS_VALID( (move_ctx).pawn_sacrifice_serpent ) )

#define CC_MOVE_CONTEXT_IS_LEGAL(move_ctx,board_size)                           \
    ( CC_POS_DESC_IS_LEGAL( (move_ctx).initial, (board_size) ) &&               \
      CC_POS_IS_LEGAL( (move_ctx).current, (board_size) ) &&                    \
      CC_POS_DESC_IS_LEGAL( (move_ctx).pawn_sacrifice_serpent, (board_size) ) )

//
// Ply context.

typedef struct CcPlyContext {
    CcPosDesc initial;
    CcPos starting; // Starting position, if different from initial, i.e. in case of repositioning.
    CcActivationDesc activation;
    CcTypedStep step_1;
    CcTypedStep step_2;
    bool is_first;
} CcPlyContext;

#define CC_PLY_CONTEXT_INVALID { .initial = CC_POS_DESC_CAST_INVALID,               \
                                 .starting = CC_POS_CAST_INVALID,                   \
                                 .activation = CC_ACTIVATION_DESC_CAST_SPENT,       \
                                 .step_1 = CC_TYPED_STEP_CAST_INVALID,              \
                                 .step_2 = CC_TYPED_STEP_CAST_INVALID,              \
                                 .is_first = false }

#define CC_PLY_CONTEXT_CAST_INVALID ( (CcPlyContext)CC_PLY_CONTEXT_INVALID )

#define CC_PLY_CONTEXT_IS_VALID(ply_ctx)                                            \
    ( CC_POS_DESC_IS_VALID( (ply_ctx).initial ) &&                                  \
      CC_POS_IS_VALID( (ply_ctx).starting ) &&                                      \
      cc_activation_desc_is_valid( (ply_ctx).activation, (ply_ctx).is_first ) &&    \
      CC_TYPED_STEP_IS_VALID( (ply_ctx).step_1 ) &&                                 \
      CC_TYPED_STEP_IS_VALID( (ply_ctx).step_2 ) )

#define CC_PLY_CONTEXT_IS_LEGAL(ply_ctx,board_size)                                 \
    ( CC_POS_DESC_IS_LEGAL( (ply_ctx).initial, (board_size) ) &&                    \
      CC_POS_IS_LEGAL( (ply_ctx).starting, (board_size) ) &&                        \
      cc_activation_desc_is_valid( (ply_ctx).activation, (ply_ctx).is_first ) &&    \
      CC_TYPED_STEP_IS_VALID( (ply_ctx).step_1 ) &&                                 \
      CC_TYPED_STEP_IS_VALID( (ply_ctx).step_2 ) )

//
// Piece context.

typedef struct CcPieceContextLink {
    CcPosDesc initial; // A piece, its initial position and tag at the start of a move.
    CcPos current; // Current position of a piece.

    struct CcPieceContextLink * next;
} CcPieceContextLink;

CcPieceContextLink * cc_piece_ctx_link__new( CcPosDesc initial, CcPos current );

CcPieceContextLink * cc_piece_ctx_link_append( CcPieceContextLink ** piece_ctx_link__iod_a,
                                               CcPosDesc initial,
                                               CcPos current );

CcPieceContextLink * cc_piece_ctx_link_duplicate_all__new( CcPieceContextLink * piece_ctx_link );

CcPieceContextLink * cc_piece_ctx_link_extend( CcPieceContextLink ** piece_ctx_link__iod_a,
                                               CcPieceContextLink ** piece_ctx_link__n );

bool cc_piece_ctx_link_free_all( CcPieceContextLink ** piece_ctx_link__f );

size_t cc_piece_ctx_link_len( CcPieceContextLink * piece_ctx_link );

CcPieceContextLink * cc_piece_ctx_link_find_unique_initial( CcPieceContextLink * piece_ctx_link,
                                                            CcPosDesc initial );

CcPieceContextLink * cc_piece_ctx_link_find_unique( CcPieceContextLink * piece_ctx_link,
                                                    CcPos current );

CcMaybeBoolEnum cc_piece_ctx_link_update_unique( CcPieceContextLink * piece_ctx_link__io,
                                                 CcPos current,
                                                 CcPos destination );

//
// Path context.

typedef struct CcPathContext {
    CcGame * game__w;
    // CcChessboard * cb_old;
    CcChessboard * cb_current;

    CcMoveContext move_ctx;
    CcPlyContext ply_ctx;
} CcPathContext;

CcPathContext * cc_path_context__new( CcGame * game );

bool cc_path_context_free_all( CcPathContext ** path_ctx__f );

CcPathContext * cc_path_context_duplicate_all__new( CcPathContext * from );

CcMaybeBoolEnum cc_path_context_is_legal( CcPathContext * path_ctx,
                                          bool do_check_move_ctx,
                                          bool do_check_ply_ctx );

bool cc_path_context_init_move( CcPathContext * path_ctx__io,
                                CcPosDesc move_init );

bool cc_path_context_init_ply( CcPathContext * path_ctx__io,
                               CcPosDesc ply_init,
                               CcTypedStep step_1,
                               CcTypedStep step_2 );


#endif /* __CC_PATH_DEFS_H__ */
