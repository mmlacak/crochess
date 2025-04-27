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
    CcPos starting; // Starting position, if different from initial, i.e. in case of repositioning.
    CcPos current; // Current position of a piece which started current move.
    CcPosDesc pawn_sacrifice_serpent; // Serpent and its position where it initiated Pawn-sacrifice.
} CcMoveContext;

#define CC_MOVE_CONTEXT_INVALID { .initial = CC_POS_DESC_CAST_INVALID,                  \
                                  .starting = CC_POS_CAST_INVALID,                      \
                                  .current = CC_POS_CAST_INVALID,                       \
                                  .pawn_sacrifice_serpent = CC_POS_DESC_CAST_INVALID }

#define CC_MOVE_CONTEXT_CAST_INVALID ( (CcMoveContext)CC_MOVE_CONTEXT_INVALID )

#define CC_MOVE_CONTEXT_IS_VALID(move_ctx)                                      \
    ( CC_POS_DESC_IS_VALID( (move_ctx).initial ) &&                             \
      CC_POS_IS_VALID( (move_ctx).starting ) &&                                 \
      CC_POS_IS_VALID( (move_ctx).current ) &&                                  \
      CC_POS_DESC_IS_VALID( (move_ctx).pawn_sacrifice_serpent ) )

#define CC_MOVE_CONTEXT_IS_LEGAL(move_ctx,board_size)                           \
    ( CC_POS_DESC_IS_LEGAL( (move_ctx).initial, (board_size) ) &&               \
      CC_POS_IS_LEGAL( (move_ctx).starting, (board_size) ) &&                   \
      CC_POS_IS_LEGAL( (move_ctx).current, (board_size) ) &&                    \
      CC_POS_DESC_IS_LEGAL( (move_ctx).pawn_sacrifice_serpent, (board_size) ) )

//
// Ply context.

typedef struct CcPlyContext {
    CcPosDesc initial;
    CcPos starting;
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
// Path context.

typedef struct CcPathContext {
    CcGame * game;
    // CcChessboard * cb_old;
    CcChessboard * cb_current;

    CcMoveContext move_ctx;
    CcPlyContext ply_ctx;
} CcPathContext;

CcPathContext * cc_path_context__new( CcGameStatusEnum status,
                                      CcVariantEnum ve,
                                      bool do_setup );

bool cc_path_context_free_all( CcPathContext ** path_ctx__f );


#endif /* __CC_PATH_DEFS_H__ */
