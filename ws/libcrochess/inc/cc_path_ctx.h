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

// TODO :: DOCS

//
// Move context.

typedef struct CcMoveContext {
    CcPosDesc initial; // A piece starting current move, its initial position and tag. // TODO :: not really needed
    CcPos current; // Current position of a piece which started current move. // TODO :: not really needed
    CcMultiStagePlyTypeEnum multi_stage; // Current stage in multi-stage (sub-)cascade.
    CcPosDesc pawn_sacrifice_serpent; // Serpent and its position where it initiated Pawn-sacrifice.
} CcMoveContext;

#define CC_MOVE_CONTEXT_INVALID { .initial = CC_POS_DESC_CAST_INVALID,                  \
                                  .current = CC_POS_CAST_INVALID,                       \
                                  .multi_stage = CC_MSPTE_None,                         \
                                  .pawn_sacrifice_serpent = CC_POS_DESC_CAST_INVALID }

#define CC_MOVE_CONTEXT_CAST_INVALID ( (CcMoveContext)CC_MOVE_CONTEXT_INVALID )

#define CC_MOVE_CONTEXT_IS_VALID(move_ctx)                                      \
    ( CC_POS_DESC_IS_VALID( (move_ctx).initial ) &&                             \
      CC_POS_IS_VALID( (move_ctx).current ) &&                                  \
      CC_MULTI_STAGE_PLY_TYPE_IS_ENUMERATOR( (move_ctx).multi_stage ) &&        \
      CC_POS_DESC_IS_VALID( (move_ctx).pawn_sacrifice_serpent ) )

#define CC_MOVE_CONTEXT_IS_LEGAL(move_ctx,board_size)                           \
    ( CC_POS_DESC_IS_LEGAL( (move_ctx).initial, (board_size) ) &&               \
      CC_POS_IS_LEGAL( (move_ctx).current, (board_size) ) &&                    \
      CC_MULTI_STAGE_PLY_TYPE_IS_ENUMERATOR( (move_ctx).multi_stage ) &&        \
      CC_POS_DESC_IS_LEGAL( (move_ctx).pawn_sacrifice_serpent, (board_size) ) )

//
// Ply context.

typedef struct CcPlyContext {
    CcPosDesc initial; // Initial position, piece (and its tag), at the start of a ply.
    CcPos starting; // Starting position, if different from initial, i.e. in case of repositioning.

    // TODO :: DOCS :: Cached, reference data to be stored in CcPathLink->act_desc while move, plies progresses.
    CcActivationDesc act_desc; // <i> Cached, reference data to be stored in CcPathLink->act_desc while move, plies progresses.

    bool is_first;
} CcPlyContext;

#define CC_PLY_CONTEXT_INVALID { .initial = CC_POS_DESC_CAST_INVALID,               \
                                 .starting = CC_POS_CAST_INVALID,                   \
                                 .act_desc = CC_ACTIVATION_DESC_CAST_SPENT,         \
                                 .is_first = false }

#define CC_PLY_CONTEXT_CAST_INVALID ( (CcPlyContext)CC_PLY_CONTEXT_INVALID )

#define CC_PLY_CONTEXT_IS_VALID(ply_ctx)                                            \
    ( CC_POS_DESC_IS_VALID( (ply_ctx).initial ) &&                                  \
      CC_POS_IS_VALID( (ply_ctx).starting ) &&                                      \
      cc_activation_desc_is_valid( (ply_ctx).act_desc, (ply_ctx).is_first ) )

#define CC_PLY_CONTEXT_IS_LEGAL(ply_ctx,board_size)                                 \
    ( CC_POS_DESC_IS_LEGAL( (ply_ctx).initial, (board_size) ) &&                    \
      CC_POS_IS_LEGAL( (ply_ctx).starting, (board_size) ) &&                        \
      cc_activation_desc_is_valid( (ply_ctx).act_desc, (ply_ctx).is_first ) )

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

// static bool _cc_path_context_init_move( CcPathContext * path_ctx__io,
//                                         CcPosDesc move_init );

// static bool _cc_path_context_init_ply( CcPathContext * path_ctx__io,
//                                        CcPosDesc ply_init );

// TODO :: DOCS :: either init move or init ply; but not both!
bool cc_path_context_init( CcPathContext * path_ctx__io,
                           CcPosDesc init_pos,
                           bool init_move_or_ply );


#endif /* __CC_PATH_DEFS_H__ */
