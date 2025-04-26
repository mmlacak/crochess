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

// todo :: DOCS

//
// Move context.

typedef struct CcMoveContext {
    CcPosDesc initial_piece; // A piece starting current move, its initial position and tag.
    CcPos starting_pos; // Starting position, if different from initial, i.e. in case of repositioning.
    CcPos current_pos; // Current position of a piece which started current move.
    CcPosDesc pawn_sacrifice_serpent; // Serpent and its position where it initiated Pawn-sacrifice.
} CcMoveContext;

#define CC_MOVE_CONTEXT_INVALID { .initial_piece = CC_POS_DESC_CAST_INVALID,            \
                                  .starting_pos = CC_POS_CAST_INVALID,                  \
                                  .current_pos = CC_POS_CAST_INVALID,                   \
                                  .pawn_sacrifice_serpent = CC_POS_DESC_CAST_INVALID }

#define CC_MOVE_CONTEXT_CAST_INVALID ( (CcPlyContext)CC_MOVE_CONTEXT_INVALID )

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

#endif /* __CC_PATH_DEFS_H__ */
