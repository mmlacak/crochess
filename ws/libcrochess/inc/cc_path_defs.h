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
// Path context.

typedef struct CcPlyContext {
    CcPosDesc initial;
    CcPos starting;
    CcActivationDesc activation;
    CcTypedStep step_1;
    CcTypedStep step_2;
} CcPlyContext;

#define CC_PLY_CONTEXT_INVALID { .initial = CC_POS_DESC_CAST_INVALID,               \
                                 .starting = CC_POS_CAST_INVALID,                   \
                                 .activation = CC_ACTIVATION_DESC_CAST_SPENT,       \
                                 .step_1 = CC_TYPED_STEP_CAST_INVALID,              \
                                 .step_2 = CC_TYPED_STEP_CAST_INVALID }

#define CC_PLY_CONTEXT_CAST_INVALID ( (CcPlyContext)CC_PLY_CONTEXT_INVALID )

#endif /* __CC_PATH_DEFS_H__ */
