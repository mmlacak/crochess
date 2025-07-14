// Copyright (c) 2021, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TYPED_STEP_H__
#define __CC_TYPED_STEP_H__

#include <stdbool.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"


//
// Serpent diagonals

typedef enum CcSerpentDiagonalEnum {
    CC_SDE_BothDiagonals = 0,
    CC_SDE_LeftDiagonal,
    CC_SDE_RightDiagonal,
} CcSerpentDiagonalEnum;

#define CC_SERPENT_DIAGONAL_IS_ENUMERATOR(sde) ( ( CC_SDE_BothDiagonals <= (sde) ) && ( (sde) <= CC_SDE_RightDiagonal ) )

#define CC_SERPENT_DIAGONAL_IS_VALID(sde) CC_SERPENT_DIAGONAL_IS_ENUMERATOR(sde)

//
// Journey types

typedef enum CcJourneyTypeEnum {
    CC_JTE_None,
    CC_JTE_Displacement, // trance-journey, light Shaman
    CC_JTE_Capture, // trance-journey, dark Shaman
    CC_JTE_DoubleCapture, // trance-journey, two dark Shamans
    CC_JTE_Viewing, // sense-journey
} CcJourneyTypeEnum;

#define CC_JOURNEY_TYPE_IS_ENUMERATOR(jte) ( ( CC_JTE_None <= (jte) ) && ( (jte) <= CC_JTE_Viewing ) )

#define CC_JOURNEY_TYPE_IS_VALID(jte) ( ( CC_JTE_None < (jte) ) && ( (jte) <= CC_JTE_Viewing ) )

#define CC_JOURNEY_TYPE_IS_ANY_CAPTURE(jte) ( ( (jte) == CC_JTE_Capture ) \
                                           || ( (jte) == CC_JTE_DoubleCapture ) )

//
// Typed step

typedef enum CcStepTypeEnum {
    CC_STE_None = 0,
    CC_STE_MovementOnly,
    CC_STE_CaptureOrMovement,
    CC_STE_CaptureOnly,
    CC_STE_Displacement,
    CC_STE_ColorChange,
    CC_STE_Entrancement,
    CC_STE_Uplifting,
    CC_STE_Miracle,
} CcStepTypeEnum;

#define CC_STEP_TYPE_IS_ENUMERATOR(ste) ( ( CC_STE_None <= (ste) ) && ( (ste) <= CC_STE_Miracle ) )

#define CC_STEP_TYPE_IS_VALID(ste) ( ( CC_STE_None < (ste) ) && ( (ste) <= CC_STE_Miracle ) )

#define CC_STEP_TYPE_IS_MOVEMENT(ste) ( ( (ste) == CC_STE_MovementOnly ) || ( (ste) == CC_STE_CaptureOrMovement ) )

#define CC_STEP_TYPE_IS_CAPTURE(ste) ( ( (ste) == CC_STE_CaptureOrMovement ) || ( (ste) == CC_STE_CaptureOnly ) )

#define CC_TYPED_STEP_INVALID { .step = CC_POS_INVALID, .type = CC_STE_None }

#define CC_TYPED_STEP_STATIC { .step = CC_POS_STATIC_STEP, .type = CC_STE_None }

typedef struct CcTypedStep {
    CcPos step; /* Step, relative position. */
    CcStepTypeEnum type; /* Type of a step. */
} CcTypedStep;

#define CC_TYPED_STEP_CAST_INVALID ( (CcTypedStep)CC_TYPED_STEP_INVALID )

#define CC_TYPED_STEP_CAST_STATIC ( (CcTypedStep)CC_TYPED_STEP_STATIC )

#define CC_TYPED_STEP_IS_VALID(ts) ( CC_POS_IS_VALID( (ts).step ) && CC_STEP_TYPE_IS_VALID( (ts).type ) )

#define CC_TYPED_STEP_IS_EQUAL(ts_1,ts_2) ( CC_POS_IS_EQUAL( (ts_1).step, (ts_2).step ) && ( (ts_1).type == (ts_2).type ) )

#define CC_TYPED_STEP(int_i,int_j,enum_type) { .step = CC_POS_CAST( (int_i), (int_j) ), .type = (CcStepTypeEnum)(enum_type) }

#define CC_TYPED_STEP_CAST(int_i,int_j,enum_type) ( (CcTypedStep)CC_TYPED_STEP( (int_i), (int_j), (enum_type) ) )

CcTypedStep cc_typed_step( CcPos step, CcStepTypeEnum type );

bool cc_typed_step_is_equal( CcTypedStep ts_1, CcTypedStep ts_2 );

//
// Linked typed steps.

typedef struct CcTypedStepLink {
    CcTypedStep step;
    struct CcTypedStepLink * next;
} CcTypedStepLink;

CcTypedStepLink * cc_typed_step_link__new( CcTypedStep step );

CcTypedStepLink * cc_typed_step_link_append( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStep step );

CcTypedStepLink * cc_typed_step_link_extend( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStepLink ** ts_link__n );

bool cc_typed_step_link_free_all( CcTypedStepLink ** ts_link__f );

size_t cc_typed_step_link_len( CcTypedStepLink * ts_link );

char * cc_typed_step_link_to_string__new( CcTypedStepLink * ts_link );


#endif /* __CC_TYPED_STEP_H__ */
