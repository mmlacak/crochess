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
    CC_SDE_LeftDiagonal, // left == \ (upper-left <--> lower-right)
    CC_SDE_RightDiagonal, // right == / (upper-right <--> lower-left)
} CcSerpentDiagonalEnum;

#define CC_SERPENT_DIAGONAL_IS_ENUMERATOR(sde) ( ( CC_SDE_BothDiagonals <= (sde) ) && ( (sde) <= CC_SDE_RightDiagonal ) )

#define CC_SERPENT_DIAGONAL_IS_VALID(sde) CC_SERPENT_DIAGONAL_IS_ENUMERATOR(sde)

//
// Journey types

typedef enum CcMultiStagePlyTypeEnum {
    CC_MSPTE_None,

    // Trance-journey stages.
    CC_MSPTE_TJ_Entranced, // 2nd Shaman is entranced
    CC_MSPTE_TJ_Displacing, // light Shaman
    CC_MSPTE_TJ_Capturing, // dark Shaman
    CC_MSPTE_TJ_DoubleCapturing, // 2nd dark Shaman entranced

    // Sense-journey stages.
    CC_MSPTE_SJ_Initiated, // (2nd) Starchild is initiated
    CC_MSPTE_SJ_Viewing, // a piece goes sight-seeing

    // Pawn-sacrifice stages.
    CC_MSPTE_PS_RitualStarted, // Serpent activated a Pyramid is activated
    CC_MSPTE_PS_Sacrificed, // a Pawn was sacrificed
    CC_MSPTE_PS_CapturingPawns, // Serpent can capture (a few) opponent's Pawns
} CcMultiStagePlyTypeEnum;

#define CC_MULTI_STAGE_PLY_TYPE_IS_ENUMERATOR(mspte) ( ( CC_MSPTE_None <= (mspte) )                         \
                                                    && ( (mspte) <= CC_MSPTE_PS_CapturingPawns ) )

#define CC_MULTI_STAGE_PLY_TYPE_IS_VALID(mspte) ( ( CC_MSPTE_None < (mspte) )                               \
                                               && ( (mspte) <= CC_MSPTE_PS_CapturingPawns ) )

#define CC_MULTI_STAGE_PLY_TYPE_IS_TRANCE_JOURNEY(mspte) ( ( (mspte) == CC_MSPTE_TJ_Displacing )            \
                                                        || ( (mspte) == CC_MSPTE_TJ_Capturing )             \
                                                        || ( (mspte) == CC_MSPTE_TJ_DoubleCapturing ) )

#define CC_MULTI_STAGE_PLY_TYPE_IS_TRANCE_CAPTURE(mspte) ( ( (mspte) == CC_MSPTE_TJ_Capturing )             \
                                                        || ( (mspte) == CC_MSPTE_TJ_DoubleCapturing ) )

//
// Typed step

#define CC_STEP_TYPE_CHAR_NONE '0'
#define CC_STEP_TYPE_CHAR_INVALID '?'

#define CC_STEP_TYPE_CHAR_MOVEMENT_ONLY '-'
#define CC_STEP_TYPE_CHAR_MOVEMENT_OR_CAPTURE '#'
#define CC_STEP_TYPE_CHAR_CAPTURE_ONLY '*'

#define CC_STEP_TYPE_CHAR_DISPLACEMENT '<'
#define CC_STEP_TYPE_CHAR_COLOR_CHANGE '%'
#define CC_STEP_TYPE_CHAR_ENTRANCEMENT '@'
#define CC_STEP_TYPE_CHAR_UPLIFTING '^'
#define CC_STEP_TYPE_CHAR_MIRACLE '"'

typedef enum CcStepTypeEnum {
    CC_STE_None = 0,
    CC_STE_MovementOnly, // 1
    CC_STE_MovementOrCapture, // 2
    CC_STE_CaptureOnly, // 3
    CC_STE_Displacement, // 4
    CC_STE_ColorChange, // 5
    CC_STE_Entrancement, // 6
    CC_STE_Uplifting, // 7
    CC_STE_Miracle, // 8
} CcStepTypeEnum;

#define CC_STEP_TYPE_IS_ENUMERATOR(ste) ( ( CC_STE_None <= (ste) ) && ( (ste) <= CC_STE_Miracle ) )

#define CC_STEP_TYPE_IS_VALID(ste) ( ( CC_STE_None < (ste) ) && ( (ste) <= CC_STE_Miracle ) )

#define CC_STEP_TYPE_IS_MOVEMENT(ste) ( ( (ste) == CC_STE_MovementOnly ) || ( (ste) == CC_STE_MovementOrCapture ) )

#define CC_STEP_TYPE_IS_CAPTURE(ste) ( ( (ste) == CC_STE_MovementOrCapture ) || ( (ste) == CC_STE_CaptureOnly ) )

char cc_step_type_as_char( CcStepTypeEnum ste );

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

bool cc_typed_step_link_are_all_valid( CcTypedStepLink * ts_link );

size_t cc_typed_step_link_len( CcTypedStepLink * ts_link );

char * cc_typed_step_link_to_string__new( CcTypedStepLink * ts_link );

// todo :: DELETE
// //
// // Typed step definition

// // todo :: DOCS
// typedef enum CcTypedStepDefEnum {
//     CC_TSDE_One = 0,
//     CC_TSDE_Two,
//     CC_TSDE_Many,
// } CcTypedStepDefEnum;

// // todo :: DOCS
// #define CC_TYPED_STEP_DEF_IS_ENUMERATOR(tsde) ( ( CC_TSDE_One <= (tsde) ) && ( (tsde) <= CC_TSDE_Many ) )

// // todo :: DOCS
// #define CC_TYPED_STEP_DEF_IS_VALID(tsde) ( CC_TYPED_STEP_DEF_IS_ENUMERATOR(tsde) ) // ( ( CC_TSDE_One <= (tsde) ) && ( (tsde) <= CC_TSDE_Many ) )

// // todo :: DOCS
// typedef struct CcTypedStepDef {
//     CcTypedStepDefEnum type;

//     union {
//         CcTypedStep step;

//         struct {
//             CcTypedStep step_1;
//             CcTypedStep step_2;
//         };

//         CcTypedStepLink * steps;
//     };
// } CcTypedStepDef;

// // todo :: DOCS
// bool cc_typed_step_def( CcTypedStep step__d,
//                         CcTypedStep step_2__d,
//                         CcTypedStepLink ** steps__d_n,
//                         CcTypedStepDef * step_def__o );

// bool cc_typed_step_def_unpack( CcTypedStepDef * step_def,
//                                CcTypedStep * step__o,
//                                CcTypedStep * step_2__o,
//                                CcTypedStepLink ** steps__o_w );

// // Convenience functions.

// // todo :: DOCS
// bool cc_typed_step_def_one( CcTypedStep step, CcTypedStepDef * step_def__o );

// // todo :: DOCS
// bool cc_typed_step_def_two( CcTypedStep step, CcTypedStep step_2, CcTypedStepDef * step_def__o );

// // todo :: DOCS
// bool cc_typed_step_def_many( CcTypedStepLink ** steps__n, CcTypedStepDef * step_def__o );
// todo :: DELETE


#endif /* __CC_TYPED_STEP_H__ */
