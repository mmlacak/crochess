// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_H__
#define __CC_POS_H__

#include <stdbool.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"


//
// Position.

#define CC_POS_INVALID { .i = CC_INVALID_COORD, .j = CC_INVALID_COORD }

#define CC_POS_STATIC_STEP { .i = 0, .j = 0 }

#define CC_POS_ORIGIN_FIELD { .i = 0, .j = 0 }

typedef struct CcPos {
    int i; /* File, horizontal coordinate. */
    int j; /* Rank, vertical coordinate. */
} CcPos;

#define CC_POS_CAST_INVALID ( (CcPos)CC_POS_INVALID )

#define CC_POS_CAST_STATIC_STEP ( (CcPos)CC_POS_STATIC_STEP )

#define CC_POS_CAST_ORIGIN_FIELD ( (CcPos)CC_POS_ORIGIN_FIELD )

#define CC_POS(int_i,int_j) { .i = (int)(int_i), .j = (int)(int_j) }

#define CC_POS_CAST(int_i,int_j) ( (CcPos)CC_POS( int_i, int_j ) )

#define CC_POS_IS_VALID(pos) ( CC_IS_COORD_2_VALID( (pos).i, (pos).j ) )

#define CC_POS_IS_STATIC_STEP(pos) ( ( (pos).i == 0 ) && ( (pos).j == 0 ) )

#define CC_POS_IS_DISAMBIGUATION(pos) ( CC_IS_COORD_VALID( (pos).i ) || CC_IS_COORD_VALID( (pos).j ) )

#define CC_POS_IS_PARTIAL(pos) ( CC_XOR( CC_IS_COORD_VALID( (pos).i ), CC_IS_COORD_VALID( (pos).j ) ) )

#define CC_POS_IS_EQUAL(pos_1,pos_2) ( ( (pos_1).i == (pos_2).i ) && ( (pos_1).j == (pos_2).j ) )


CcPos cc_pos( int i, int j );

bool cc_pos_is_valid( CcPos pos );

bool cc_pos_is_static_step( CcPos pos );

bool cc_pos_is_disambiguation( CcPos pos );

bool cc_pos_is_partial( CcPos pos );

bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 );

bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 );

CcPos cc_pos_add( CcPos pos, CcPos step, int count );

CcPos cc_pos_difference( CcPos start, CcPos destination );

CcPos cc_pos_calc_step( CcPos start, CcPos destination );

bool cc_pos_to_string( CcPos pos, cc_char_8 * pos_str__o );

//
// Typed step

// TODO :: DOCS
typedef enum CcStepTypeEnum {
    CC_STE_None = 0, /* Undefined step type. */
    CC_STE_MovementOnly, /* Just a step, movement. It can still cause side-effects other than capture. */
    CC_STE_CaptureOrMovement,  /* Capturing step, i.e. movement with or without capture. */
    CC_STE_CaptureOnly,  /* Capturing step, i.e. movement only if piece can capture. */

    CC_STE_Displacement,
    CC_STE_ColorChange,
    CC_STE_Entrancement,
    CC_STE_Uplifting,
    CC_STE_Miracle,
    // CC_STE_Alternative, /* Alternative step; one of displacement-, color-change-, entrancement-, uplifting-, miracle-steps. */
} CcStepTypeEnum;
// TODO :: maybe split alternative steps, each into its own proper (?)

#define CC_STEP_TYPE_IS_ENUMERATOR(ste) ( ( CC_STE_None <= (ste) ) && ( (ste) <= CC_STE_Miracle ) )

#define CC_STEP_TYPE_IS_VALID(ste) ( ( CC_STE_None < (ste) ) && ( (ste) <= CC_STE_Miracle ) )

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

//
// Position descriptor.

#define CC_POS_DESC_INVALID { .pos = CC_POS_INVALID, .piece = CC_PE_None, .tag = CC_TE_None, .momentum = 0 }

#define CC_POS_DESC_STATIC_STEP { .pos = CC_POS_STATIC_STEP, .piece = CC_PE_None, .tag = CC_TE_None, .momentum = 0 }

typedef struct CcPosDesc {
    CcPos pos; /* A position. */
    CcPieceType piece; /* Piece found at position. */
    CcTagType tag; /* Tag found at position. */
    cc_uint_t momentum; /* Momentum a moving piece (different from static piece found at this position!) had when this position was reached. */
} CcPosDesc;

#define CC_POS_DESC_CAST_INVALID ( (CcPosDesc)CC_POS_DESC_INVALID )

#define CC_POS_DESC_CAST_STATIC_STEP ( (CcPosDesc)CC_POS_DESC_STATIC_STEP )

#define CC_POS_DESC(int_i,int_j,piece_enum,tag_enum,uint_momentum) \
    { .pos = CC_POS_CAST( (int_i), (int_j) ), .piece = (CcPieceType)(piece_enum), .tag = (CcTagType)(tag_enum), .momentum = (cc_uint_t)(uint_momentum) }

#define CC_POS_DESC_CAST(int_i,int_j,piece_enum,tag_enum,uint_momentum) \
    ( (CcPosDesc)CC_POS_DESC( (int_i), (int_j), (piece_enum), (tag_enum), (uint_momentum) ) )

#define CC_POS_DESC_IS_VALID(pd) \
    ( CC_POS_IS_VALID( (pd).pos ) && CC_PIECE_IS_ENUMERATOR( (pd).piece ) && ( CC_TAG_IS_ENUMERATOR( (pd).tag ) ) )
    // <!> Do not use CC_PIECE_IS_VALID(), CC_TAG_IS_VALID(), having no piece, tag is still valid position descriptor!

#define CC_POS_DESC_IS_EQUAL(pd_1,pd_2) \
    ( CC_POS_IS_EQUAL( (pd_1).pos, (pd_2).pos ) && ( (pd_1).piece == (pd_2).piece ) && ( (pd_1).tag == (pd_2).tag ) && ( (pd_1).momentum == (pd_2).momentum ) )

CcPosDesc cc_pos_desc( CcPos pos, CcPieceType piece, CcTagType tag, cc_uint_t momentum );

bool cc_pos_desc_is_valid( CcPosDesc pd );

bool cc_pos_desc_is_equal( CcPosDesc pd_1, CcPosDesc pd_2 );

bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2 );

bool cc_pos_desc_to_string( CcPosDesc pd,
                                  cc_char_16 * pd_str__o );


#endif /* __CC_POS_H__ */
