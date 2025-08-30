// Copyright (c) 2021, 2025 Mario Mlačak, mmlacak@gmail.com
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

#define CC_POS_IS_LEGAL(pos,board_size) ( CC_IS_POS_ON_BOARD( (board_size), (pos).i, (pos).j ) )

#define CC_POS_IS_LEGAL_DISAMBIGUATION(pos,board_size) ( CC_IS_DISAMBIGUATION_ON_BOARD( (board_size), (pos).i, (pos).j ) )


bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 );

CcPos cc_pos_add( CcPos pos, int i, int j );

CcPos cc_pos_add_steps( CcPos pos, CcPos step, int count );

CcPos cc_pos_difference( CcPos start, CcPos destination );

CcPos cc_pos_calc_step( CcPos start, CcPos destination );

bool cc_pos_are_same_color( CcPos start, CcPos destination );

bool cc_pos_piece_are_same_color( CcPos pos, CcPieceTagType piece );

bool cc_pos_step_is_short_jump( CcPos step );

bool cc_pos_step_is_long_jump( CcPos step );

bool cc_pos_to_string( CcPos pos, cc_char_8 * pos_str__o );

//
// Linked positions.

typedef struct CcPosLink {
    CcPos pos;
    struct CcPosLink * next;
} CcPosLink;

CcPosLink * cc_pos_link__new( CcPos pos );

CcPosLink * cc_pos_link_append( CcPosLink ** pos_link__iod_a,
                                CcPos pos );

CcPosLink * cc_pos_link_duplicate_all__new( CcPosLink * pos_link );

CcPosLink * cc_pos_link_extend( CcPosLink ** pos_link__iod_a,
                                CcPosLink ** pos_link__n );

bool cc_pos_link_free_all( CcPosLink ** pos_link__f );

size_t cc_pos_link_len( CcPosLink * pos_link );

char * cc_pos_link_to_string__new( CcPosLink * pos_link );

//
// Position descriptor.

#define CC_POS_DESC_INVALID { .pos = CC_POS_INVALID, .piece = CC_PTE_None }

#define CC_POS_DESC_STATIC_STEP { .pos = CC_POS_STATIC_STEP, .piece = CC_PTE_None }

typedef struct CcPosDesc {
    CcPos pos; /* A position. */
    CcPieceTagType piece; /* Piece found at position. */
} CcPosDesc;

#define CC_POS_DESC_CAST_INVALID ( (CcPosDesc)CC_POS_DESC_INVALID )

#define CC_POS_DESC_CAST_STATIC_STEP ( (CcPosDesc)CC_POS_DESC_STATIC_STEP )

#define CC_POS_DESC_COORDS(int_i,int_j,piece_enum)                  \
    { .pos = CC_POS_CAST( (int_i), (int_j) ),                       \
      .piece = (CcPieceTagType)(piece_enum) }

#define CC_POS_DESC_COORDS_CAST(int_i,int_j,piece_enum)             \
    ( (CcPosDesc)CC_POS_DESC_COORDS( (int_i), (int_j), (piece_enum) ) )

#define CC_POS_DESC(pos,piece_enum) { .pos = (pos), .piece = (CcPieceTagType)(piece_enum) }

#define CC_POS_DESC_CAST(pos,piece_enum) ( (CcPosDesc){ .pos = (pos), .piece = (CcPieceTagType)(piece_enum) } )

#define CC_POS_DESC_IS_VALID(pd) \
    ( CC_POS_IS_VALID( (pd).pos ) && CC_PIECE_IS_ENUMERATOR( (pd).piece ) ) )
    // <!> Do not use CC_PIECE_IS_VALID(); having no piece, tag is still valid position descriptor!

#define CC_POS_DESC_IS_LEGAL(pd,board_size) \
    ( CC_POS_IS_LEGAL( (pd).pos, board_size ) && CC_PIECE_IS_ENUMERATOR( (pd).piece ) )
    // <!> Do not use CC_PIECE_IS_VALID(); having no piece, tag is still valid position descriptor!

#define CC_POS_DESC_IS_EQUAL(pd_1,pd_2)                 \
    ( CC_POS_IS_EQUAL( (pd_1).pos, (pd_2).pos ) &&      \
      ( (pd_1).piece == (pd_2).piece ) )

bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2, bool compare_only_piece_types );

bool cc_pos_desc_to_string( CcPosDesc pd,
                            cc_char_16 * pd_str__o );

//
// Linked position descriptor.

typedef struct CcPosDescLink {
    CcPosDesc pd;
    struct CcPosDescLink * next;
} CcPosDescLink;

CcPosDescLink * cc_pos_desc_link__new( CcPosDesc pd );

CcPosDescLink * cc_pos_desc_link_append( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDesc pd );

CcPosDescLink * cc_pos_desc_link_duplicate_all__new( CcPosDescLink * pd_link );

CcPosDescLink * cc_pos_desc_link_extend( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDescLink ** pd_link__n );

bool cc_pos_desc_link_free_all( CcPosDescLink ** pd_link__f );

size_t cc_pos_desc_link_len( CcPosDescLink * pd_link );

char * cc_pos_desc_link_to_string__new( CcPosDescLink * pd_link );

//
// Momentum.

typedef enum CcMomentumUsageEnum {
    CC_MUE_NotUsing = 0,
    CC_MUE_Accumulating,
    CC_MUE_Spending,
} CcMomentumUsageEnum;

#define CC_MOMENTUM_USAGE_IS_ENUMERATOR(mue) ( ( CC_MUE_NotUsing <= (mue) ) && ( (mue) <= CC_MUE_Spending ) )

#define CC_MOMENTUM_USAGE_IS_VALID(mue) CC_MOMENTUM_USAGE_IS_ENUMERATOR(mue) // ( ( CC_MUE_NotUsing < (mue) ) && ( (mue) <= CC_MUE_Spending ) )

CcMaybeBoolEnum cc_calc_momentum( CcMomentumUsageEnum usage,
                                  cc_uint_t count,
                                  cc_uint_t * momentum__io );

char cc_momentum_usage_as_char( CcMomentumUsageEnum usage );

//
// Activation descriptor.

typedef struct CcActivationDesc {
    CcPieceTagType activator;
    cc_uint_t momentum;
    CcMomentumUsageEnum usage;
} CcActivationDesc;

#define CC_ACTIVATION_DESC_INITIAL { .activator = CC_PTE_None, .momentum = 0, .usage = CC_MUE_Accumulating }

#define CC_ACTIVATION_DESC_STATIC { .activator = CC_PTE_None, .momentum = 0, .usage = CC_MUE_NotUsing }

#define CC_ACTIVATION_DESC_SPENT { .activator = CC_PTE_None, .momentum = 0, .usage = CC_MUE_Spending }

#define CC_ACTIVATION_DESC_CAST_INITIAL ( (CcActivationDesc)CC_ACTIVATION_DESC_INITIAL )

#define CC_ACTIVATION_DESC_CAST_STATIC ( (CcActivationDesc)CC_ACTIVATION_DESC_STATIC )

#define CC_ACTIVATION_DESC_CAST_SPENT ( (CcActivationDesc)CC_ACTIVATION_DESC_SPENT )

#define CC_ACTIVATION_DESC_IS_EQUAL(ad_1,ad_2) ( ( (ad_1).activator == (ad_2).activator ) && \
                                                 ( (ad_1).momentum == (ad_2).momentum ) && \
                                                 ( (ad_1).usage == (ad_2).usage ))

bool cc_activation_desc_is_valid( CcActivationDesc act_desc,
                                  CcPieceTagType moving,
                                  bool is_first_ply );

CcMaybeBoolEnum cc_activation_desc_calc_momentum( CcActivationDesc * act_desc__io,
                                                  cc_uint_t count );

bool cc_activation_desc_update_activator( CcActivationDesc * act_desc__io,
                                          CcPieceTagType moving,
                                          bool is_first_ply,
                                          CcPieceTagType new_activator );

bool cc_activation_desc_is_usable( CcActivationDesc act_desc,
                                   CcPieceTagType moving,
                                   bool is_first_ply );

bool cc_activation_desc_as_string( CcActivationDesc act_desc,
                                   cc_char_32 * act_dest_str__o );


#endif /* __CC_POS_H__ */
