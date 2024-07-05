// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_H__
#define __CC_POS_H__

#include <stdbool.h>

#include "cc_defines.h"
#include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"

/**
    @file cc_pos.h
    @brief Position, position + piece, linked list of position + piece.
*/


//
// Position.

/**
    Invalid position value.
*/
#define CC_POS_INVALID { .i = CC_INVALID_COORD, .j = CC_INVALID_COORD }

/**
    Static position value, i.e. no-movement step.
*/
#define CC_POS_STATIC_STEP { .i = 0, .j = 0 }

/**
    Origin field, i.e. coordinate system start.
*/
#define CC_POS_ORIGIN_FIELD { .i = 0, .j = 0 }

/**
    Structure holding a position, either absolute or relative,
    i.e. either a location or a step.
*/
typedef struct CcPos {
    int i; /**< File, horizontal coordinate. */
    int j; /**< Rank, vertical coordinate. */
} CcPos;

/**
    Casted invalid position value.
*/
#define CC_POS_CAST_INVALID ( (CcPos)CC_POS_INVALID )

/**
    Casted static position value, i.e. no-movement step.
*/
#define CC_POS_CAST_STATIC_STEP ( (CcPos)CC_POS_STATIC_STEP )

/**
    Casted origin field, i.e. coordinate system start.
*/
#define CC_POS_CAST_ORIGIN_FIELD ( (CcPos)CC_POS_ORIGIN_FIELD )

/**
    Macro definition for a position.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @return Position with a given coordinates.
*/
#define CC_POS(int_i,int_j) { .i = (int)(int_i), .j = (int)(int_j) }

/**
    Macro definition for a casted position.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @return Casted position with a given coordinates.
*/
#define CC_POS_CAST(int_i,int_j) ( (CcPos)CC_POS( int_i, int_j ) )

/**
    Macro expression to evaluate whether given position is valid.

    @param pos A position.

    @note
    Position is valid if both coordinates are valid.

    @see CcPos

    @return `true` if valid position, `false` otherwise.
*/
#define CC_POS_IS_VALID(pos) ( CC_IS_COORD_2_VALID( (pos).i, (pos).j ) )

/**
    Macro expression to evaluate whether given position is static step.

    @param pos A position.

    @see CcPos

    @return `true` if static step, `false` otherwise.
*/
#define CC_POS_IS_STATIC_STEP(pos) ( ( (pos).i == 0 ) && ( (pos).j == 0 ) )

/**
    Macro expression to evaluate whether given position is disambiguation.

    @param pos A position.

    @note
    Disambiguation is any position with at least one valid coordinate.

    @see CcPos

    @return `true` if disambiguation, `false` otherwise.
*/
#define CC_POS_IS_DISAMBIGUATION(pos) ( CC_IS_COORD_VALID( (pos).i ) || CC_IS_COORD_VALID( (pos).j ) )

/**
    Macro expression to evaluate whether given position is partial.

    @param pos A position.

    @note
    Partial position is any which have exactly one valid coordinate.

    @see CcPos

    @return `true` if partial, `false` otherwise.
*/
#define CC_POS_IS_PARTIAL(pos) ( CC_XOR( CC_IS_COORD_VALID( (pos).i ), CC_IS_COORD_VALID( (pos).j ) ) )

/**
    Macro expression to evaluate whether given positions are equal.

    @param pos_1 A position.
    @param pos_2 Other position.

    @see CcPos

    @return `true` if equal, `false` otherwise.
*/
#define CC_POS_IS_EQUAL(pos_1,pos_2) ( ( (pos_1).i == (pos_2).i ) && ( (pos_1).j == (pos_2).j ) )


/**
    Function returns a position.

    @param i File, horizontal coordinate.
    @param j Rank, vertical coordinate.

    @return Positions with a given coordinates.
*/
CcPos cc_pos( int i, int j );

/**
    Function checks if position is valid.

    @param pos A position.

    @see CC_POS_INVALID

    @return `true` if position is valid, `false` otherwise.
*/
bool cc_pos_is_valid( CcPos pos );

/**
    Function checks if position is static step.

    @param pos A position.

    @see CC_POS_STATIC_STEP

    @return `true` if position is static step, `false` otherwise.
*/
bool cc_pos_is_static_step( CcPos pos );

/**
    Function checks if position is disambiguation.

    @param pos A position.

    @note
    Disambiguation is a position with at least one known (valid) coordinate,
    and the other might be unknown (invalid).

    @return `true` if position is a disambiguation, `false` otherwise.
*/
bool cc_pos_is_disambiguation( CcPos pos );

/**
    Function checks if position is partial.

    @param pos A position.

    @note
    Partial position has one known (valid) coordinate,
    and the other is unknown (invalid).

    @return `true` if position is partial, `false` otherwise.
*/
bool cc_pos_is_partial( CcPos pos );

/**
    Function checks if two positions are the same.

    @param pos_1 A position.
    @param pos_2 An other position.

    @return `true` if positions are the same, `false` otherwise.
*/
bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 );

/**
    Function checks if two positions are congruent.

    @param pos_1 A position.
    @param pos_2 An other position.

    @note
    For positions to be congruent, at least one set of coordinates (files,
    or ranks) from both positions has to be valid, and the same.

    @return `true` if positions are congruent, `false` otherwise.
*/
bool cc_pos_is_congruent( CcPos pos_1, CcPos pos_2 );

/**
    Function adds step to position.

    @param pos A position to add to.
    @param step A step to be added.
    @param count Count of steps to be added.

    @note
    Function adds valid coordinates, if both `pos` and `step` arguments are one of:
    - valid positions
    - file disambiguations
    - rank disambiguations.

    @note
    If `pos` and `step` have no common valid coordinates, result is invalid position,
    e.g. if a rank disambiguation is added to a file disambiguation.

    @return A position with added step if successful, invalid position otherwise.
*/
CcPos cc_pos_add( CcPos pos, CcPos step, int count );

/**
    Function returns difference between two given positions.

    @param start Starting position.
    @param destination Destination field.

    @note
    Function subtracts valid coordinates, if both positions are one of:
    - valid positions
    - file disambiguations
    - rank disambiguations.

    @note
    If given positions have no common valid coordinates, result is invalid position,
    e.g. if a rank disambiguation is subtracted from a file disambiguation.

    @return A position difference if successful, invalid position otherwise.
*/
CcPos cc_pos_difference( CcPos start, CcPos destination );

/**
    Function returns step from start to destination field.

    @param start Starting from.
    @param destination Destination field.

    @note
    Returned step might not be legal step for any given piece,
    it's just a calculated value.

    @return A valid step if successful, invalid otherwise.
*/
CcPos cc_pos_calc_step( CcPos start, CcPos destination );

/**
    Function converts position into a user-readable `<file char><rank number>` notation.

    @param pos A position.
    @param pos_str__o An _output_ parameter, short string array.

    @note
    Coordinates outside chessboard are converted into short integers, if possible.

    @note
    If outside of 2 decimal places, coordinate is represented as asterisk.

    @return `true` if successful, `false` otherwise.
*/
bool cc_pos_to_short_string( CcPos pos, cc_char_8 * pos_str__o );

//
// Step + type

/**
    Step types enumeration.
*/
typedef enum CcStepTypeEnum {
    CC_STE_None = 0, /**< Undefined step type. */
    CC_STE_Movement, /**< Just a step, movement. It can still cause side-effects other than capture. */
    CC_STE_Capture,  /**< Capturing step, i.e. movement + capture. */
    CC_STE_Alternative, /**< Alternative step; one of color-change-, entrancement-, uplifting-, miracle-steps. */
} CcStepTypeEnum;
// TODO :: CC_STE_All --> convert CcStepTypeEnum into bit-field (?)

/**
    Invalid step + type value.
*/
#define CC_TYPED_STEP_INVALID { .step = CC_POS_INVALID, .type = CC_STE_None }

/**
    Static step + type value, i.e. no-movement step.
*/
#define CC_TYPED_STEP_STATIC { .step = CC_POS_STATIC_STEP, .type = CC_STE_Movement }

/**
    Structure holding a step and its type.
    Step is always relative to current position.
*/
typedef struct CcTypedStep {
    CcPos step; /**< Step, relative position. */
    CcStepTypeEnum type; /**< Type of a step. */
} CcTypedStep;

/**
    Casted invalid position descriptor value.
*/
#define CC_TYPED_STEP_CAST_INVALID ( (CcTypedStep)CC_TYPED_STEP_INVALID )

/**
    Casted static position descriptor value, i.e. no-movement step.
*/
#define CC_TYPED_STEP_CAST_STATIC ( (CcTypedStep)CC_TYPED_STEP_STATIC )

/**
    Macro expression to evaluate whether given typed step is valid.

    @param ts A typed step.

    @note
    Typed step is valid if both coordinates of a `step` member are valid,
    and `type` is not `CC_STE_None`.

    @see CcTypedStep

    @return `true` if valid typed step, `false` otherwise.
*/
#define CC_TYPED_STEP_IS_VALID(ts) ( CC_POS_IS_VALID( (ts).step ) && ( (ts).type != CC_STE_None ) )

/**
    Macro expression to evaluate whether given typed steps are equal.

    @param ts_1 A typed step.
    @param ts_2 An other typed step.

    @see CcTypedStep

    @return `true` if equal, `false` otherwise.
*/
#define CC_TYPED_STEP_IS_EQUAL(ts_1,ts_2) ( CC_POS_IS_EQUAL( (ts_1).step, (ts_2).step ) && ( (ts_1).type == (ts_2).type ) )

/**
    Function returns a step + type.

    @param step Step, relative position.
    @param type Type of a step.

    @return Step and its type.
*/
CcTypedStep cc_typed_step( CcPos step, CcStepTypeEnum type );

/**
    Macro definition for a typed step.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param enum_type Type of a step, one of `CcStepTypeEnum` values.

    @return Typed step with a given coordinates.
*/
#define CC_TYPED_STEP(int_i,int_j,enum_type) { .step = CC_POS_CAST( (int_i), (int_j) ), .type = (CcStepTypeEnum)(enum_type) }

/**
    Macro definition for a casted, typed step.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param enum_type Type of a step, one of `CcStepTypeEnum` values.

    @return Casted, typed step with a given coordinates.
*/
#define CC_TYPED_STEP_CAST(int_i,int_j,enum_type) ( (CcTypedStep)CC_TYPED_STEP( (int_i), (int_j), (enum_type) ) )

/**
    Function checks if two typed steps are the same.

    @param ts_1 A typed step.
    @param ts_2 An other typed step.

    @return `true` if typed steps are the same, `false` otherwise.
*/
bool cc_typed_step_is_equal( CcTypedStep ts_1, CcTypedStep ts_2 );


//
// Linked typed steps.

/**
    A linked list of positions.
*/
typedef struct CcTypedStepLink {
    CcTypedStep step; /**< A step + type. */
    struct CcTypedStepLink * next; /**< Link to next typed step. */
} CcTypedStepLink;

/**
    Function allocates a new linked typed step.

    @param step A typed step.

    @return Pointer to a newly allocated linked typed step if successful, `NULL` otherwise.
*/
CcTypedStepLink * cc_typed_step_link__new( CcTypedStep step );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param ts_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param step A typed step.

    @note
    Linked list `*ts_link__iod_a` can be `NULL`, a linked typed step will still be
    allocated, and weak pointer to it returned.

    @note
    If linked list `*ts_link__iod_a` is `NULL`, it will be initialized
    with a newly allocated linked typed step as its only element.

    @note
    Pointer `ts_link__iod_a` has ownership over given linked list, takes ownership
    over newly allocated typed step, and retains ownership after function returns.

    @return
    A weak pointer to a newly allocated linked typed step if successful,
    `NULL` otherwise.
*/
CcTypedStepLink * cc_typed_step_link_append( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStep step );

/**
    Extends existing linked list with a another linked list.

    @param ts_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param ts_link__n Linked list with which to extend existing steps.

    @note
    If linked list to extend (`ts_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    `ts_link__n`.

    @note
    Extending linked list `ts_link__n` has its ownership transferred to
    extended linked list `ts_link__iod_a`; as a result, inner pointer of
    `ts_link__n` is `NULL`-ed.

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcTypedStepLink * cc_typed_step_link_extend( CcTypedStepLink ** ts_link__iod_a,
                                             CcTypedStepLink ** ts_link__n );

/**
    Frees all typed steps in a linked list.

    @param ts_link__f Linked list of typed steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_typed_step_link_free_all( CcTypedStepLink ** ts_link__f );

/**
    Function returns length of a linked list.

    @param ts_link A linked list of typed steps.

    @return Length of a linked list if successful, `0` otherwise.
*/
size_t cc_typed_step_link_len( CcTypedStepLink * ts_link );

/**
    Function returns string containing user-readable representation of a linked typed steps.

    @param ts_link A linked list of typed steps.

    @see cc_pos_to_short_string()

    @return A newly allocated, zero-terminated string if successful, `NULL` otherwise.
*/
char * cc_typed_step_link_to_short_string__new( CcTypedStepLink * ts_link );


//
// Position descriptor.

/**
    Invalid position descriptor value.
*/
#define CC_POS_DESC_INVALID { .pos = CC_POS_INVALID, .piece = CC_PE_None, .tag = CC_TE_None, .momentum = 0 }

/**
    Static position descriptor value, i.e. no-movement step.
*/
#define CC_POS_DESC_STATIC_STEP { .pos = CC_POS_STATIC_STEP, .piece = CC_PE_None, .tag = CC_TE_None, .momentum = 0 }

/**
    Position descriptor; holding a position, and a piece and a tag found at it.
    Structure also contains momentum a moving piece had when this position was reached.

    @note
    Moving piece is separate from static piece found at this position.
*/
typedef struct CcPosDesc {
    CcPos pos; /**< A position. */
    CcPieceEnum piece; /**< Piece found at position. */
    CcTagEnum tag; /**< Tag found at position. */
    uint momentum; /**< Momentum a moving piece (different from static piece found at this position!) had when this position was reached. */
} CcPosDesc;

/**
    Casted invalid position descriptor value.
*/
#define CC_POS_DESC_CAST_INVALID ( (CcPosDesc)CC_POS_DESC_INVALID )

/**
    Casted static position descriptor value, i.e. no-movement step.
*/
#define CC_POS_DESC_CAST_STATIC_STEP ( (CcPosDesc)CC_POS_DESC_STATIC_STEP )

/**
    Macro which constructs position descriptor struct.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece_enum A piece.
    @param tag_enum A tag.
    @param uint_momentum Momentum.

    @see CcPosDesc

    @return Position descriptor value.
*/
#define CC_POS_DESC(int_i,int_j,piece_enum,tag_enum,uint_momentum) \
    { .pos = CC_POS_CAST( (int_i), (int_j) ), .piece = (CcPieceEnum)(piece_enum), .tag = (CcTagEnum)(tag_enum), .momentum = (uint)(uint_momentum) }

/**
    Macro which constructs casted position descriptor struct.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece_enum A piece.
    @param tag_enum A tag.
    @param uint_momentum Momentum.

    @see CcPosDesc

    @return Casted position descriptor value.
*/
#define CC_POS_DESC_CAST(int_i,int_j,piece_enum,tag_enum,uint_momentum) \
    ( (CcPosDesc)CC_POS_DESC( (int_i), (int_j), (piece_enum), (tag_enum), (uint_momentum) ) )

/**
    Macro expression to evaluate whether given position descriptor is valid.

    @param pd A position descriptor.

    @see CcPosDesc

    @return `true` if valid position descriptor, `false` otherwise.
*/
#define CC_POS_DESC_IS_VALID(pd) \
    ( CC_POS_IS_VALID( (pd).pos ) && CC_PIECE_IS_VALID( (pd).piece ) && ( CC_TAG_IS_VALID( (pd).tag ) ) )

/**
    Macro expression to evaluate whether given position descriptor is equal to another.

    @param pd_1 A position descriptor.
    @param pd_2 Other position descriptor.

    @see CcPosDesc

    @return `true` if equal, `false` otherwise.
*/
#define CC_POS_DESC_IS_EQUAL(pd_1,pd_2) \
    ( CC_POS_IS_EQUAL( (pd_1).pos, (pd_2).pos ) && ( (pd_1).piece == (pd_2).piece ) && ( (pd_1).tag == (pd_2).tag ) && ( (pd_1).momentum == (pd_2).momentum ) )

/**
    Function returns position descriptor value.

    @param pos A position.
    @param piece A piece.
    @param tag A  tag.

    @return Position descriptor value.
*/
CcPosDesc cc_pos_desc( CcPos pos, CcPieceEnum piece, CcTagEnum tag );

/**
    Function checks if position + piece is valid.

    @param pd A position + piece.

    @see CC_POS_INVALID

    @return `true` if position + piece is valid, `false` otherwise.
*/
bool cc_pos_desc_is_valid( CcPosDesc pd );

/**
    Function checks if two position + piece values are the same.

    @param pd_1 A position + piece.
    @param pd_2 An other position + piece.

    @return `true` if position + piece values are the same, `false` otherwise.
*/
bool cc_pos_desc_is_equal( CcPosDesc pd_1, CcPosDesc pd_2 );

/**
    Function checks if two position + piece values are the congruent.

    @param pd_1 A position + piece.
    @param pd_2 An other position + piece.

    @note
    For positions to be congruent, at least one set of coordinates (files,
    or ranks) from both positions has to be valid, and the same.

    @note
    For pieces to be congruent, they have to be valid, and the same type,
    e.g two Rooks.

    @return `true` if positions are congruent, `false` otherwise.
*/
bool cc_pos_desc_is_congruent( CcPosDesc pd_1, CcPosDesc pd_2 );

/**
    Function converts position + piece value into a user-readable
    `<file char><rank number><piece>` notation.

    @param pd A position + piece.
    @param pd_str__o An _output_ parameter, short string array.

    @note
    Coordinates outside chessboard are converted into short integers, if possible.

    @note
    If outside of 2 decimal places, coordinate is represented as asterisk.

    @return `true` if successful, `false` otherwise.
*/
bool cc_pos_desc_to_short_string( CcPosDesc pd,
                                  cc_char_16 * pd_str__o );


//
// Linked list of positions + pieces + tags.

/**
    Convenience macro to allocate new position descriptor value to position link.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece A piece.
    @param tag A tag.
    @param momentum A momentum.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_desc_link__new()
*/
#define CC_POS_DESC_LINK__NEW(int_i,int_j,piece,tag,momentum) \
    ( cc_pos_desc_link__new( CC_POS_DESC_CAST( (int_i), (int_j), (piece), (tag), (momentum) ) ) )

/**
    Macro to append a newly allocated position descriptor value to position link.

    @param ptr_ptr__pd_link__iod _Optional_, _input/output_ parameter; a position linked list.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece A piece.
    @param tag A tag.
    @param momentum A momentum.

    @see cc_pos_desc_link_append()

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
#define CC_POS_DESC_LINK_APPEND(ptr_ptr__pd_link__iod,int_i,int_j,piece,tag,momentum) \
    ( cc_pos_desc_link_append( (ptr_ptr__pd_link__iod), CC_POS_DESC_CAST( (int_i), (int_j), (piece), (tag), (momentum) ) ) )

/**
    A linked list of position descriptors.
*/
typedef struct CcPosDescLink {
    CcPosDesc pd; /**< A position descriptor. */
    struct CcPosDescLink * next; /**< Link to next position. */
} CcPosDescLink;

/**
    Function allocates a new linked position.

    @param pd A position descriptor value.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosDescLink * cc_pos_desc_link__new( CcPosDesc pd );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param pd_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param pd A position descriptor value.

    @note
    Linked list `*pd_link__iod_a` can be `NULL`, a linked position will still be
    allocated, and weak pointer to it returned.

    @note
    If linked list `*pd_link__iod_a` is `NULL`, it will be initialized
    with a newly allocated linked position as its only element.

    @note
    Pointer `pd_link__iod_a` has ownership over given linked list, takes ownership
    over newly allocated position item, and retains ownership after function returns.

    @return
    A weak pointer to a newly allocated linked position if successful,
    `NULL` otherwise.
*/
CcPosDescLink * cc_pos_desc_link_append( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDesc pd );

/**
    Duplicates a given position descriptor linked list into a newly allocated.

    @param pd_link__io Linked list to duplicate.

    @return
    A pointer to newly allocated linked list if successful, `NULL` otherwise.
*/
CcPosDescLink * cc_pos_desc_link_duplicate_all__new( CcPosDescLink * pd_link__io );

/**
    Extends existing linked list with another linked list.

    @param pd_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param pd_link__n Linked list with which to extend existing steps.

    @note
    If linked list to extend (`pd_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    `pd_link__n`.

    @note
    Extending linked list `pd_link__n` has its ownership transferred to
    extended linked list `pd_link__iod_a`; as a result, inner pointer of
    `pd_link__n` is `NULL`-ed.

    @warning
    Function does *not* check if there is a common position in a given linked lists.
    Use `cc_join_pd_links()` function if `pd_link__iod_a` might end with the same
    position with which `pd_link__n` is starting.

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcPosDescLink * cc_pos_desc_link_extend( CcPosDescLink ** pd_link__iod_a,
                                         CcPosDescLink ** pd_link__n );

/**
    Frees all positions in a linked list.

    @param pd_link__f Linked list of positions.

    @return `true` if successful, `false` otherwise.
*/
bool cc_pos_desc_link_free_all( CcPosDescLink ** pd_link__f );

/**
    Function returns length of a linked list.

    @param pd_link A linked list of positions.

    @return Length of a linked list if successful, `0` otherwise.
*/
size_t cc_pos_desc_link_len( CcPosDescLink * pd_link );

/**
    Function returns string containing user-readable representation of a linked positions.

    @param pd_link A linked list of positions.

    @see cc_pos_to_short_string()

    @return A newly allocated, zero-terminated string if successful, `NULL` otherwise.
*/
char * cc_pos_desc_link_to_short_string__new( CcPosDescLink * pd_link );


//
// Linked paths.

/**
    A linked list of paths.
*/
typedef struct CcPathLink {
    CcPosDescLink * path; /**< Link to a path. */
    struct CcPathLink * next; /**< Link to next position. */
} CcPathLink;

/**
    Function allocates a new linked path.

    @param pd_link__n A linked typed steps.

    @note
    Linked typed steps `pd_link__n` will have its ownership transferred to newly allocated path,
    and its inner pointer will be `NULL`-ed.

    @return Pointer to a newly allocated linked path if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link__new( CcPosDescLink ** pd_link__n );

/**
    Function appends a newly allocated linked path to a given linked list.

    @param path_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param pd_link__n Linked typed steps.

    @note
    Linked typed steps `pd_link__n` will have its ownership transferred to newly allocated path,
    and its inner pointer will be `NULL`-ed.

    @note
    Linked list `*path_link__iod_a` can be `NULL`, a linked path will still be
    allocated, and weak pointer to it returned.

    @note
    If linked list `*path_link__iod_a` is `NULL`, it will be initialized
    with a newly allocated linked path as its only element.

    @note
    Pointer `path_link__iod_a` has ownership over given linked list, takes ownership
    over newly allocated path, and retains ownership after function returns.

    @return
    A weak pointer to a newly allocated linked path if successful,
    `NULL` otherwise.
*/
CcPathLink * cc_path_link_append( CcPathLink ** path_link__iod_a,
                                  CcPosDescLink ** pd_link__n );

/**
    Extends existing linked list with a another linked list.

    @param path_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param path_link__n Linked list with which to extend existing steps.

    @note
    If linked list to extend (`path_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    `path_link__n`.

    @note
    Extending linked list `path_link__n` has its ownership transferred to
    extended linked list `path_link__iod_a`; as a result, inner pointer of
    `path_link__n` is `NULL`-ed.

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcPathLink * cc_path_link_extend( CcPathLink ** path_link__iod_a,
                                  CcPathLink ** path_link__n );

/**
    Frees all paths in a linked list.

    @param path_link__f Linked list of paths.

    @return `true` if successful, `false` otherwise.
*/
bool cc_path_link_free_all( CcPathLink ** path_link__f );

/**
    Function returns length of a linked list.

    @param path_link A linked list of paths.

    @return Length of a linked list if successful, `0` otherwise.
*/
size_t cc_path_link_len( CcPathLink * path_link );


#endif /* __CC_POS_H__ */
