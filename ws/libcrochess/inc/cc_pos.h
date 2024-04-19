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
    Function checks if two positions are the congruent.

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
CcPos cc_pos_add( CcPos pos, CcPos step, unsigned int count );

/**
    Function subtracts step from position.

    @param pos A position to subtract from.
    @param step A step to be subtracted.
    @param count Count of steps to be subtracted.

    @note
    Function subtracts valid coordinates, if both `pos` and `step` arguments are one of:
    - valid positions
    - file disambiguations
    - rank disambiguations.

    @note
    If `pos` and `step` have no common valid coordinates, result is invalid position,
    e.g. if a rank disambiguation is subtracted from a file disambiguation.

    @return A position with subtracted step if successful, invalid position otherwise.
*/
CcPos cc_pos_subtract( CcPos pos, CcPos step, unsigned int count );

/**
    Function returns difference between two given positions.

    @param pos_1 A position.
    @param pos_2 An other position.

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
CcPos cc_pos_difference( CcPos pos_1, CcPos pos_2 );

/**
    Function returns step from start to destination field.

    @param start Starting from.
    @param destination Destination field.

    @note
    Returned step might not be legal step for any given piece,
    it's just a calculated value.

    @return A valid step if successful, invalid otherwise.
*/
CcPos cc_pos_step( CcPos start, CcPos destination );

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
bool cc_pos_to_short_string( CcPos pos, cc_char_8 * restrict pos_str__o );

//
// Linked positions.

/**
    Convenience macro to allocate new position value to position link.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link__new()
*/
#define CC_POS_LINK__NEW(int_i,int_j) \
    ( cc_pos_link__new( CC_POS_CAST( (int_i), (int_j) ) ) )

/**
    Macro to append a newly allocated position value to position link.

    @param ptr_ptr__pos_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @see cc_pos_link_append()

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
#define CC_POS_LINK_APPEND(ptr_ptr__pos_link__iod_a,int_i,int_j) \
    ( cc_pos_link_append( (ptr_ptr__pos_link__iod_a), CC_POS_CAST( (int_i), (int_j) ) ) )

/**
    A linked list of positions.
*/
typedef struct CcPosLink {
    CcPos pos; /**< A position. */
    struct CcPosLink * next; /**< Link to a next position. */
} CcPosLink;

/**
    Function allocates a new linked position.

    @param pos A position.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link__new( CcPos pos );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param pos_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param pos A position.

    @note
    Linked list `*pos_link__iod_a` can be `NULL`, a linked position will still be
    allocated, and weak pointer to it returned.

    @note
    If linked list `*pos_link__iod_a` is `NULL`, it will be initialized
    with a newly allocated linked position as its only element.

    @note
    Pointer `pos_link__iod_a` has ownership over given linked list, takes ownership
    over newly allocated position, and retains ownership after function returns.

    @return
    A weak pointer to a newly allocated linked position if successful,
    `NULL` otherwise.
*/
CcPosLink * cc_pos_link_append( CcPosLink ** restrict pos_link__iod_a,
                                CcPos pos );

/**
    Extends existing linked list with a another linked list.

    @param pos_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param pos_link__n Linked list with which to extend existing steps.

    @note
    If linked list to extend (`pos_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    `pos_link__n`.

    @note
    Extending linked list `pos_link__n` has its ownership transferred to
    extended linked list `pos_link__iod_a`; as a result, inner pointer of
    `pos_link__n` is `NULL`-ed.

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link_extend( CcPosLink ** restrict pos_link__iod_a,
                                CcPosLink ** restrict pos_link__n );

/**
    Frees all positions in a linked list.

    @param pos_link__f Linked list of positions.

    @return `true` if successful, `false` otherwise.
*/
bool cc_pos_link_free_all( CcPosLink ** restrict pos_link__f );

/**
    Function returns length of a linked list.

    @param pos_link A linked list of positions.

    @return Length of a linked list if successful, `0` otherwise.
*/
size_t cc_pos_link_len( CcPosLink * restrict pos_link );

/**
    Function returns string containing user-readable representation of a linked positions.

    @param pos_link A linked list of positions.

    @see cc_pos_to_short_string()

    @return A newly allocated, zero-terminated string if successful, `NULL` otherwise.
*/
char * cc_pos_link_to_short_string__new( CcPosLink * restrict pos_link );


//
// Position + piece + tag.

/**
    Invalid position + piece + tag value.
*/
#define CC_POS_PIECE_TAG_INVALID { .pos = CC_POS_INVALID, .piece = CC_PIECE_INVALID, .tag = CC_TAG_INVALID }

/**
    Static position + piece + tag value, i.e. no-movement step.
*/
#define CC_POS_PIECE_TAG_STATIC_STEP { .pos = CC_POS_STATIC_STEP, .piece = CC_PIECE_INVALID, .tag = CC_TAG_INVALID }

/**
    Structure holding a position, usually absolute, i.e. a location.
    Piece and tag are the ones (usually) found at location.
*/
typedef struct CcPosPieceTag {
    CcPos pos; /**< A position. */
    CcPieceEnum piece; /**< Piece, e.g. the one found at position. */
    CcTagEnum tag; /**< Tag, e.g. the one found at position. */
} CcPosPieceTag;

/**
    Casted invalid position + piece + tag value.
*/
#define CC_POS_PIECE_TAG_CAST_INVALID ( (CcPosPieceTag)CC_POS_PIECE_TAG_INVALID )

/**
    Casted static position + piece + tag value, i.e. no-movement step.
*/
#define CC_POS_PIECE_TAG_CAST_STATIC_STEP ( (CcPosPieceTag)CC_POS_PIECE_TAG_STATIC_STEP )

/**
    Macro which constructs position + piece + tag struct.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece_enum A piece.
    @param tag_enum A tag.

    @see CcPosPieceTag

    @return Position + piece + tag value.
*/
#define CC_POS_PIECE_TAG(int_i,int_j,piece_enum,tag_enum) \
    { .pos = CC_POS_CAST( (int_i), (int_j) ), .piece = (CcPieceEnum)(piece_enum), .tag = (CcTagEnum)(tag_enum) }

/**
    Macro which constructs casted position + piece + tag struct.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece_enum A piece.
    @param tag_enum A tag.

    @see CcPosPieceTag

    @return Casted position + piece + tag value.
*/
#define CC_POS_PIECE_TAG_CAST(int_i,int_j,piece_enum,tag_enum) \
    ( (CcPosPieceTag)CC_POS_PIECE_TAG( (int_i), (int_j), (piece_enum), (tag_enum) ) )

/**
    Macro expression to evaluate whether given position + piece + tag is valid.

    @param ppt A position + piece + tag.

    @see CcPosPieceTag

    @return `true` if valid position + piece + tag, `false` otherwise.
*/
#define CC_POS_PIECE_TAG_IS_VALID(ppt) \
    ( CC_POS_IS_VALID( (ppt).pos ) && CC_PIECE_IS_VALID( (ppt).piece ) && ( CC_TAG_IS_VALID( (ppt).tag ) ) )

/**
    Macro expression to evaluate whether given position + piece + tag is equal to another.

    @param ppt_1 A position + piece + tag.
    @param ppt_2 Other position + piece + tag.

    @see CcPosPieceTag

    @return `true` if equal, `false` otherwise.
*/
#define CC_POS_PIECE_TAG_IS_EQUAL(ppt_1,ppt_2) \
    ( CC_POS_IS_EQUAL( (ppt_1).pos, (ppt_2).pos ) && ( (ppt_1).piece == (ppt_2).piece ) && ( (ppt_1).tag == (ppt_2).tag ) )

/**
    Function returns position + piece + tag value.

    @param pos A position.
    @param piece A piece.
    @param tag A  tag.

    @return Position + piece + tag value.
*/
CcPosPieceTag cc_pos_piece_tag( CcPos pos, CcPieceEnum piece, CcTagEnum tag );

/**
    Function checks if position + piece is valid.

    @param ppt A position + piece.

    @see CC_POS_INVALID

    @return `true` if position + piece is valid, `false` otherwise.
*/
bool cc_pos_piece_tag_is_valid( CcPosPieceTag ppt );

/**
    Function checks if two position + piece values are the same.

    @param ppt_1 A position + piece.
    @param ppt_2 An other position + piece.

    @return `true` if position + piece values are the same, `false` otherwise.
*/
bool cc_pos_piece_tag_is_equal( CcPosPieceTag ppt_1, CcPosPieceTag ppt_2 );

/**
    Function checks if two position + piece values are the congruent.

    @param ppt_1 A position + piece.
    @param ppt_2 An other position + piece.

    @note
    For positions to be congruent, at least one set of coordinates (files,
    or ranks) from both positions has to be valid, and the same.

    @note
    For pieces to be congruent, they have to be valid, and the same type,
    e.g two Rooks.

    @return `true` if positions are congruent, `false` otherwise.
*/
bool cc_pos_piece_tag_is_congruent( CcPosPieceTag ppt_1, CcPosPieceTag ppt_2 );

/**
    Function converts position + piece value into a user-readable
    `<file char><rank number><piece>` notation.

    @param ppt A position + piece.
    @param ppt_str__o An _output_ parameter, short string array.

    @note
    Coordinates outside chessboard are converted into short integers, if possible.

    @note
    If outside of 2 decimal places, coordinate is represented as asterisk.

    @return `true` if successful, `false` otherwise.
*/
bool cc_pos_piece_tag_to_short_string( CcPosPieceTag ppt,
                                       cc_char_16 * restrict ppt_str__o );


//
// Linked list of positions + pieces + tags.

/**
    Convenience macro to allocate new position + piece + tag value to position link.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece A piece.
    @param tag A tag.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_ppt_link__new()
*/
#define CC_PPT_LINK__NEW(int_i,int_j,piece,tag) \
    ( cc_ppt_link__new( CC_POS_PIECE_TAG_CAST( (int_i), (int_j), (piece), (tag) ) ) )

/**
    Macro to append a newly allocated position + piece + tag value to position link.

    @param ptr_ptr__ppt_link__iod _Optional_, _input/output_ parameter; a position linked list.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece A piece.
    @param tag A tag.

    @see cc_ppt_link_append()

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
#define CC_PPT_LINK_APPEND(ptr_ptr__ppt_link__iod,int_i,int_j,piece,tag) \
    ( cc_ppt_link_append( (ptr_ptr__ppt_link__iod), CC_POS_PIECE_TAG_CAST( (int_i), (int_j), (piece), (tag) ) ) )

/**
    A linked list of positions, with pieces and tags on them.
*/
typedef struct CcPptLink {
    CcPosPieceTag ppt; /**< A position + piece + tag. */
    struct CcPptLink * next; /**< Link to a next position. */
} CcPptLink;

/**
    Function allocates a new linked position.

    @param ppt A position + piece + tag value.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPptLink * cc_ppt_link__new( CcPosPieceTag ppt );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param ppt_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param ppt A position + piece + tag value.

    @note
    Linked list `*ppt_link__iod_a` can be `NULL`, a linked position will still be
    allocated, and weak pointer to it returned.

    @note
    If linked list `*ppt_link__iod_a` is `NULL`, it will be initialized
    with a newly allocated linked position as its only element.

    @note
    Pointer `ppt_link__iod_a` has ownership over given linked list, takes ownership
    over newly allocated position item, and retains ownership after function returns.

    @return
    A weak pointer to a newly allocated linked position if successful,
    `NULL` otherwise.
*/
CcPptLink * cc_ppt_link_append( CcPptLink ** restrict ppt_link__iod_a,
                                CcPosPieceTag ppt );

/**
    Duplicates a given position + piece + tag linked list into a newly allocated.

    @param ppt_link__io Linked list to duplicate.

    @return
    A pointer to newly allocated linked list if successful, `NULL` otherwise.
*/
CcPptLink * cc_ppt_link_duplicate_all__new( CcPptLink * restrict ppt_link__io );

/**
    Extends existing linked list with another linked list.

    @param ppt_link__iod_a **Ownership**, _optional_ _input/output_ parameter, linked list.
    @param ppt_link__n Linked list with which to extend existing steps.

    @note
    If linked list to extend (`ppt_link__iod_a`) hasn't been allocated yet,
    this will initialize it with content of an extending linked list, i.e.
    `ppt_link__n`.

    @note
    Extending linked list `ppt_link__n` has its ownership transferred to
    extended linked list `ppt_link__iod_a`; as a result, inner pointer of
    `ppt_link__n` is `NULL`-ed.

    @warning
    Function does *not* check if there is a common position in a given linked lists.
    Use `cc_join_ppt_links()` function if `ppt_link__iod_a` might end with the same
    position with which `ppt_link__n` is starting.

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcPptLink * cc_ppt_link_extend( CcPptLink ** restrict ppt_link__iod_a,
                                CcPptLink ** restrict ppt_link__n );

/**
    Frees all positions in a linked list.

    @param ppt_link__f Linked list of positions.

    @return `true` if successful, `false` otherwise.
*/
bool cc_ppt_link_free_all( CcPptLink ** restrict ppt_link__f );

/**
    Function returns length of a linked list.

    @param ppt_link A linked list of positions.

    @return Length of a linked list if successful, `0` otherwise.
*/
size_t cc_ppt_link_len( CcPptLink * restrict ppt_link );

/**
    Function returns a new linked list of positions.

    @param ppt_link A linked list of positions + pieces + tags.

    @return Newly allocated linked list if successful, `NULL` otherwise.
*/
CcPosLink * cc_ppt_link_convert_to_pos_link__new( CcPptLink * restrict ppt_link );

/**
    Function returns string containing user-readable representation of a linked positions.

    @param ppt_link A linked list of positions.

    @see cc_pos_to_short_string()

    @return A newly allocated, zero-terminated string if successful, `NULL` otherwise.
*/
char * cc_ppt_link_to_short_string__new( CcPptLink * restrict ppt_link );


#endif /* __CC_POS_H__ */
