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
    Function checks if position is a disambiguation, i.e. a partial position.

    @param pos A position.

    @note
    Disambiguation is a position with at least one known (valid) coordinate,
    and the other might be unknown (invalid).

    @return `true` if position is a disambiguation, `false` otherwise.
*/
bool cc_pos_is_disambiguation( CcPos pos );

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
    Function returns momentum from start to destination field.

    @param start Starting from.
    @param destination Destination field.

    @note
    Step used to count momentum might not be legal step for any given piece,
    it's just a calculated value.

    @return Momentum, i.e. count of steps from starting to destination field.
*/
int cc_pos_momentum( CcPos start, CcPos destination );

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
// Position + piece + tag.

/**
    Invalid position + piece + tag value.
*/
#define CC_POS_PIECE_TAG_INVALID { .pos = CC_POS_INVALID, .piece = CC_PE_None, .tag = CC_TE_None }

/**
    Static position + piece + tag value, i.e. no-movement step.
*/
#define CC_POS_PIECE_TAG_STATIC_STEP { .pos = CC_POS_STATIC_STEP, .piece = CC_PE_None, .tag = CC_TE_None }

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
    Convenience macro which returns position + piece struct.

    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.
    @param piece A piece.
    @param tag A tag.

    @return Position + piece + tag value.

    @see cc_pos_piece_tag(), cc_pos()
*/
#define CC_POS_PIECE(int_i,int_j,piece,tag) \
    ( cc_pos_piece_tag( cc_pos( (int_i), (int_j) ), (piece), (tag) ) )


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
// Linked positions.

/**
    Convenience macro to allocate new position + piece + tag value to position link.

    @param pos A position.
    @param piece A piece.
    @param tag A tag.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link__new()
*/
#define CC_POS_LINK__NEW(pos,piece,tag) \
    ( cc_pos_link__new( cc_pos_piece_tag( (pos), (piece), (tag) ) ) )

/**
    Macro to append a newly allocated position + piece + tag value to position link.

    @param ptr__pos_link__io A position linked list, to be appended.
    @param pos A position.
    @param piece A piece.
    @param tag A tag.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link_append()
*/
#define CC_POS_LINK_APPEND(ptr__pos_link__io,pos,piece,tag) \
    ( cc_pos_link_append( (ptr__pos_link__io), cc_pos_piece_tag( (pos), (piece), (tag) ) ) )

/**
    Macro to initialize or append a position linked list, with position + piece + tag value.

    @param ptr_ptr__pos_link__io A position linked list, to be appended.
    @param pos A position.
    @param piece A piece.
    @param tag A tag.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.

    @see cc_pos_link_expand()
*/
#define CC_POS_LINK_APPEND_IF(ptr_ptr__pos_link__io,pos,piece,tag) \
    ( cc_pos_link_expand( (ptr_ptr__pos_link__io), cc_pos_piece_tag( (pos), (piece), (tag) ) ) )

/**
    A linked list of positions, with pieces and tags on them.
*/
typedef struct CcPosLink {
    CcPosPieceTag ppt; /**< A position + piece + tag. */
    struct CcPosLink * next; /**< Link to a next position. */
} CcPosLink;

/**
    Function allocates a new linked position.

    @param ppt A position + piece + tag value.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link__new( CcPosPieceTag ppt );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param pos_link__io _Input/output_ parameter, linked list.
    @param ppt A position + piece + tag value.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link_append( CcPosLink * restrict pos_link__io,
                                CcPosPieceTag ppt );

/**
    Allocates a new linked position, appends it to a linked list.

    @param pos_link__io _Input/output_ parameter, linked list, can be `NULL`.
    @param ppt A position + piece + tag value.

    @note
    Linked list `*pos_link__io` can be `NULL`, a linked position will still be
    allocated, and returned.

    @note
    If linked list `*pos_link__io` is `NULL`, it will be initialized,
    with a newly allocated linked position as its first element.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcPosLink * cc_pos_link_expand( CcPosLink ** restrict pos_link__io,
                                CcPosPieceTag ppt );

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


#endif /* __CC_POS_H__ */
