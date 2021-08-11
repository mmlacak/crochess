// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PLY_H__
#define __CC_PLY_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_step.h"

/**
    @file cc_ply.h
    @brief Ply enumeration, structures, and related functions.
*/

/**
    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.
*/
typedef enum CcPlyLinkEnum
{
    CC_PLE_Ply, /**< Just one ply, starting or continuing cascade. If cascading, corresponds to `~`. */
    CC_PLE_Teleportation, /**< Teleportation of piece. Corresponds to `|`. */
    CC_PLE_FailedTeleportationOblation, /**< Failed teleportation, piece is oblationed, corresponds to `||`. */
    CC_PLE_FailedTeleportation, /**< Failed teleportation, piece is not oblationed, corresponds to `||`. */
    CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `::`. */
} CcPlyLinkEnum;


/**
    Ply structure, linked list.

    Contains union of structures, which correspond to a ply link.

    For instance, `ply->teleport` is used when linkage is `CC_PLE_Teleportation`.
*/
// TODO :: DOCS
typedef struct CcPly
{
    CcPlyLinkEnum link; /**< Type of link, of this ply, related to previous ply in a cascade.  */
    CcPieceEnum piece; /**< Piece. */
    CcStep * steps; /**< Steps taken by the piece. */
    struct CcPly * next; /**< Next ply in a cascade. */
} CcPly;

/**
    Returns newly allocated ply.

    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param steps_n Steps, linked list, can be `NULL`.
    @param i File.
    @param j Rank.
    @param captured_n Captured pieces + fields, linked list, can be `NULL`.

    @warning
    Takes ownership of steps, inner pointer will be set to `NULL`,
    for ordinary cascade, Wave teleportation, trance-journey and pawn-sacrifice,
    if valid ply is produced.

    @warning
    Takes ownership of captured piece-fields, inner pointer will be set to `NULL`,
    for double trance-journey, if valid ply is produced.

    @warning
    If no valid ply is produced, steps and captured piece-fields are still valid,
    and accessible.

    @return
    A newly allocated ply, is successful, `NULL` otherwise.
*/
// TODO :: DOCS
CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece, CcStep ** restrict steps_n );


/**
    Appends newly allocated ply to a linked list.

    @param plies Plies, linked list, can be `NULL`.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param steps_n Steps, linked list, can be `NULL`.
    @param i File.
    @param j Rank.
    @param captured_n Captured pieces + fields, linked list, can be `NULL`.

    @note
    A new ply is appended to `plies`, if it's a valid pointer.

    @note
    If not, appending is not done, but a new ply is still returned.

    @warning
    Takes ownership of steps, inner pointer will be set to `NULL`,
    for ordinary cascade, Wave teleportation, trance-journey and pawn-sacrifice,
    if valid ply is produced.

    @warning
    Takes ownership of captured piece-fields, inner pointer will be set to `NULL`,
    for double trance-journey, if valid ply is produced.

    @warning
    If no valid ply is produced, steps and captured piece-fields are still valid,
    and accessible.

    @see cc_ply_new()

    @return
    A newly allocated ply, is successful, `NULL` otherwise.
*/
// TODO :: DOCS
CcPly * cc_ply_append_new( CcPly * const restrict plies,
                           CcPlyLinkEnum link,
                           CcPieceEnum piece,
                           CcStep ** restrict steps_n );

/**
    Frees all plies in a linked list, and all associated entities.

    @param plies_f Linked list of plies.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_ply_free_all_plies( CcPly ** const plies_f );

/**
    Checks whether any step in a ply has side-effects.

    @param ply A ply.

    @return `true` if any step has side-effects, `false` otherwise.
*/
bool cc_ply_contains_side_effects( CcPly const * const restrict ply );

/**
    Function returns count of steps owned by a given ply.

    @param ply A ply.
    @param include_starting_pos Flag if starting position (technically, not a step!) should be included in returned count.

    @return Count of steps if successful, `0` otherwise.
*/
size_t cc_ply_step_count( CcPly const * const restrict ply, bool include_starting_pos );

#endif /* __CC_PLY_H__ */
