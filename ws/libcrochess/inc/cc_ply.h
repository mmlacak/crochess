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

    @warning
    `steps` change meaning, depending on ply `link`.
    `steps` can have only one item in a linked list, if a single destination field is needed.
    `steps` can be empty (`NULL`) for certain ply links.

    |                             `link` |                                          `steps` |
    | ---------------------------------: | -----------------------------------------------: |
    |                         CC_PLE_Ply |                           steps taken by a piece |
    |               CC_PLE_Teleportation | steps taken if Wave, otherwise destination field |
    | CC_PLE_FailedTeleportationOblation |                        steps are empty (ignored) |
    |         CC_PLE_FailedTeleportation |                                destination field |
    |               CC_PLE_TranceJourney |                           steps taken by a piece |
    |           CC_PLE_DualTranceJourney |              fields at which pieces are captured |
    |         CC_PLE_FailedTranceJourney |                        steps are empty (ignored) |
    |               CC_PLE_PawnSacrifice |                         steps taken by a Serpent |
*/
typedef struct CcPly
{
    CcPlyLinkEnum link; /**< Type of link, of this ply, related to previous ply in a cascade.  */
    CcPieceEnum piece; /**< A piece taking a ride. */
    CcStep * steps; /**< Steps taken by the piece. */
    struct CcPly * next; /**< Next ply in a cascade. */
} CcPly;

/**
    Returns newly allocated ply.

    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param steps_n Steps, linked list, can be `NULL`.

    @warning
    Takes ownership of steps, inner pointer will be set to `NULL`, if valid ply is produced.

    @warning
    If no valid ply is produced, steps are still valid, and accessible.

    @return
    A newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece, CcStep ** restrict steps_n );


/**
    Appends a newly allocated ply to a given linked list.

    @param plies Plies, linked list.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param steps_n Steps, linked list, can be `NULL`.

    @see cc_ply_new()

    @return
    A newly allocated ply, is successful, `NULL` otherwise.
*/
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
