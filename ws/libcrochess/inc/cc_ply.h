// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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
    CC_PLE_FailedTeleportation, /**< Failed teleportation, corresponds to `||`. */
    CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `::`. */
} CcPlyLinkEnum;

/**
    Function returns string symbol, as used in algebraic notation, for a given ply link.

    @param ple A ply linkage.

    @note
    Returned string is pre-defined in library, not allocated one, so it needs no `free()`-ing.

    @return String symbol if link is valid, `NULL` otherwise.
*/
char * cc_ply_link_symbol( CcPlyLinkEnum ple );


/**
    Ply structure, linked list.

    @warning
    `steps` change meaning, depending on ply `link`.
    `steps` can have only one item in a linked list, if a single destination field is needed.
    `steps` can be empty (`NULL`) for certain ply links.

    |                             `link` |                                                                      `steps` |
    | ---------------------------------: | ---------------------------------------------------------------------------: |
    |                         CC_PLE_Ply |                                                       steps taken by a piece |
    |               CC_PLE_Teleportation |                             steps taken if Wave, otherwise destination field |
    |         CC_PLE_FailedTeleportation | steps are empty (`NULL`) if piece is oblationed, destination field otherwise |
    |               CC_PLE_TranceJourney |                                                       steps taken by a piece |
    |           CC_PLE_DualTranceJourney |                                          fields at which pieces are captured |
    |         CC_PLE_FailedTranceJourney |                                                     steps are empty (`NULL`) |
    |               CC_PLE_PawnSacrifice |                                                     steps taken by a Serpent |
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
    @param steps__n Steps, linked list, can be `NULL`.

    @warning
    Takes ownership of steps, inner pointer will be set to `NULL`, if valid ply is produced.

    @warning
    If no valid ply is produced, steps are still valid, and accessible.

    @return
    A newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_new( CcPlyLinkEnum link,
                    CcPieceEnum piece,
                    CcStep ** restrict steps__n );


/**
    Appends a newly allocated ply to a given linked list.

    @param plies__io _Input/ouput_ parameter, plies linked list.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param steps__n Steps, linked list, can be `NULL`.

    @see cc_ply_new()

    @return
    Weak pointer to a newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_append( CcPly * restrict plies__io,
                       CcPlyLinkEnum link,
                       CcPieceEnum piece,
                       CcStep ** restrict steps__n );

/**
    Allocates a new ply, appends it to a linked list.

    @param plies__io _Input/output_ parameter, linked list of plies, to which a newly allocated ply is appended, can be `NULL`.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param steps__n Steps, linked list, can be `NULL`.

    @note
    Linked list `*plies__io` can be `NULL`, a ply will still be allocated, and returned.

    @note
    If linked list `*plies__io` is `NULL`, it will be initialized,
    with a newly allocated ply as its first element.

    @return
    Weak pointer to a newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_append_or_init( CcPly ** restrict plies__io,
                               CcPlyLinkEnum link,
                               CcPieceEnum piece,
                               CcStep ** restrict steps__n );

/**
    Duplicates a given plies into a newly allocated linked list.

    @param plies Linked list to duplicate.

    @return
    A newly allocated plies, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_duplicate_all_new( CcPly * restrict plies );

/**
    Frees all plies in a linked list, and all associated entities.

    @param plies__f Linked list of plies.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_ply_free_all_plies( CcPly ** restrict plies__f );

/**
    Checks whether any step in a ply has side-effects.

    @param ply A ply.

    @return `true` if any step has side-effects, `false` otherwise.
*/
bool cc_ply_contains_side_effects( CcPly * restrict ply );

/**
    Function returns count of steps owned by a given ply.

    @param ply A ply.
    @param usage A step formatting usage, used to filter out steps less relevant for a given formatting.
    @param include_starting_pos Flag if starting position (technically, not a step!) should be included in returned count.

    @see CcFormatStepUsageEnum

    @return Count of steps if successful, `0` otherwise.
*/
size_t cc_ply_step_count( CcPly * restrict ply,
                          CcFormatStepUsageEnum usage,
                          bool include_starting_pos );

#endif /* __CC_PLY_H__ */
