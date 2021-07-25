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
    CC_PLE_Teleportation, /**< Teleportation of piece (but not Wave!). Corresponds to `|`. */
    CC_PLE_TeleportationWave, /**< Teleportation of Wave, corresponds to `|`. */
    CC_PLE_FailedTeleportationOblation, /**< Failed teleportation, piece is oblationed, corresponds to `||`. */
    CC_PLE_FailedTeleportation, /**< Failed teleportation, piece is not oblationed, corresponds to `||`. */
    CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `::`. */
} CcPlyLinkEnum;


/**
    Piece + field structure, linked list.

    Used for enumerating pieces captured in a dual trance-journey.
*/
typedef struct CcPieceField
{
    CcPieceEnum piece; /**< A piece */
    int i; /**< File. */
    int j; /**< Rank. */
    struct CcPieceField * next; /**< Next piece-field in a linked list. */
} CcPieceField;

/**
    Allocates a new piece-field.

    @param piece A piece.
    @param i File.
    @param j Rank.

    @return A newly allocated piece-field.
*/
CcPieceField * cc_ply_piece_field_new( CcPieceEnum piece, int i, int j );

/**
    Allocates a new piece-field, appends it to a linked list.

    @param piece_fields Linked list of piece-fields, to which a newly allocated piece-field is appended.
    @param piece A piece.
    @param i File.
    @param j Rank.

    @note
    Linked list `piece_fields` can be `NULL`, a piece-field will still be allocated, and returned.

    @return
    A newly allocated piece-field, is successful, `NULL` otherwise.
*/
CcPieceField * cc_ply_piece_field_append_new( CcPieceField * const restrict piece_fields,
                                              CcPieceEnum piece,
                                              int i,
                                              int j );

/**
    Frees all piece-fields in a linked list.

    @param piece_fields_f Linked list of piece-fields.

    @return `true` if successful, `false` otherwise.

*/
bool cc_ply_piece_field_free_all( CcPieceField ** const piece_fields_f );


/**
    Ply structure, linked list.

    Contains union of structures, which correspond to a ply link.

    For instance, `ply->teleport` is used when linkage is `CC_PLE_Teleportation`.
*/
typedef struct CcPly
{
    CcPlyLinkEnum link; /**< Type of link, of this ply, related to previous ply in a cascade.  */
    CcPieceEnum piece; /**< Piece. */

    union
    {
        struct  { CcStep * steps; /**< Steps taken by the piece. */
                } ply; /**< Ordinary, or cascading ply. */

        struct  { int i; /**< File where piece teleported. */
                  int j; /**< Rank where piece teleported. */
                } teleport; /**< Teleporting piece (not a Wave!). */

        struct  { CcStep * steps; /**< Steps taken by the Wave. */
                } teleport_wave; /**< Teleporting Wave. */

        struct  { int i; /**< File where piece reappeared. */
                  int j; /**< Rank where piece reappeared. */
                } failed_teleport; /**< Failed teleportation, piece is not oblationed. */

        struct  { int i; /**< File of a actual start along pattern, might be different from starting position for a dark Shaman. */
                  int j; /**< Rank of a actual start along pattern, might be different from starting position for a dark Shaman. */
                  CcStep * steps; /**< Steps in a trance-journey. */
                } trance_journey; /**< Trance-journey ply. */

        struct  { CcPieceField * captured; /**< Linked list of pieces, and positions where they were captured. */
                } dual_trance_journey; /**< Dual trance-journey. */

        struct  { CcStep * steps; /**< Steps in a pawn-sacrifice. */
                } pawn_sacrifice; /**< Pawn-sacrifice ply. */
    }; /**< Union of all substructures used by different ply linkage. */

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
CcPly * cc_ply_new( CcPlyLinkEnum link, CcPieceEnum piece,
                    CcStep ** restrict steps_n, int i, int j,
                    CcPieceField ** restrict captured_n );


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
CcPly * cc_ply_append_new( CcPly * const restrict plies,
                           CcPlyLinkEnum link, CcPieceEnum piece,
                           CcStep ** restrict steps_n, int i, int j,
                           CcPieceField ** restrict captured_n );

/**
    Frees all plies in a linked list, and all associated entities.

    @param plies_f Linked list of plies.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_ply_free_all_plies( CcPly ** const plies_f );


/** @defgroup ply_convenience The ply conveniences
 *  The ply convenience functions are meant to be used instead of `cc_ply_new()`, and `cc_ply_append_new()`.

    They have minimal set of arguments required by the type of a ply (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_ply_new(), cc_ply_append_new()
 *  @{
 */

/** @defgroup ply_convenience_new The new ply conveniences
 *  The new ply convenience functions  are meant to be used instead of `cc_ply_new()`.

    They have minimal set of arguments required by the type of a ply (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_ply_new()
 *  @{
 */

CcPly * cc_ply_cascade_new( CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_teleport_new( CcPieceEnum piece, int i, int j );
CcPly * cc_ply_teleport_wave_new( CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_failed_teleport_oblation_new( CcPieceEnum piece );
CcPly * cc_ply_failed_teleport_new( CcPieceEnum piece, int i, int j );
CcPly * cc_ply_trance_journey_new( CcPieceEnum piece, CcStep ** restrict steps_n, int i, int j );
CcPly * cc_ply_dual_trance_journey_new( CcPieceField ** restrict captured );
CcPly * cc_ply_failed_trance_journey_new( CcPieceEnum piece );
CcPly * cc_ply_pawn_sacrifice_new( CcPieceEnum piece, CcStep ** restrict steps_n );

/** @} */ // end of ply_convenience_new


/** @defgroup ply_convenience_append The append new ply conveniences
 *  The append new ply convenience functions are meant to be used instead of `cc_ply_append_new()`.

    They have minimal set of arguments required by the type of a ply (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_ply_append_new()
 *  @{
 */

CcPly * cc_ply_cascade_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_teleport_append_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j );
CcPly * cc_ply_teleport_wave_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n );
CcPly * cc_ply_failed_teleport_oblation_append_new( CcPly * const restrict plies, CcPieceEnum piece );
CcPly * cc_ply_failed_teleport_append_new( CcPly * const restrict plies, CcPieceEnum piece, int i, int j );
CcPly * cc_ply_trance_journey_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n, int i, int j );
CcPly * cc_ply_dual_trance_journey_append_new( CcPly * const restrict plies, CcPieceField ** restrict captured );
CcPly * cc_ply_failed_trance_journey_append_new( CcPly * const restrict plies, CcPieceEnum piece );
CcPly * cc_ply_pawn_sacrifice_append_new( CcPly * const restrict plies, CcPieceEnum piece, CcStep ** restrict steps_n );

/** @} */ // end of ply_convenience_append

/** @} */ // end of ply_convenience


/**
    Returns steps, if a ply has ones.

    @param ply A ply.

    @note
    A ply has steps, if its linkage is ordinary cascading, Wave teleportation, trance-journey, or pawn-sacrifice.

    @return Steps, if successful, `NULL` otherwise.
*/
CcStep * cc_ply_get_steps( CcPly const * const restrict ply );

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
