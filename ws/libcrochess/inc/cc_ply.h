// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PLY_H__
#define __CC_PLY_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_step.h"

/**
    @file cc_ply.h
    @brief Ply definition. Plies linked list.
*/

/**
    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.
*/
typedef enum CcPlyLinkEnum
{
    CC_PLE_None, /**< Ply link not found, uninitialized, or error happened. */
    CC_PLE_StartingPly, /**< Just first ply, standalone or starting a cascade. */
    CC_PLE_CascadingPly, /**< Just one ply, continuing cascade. Corresponds to `~`. */
    CC_PLE_Teleportation, /**< Teleportation of piece. Corresponds to `|`. */
    CC_PLE_FailedTeleportation, /**< Failed teleportation, corresponds to `||`. */
    CC_PLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `;;`. */
} CcPlyLinkEnum;


#define CC_PLY_LINK_IS_NONE(ple) ( (ple) == CC_PLE_None )
#define CC_PLY_LINK_IS_STARTING(ple) ( (ple) == CC_PLE_StartingPly )
#define CC_PLY_LINK_IS_CASCADING(ple) ( (ple) == CC_PLE_CascadingPly )

#define CC_PLY_LINK_IS_TELEPORTATION(ple) ( (ple) == CC_PLE_Teleportation )
#define CC_PLY_LINK_IS_FAILED_TELEPORTATION(ple) ( (ple) == CC_PLE_FailedTeleportation )

#define CC_PLY_LINK_IS_TRANCE_JOURNEY(ple) ( (ple) == CC_PLE_TranceJourney )
#define CC_PLY_LINK_IS_DUAL_TRANCE_JOURNEY(ple) ( (ple) == CC_PLE_DualTranceJourney )
#define CC_PLY_LINK_IS_FAILED_TRANCE_JOURNEY(ple) ( (ple) == CC_PLE_FailedTranceJourney )

#define CC_PLY_LINK_IS_PAWN_SACRIFICE(ple) ( (ple) == CC_PLE_PawnSacrifice )


#define CC_PLY_LINK_IS_VALID(ple) ( (ple) != CC_PLE_None )

#define CC_PLY_LINK_IS_ANY_TELEPORTATION(ple) ( ( (ple) == CC_PLE_Teleportation )           \
                                             || ( (ple) == CC_PLE_FailedTeleportation )

#define CC_PLY_LINK_IS_ANY_TRANCE_JOURNEY(ple) ( ( (ple) == CC_PLE_TranceJourney )          \
                                              || ( (ple) == CC_PLE_DualTranceJourney )      \
                                              || ( (ple) == CC_PLE_FailedTranceJourney ) )


/**
    Function returns string symbol, as used in algebraic notation, for a given ply link.

    @param ple A ply linkage.

    @note
    Returned string is pre-defined in library, not allocated one, so it needs no `free()`-ing.

    @return String symbol if link is valid, `NULL` otherwise.
*/
char const * cc_ply_link_symbol( CcPlyLinkEnum ple );

#define CC_MAX_LEN_PLY_LINK_SYMBOL (3)


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
    char * notation; /**< Copy of move notation, originating this ply. */

    CcPlyLinkEnum link; /**< Type of link, of this ply, related to previous ply in a cascade.  */
    CcPieceEnum piece; /**< A piece being moved. */
    CcLosingTagEnum lost_tag; /**< Flag, whether moving piece has lost its tag. */
    CcStep * steps; /**< Steps taken by the piece. */

    struct CcPly * next; /**< Next ply in a cascade. */
} CcPly;

/**
    Returns newly allocated ply.

    @param start_an__d _Optional_, start of a ply notation substring. Can be `NULL` if so `notation` member is initialized to `NULL`.
    @param end_an__d _Optional_, end of a ply notation substring. Can be `NULL`, if so whole zero-terminated string is copied.
    @param max_len__d _Optional_ parameter, maximum length of a string to copy. Can be `0`, if so whole zero-terminated string is copied.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param lost_tag Flag, if moving piece has lost its tag.
    @param steps__n Steps, linked list, can be `NULL`.

    @warning
    If no optional arguments (`end_an__d`, `max_len__d`) are given, given
    string (`start_an__d`) has to be zero-terminated, or `NULL`.

    @warning
    Takes ownership of steps, inner pointer will be set to `NULL`, if valid ply is produced.

    @warning
    If no valid ply is produced, steps are still valid, and accessible.

    @return
    A newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply__new( char const * restrict start_an__d,
                     char const * restrict end_an__d,
                     size_t max_len__d,
                     CcPlyLinkEnum link,
                     CcPieceEnum piece,
                     CcLosingTagEnum lost_tag,
                     CcStep ** restrict steps__n );

/**
    Appends a newly allocated ply to a given linked list.

    @param plies__io _Input/ouput_ parameter, plies linked list.
    @param start_an__d _Optional_, start of a ply notation substring. Can be `NULL` if so `notation` member is initialized to `NULL`.
    @param end_an__d _Optional_, end of a ply notation substring. Can be `NULL`, if so whole zero-terminated string is copied.
    @param max_len__d _Optional_ parameter, maximum length of a string to copy. Can be `0`, if so whole zero-terminated string is copied.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param lost_tag Flag, if moving piece has lost its tag.
    @param steps__n Steps, linked list, can be `NULL`.

    @see cc_ply__new()

    @return
    Weak pointer to a newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_append( CcPly * restrict plies__io,
                       char const * restrict start_an__d,
                       char const * restrict end_an__d,
                       size_t max_len__d,
                       CcPlyLinkEnum link,
                       CcPieceEnum piece,
                       CcLosingTagEnum lost_tag,
                       CcStep ** restrict steps__n );

/**
    Allocates a new ply, appends it to a linked list.

    @param plies__io _Input/output_ parameter, linked list of plies, to which a newly allocated ply is appended, can be `NULL`.
    @param start_an__d _Optional_, start of a ply notation substring. Can be `NULL`, if so `notation` member is initialized to `NULL`.
    @param end_an__d _Optional_, end of a ply notation substring. Can be `NULL` if so whole zero-terminated string is copied.
    @param max_len__d _Optional_ parameter, maximum length of a string to copy. Can be `0`, if so whole zero-terminated string is copied.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param lost_tag Flag, if moving piece has lost its tag.
    @param steps__n Steps, linked list, can be `NULL`.

    @note
    Linked list `*plies__io` can be `NULL`, a ply will still be allocated, and returned.

    @note
    If linked list `*plies__io` is `NULL`, it will be initialized,
    with a newly allocated ply as its first element.

    @return
    Weak pointer to a newly allocated ply, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_append_if( CcPly ** restrict plies__io,
                          char const * restrict start_an__d,
                          char const * restrict end_an__d,
                          size_t max_len__d,
                          CcPlyLinkEnum link,
                          CcPieceEnum piece,
                          CcLosingTagEnum lost_tag,
                          CcStep ** restrict steps__n );

/**
    Duplicates a given plies into a newly allocated linked list.

    @param plies Linked list to duplicate.

    @return
    A newly allocated plies, is successful, `NULL` otherwise.
*/
CcPly * cc_ply_duplicate_all__new( CcPly * restrict plies );

// TODO :: DOCS
CcPly * cc_ply_extend( CcPly ** restrict plies__io,
                       CcPly ** restrict plies__n );

// TODO :: DOCS
CcPly * cc_ply_extend_if( CcPly ** restrict plies__iod,
                          CcPly ** restrict plies__n );

/**
    Frees all plies in a linked list, and all associated entities.

    @param plies__f Linked list of plies.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_ply_free_all( CcPly ** restrict plies__f );

/**
    Checks whether any step in a ply has side-effects.

    @param ply A ply.

    @return `true` if any step has side-effects, `false` otherwise.
*/
bool cc_ply_contains_side_effects( CcPly * restrict ply );

/**
    Function returns last active piece for a ply, within a given linked list.

    Last active piece for a ply is piece in that ply, if it's active.

    @param plies A linked list of plies.
    @param ply__d _Optional_, can be `NULL`; a ply within given linked list.

    @note
    If `ply` is `NULL`, last active piece in a complete linked list is returned.

    @note
    If `ply` is given, but does not belong to a given linked list,
    failure is indicated by returning `CC_PE_None`.

    @return Last active piece if successful, `CC_PE_None` otherwise.
*/
CcPieceEnum cc_ply_last_active_piece( CcPly * restrict plies,
                                      CcPly * restrict ply__d );

/**
    Function returns new string, containing user-readable representation of a plies.

    @param plies A queue of plies.

    @return A newly allocated, zero-termianted string if successful, `NULL` otherwise
*/
char * cc_ply_all_to_short_string__new( CcPly * restrict plies );


#endif /* __CC_PLY_H__ */
