// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSED_PLY_H__
#define __CC_PARSED_PLY_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_parsed_step.h"

/**
    @file cc_parsed_ply.h
    @brief Parsed ply, linked list definitions.
*/

/**
    Ply link enumeration.

    This enumerates different ways plies can cascade,
    and directly corresponds to cascading plies separators and terminators.
*/
typedef enum CcParsedPlyLinkEnum {
    CC_PPLE_None, /**< Ply link not found, uninitialized, or error happened. */
    CC_PPLE_StartingPly, /**< Just first ply, standalone or starting a cascade. */
    CC_PPLE_CascadingPly, /**< Just one ply, continuing cascade. Corresponds to `~`. */
    CC_PPLE_Teleportation, /**< Teleportation of piece. Corresponds to `|`. */
    CC_PPLE_TeleportationReemergence, /**< Failed teleportation, corresponds to `||`. */
    CC_PPLE_TeleportationOblation, /**< Failed teleportation, corresponds to `|||`. */
    CC_PPLE_TranceJourney, /**< Trance-journey, corresponds to `@`. */
    CC_PPLE_DualTranceJourney, /**< Double trance-journey, corresponds to `@@`. */
    CC_PPLE_FailedTranceJourney, /**< Failed trance-journey, corresponds to `@@@`. */
    CC_PPLE_PawnSacrifice, /**< Pawn sacrifice, corresponds to `;;`. */
    CC_PPLE_SenseJourney, /**< Sense-journey, corresponds to `"`. */
    CC_PPLE_FailedSenseJourney, /**< Failed sense-journey, corresponds to `'`. */
} CcParsedPlyLinkEnum;


#define CC_PARSED_PLY_LINK_IS_NONE(ple) ( (ple) == CC_PPLE_None )
#define CC_PARSED_PLY_LINK_IS_STARTING(ple) ( (ple) == CC_PPLE_StartingPly )
#define CC_PARSED_PLY_LINK_IS_CASCADING(ple) ( (ple) == CC_PPLE_CascadingPly )

#define CC_PARSED_PLY_LINK_IS_TELEPORTATION(ple) ( (ple) == CC_PPLE_Teleportation )
#define CC_PARSED_PLY_LINK_IS_TELEPORTATION_REEMERGENCE(ple) ( (ple) == CC_PPLE_TeleportationReemergence )
#define CC_PARSED_PLY_LINK_IS_TELEPORTATION_OBLATION(ple) ( (ple) == CC_PPLE_TeleportationOblation )

#define CC_PARSED_PLY_LINK_IS_TRANCE_JOURNEY(ple) ( (ple) == CC_PPLE_TranceJourney )
#define CC_PARSED_PLY_LINK_IS_DUAL_TRANCE_JOURNEY(ple) ( (ple) == CC_PPLE_DualTranceJourney )
#define CC_PARSED_PLY_LINK_IS_FAILED_TRANCE_JOURNEY(ple) ( (ple) == CC_PPLE_FailedTranceJourney )

#define CC_PARSED_PLY_LINK_IS_PAWN_SACRIFICE(ple) ( (ple) == CC_PPLE_PawnSacrifice )

#define CC_PARSED_PLY_LINK_IS_SENSE_JOURNEY(ple) ( (ple) == CC_PPLE_SenseJourney )
#define CC_PARSED_PLY_LINK_IS_FAILED_SENSE_JOURNEY(ple) ( (ple) == CC_PPLE_FailedSenseJourney )


#define CC_PARSED_PLY_LINK_IS_VALID(ple) ( (ple) != CC_PPLE_None )

#define CC_PARSED_PLY_LINK_IS_ANY_TELEPORTATION(ple) ( ( (ple) == CC_PPLE_Teleportation )               \
                                                    || ( (ple) == CC_PPLE_TeleportationReemergence )    \
                                                    || ( (ple) == CC_PPLE_TeleportationOblation )

#define CC_PARSED_PLY_LINK_IS_ANY_TRANCE_JOURNEY(ple) ( ( (ple) == CC_PPLE_TranceJourney )          \
                                                     || ( (ple) == CC_PPLE_DualTranceJourney )      \
                                                     || ( (ple) == CC_PPLE_FailedTranceJourney ) )

#define CC_PARSED_PLY_LINK_IS_ANY_SENSE_JOURNEY(ple) ( ( (ple) == CC_PPLE_SenseJourney )        \
                                                    || ( (ple) == CC_PPLE_FailedSenseJourney )


/**
    Function returns string symbol, as used in algebraic notation, for a given ply link.

    @param ple A ply linkage.

    @note
    Returned string is pre-defined in library, not allocated one, so it needs no `free()`-ing.

    @return String symbol if link is valid, `NULL` otherwise.
*/
char const * cc_parsed_ply_link_symbol( CcParsedPlyLinkEnum ple );

#define CC_MAX_LEN_PARSED_PLY_LINK_SYMBOL (3)


/**
    Ply structure, linked list.

    @warning
    `steps` change meaning, depending on ply `link`.
    `steps` can have only one item in a linked list, if a single destination field is needed.
    `steps` can be empty (`NULL`) for certain ply links.

    |                             `link` | `steps`                                                                                                  |
    | ---------------------------------: | :------------------------------------------------------------------------------------------------------- |
    |                         CC_PPLE_Ply | steps taken by a piece                                                                                   |
    |               CC_PPLE_Teleportation | steps taken if Wave, otherwise destination field                                                         |
    |    CC_PPLE_TeleportationReemergence | destination field                                                                                        |
    |       CC_PPLE_TeleportationOblation | steps are empty (`NULL`)                                                                                 |
    |               CC_PPLE_TranceJourney | steps taken by entranced Shaman                                                                          |
    |           CC_PPLE_DualTranceJourney | fields at which pieces are captured, `side_effect` contains captured, or displaced piece, and lost tag   |
    |         CC_PPLE_FailedTranceJourney | steps are empty (`NULL`)                                                                                 |
    |               CC_PPLE_PawnSacrifice | steps taken by a Serpent                                                                                 |
    |                CC_PPLE_SenseJourney | steps taken by uplifted piece                                                                            |
    |          CC_PPLE_FailedSenseJourney | steps are empty (`NULL`)                                                                                 |
*/
typedef struct CcParsedPly {
    char * notation; /**< Copy of move notation, originating this ply. */

    CcParsedPlyLinkEnum link; /**< Type of link, of this ply, related to previous ply in a cascade.  */
    CcPieceEnum piece; /**< A piece being moved. */
    CcLosingTagEnum lost_tag; /**< Flag, whether moving piece has lost its tag. */
    CcParsedStep * steps; /**< Steps taken by the piece. */

    struct CcParsedPly * next; /**< Next ply in a cascade. */
} CcParsedPly;

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
    A newly allocated ply if successful, `NULL` otherwise.
*/
CcParsedPly * cc_parsed_ply__new( char const * start_an__d,
                                  char const * end_an__d,
                                  size_t max_len__d,
                                  CcParsedPlyLinkEnum link,
                                  CcPieceEnum piece,
                                  CcLosingTagEnum lost_tag,
                                  CcParsedStep ** steps__n );

/**
    Appends a newly allocated ply to a given linked list.

    @param plies__iod_a **Ownership**, _optional_ _input/ouput_ parameter; plies linked list.
    @param start_an__d _Optional_, start of a ply notation substring. Can be `NULL` if so `notation` member is initialized to `NULL`.
    @param end_an__d _Optional_, end of a ply notation substring. Can be `NULL`, if so whole zero-terminated string is copied.
    @param max_len__d _Optional_ parameter, maximum length of a string to copy. Can be `0`, if so whole zero-terminated string is copied.
    @param link Link to previous ply in a cascade.
    @param piece A piece.
    @param lost_tag Flag, if moving piece has lost its tag.
    @param steps__n Steps, linked list, can be `NULL`.

    @note
    Linked list `*plies__iod_a` can be `NULL`, a ply will still be allocated,
    and weak pointer to it returned.

    @note
    If linked list `*plies__iod_a` is `NULL`, it will be initialized,
    with a newly allocated ply as its only element.

    @note
    Pointer `plies__iod_a` has ownership over given linked list, takes ownership
    over newly allocated ply, and retains ownership after function returns.

    @see cc_parsed_ply__new()

    @return
    Weak pointer to a newly allocated ply if successful, `NULL` otherwise.
*/
CcParsedPly * cc_parsed_ply_append( CcParsedPly ** plies__iod_a,
                                    char const * start_an__d,
                                    char const * end_an__d,
                                    size_t max_len__d,
                                    CcParsedPlyLinkEnum link,
                                    CcPieceEnum piece,
                                    CcLosingTagEnum lost_tag,
                                    CcParsedStep ** steps__n );

/**
    Duplicates a given plies into a newly allocated linked list.

    @param plies Linked list to duplicate.

    @return
    A newly allocated plies if successful, `NULL` otherwise.
*/
CcParsedPly * cc_parsed_ply_duplicate_all__new( CcParsedPly * plies );

/**
    Extends existing linked list with a newly allocated plies.

    @param plies__iod_a **Ownership**, _optional_ _input/ouput_ parameter; plies linked list.
    @param plies__n Linked list with which to extend existing plies.

    @see cc_parsed_ply_append(), cc_parsed_ply__new()

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcParsedPly * cc_parsed_ply_extend( CcParsedPly ** plies__iod_a,
                                    CcParsedPly ** plies__n );

/**
    Frees all plies in a linked list, and all associated entities.

    @param plies__f Linked list of plies.

    @warning
    In case of an error, function will continue to free accessible resources,
    failure will still be reported as such.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parsed_ply_free_all( CcParsedPly ** plies__f );

/**
    Checks whether any step in a ply has side-effects.

    @param ply A ply.

    @return `true` if any step has side-effects, `false` otherwise.
*/
bool cc_parsed_ply_contains_side_effects( CcParsedPly * ply );

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
CcPieceEnum cc_parsed_ply_last_active_piece( CcParsedPly * plies,
                                             CcParsedPly * ply__d );

/**
    Function returns new string, containing user-readable representation of a plies.

    @param plies A queue of plies.

    @return A newly allocated, zero-terminated string if successful, `NULL` otherwise
*/
char * cc_parsed_ply_all_to_short_string__new( CcParsedPly * plies );


#endif /* __CC_PARSED_PLY_H__ */