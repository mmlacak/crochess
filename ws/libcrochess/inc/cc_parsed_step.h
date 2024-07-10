// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSED_STEP_H__
#define __CC_PARSED_STEP_H__

#include <stddef.h>

#include "cc_pos.h"
#include "cc_parsed_side_effect.h"

/**
    @file cc_parsed_step.h
    @brief Parsed step, linked list definitions.
*/

/**
    Macro to check if a given step link is valid.

    @param sle Step link, enumerated value.

    @see CcParsedStepLinkEnum

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_STEP_LINK_VALID(sle) ( (sle) != CC_PSLE_None )

/**
    Macro to check if a given step link is destination.

    @param sle Step link, enumerated value.

    @see CcParsedStepLinkEnum

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_STEP_LINK_DESTINATION(sle) ( ( (sle) == CC_PSLE_Destination ) || ( (sle) == CC_PSLE_JustDestination ) )

/**
    Step link enumeration.
*/
typedef enum CcParsedStepLinkEnum {
    CC_PSLE_None, /**< Step link not found, uninitialized, not parsed yet, or error happened. */
    CC_PSLE_Start, /**< Position from which a piece started moving. */
    CC_PSLE_Reposition, /**< In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_PSLE_Next, /**< Step immediately following previous, separated by . (dot). */
    CC_PSLE_Distant, /**< Step not immediately following previous, separated by .. (double-dot). */
    CC_PSLE_Destination, /**< Step to destination field, separated by - (hyphen). */
    CC_PSLE_JustDestination, /**< Just destination field, no separators, no other steps. */
} CcParsedStepLinkEnum;

/**
    Function returns string symbol, as used in algebraic notation, for a given step link.

    @param sle A step linkage.

    @note
    Returned string is pre-defined in library, not allocated one, so it needs no `free()`-ing.

    @return String symbol if link is valid, `NULL` otherwise.
*/
char const * cc_parsed_step_link_symbol( CcParsedStepLinkEnum sle );

// DOCS
#define CC_MAX_LEN_PARSED_STEP_LINK_SYMBOL (2)


/**
    Step structure, linked list.
*/
typedef struct CcParsedStep {
    CcParsedStepLinkEnum link; /**< Type of a link to previous step. */
    CcPos field; /**< Field of a step. */
    CcParsedSideEffect side_effect; /**< Side-effect structure. */

    struct CcParsedStep * next; /**< Next step in a linked list. */
} CcParsedStep;

/**
    Returns a newly allocated step.

    @param link Type of a link to previous step.
    @param field Field.
    @param side_effect Side-effect structure.

    @return
    A newly allocated step if successful, `NULL` otherwise.
*/
CcParsedStep * cc_parsed_step__new( CcParsedStepLinkEnum link,
                                    CcPos field,
                                    CcParsedSideEffect side_effect );

/**
    Appends a newly allocated step to a given linked list.

    @param steps__iod_a **Ownership**, _optional_ _input/output_ parameter; linked list to which a new step is appended.
    @param link Type of a link to previous step.
    @param field Field.
    @param side_effect Side-effect structure.

    @note
    Linked list `*steps__iod_a` can be `NULL`, a step will still be allocated,
    and weak pointer to it returned.

    @note
    If linked list `*steps__iod_a` is `NULL`, it will be initialized,
    with a newly allocated step as its only element.

    @note
    Pointer `steps__iod_a` has ownership over given linked list, takes ownership
    over newly allocated step, and retains ownership after function returns.

    @return
    Weak pointer to a newly allocated step if successful, `NULL` otherwise.
*/
CcParsedStep * cc_parsed_step_append( CcParsedStep ** steps__iod_a,
                                      CcParsedStepLinkEnum link,
                                      CcPos field,
                                      CcParsedSideEffect side_effect );

/**
    Duplicates a given steps into a newly allocated linked list.

    @param steps Linked list to duplicate.

    @return
    A newly allocated steps if successful, `NULL` otherwise.
*/
CcParsedStep * cc_parsed_step_duplicate_all__new( CcParsedStep * steps );

/**
    Extends existing linked list with step(s).

    @param steps__iod_a **Ownership**, _optional_ _input/output_ parameter; linked list to extend.
    @param steps__n Linked list with which to extend existing steps.

    @note
    If linked list to extend (`steps__iod_a`) hasn't been allocated yet,
    this will initialize it     with content of an extending linked list,
    i.e. `steps__n`.

    @note
    Extending linked list `steps__n` has its ownership transferred to
    extended linked list `steps__iod_a`; as a result, inner pointer of
    `steps__n` is `NULL`-ed.

    @return
    Weak pointer to extending portion of a linked list if successful, `NULL` otherwise.
*/
CcParsedStep * cc_parsed_step_extend( CcParsedStep ** steps__iod_a,
                                      CcParsedStep ** steps__n );

/**
    Function returning count of steps, based on usage.

    @param steps Steps, a linked list.

    @return Count of steps if successful, `0` otherwise.
*/
size_t cc_parsed_step_count( CcParsedStep * steps );

/**
    Function finds starting step.

    @param steps A linked list of steps.

    @return Starting step if successful, `NULL` otherwise
*/
CcParsedStep * cc_parsed_step_find_start( CcParsedStep * steps );

/**
    Function finds destination step.

    @param steps A linked list of steps.

    @note
    Destination step is one preceded by destination separator, i.e. `-` (hyphen),
    or is the last, non-starting step in a given list.

    @return Destination step if successful, `NULL` otherwise
*/
CcParsedStep * cc_parsed_step_find_destination( CcParsedStep * steps );

/**
    Frees all steps in a linked list.

    @param steps__f Linked list of steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_parsed_step_free_all( CcParsedStep ** steps__f );

/**
    Function returns new string, containing user-readable representation of a steps.

    @param steps A linked list of steps.

    @note
    Each step is preceded by the same separator as used in AN, e.g. `..` (double dot) is used for a distant step.

    @note
    Steps with unknown linkage are preceded by `?`.

    @note
    Starting step is preceded by ` (back-tick).

    @return A newly allocated, zero-terminated string if successful, `NULL` otherwise
*/
char * cc_parsed_step_all_to_short_string__new( CcParsedStep * steps );


/** @defgroup step_convenience The step conveniences
 *  The step convenience functions are meant to be used instead of `cc_parsed_step__new()`,
    and `cc_parsed_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_parsed_step__new(), cc_parsed_step_append()
 *  @{
 */

/** @defgroup step_convenience_new The new step conveniences
 *  The new step convenience functions are meant to be used instead of `cc_parsed_step__new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_parsed_step__new()
 *  @{
 */

CcParsedStep * cc_parsed_step_none__new( CcParsedStepLinkEnum link, CcPos field );

CcParsedStep * cc_parsed_step_capture__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceEnum piece,
                                            CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_displacement__new( CcParsedStepLinkEnum link, CcPos field,
                                                 CcPieceEnum piece,
                                                 CcLosingTagEnum lost_tag,
                                                 CcPos destination );

CcParsedStep * cc_parsed_step_en_passant__new( CcParsedStepLinkEnum link, CcPos field,
                                               CcPieceEnum pawn,
                                               CcPos distant );

CcParsedStep * cc_parsed_step_castle__new( CcParsedStepLinkEnum link, CcPos field,
                                           CcPieceEnum rook,
                                           CcPos start,
                                           CcPos destination );

CcParsedStep * cc_parsed_step_promote__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceEnum captured,
                                            CcLosingTagEnum lost_tag,
                                            CcPieceEnum promoted_to );

CcParsedStep * cc_parsed_step_tag_for_promotion__new( CcParsedStepLinkEnum link, CcPos field,
                                                      CcPieceEnum captured,
                                                      CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_convert__new( CcParsedStepLinkEnum link, CcPos field,
                                            CcPieceEnum piece,
                                            CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_failed_conversion__new( CcParsedStepLinkEnum link, CcPos field );

CcParsedStep * cc_parsed_step_demote__new( CcParsedStepLinkEnum link, CcPos field,
                                           CcPieceEnum piece,
                                           CcLosingTagEnum lost_tag,
                                           CcPos distant );

CcParsedStep * cc_parsed_step_resurrect__new( CcParsedStepLinkEnum link, CcPos field,
                                              CcPieceEnum piece,
                                              CcPos destination );

CcParsedStep * cc_parsed_step_failed_resurrection__new( CcParsedStepLinkEnum link, CcPos field );

/** @} */ // end of step_convenience_new


/** @defgroup step_convenience_append The append new step conveniences
 *  The append new step convenience functions are meant to be used instead of `cc_parsed_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_parsed_step_append()
 *  @{
 */

CcParsedStep * cc_parsed_step_none_append( CcParsedStep ** steps__iod_a,
                                           CcParsedStepLinkEnum link,
                                           CcPos field );

CcParsedStep * cc_parsed_step_capture_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceEnum piece,
                                              CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_displacement_append( CcParsedStep ** steps__iod_a,
                                                   CcParsedStepLinkEnum link,
                                                   CcPos field,
                                                   CcPieceEnum piece,
                                                   CcLosingTagEnum lost_tag,
                                                   CcPos destination );

CcParsedStep * cc_parsed_step_en_passant_append( CcParsedStep ** steps__iod_a,
                                                 CcParsedStepLinkEnum link,
                                                 CcPos field,
                                                 CcPieceEnum pawn,
                                                 CcPos distant );

CcParsedStep * cc_parsed_step_castle_append( CcParsedStep ** steps__iod_a,
                                             CcParsedStepLinkEnum link,
                                             CcPos field,
                                             CcPieceEnum rook,
                                             CcPos start,
                                             CcPos destination );

CcParsedStep * cc_parsed_step_promote_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceEnum captured,
                                              CcLosingTagEnum lost_tag,
                                              CcPieceEnum promoted_to );

CcParsedStep * cc_parsed_step_tag_for_promotion_append( CcParsedStep ** steps__iod_a,
                                                        CcParsedStepLinkEnum link,
                                                        CcPos field,
                                                        CcPieceEnum captured,
                                                        CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_convert_append( CcParsedStep ** steps__iod_a,
                                              CcParsedStepLinkEnum link,
                                              CcPos field,
                                              CcPieceEnum piece,
                                              CcLosingTagEnum lost_tag );

CcParsedStep * cc_parsed_step_failed_conversion_append( CcParsedStep ** steps__iod_a,
                                                        CcParsedStepLinkEnum link,
                                                        CcPos field );

CcParsedStep * cc_parsed_step_demote_append( CcParsedStep ** steps__iod_a,
                                             CcParsedStepLinkEnum link,
                                             CcPos field,
                                             CcPieceEnum piece,
                                             CcLosingTagEnum lost_tag,
                                             CcPos distant );

CcParsedStep * cc_parsed_step_resurrect_append( CcParsedStep ** steps__iod_a,
                                                CcParsedStepLinkEnum link,
                                                CcPos field,
                                                CcPieceEnum piece,
                                                CcPos destination );

CcParsedStep * cc_parsed_step_failed_resurrection_append( CcParsedStep ** steps__iod_a,
                                                          CcParsedStepLinkEnum link,
                                                          CcPos field );

/** @} */ // end of step_convenience_append


/** @} */ // end of step_convenience


#endif /* __CC_PARSED_STEP_H__ */
