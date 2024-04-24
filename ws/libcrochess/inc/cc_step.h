// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEP_H__
#define __CC_STEP_H__

#include <stddef.h>

#include "cc_pos.h"
#include "cc_side_effect.h"

/**
    @file cc_step.h
    @brief Steps linked list, storage for link type, field, side-effect.
*/

/**
    Macro to check if a given step link is valid.

    @param sle Step link, enumerated value.

    @see CcStepLinkEnum

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_STEP_LINK_VALID(sle) ( (sle) != CC_SLE_None )

/**
    Macro to check if a given step link is destination.

    @param sle Step link, enumerated value.

    @see CcStepLinkEnum

    @return `1` if valid, `0` otherwise.
*/
#define CC_IS_STEP_LINK_DESTINATION(sle) ( ( (sle) == CC_SLE_Destination ) || ( (sle) == CC_SLE_JustDestination ) )

/**
    Step link enumeration.
*/
typedef enum CcStepLinkEnum {
    CC_SLE_None, /**< Step link not found, uninitialized, not parsed yet, or error happened. */
    CC_SLE_Start, /**< Position from which a piece started moving. */
    CC_SLE_Reposition, /**< In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_SLE_Next, /**< Step immediately following previous, separated by . (dot). */
    CC_SLE_Distant, /**< Step not immediately following previous, separated by .. (double-dot). */
    CC_SLE_Destination, /**< Step to destination field, separated by - (hyphen). */
    CC_SLE_JustDestination, /**< Just destination field, no separators, no other steps. */
} CcStepLinkEnum;

/**
    Function returns string symbol, as used in algebraic notation, for a given step link.

    @param sle A step linkage.

    @note
    Returned string is pre-defined in library, not allocated one, so it needs no `free()`-ing.

    @return String symbol if link is valid, `NULL` otherwise.
*/
char const * cc_step_link_symbol( CcStepLinkEnum sle );

#define CC_MAX_LEN_STEP_LINK_SYMBOL (2)


/**
    Step structure, linked list.
*/
typedef struct CcStep {
    CcStepLinkEnum link; /**< Type of a link to previous step. */
    CcPos field; /**< Field of a step. */
    CcSideEffect side_effect; /**< Side-effect structure. */

    struct CcStep * next; /**< Next step in a linked list. */
} CcStep;

/**
    Returns a newly allocated step.

    @param link Type of a link to previous step.
    @param field Field.
    @param side_effect Side-effect structure.

    @return
    A newly allocated step if successful, `NULL` otherwise.
*/
CcStep * cc_step__new( CcStepLinkEnum link,
                       CcPos field, CcSideEffect side_effect );

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
CcStep * cc_step_append( CcStep ** steps__iod_a,
                         CcStepLinkEnum link, CcPos field, CcSideEffect side_effect );

/**
    Duplicates a given steps into a newly allocated linked list.

    @param steps__io Linked list to duplicate.

    @return
    A newly allocated steps if successful, `NULL` otherwise.
*/
CcStep * cc_step_duplicate_all__new( CcStep * steps__io );

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
CcStep * cc_step_extend( CcStep ** steps__iod_a,
                         CcStep ** steps__n );

/**
    Function returning count of steps, based on usage.

    @param steps Steps, a linked list.

    @return Count of steps if successful, `0` otherwise.
*/
size_t cc_step_count( CcStep * steps );

/**
    Function finds starting step.

    @param steps A linked list of steps.

    @return Starting step if successful, `NULL` otherwise
*/
CcStep * cc_step_find_start( CcStep * steps );

/**
    Function finds destination step.

    @param steps A linked list of steps.

    @note
    Destination step is one preceded by destination separator, i.e. `-` (hyphen),
    or is the last, non-starting step in a given list.

    @return Destination step if successful, `NULL` otherwise
*/
CcStep * cc_step_find_destination( CcStep * steps );

/**
    Frees all steps in a linked list.

    @param steps__f Linked list of steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_step_free_all( CcStep ** steps__f );

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
char * cc_step_all_to_short_string__new( CcStep * steps );


/** @defgroup step_convenience The step conveniences
 *  The step convenience functions are meant to be used instead of `cc_step__new()`,
    and `cc_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step__new(), cc_step_append()
 *  @{
 */

/** @defgroup step_convenience_new The new step conveniences
 *  The new step convenience functions are meant to be used instead of `cc_step__new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step__new()
 *  @{
 */

CcStep * cc_step_none__new( CcStepLinkEnum link, CcPos field );

CcStep * cc_step_capture__new( CcStepLinkEnum link, CcPos field,
                               CcPieceEnum piece, CcLosingTagEnum lost_tag );

CcStep * cc_step_displacement__new( CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos destination );

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, CcPos field,
                                  CcPieceEnum pawn, CcPos distant );

CcStep * cc_step_castle__new( CcStepLinkEnum link, CcPos field,
                              CcPieceEnum rook, CcPos start, CcPos destination );

CcStep * cc_step_promote__new( CcStepLinkEnum link, CcPos field,
                               CcPieceEnum captured, CcLosingTagEnum lost_tag, CcPieceEnum promoted_to );

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, CcPos field,
                                         CcPieceEnum captured, CcLosingTagEnum lost_tag );

CcStep * cc_step_convert__new( CcStepLinkEnum link, CcPos field,
                               CcPieceEnum piece, CcLosingTagEnum lost_tag );

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, CcPos field );

CcStep * cc_step_demote__new( CcStepLinkEnum link, CcPos field,
                              CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos distant );

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece, CcPos destination );

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, CcPos field );

/** @} */ // end of step_convenience_new


/** @defgroup step_convenience_append The append new step conveniences
 *  The append new step convenience functions are meant to be used instead of `cc_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append()
 *  @{
 */

CcStep * cc_step_none_append( CcStep ** steps__iod_a,
                              CcStepLinkEnum link, CcPos field );

CcStep * cc_step_capture_append( CcStep ** steps__iod_a,
                                 CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece, CcLosingTagEnum lost_tag );

CcStep * cc_step_displacement_append( CcStep ** steps__iod_a,
                                      CcStepLinkEnum link, CcPos field,
                                      CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos destination );

CcStep * cc_step_en_passant_append( CcStep ** steps__iod_a,
                                    CcStepLinkEnum link, CcPos field,
                                    CcPieceEnum pawn, CcPos distant );

CcStep * cc_step_castle_append( CcStep ** steps__iod_a,
                                CcStepLinkEnum link, CcPos field,
                                CcPieceEnum rook, CcPos start, CcPos destination );

CcStep * cc_step_promote_append( CcStep ** steps__iod_a,
                                 CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum captured, CcLosingTagEnum lost_tag, CcPieceEnum promoted_to );

CcStep * cc_step_tag_for_promotion_append( CcStep ** steps__iod_a,
                                           CcStepLinkEnum link, CcPos field,
                                           CcPieceEnum captured, CcLosingTagEnum lost_tag );

CcStep * cc_step_convert_append( CcStep ** steps__iod_a,
                                 CcStepLinkEnum link, CcPos field,
                                 CcPieceEnum piece, CcLosingTagEnum lost_tag );

CcStep * cc_step_failed_conversion_append( CcStep ** steps__iod_a,
                                           CcStepLinkEnum link, CcPos field );

CcStep * cc_step_demote_append( CcStep ** steps__iod_a,
                                CcStepLinkEnum link, CcPos field,
                                CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos distant );

CcStep * cc_step_resurrect_append( CcStep ** steps__iod_a,
                                   CcStepLinkEnum link, CcPos field,
                                   CcPieceEnum piece, CcPos destination );

CcStep * cc_step_failed_resurrection_append( CcStep ** steps__iod_a,
                                             CcStepLinkEnum link, CcPos field );

/** @} */ // end of step_convenience_append


/** @} */ // end of step_convenience


#endif /* __CC_STEP_H__ */
