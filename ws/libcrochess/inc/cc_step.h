// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEP_H__
#define __CC_STEP_H__

#include <stddef.h>

#include "cc_side_effect.h"

/**
    @file cc_step.h
    @brief Steps linked list, storage for link type, field, side-effect.
*/


/**
    Step link enumeration.
*/
typedef enum CcStepLinkEnum
{
    CC_SLE_None, /**< Step link not found, uninitialized, or error happened. */
    CC_SLE_Start, /**< Position from which a piece started moving. */
    CC_SLE_Reposition, /**< In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_SLE_Next, /**< Step immediately following previous, separated by . (dot). */
    CC_SLE_Distant, /**< Step not immediately following previous, separated by .. (double-dot). */
    CC_SLE_Destination, /**< Step to destination field, separated by - (hyphen). */
} CcStepLinkEnum;

/**
    Function returns string symbol, as used in algebraic notation, for a given step link.

    @param sle A step linkage.

    @note
    Returned string is pre-defined in library, not allocated one, so it needs no `free()`-ing.

    @return String symbol if link is valid, `NULL` otherwise.
*/
char const * cc_step_link_symbol( CcStepLinkEnum sle );

/**
    Step formatting usage enumeration.

    This is used by formatter(s) to display only relevant
    portion of algebraic notation, based on actual usage.

    Usage is ordered in a sense that each item also implicitly
    covers all previous usages.

    For instance, having `CC_FSUE_Clarification` usage means
    that all steps with `CC_FSUE_User` will also be present
    in the output.

    Usage `CC_FSUE_User` indicates step(s) are originating from user input.
    Additional step(s) can be added by parser, rules engine, ...
    to clarify transformation(s).
*/
typedef enum CcFormatStepUsageEnum
{
    CC_FSUE_User, /**< User input, always present in output. */
    CC_FSUE_Clarification, /**< Clarification step(s), usually added by parser. */
    CC_FSUE_Clarification_NoOutput, /**< Clarification step(s), usually added by parser.
                                         Not needed in output for correct algebraic notation,
                                         e.g. starting position in a simple ply. */
    CC_FSUE_Addition, /**< Additional step(s), e.g. added by rules engine,
                           to count momentum for a Serpent's ply. */
    CC_FSUE_Debug, /**< Debug step(s) added by developer. */
} CcFormatStepUsageEnum;


/**
    Step structure, linked list.
*/
typedef struct CcStep
{
    CcStepLinkEnum link; /**< Type of a link to previous step. */
    int i; /**< File of a step. */
    int j; /**< Rank of a step. */
    CcSideEffect side_effect; /**< Side-effect structure. */
    CcFormatStepUsageEnum usage; /**< Step formatting usage. */
    struct CcStep * next; /**< Next step in a linked list. */
} CcStep;

/**
    Returns a newly allocated step.

    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.
    @param usage Step formatting usage.

    @return
    A newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step__new( CcStepLinkEnum link,
                       int i, int j, CcSideEffect side_effect,
                       CcFormatStepUsageEnum usage );

/**
    Appends a newly allocated step to a given linked list.

    @param steps__io Linked list to which a new step is appended.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.
    @param usage Step formatting usage.

    @return
    Weak pointer to a newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append( CcStep * restrict steps__io,
                         CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                         CcFormatStepUsageEnum usage );

/**
    Allocates a new step, appends it to a linked list.

    @param steps__io Linked list of steps, to which a newly allocated step is appended, can be `NULL`.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.
    @param usage Step formatting usage.

    @note
    Linked list `*steps__io` can be `NULL`, a step will still be allocated, and returned.

    @note
    If linked list `*steps__io` is `NULL`, it will be initialized,
    with a newly allocated step as its first element.

    @return
    Weak pointer to a newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append_or_init( CcStep ** restrict steps__io,
                                 CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                                 CcFormatStepUsageEnum usage );

/**
    Duplicates a given steps into a newly allocated linked list.

    @param steps__io Linked list to duplicate.

    @return
    A newly allocated steps, is successful, `NULL` otherwise.
*/
CcStep * cc_steps_duplicate_all__new( CcStep * restrict steps__io );

/**
    Checks if a given step is valid.

    @param step A step, technically a linked list of steps.
    @param board_size A chessboard size.

    @note
    Only single step is checked; neither an entire linked list, nor `link` member are checked.

    @return `true` if valid, `false` otherwise.
*/
bool cc_step_is_valid( CcStep * restrict step, unsigned int board_size );

/**
    Checks if all steps in a given linked list are valid.

    @param steps A linked list of steps.
    @param board_size A chessboard size.

    @return `true` if valid, `false` otherwise.
*/
bool cc_steps_are_valid( CcStep * restrict steps, unsigned int board_size );

/**
    Frees all steps in a linked list.

    @param steps__f Linked list of steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_steps_free_all( CcStep ** restrict steps__f );


/** @defgroup step_convenience The step conveniences
 *  The step convenience functions are meant to be used instead of `cc_step__new()`,
    `cc_step_append()` and `cc_step_append_or_init()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step__new(), cc_step_append(), cc_step_append_or_init()
 *  @{
 */

/** @defgroup step_convenience_new The new step conveniences
 *  The new step convenience functions are meant to be used instead of `cc_step__new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step__new()
 *  @{
 */

CcStep * cc_step_none__new( CcStepLinkEnum link, int i, int j,
                            CcFormatStepUsageEnum usage );

CcStep * cc_step_capture__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, CcTagEnum lost_tag,
                               CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement__new( CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, int i, int j,
                                  CcPieceEnum piece, int dest_i, int dest_j,
                                  CcFormatStepUsageEnum usage );

CcStep * cc_step_castle__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_promote__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece,
                               CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, int i, int j,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_convert__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, CcTagEnum lost_tag,
                               CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, int i, int j,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_demote__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j,
                                 CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage );

/** @} */ // end of step_convenience_new


/** @defgroup step_convenience_append The append new step conveniences
 *  The append new step convenience functions are meant to be used instead of `cc_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append()
 *  @{
 */

CcStep * cc_step_none_append( CcStep * restrict steps__io,
                              CcStepLinkEnum link, int i, int j,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_capture_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag,
                                 CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement_append( CcStep * restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                      CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant_append( CcStep * restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage );

CcStep * cc_step_castle_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage );

CcStep * cc_step_promote_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece,
                                 CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage );

CcStep * cc_step_convert_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag,
                                 CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcFormatStepUsageEnum usage );

CcStep * cc_step_demote_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect_append( CcStep * restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, int dest_i, int dest_j,
                                   CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection_append( CcStep * restrict steps__io,
                                             CcStepLinkEnum link, int i, int j,
                                             CcFormatStepUsageEnum usage );

/** @} */ // end of step_convenience_append



/** @defgroup step_convenience_append_or_init The append new or init step conveniences
 *  The append new or init step convenience functions are meant to be used instead of
    `cc_step_append_or_init()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append_or_init()
 *  @{
 */

CcStep * cc_step_none_append_or_init( CcStep ** restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcFormatStepUsageEnum usage );

CcStep * cc_step_capture_append_or_init( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, CcTagEnum lost_tag,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement_append_or_init( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, int i, int j,
                                              CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                              CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant_append_or_init( CcStep ** restrict steps__io,
                                            CcStepLinkEnum link, int i, int j,
                                            CcPieceEnum piece, int dest_i, int dest_j,
                                            CcFormatStepUsageEnum usage );

CcStep * cc_step_castle_append_or_init( CcStep ** restrict steps__io,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage );

CcStep * cc_step_promote_append_or_init( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion_append_or_init( CcStep ** restrict steps__io,
                                                   CcStepLinkEnum link, int i, int j,
                                                   CcFormatStepUsageEnum usage );

CcStep * cc_step_convert_append_or_init( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, CcTagEnum lost_tag,
                                         CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion_append_or_init( CcStep ** restrict steps__io,
                                                   CcStepLinkEnum link, int i, int j,
                                                   CcFormatStepUsageEnum usage );

CcStep * cc_step_demote_append_or_init( CcStep ** restrict steps__io,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect_append_or_init( CcStep ** restrict steps__io,
                                           CcStepLinkEnum link, int i, int j,
                                           CcPieceEnum piece, int dest_i, int dest_j,
                                           CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection_append_or_init( CcStep ** restrict steps__io,
                                                     CcStepLinkEnum link, int i, int j,
                                                     CcFormatStepUsageEnum usage );

/** @} */ // end of step_convenience_append_or_init

/** @} */ // end of step_convenience


/**
    Function returning count of steps, based on usage.

    @param steps Steps, a linked list.
    @param usage Step formatting usage.

    @note
    Each usage item also implicitly includes all previous usages.

    @note
    For instance, having `CC_FSUE_Clarification` usage means that
    all nodes with `CC_FSUE_User` will also be present in the output.

    @see
    CcFormatStepUsageEnum for order of usages.

    @return Count of steps if successful, `0` otherwise.
*/
size_t cc_step_count_usage( CcStep * restrict steps,
                            CcFormatStepUsageEnum usage );


#endif /* __CC_STEP_H__ */
