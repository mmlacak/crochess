// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEP_H__
#define __CC_STEP_H__

#include <stdbool.h>
#include <stdlib.h>

#include "cc_piece.h"
#include "cc_tag.h"

/**
    @file cc_step.h
    @brief Step enumerations, structures, and related functions.
*/


/**
    Step link enumeration.
*/
typedef enum CcStepLinkEnum
{
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
char const * cc_step_link_symbol( CcStepLinkEnum const sle );


/**
    Step side-effect enumeration.
*/
typedef enum CcSideEffectEnum
{
    CC_SEE_None, /**< No side effects. */
    CC_SEE_Capture, /**< Corresponds to `*`. */
    CC_SEE_Displacement, /**< Corresponds to `<`. */
    CC_SEE_EnPassant, /**< Corresponds to `:`. */
    CC_SEE_Castle, /**< Corresponds to `&`. */
    CC_SEE_Promotion, /**< Corresponds to `=`. */
    CC_SEE_TagForPromotion, /**< Corresponds to `=`. */
    CC_SEE_Conversion, /**< Corresponds to `%`. */
    CC_SEE_FailedConversion, /**< Corresponds to `%%`. */
    CC_SEE_Demotion, /**< Corresponds to `>`. */
    CC_SEE_Resurrection, /**< Corresponds to `$`. */
    CC_SEE_FailedResurrection, /**< Corresponds to `$$`. */
} CcSideEffectEnum;

/**
    Function checks if given side-effect enum is castling.

    @param see An enum

    @return `true` if castling, `false` otherwise.
*/
bool cc_side_effect_enum_is_castling( CcSideEffectEnum const see );


/**
    Step side-effect structure.
*/
typedef struct CcSideEffect
{
    CcSideEffectEnum type; /**< Type of side-effect. */

    union
    {
        struct  { CcPieceEnum piece; /**< Piece which has been captured. */
                  CcTagEnum lost_tag; /**< Flag, whether captured piece has lost its tag. */
                } capture; /**< Capture. */

        struct  { CcPieceEnum piece; /**< Piece which has been displaced. */
                  CcTagEnum lost_tag; /**< Flag, whether displaced piece has lost its tag. */
                  int dest_i; /**< File, displacement destination. */
                  int dest_j; /**< Rank, displacement destination. */
                } displacement; /**< Displacement, used during light Shaman's trance-journey. */

        struct  { CcPieceEnum piece; /**< Pawn which has been captured. */
                  int dest_i; /**< File of captured Pawn. */
                  int dest_j; /**< Rank of captured Pawn. */
                } en_passant; /**< En passant. */

        struct  { CcPieceEnum rook; /**< Rook which castled. */
                  int start_i; /**< File, starting position of a Rook. */
                  int start_j; /**< Rank, starting position of a Rook. */
                  int dest_i; /**< File, destination of a Rook. */
                  int dest_j; /**< Rank, destination of a Rook. */
                } castle; /**< Castling. */

        struct  { CcPieceEnum piece; /**< Piece to which Pawn has been promoted. */
                } promote; /**< Promotion. */

        struct  { CcPieceEnum piece; /**< Piece which has been converted. */
                  CcTagEnum lost_tag; /**< Flag, if converted piece has lost its tag. */
                } convert; /**< Conversion. */

        struct  { CcPieceEnum piece; /**< Piece which has been demoted to Pawn. */
                  int dest_i; /**< File, at which demoting happened. */
                  int dest_j; /**< Rank, at which demoting happened. */
                } demote; /**< Demoting. */

        struct  { CcPieceEnum piece; /**< Piece whic has been resurrected. */
                  int dest_i; /**< File, at which resurrection happened. */
                  int dest_j; /**< Rank, at which resurrection happened. */
                } resurrect; /**< Resurrection. */
    }; /**< Union of all substructures used by different step side-effects. */
} CcSideEffect;

/**
    Function returning step side-effect structure.

    @param type Side-effect enum.
    @param piece A piece.
    @param lost_tag Flag, if Pawn has lost its delayed promotion tag.
    @param start_i File, starting position.
    @param start_j Rank, starting position.
    @param dest_i File, destination position.
    @param dest_j Rank, destination position.

    @note
    Side-effect structure is returned as a value on the stack,
    not as a pointer to a newly allocated memory block on the heap.

    @return Step side-effect structure.
*/
CcSideEffect cc_side_effect( CcSideEffectEnum const type,
                             CcPieceEnum const piece,
                             CcTagEnum const lost_tag,
                             int const start_i, int const start_j,
                             int const dest_i, int const dest_j );

/** @defgroup side_effect_convenience The side-effect conveniences
 *  The side-effect convenience functions are meant to be used instead of `cc_side_effect()`.

    They have minimal set of arguments required by the type of a side-effect,
    otherwise they behave exactly as their generic progenitor.

    @see cc_side_effect()
 *  @{
 */

CcSideEffect cc_side_effect_none();
CcSideEffect cc_side_effect_capture( CcPieceEnum const piece, CcTagEnum const lost_tag );
CcSideEffect cc_side_effect_displacement( CcPieceEnum const piece, CcTagEnum const lost_tag, int const dest_i, int const dest_j );
CcSideEffect cc_side_effect_en_passant( CcPieceEnum const piece, int const dest_i, int const dest_j );
CcSideEffect cc_side_effect_castle( CcPieceEnum const rook, int const start_i, int const start_j, int const dest_i, int const dest_j );
CcSideEffect cc_side_effect_promote( CcPieceEnum const piece );
CcSideEffect cc_side_effect_tag_for_promotion();
CcSideEffect cc_side_effect_convert( CcPieceEnum const piece, CcTagEnum const lost_tag );
CcSideEffect cc_side_effect_failed_conversion();
CcSideEffect cc_side_effect_demote( CcPieceEnum const piece, int const dest_i, int const dest_j );
CcSideEffect cc_side_effect_resurrect( CcPieceEnum const piece, int const dest_i, int const dest_j );
CcSideEffect cc_side_effect_failed_resurrection();

/** @} */ // end of ply_convenience_new


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
CcStep * cc_step_new( CcStepLinkEnum const link,
                      int const i, int const j, CcSideEffect const side_effect,
                      CcFormatStepUsageEnum const usage );

/**
    Appends a newly allocated step to a given linked list.

    @param steps Linked list to which a new step is appended.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.
    @param usage Step formatting usage.

    @return
    Weak pointer to a newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append( CcStep * const restrict steps,
                         CcStepLinkEnum const link, int const i, int const j, CcSideEffect const side_effect,
                         CcFormatStepUsageEnum const usage );

/**
    Allocates a new step, appends it to a linked list.

    @param steps_io Linked list of steps, to which a newly allocated step is appended, can be `NULL`.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.
    @param usage Step formatting usage.

    @note
    Linked list `*steps_io` can be `NULL`, a step will still be allocated, and returned.

    @note
    If linked list `*steps_io` is `NULL`, it will be initialized,
    with a newly allocated step as its first element.

    @return
    Weak pointer to a newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append_or_init( CcStep ** const restrict steps_io,
                                 CcStepLinkEnum const link, int const i, int const j, CcSideEffect const side_effect,
                                 CcFormatStepUsageEnum const usage );

/**
    Duplicates a given steps into a newly allocated linked list.

    @param steps Linked list to duplicate.

    @return
    A newly allocated steps, is successful, `NULL` otherwise.
*/
CcStep * cc_step_duplicate_all_new( CcStep const * const restrict steps );

/**
    Frees all steps in a linked list.

    @param steps__f Linked list of steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_step_free_all_steps( CcStep ** const restrict steps__f );


/** @defgroup step_convenience The step conveniences
 *  The step convenience functions are meant to be used instead of `cc_step_new()`, and `cc_step_append()`

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_new(), cc_step_append()
 *  @{
 */

/** @defgroup step_convenience_new The new step conveniences
 *  The new step convenience functions are meant to be used instead of `cc_step_new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_new()
 *  @{
 */

CcStep * cc_step_none_new( CcStepLinkEnum const link, int const i, int const j,
                           CcFormatStepUsageEnum const usage );

CcStep * cc_step_capture_new( CcStepLinkEnum const link, int const i, int const j,
                              CcPieceEnum const piece, CcTagEnum const lost_tag,
                              CcFormatStepUsageEnum const usage );

CcStep * cc_step_displacement_new( CcStepLinkEnum const link, int const i, int const j,
                                   CcPieceEnum const piece, CcTagEnum const lost_tag, int const dest_i, int const dest_j,
                                   CcFormatStepUsageEnum const usage );

CcStep * cc_step_en_passant_new( CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece, int const dest_i, int const dest_j,
                                 CcFormatStepUsageEnum const usage );

CcStep * cc_step_castle_new( CcStepLinkEnum const link, int const i, int const j,
                             CcPieceEnum const rook, int const start_i, int const start_j, int const dest_i, int const dest_j,
                             CcFormatStepUsageEnum const usage );

CcStep * cc_step_promote_new( CcStepLinkEnum const link, int const i, int const j,
                              CcPieceEnum const piece,
                              CcFormatStepUsageEnum const usage );

CcStep * cc_step_tag_for_promotion_new( CcStepLinkEnum const link, int const i, int const j,
                                        CcFormatStepUsageEnum const usage );

CcStep * cc_step_convert_new( CcStepLinkEnum const link, int const i, int const j,
                              CcPieceEnum const piece, CcTagEnum const lost_tag,
                              CcFormatStepUsageEnum const usage );

CcStep * cc_step_failed_conversion_new( CcStepLinkEnum const link, int const i, int const j,
                                        CcFormatStepUsageEnum const usage );

CcStep * cc_step_demote_new( CcStepLinkEnum const link, int const i, int const j,
                             CcPieceEnum const piece, int const dest_i, int const dest_j,
                             CcFormatStepUsageEnum const usage );

CcStep * cc_step_resurrect_new( CcStepLinkEnum const link, int const i, int const j,
                                CcPieceEnum const piece, int const dest_i, int const dest_j,
                                CcFormatStepUsageEnum const usage );

CcStep * cc_step_failed_resurrection_new( CcStepLinkEnum const link, int const i, int const j,
                                          CcFormatStepUsageEnum const usage );

/** @} */ // end of step_convenience_new


/** @defgroup step_convenience_append The append new step conveniences
 *  The append new step convenience functions are meant to be used instead of `cc_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append()
 *  @{
 */

CcStep * cc_step_none_append( CcStep * const restrict steps,
                              CcStepLinkEnum const link, int const i, int const j,
                              CcFormatStepUsageEnum const usage );

CcStep * cc_step_capture_append( CcStep * const restrict steps,
                                 CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece, CcTagEnum const lost_tag,
                                 CcFormatStepUsageEnum const usage );

CcStep * cc_step_displacement_append( CcStep * const restrict steps,
                                      CcStepLinkEnum const link, int const i, int const j,
                                      CcPieceEnum const piece, CcTagEnum const lost_tag, int const dest_i, int const dest_j,
                                      CcFormatStepUsageEnum const usage );

CcStep * cc_step_en_passant_append( CcStep * const restrict steps,
                                    CcStepLinkEnum const link, int const i, int const j,
                                    CcPieceEnum const piece, int const dest_i, int const dest_j,
                                    CcFormatStepUsageEnum const usage );

CcStep * cc_step_castle_append( CcStep * const restrict steps,
                                CcStepLinkEnum const link, int const i, int const j,
                                CcPieceEnum const rook, int const start_i, int const start_j, int const dest_i, int const dest_j,
                                CcFormatStepUsageEnum const usage );

CcStep * cc_step_promote_append( CcStep * const restrict steps,
                                 CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece,
                                 CcFormatStepUsageEnum const usage );

CcStep * cc_step_tag_for_promotion_append( CcStep * const restrict steps,
                                           CcStepLinkEnum const link, int const i, int const j,
                                           CcFormatStepUsageEnum const usage );

CcStep * cc_step_convert_append( CcStep * const restrict steps,
                                 CcStepLinkEnum const link, int const i, int const j,
                                 CcPieceEnum const piece, CcTagEnum const lost_tag,
                                 CcFormatStepUsageEnum const usage );

CcStep * cc_step_failed_conversion_append( CcStep * const restrict steps,
                                           CcStepLinkEnum const link, int const i, int const j,
                                           CcFormatStepUsageEnum const usage );

CcStep * cc_step_demote_append( CcStep * const restrict steps,
                                CcStepLinkEnum const link, int const i, int const j,
                                CcPieceEnum const piece, int const dest_i, int const dest_j,
                                CcFormatStepUsageEnum const usage );

CcStep * cc_step_resurrect_append( CcStep * const restrict steps,
                                   CcStepLinkEnum const link, int const i, int const j,
                                   CcPieceEnum const piece, int const dest_i, int const dest_j,
                                   CcFormatStepUsageEnum const usage );

CcStep * cc_step_failed_resurrection_append( CcStep * const restrict steps,
                                             CcStepLinkEnum const link, int const i, int const j,
                                             CcFormatStepUsageEnum const usage );

/** @} */ // end of step_convenience_append

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
size_t cc_step_count_usage( CcStep const * const restrict steps,
                            CcFormatStepUsageEnum const usage );


#endif /* __CC_STEP_H__ */
