// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_STEP_H__
#define __CC_STEP_H__

#include <stdbool.h>

#include "cc_piece.h"

/**
    @file cc_step.h
    @brief Step enumerations, structures, and related functions.
*/


/**
    Step link enumeration.
*/
typedef enum CcStepLinkEnum
{
    CC_SLE_Start,
    CC_SLE_Next,
    CC_SLE_Distant,
    CC_SLE_Destination,
} CcStepLinkEnum;


/**
    Step side-effect enumeration.
*/
typedef enum CcSideEffectEnum
{
    CC_SEE_None,
    CC_SEE_Capture,
    CC_SEE_Displacement,
    CC_SEE_EnPassant,
    CC_SEE_Castle,
    CC_SEE_Promotion,
    CC_SEE_TagForPromotion,
    CC_SEE_Conversion,
    CC_SEE_FailedConversion,
    CC_SEE_Demotion,
    CC_SEE_Resurrection,
    CC_SEE_FailedResurrection,
} CcSideEffectEnum;

/**
    Step side-effect structure.
*/
typedef struct CcSideEffect
{
    CcSideEffectEnum type; /**< Type of side-effect. */

    union
    {
        struct  { CcPieceEnum piece; /**< Piece which has been captured. */
                  bool is_promo_tag_lost; /**< Flag, whether captured Pawn has lost its delayed promotion tag. */
                } capture; /**< Capture. */

        struct  { CcPieceEnum piece; /**< Piece which has been displaced. */
                  bool is_promo_tag_lost; /**< Flag, whether displaced Pawn has lost its delayed promotion tag. */
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
                  bool is_promo_tag_lost; /**< Flag, if converted Pawn has lost its delayed promotion tag. */
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
    @param is_promo_tag_lost Flag, if Pawn has lost its delayed promotion tag.
    @param start_i File, starting position.
    @param start_j Rank, starting position.
    @param dest_i File, destination position.
    @param dest_j Rank, destination position.

    @note
    Side-effect structure is returned as a value on the stack,
    not as a pointer to a newly allocated memory block on the heap.

    @return Step side-effect structure.
*/
CcSideEffect cc_side_effect( CcSideEffectEnum type,
                             CcPieceEnum piece,
                             bool is_promo_tag_lost,
                             int start_i, int start_j,
                             int dest_i, int dest_j );

/** @defgroup side_effect_convenience The side-effect conveniences
 *  The side-effect convenience functions are meant to be used instead of `cc_side_effect()`.

    They have minimal set of arguments required by the type of a side-effect,
    otherwise they behave exactly as their generic progenitor.

    @see cc_side_effect()
 *  @{
 */

CcSideEffect cc_side_effect_none();
CcSideEffect cc_side_effect_capture( CcPieceEnum piece, bool is_promo_tag_lost );
CcSideEffect cc_side_effect_displacement( CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j );
CcSideEffect cc_side_effect_en_passant( CcPieceEnum piece, int dest_i, int dest_j );
CcSideEffect cc_side_effect_castle( CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );
CcSideEffect cc_side_effect_promote( CcPieceEnum piece );
CcSideEffect cc_side_effect_tag_for_promotion();
CcSideEffect cc_side_effect_convert( CcPieceEnum piece, bool is_promo_tag_lost );
CcSideEffect cc_side_effect_failed_conversion();
CcSideEffect cc_side_effect_demote( CcPieceEnum piece, int dest_i, int dest_j );
CcSideEffect cc_side_effect_resurrect( CcPieceEnum piece, int dest_i, int dest_j );
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
CcStep * cc_step_new( CcStepLinkEnum link,
                      int i, int j, CcSideEffect side_effect,
                      CcFormatStepUsageEnum usage );

/**
    Appends a newly allocated step to a given linked list.

    @param steps Linked list to which a new step is appended, can be `NULL`.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.
    @param usage Step formatting usage.

    @note
    A new step is appended to `steps`, if it's a valid pointer.

    @note
    If not, appending is not done, but a new step is still returned.

    @see cc_step_new()

    @return
    A newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append_new( CcStep * const restrict steps,
                             CcStepLinkEnum link, int i, int j, CcSideEffect side_effect,
                             CcFormatStepUsageEnum usage );

/**
    Frees all steps in a linked list.

    @param steps_f Linked list of steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_step_free_all_steps( CcStep ** const steps_f );


/** @defgroup step_convenience The step conveniences
 *  The step convenience functions are meant to be used instead of `cc_step_new()`, and `cc_step_append_new()`

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_new(), cc_step_append_new()
 *  @{
 */

/** @defgroup step_convenience_new The new step conveniences
 *  The new step convenience functions are meant to be used instead of `cc_step_new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_new()
 *  @{
 */

CcStep * cc_step_none_new( CcStepLinkEnum link, int i, int j,
                           CcFormatStepUsageEnum usage );

CcStep * cc_step_capture_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, bool is_promo_tag_lost,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement_new( CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j,
                                   CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant_new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j,
                                 CcFormatStepUsageEnum usage );

CcStep * cc_step_castle_new( CcStepLinkEnum link, int i, int j,
                             CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                             CcFormatStepUsageEnum usage );

CcStep * cc_step_promote_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion_new( CcStepLinkEnum link, int i, int j,
                                        CcFormatStepUsageEnum usage );

CcStep * cc_step_convert_new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, bool is_promo_tag_lost,
                              CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion_new( CcStepLinkEnum link, int i, int j,
                                        CcFormatStepUsageEnum usage );

CcStep * cc_step_demote_new( CcStepLinkEnum link, int i, int j,
                             CcPieceEnum piece, int dest_i, int dest_j,
                             CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect_new( CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, int dest_i, int dest_j,
                                CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection_new( CcStepLinkEnum link, int i, int j,
                                          CcFormatStepUsageEnum usage );

/** @} */ // end of step_convenience_new


/** @defgroup step_convenience_append The append new step conveniences
 *  The append new step convenience functions are meant to be used instead of `cc_step_append_new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append_new()
 *  @{
 */

CcStep * cc_step_none_append_new( CcStep * const restrict steps,
                                  CcStepLinkEnum link, int i, int j,
                                  CcFormatStepUsageEnum usage );

CcStep * cc_step_capture_append_new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum piece, bool is_promo_tag_lost,
                                     CcFormatStepUsageEnum usage );

CcStep * cc_step_displacement_append_new( CcStep * const restrict steps,
                                          CcStepLinkEnum link, int i, int j,
                                          CcPieceEnum piece, bool is_promo_tag_lost, int dest_i, int dest_j,
                                          CcFormatStepUsageEnum usage );

CcStep * cc_step_en_passant_append_new( CcStep * const restrict steps,
                                        CcStepLinkEnum link, int i, int j,
                                        CcPieceEnum piece, int dest_i, int dest_j,
                                        CcFormatStepUsageEnum usage );

CcStep * cc_step_castle_append_new( CcStep * const restrict steps,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage );

CcStep * cc_step_promote_append_new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum piece,
                                     CcFormatStepUsageEnum usage );

CcStep * cc_step_tag_for_promotion_append_new( CcStep * const restrict steps,
                                               CcStepLinkEnum link, int i, int j,
                                               CcFormatStepUsageEnum usage );

CcStep * cc_step_convert_append_new( CcStep * const restrict steps,
                                     CcStepLinkEnum link, int i, int j,
                                     CcPieceEnum piece, bool is_promo_tag_lost,
                                     CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_conversion_append_new( CcStep * const restrict steps,
                                               CcStepLinkEnum link, int i, int j,
                                               CcFormatStepUsageEnum usage );

CcStep * cc_step_demote_append_new( CcStep * const restrict steps,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, int dest_i, int dest_j,
                                    CcFormatStepUsageEnum usage );

CcStep * cc_step_resurrect_append_new( CcStep * const restrict steps,
                                       CcStepLinkEnum link, int i, int j,
                                       CcPieceEnum piece, int dest_i, int dest_j,
                                       CcFormatStepUsageEnum usage );

CcStep * cc_step_failed_resurrection_append_new( CcStep * const restrict steps,
                                                 CcStepLinkEnum link, int i, int j,
                                                 CcFormatStepUsageEnum usage );

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
size_t cc_step_count_usage( CcStep const * const restrict steps, CcFormatStepUsageEnum usage );


#endif /* __CC_STEP_H__ */
