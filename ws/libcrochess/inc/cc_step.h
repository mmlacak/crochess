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


// /**
//     Macro to allocate new steps queue.

//     @param step_link A step link enum.
//     @param int_i File, horizontal coordinate.
//     @param int_j Rank, vertical coordinate.

//     @note
//     Side-effect in newly allocated ssteps queue is initialized as invalid.

//     @return Pointer to a newly allocated steps queue if successful, `NULL` otherwise.

//     @see cc_steps__new(), CC_SIDE_EFFECT_CAST_INVALID
// */
// #define CC_STEPS__NEW(step_link,int_i,int_j) \
//     cc_steps__new( (step_link), cc_pos( (int_i), (int_j) ), CC_SIDE_EFFECT_CAST_INVALID )

// /**
//     Macro to append a newly allocated step to existing queue.

//     @param ptr__steps__io A steps queue, to be appended.
//     @param step_link A step link enum.
//     @param int_i File, horizontal coordinate.
//     @param int_j Rank, vertical coordinate.

//     @note
//     Side-effect in newly allocated ssteps queue is initialized as invalid.

//     @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.

//     @see cc_steps_append(), CC_SIDE_EFFECT_CAST_INVALID
// */
// #define CC_STEPS_APPEND(ptr__steps__io,step_link,int_i,int_j) \
//     cc_steps_append( (ptr__steps__io), (step_link), cc_pos( (int_i), (int_j) ), CC_SIDE_EFFECT_CAST_INVALID )

// /**
//     Macro to append a newly allocated step to steps queue, which might not be alocated yet.

//     @param ptr_ptr__steps__io A steps queue, to be appended.
//     @param step_link A step link enum.
//     @param int_i File, horizontal coordinate.
//     @param int_j Rank, vertical coordinate.

//     @note
//     Side-effect in newly allocated ssteps queue is initialized as invalid.

//     @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.

//     @see cc_steps_append_if(), CC_SIDE_EFFECT_CAST_INVALID
// */
// #define CC_STEPS_APPEND_IF(ptr_ptr__steps__io,step_link,int_i,int_j) \
//     cc_steps_append_if( (ptr_ptr__steps__io), (step_link), cc_pos( (int_i), (int_j) ), CC_SIDE_EFFECT_CAST_INVALID )


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
    Step structure, linked list.
*/
typedef struct CcStep
{
    CcStepLinkEnum link; /**< Type of a link to previous step. */

// TODO :: change to CcPos (?)
    int i; /**< File of a step. */
    int j; /**< Rank of a step. */
// TODO :: change to CcPos (?)

    CcSideEffect side_effect; /**< Side-effect structure. */

    struct CcStep * next; /**< Next step in a linked list. */
} CcStep;

/**
    Returns a newly allocated step.

    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.

    @return
    A newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step__new( CcStepLinkEnum link,
                       int i, int j, CcSideEffect side_effect );

/**
    Appends a newly allocated step to a given linked list.

    @param steps__io Linked list to which a new step is appended.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.

    @return
    Weak pointer to a newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append( CcStep * restrict steps__io,
                         CcStepLinkEnum link, int i, int j, CcSideEffect side_effect );

/**
    Allocates a new step, appends it to a linked list.

    @param steps__io Linked list of steps, to which a newly allocated step is appended, can be `NULL`.
    @param link Type of a link to previous step.
    @param i File.
    @param j Rank.
    @param side_effect Side-effect structure.

    @note
    Linked list `*steps__io` can be `NULL`, a step will still be allocated, and returned.

    @note
    If linked list `*steps__io` is `NULL`, it will be initialized,
    with a newly allocated step as its first element.

    @return
    Weak pointer to a newly allocated step, is successful, `NULL` otherwise.
*/
CcStep * cc_step_append_if( CcStep ** restrict steps__io,
                            CcStepLinkEnum link, int i, int j, CcSideEffect side_effect );

/**
    Duplicates a given steps into a newly allocated linked list.

    @param steps__io Linked list to duplicate.

    @return
    A newly allocated steps, is successful, `NULL` otherwise.
*/
CcStep * cc_step_duplicate_all__new( CcStep * restrict steps__io );

/**
    Function returning count of steps, based on usage.

    @param steps Steps, a linked list.

    @return Count of steps if successful, `0` otherwise.
*/
size_t cc_step_count( CcStep * restrict steps );

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
bool cc_step_all_are_valid( CcStep * restrict steps, unsigned int board_size );

// /**
//     Function checks if positions are the congruent with a given steps.

//     @param steps Queue of steps.
//     @param positions Linked list of positions.

//     @note
//     Positions are assumed to be complete path over all step- (or capture-) fields.

//     @note
//     Each step is then expected to be found within positions, in the same order,
//     and with appropriate distance. For instance, if step is linked as a distant,
//     it shouldn't immediately follow previous step in `positions`.

//     @see cc_pos_is_congruent()

//     @return `true` if positions are congruent with steps, `false` otherwise.
// */
// bool cc_steps_are_congruent( CcSteps * restrict steps,
//                              CcPosLink * restrict positions );

/**
    Frees all steps in a linked list.

    @param steps__f Linked list of steps.

    @return `true` if successful, `false` otherwise.
*/
bool cc_step_free_all( CcStep ** restrict steps__f );

// /**
//     Function returns new string, containing user-readable representation of a steps.

//     @param steps A queue of steps.

//     @note
//     Each step is preceeded by the same separator as used in AN, e.g. `..` (double dot) is used for a distant step.

//     @note
//     Steps with unknown linkage are preceeded by `?`.

//     @note
//     Starting step is preceeded by ` (back-tick).

//     @return A newly allocated, zero-termianted string if successful, `NULL` otherwise
// */
// char * cc_steps_to_short_string__new( CcSteps * restrict steps );


/** @defgroup step_convenience The step conveniences
 *  The step convenience functions are meant to be used instead of `cc_step__new()`,
    `cc_step_append()` and `cc_step_append_if()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step__new(), cc_step_append(), cc_step_append_if()
 *  @{
 */

/** @defgroup step_convenience_new The new step conveniences
 *  The new step convenience functions are meant to be used instead of `cc_step__new()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step__new()
 *  @{
 */

CcStep * cc_step_none__new( CcStepLinkEnum link, int i, int j );

CcStep * cc_step_capture__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, CcTagEnum lost_tag );

CcStep * cc_step_displacement__new( CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j );

CcStep * cc_step_en_passant__new( CcStepLinkEnum link, int i, int j,
                                  CcPieceEnum piece, int dest_i, int dest_j );

CcStep * cc_step_castle__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );

CcStep * cc_step_promote__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece );

CcStep * cc_step_tag_for_promotion__new( CcStepLinkEnum link, int i, int j );

CcStep * cc_step_convert__new( CcStepLinkEnum link, int i, int j,
                               CcPieceEnum piece, CcTagEnum lost_tag );

CcStep * cc_step_failed_conversion__new( CcStepLinkEnum link, int i, int j );

CcStep * cc_step_demote__new( CcStepLinkEnum link, int i, int j,
                              CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j );

CcStep * cc_step_resurrect__new( CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, int dest_i, int dest_j );

CcStep * cc_step_failed_resurrection__new( CcStepLinkEnum link, int i, int j );

/** @} */ // end of step_convenience_new


/** @defgroup step_convenience_append The append new step conveniences
 *  The append new step convenience functions are meant to be used instead of `cc_step_append()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append()
 *  @{
 */

CcStep * cc_step_none_append( CcStep * restrict steps__io,
                              CcStepLinkEnum link, int i, int j );

CcStep * cc_step_capture_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag );

CcStep * cc_step_displacement_append( CcStep * restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j );

CcStep * cc_step_en_passant_append( CcStep * restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, int dest_i, int dest_j );

CcStep * cc_step_castle_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );

CcStep * cc_step_promote_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece );

CcStep * cc_step_tag_for_promotion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j );

CcStep * cc_step_convert_append( CcStep * restrict steps__io,
                                 CcStepLinkEnum link, int i, int j,
                                 CcPieceEnum piece, CcTagEnum lost_tag );

CcStep * cc_step_failed_conversion_append( CcStep * restrict steps__io,
                                           CcStepLinkEnum link, int i, int j );

CcStep * cc_step_demote_append( CcStep * restrict steps__io,
                                CcStepLinkEnum link, int i, int j,
                                CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j );

CcStep * cc_step_resurrect_append( CcStep * restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, int dest_i, int dest_j );

CcStep * cc_step_failed_resurrection_append( CcStep * restrict steps__io,
                                             CcStepLinkEnum link, int i, int j );

/** @} */ // end of step_convenience_append


/** @defgroup step_convenience_append_or_init The append new or init step conveniences
 *  The append new or init step convenience functions are meant to be used instead of
    `cc_step_append_if()`.

    They have minimal set of arguments required by the type of a step (its linkage),
    otherwise they behave exactly as their generic progenitor.

    @see cc_step_append_if()
 *  @{
 */

CcStep * cc_step_none_append_if( CcStep ** restrict steps__io,
                                 CcStepLinkEnum link, int i, int j );

CcStep * cc_step_capture_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag );

CcStep * cc_step_displacement_append_if( CcStep ** restrict steps__io,
                                         CcStepLinkEnum link, int i, int j,
                                         CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j );

CcStep * cc_step_en_passant_append_if( CcStep ** restrict steps__io,
                                       CcStepLinkEnum link, int i, int j,
                                       CcPieceEnum piece, int dest_i, int dest_j );

CcStep * cc_step_castle_append_if( CcStep ** restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum rook, int start_i, int start_j, int dest_i, int dest_j );

CcStep * cc_step_promote_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece );

CcStep * cc_step_tag_for_promotion_append_if( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, int i, int j );

CcStep * cc_step_convert_append_if( CcStep ** restrict steps__io,
                                    CcStepLinkEnum link, int i, int j,
                                    CcPieceEnum piece, CcTagEnum lost_tag );

CcStep * cc_step_failed_conversion_append_if( CcStep ** restrict steps__io,
                                              CcStepLinkEnum link, int i, int j );

CcStep * cc_step_demote_append_if( CcStep ** restrict steps__io,
                                   CcStepLinkEnum link, int i, int j,
                                   CcPieceEnum piece, CcTagEnum lost_tag, int dest_i, int dest_j );

CcStep * cc_step_resurrect_append_if( CcStep ** restrict steps__io,
                                      CcStepLinkEnum link, int i, int j,
                                      CcPieceEnum piece, int dest_i, int dest_j );

CcStep * cc_step_failed_resurrection_append_if( CcStep ** restrict steps__io,
                                                CcStepLinkEnum link, int i, int j );

/** @} */ // end of step_convenience_append_or_init

/** @} */ // end of step_convenience


#endif /* __CC_STEP_H__ */
