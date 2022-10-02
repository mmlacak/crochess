// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEPS_H__
#define __CC_STEPS_H__

#include <stdbool.h>

#include "cc_pos.h"
#include "cc_parse_defs.h"

/**
    @file cc_steps.h
    @brief Steps queue, functions declarations.
*/

/**
    Macro to allocate new steps queue.

    @param step_link A step link enum.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @note
    Side-effect in newly allocated ssteps queue is initialized as invalid.

    @return Pointer to a newly allocated steps queue if successful, `NULL` otherwise.

    @see cc_steps__new(), CC_SIDE_EFFECT_CAST_INVALID
*/
#define CC_STEPS__NEW(step_link,int_i,int_j) \
    cc_steps__new( (step_link), cc_pos( (int_i), (int_j) ), CC_SIDE_EFFECT_CAST_INVALID )

/**
    Macro to append a newly allocated step to existing queue.

    @param ptr__steps__io A steps queue, to be appended.
    @param step_link A step link enum.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @note
    Side-effect in newly allocated ssteps queue is initialized as invalid.

    @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.

    @see cc_steps_append(), CC_SIDE_EFFECT_CAST_INVALID
*/
#define CC_STEPS_APPEND(ptr__steps__io,step_link,int_i,int_j) \
    cc_steps_append( (ptr__steps__io), (step_link), cc_pos( (int_i), (int_j) ), CC_SIDE_EFFECT_CAST_INVALID )

/**
    Macro to append a newly allocated step to steps queue, which might not be alocated yet.

    @param ptr_ptr__steps__io A steps queue, to be appended.
    @param step_link A step link enum.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @note
    Side-effect in newly allocated ssteps queue is initialized as invalid.

    @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.

    @see cc_steps_append_if(), CC_SIDE_EFFECT_CAST_INVALID
*/
#define CC_STEPS_APPEND_IF(ptr_ptr__steps__io,step_link,int_i,int_j) \
    cc_steps_append_if( (ptr_ptr__steps__io), (step_link), cc_pos( (int_i), (int_j) ), CC_SIDE_EFFECT_CAST_INVALID )


/**
    A queue of steps.
*/
typedef struct CcSteps {
    CcStepLinkEnum step_link; /**< Step link enum. */
    CcPos pos; /**< Position. */
    CcSideEffect side_effect; /**< Side-effect. */

    struct CcSteps * prev; /**< Link to previous position. */
    struct CcSteps * next; /**< Link to next position. */
} CcSteps;

/**
    Function allocates a new steps queue.

    @param step_link A step link enum.
    @param pos A position.
    @param side_effect A side-effect.

    @return Pointer to a newly allocated step if successful, `NULL` otherwise.
*/
CcSteps * cc_steps__new( CcStepLinkEnum step_link,
                         CcPos pos,
                         CcSideEffect side_effect );

/**
    Function appends a newly allocated step to a given queue.

    @param steps__io _Input/output_ parameter, queue.
    @param step_link A step link enum.
    @param pos A position.
    @param side_effect A side-effect.

    @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.
*/
CcSteps * cc_steps_append( CcSteps * restrict steps__io,
                           CcStepLinkEnum step_link,
                           CcPos pos,
                           CcSideEffect side_effect );

/**
    Appends newly allocated step to a steps queue, which might not be alocated yet.

    @param steps__io _Input/output_ parameter, queue, can be `NULL`.
    @param step_link A step link enum.
    @param pos A position.
    @param side_effect A side-effect.

    @note
    Queue `*steps__io` can be `NULL`, a step will still be allocated, and returned.

    @note
    If queue `*steps__io` is `NULL`, it will be initialized,
    with a newly allocated step as its first element.

    @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.
*/
CcSteps * cc_steps_append_if( CcSteps ** restrict steps__io,
                              CcStepLinkEnum step_link,
                              CcPos pos,
                              CcSideEffect side_effect );

/**
    Function checks if positions are the congruent with a given steps.

    @param steps Queue of steps.
    @param positions Linked list of positions.

    @note
    Positions are assumed to be complete path over all step- (or capture-) fields.

    @note
    Each step is then expected to be found within positions, in the same order,
    and with appropriate distance. For instance, if step is linked as a distant,
    it shouldn't immediately follow previous step in `positions`.

    @see cc_pos_is_congruent()

    @return `true` if positions are congruent with steps, `false` otherwise.
*/
bool cc_steps_are_congruent( CcSteps * restrict steps,
                             CcPosLink * restrict positions );

/**
    Frees all steps in a queue.

    @param steps__f A steps queue to free.

    @return `true` if successful, `false` otherwise.
*/
bool cc_steps_free_all( CcSteps ** restrict steps__f );

/**
    Function returns length of a queue.

    @param steps A queue of steps.

    @return Length of a queue if successful, `0` otherwise.
*/
size_t cc_steps_len( CcSteps * restrict steps );

/**
    Function returns new string, containing user-readable representation of a steps.

    @param steps A queue of steps.

    @note
    Each step is preceeded by the same separator as used in AN, e.g. `..` (double dot) is used for a distant step.

    @note
    Steps with unknown linkage are preceeded by `?`.

    @note
    Starting step is preceeded by ` (back-tick).

    @return A newly allocated, zero-termianted string if successful, `NULL` otherwise
*/
char * cc_steps_to_short_string__new( CcSteps * restrict steps );


#endif /* __CC_STEPS_H__ */
