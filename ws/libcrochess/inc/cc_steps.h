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

    @return Pointer to a newly allocated steps queue if successful, `NULL` otherwise.

    @see cc_steps__new()
*/
#define CC_STEPS__NEW(step_link,int_i,int_j) \
    cc_steps__new( (step_link), cc_pos( (int_i), (int_j) ) )

/**
    Macro to append a newly allocated step to existing queue.

    @param ptr__pos_link__io a position linked list, to be appended.
    @param step_link A step link enum.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.

    @see cc_steps_append()
*/
#define CC_STEPS_APPEND(ptr__steps__io,step_link,int_i,int_j) \
    cc_steps_append( (ptr__steps__io), (step_link), cc_pos( (int_i), (int_j) ) )

/**
    Macro to append a newly allocated step to steps queue, which might not be alocated yet.

    @param ptr_ptr__pos_link__io a position linked list, to be appended.
    @param step_link A step link enum.
    @param int_i File, horizontal coordinate.
    @param int_j Rank, vertical coordinate.

    @return A weak pointer to a newly allocated step if successful, `NULL` otherwise.

    @see cc_steps_append_or_init()
*/
#define CC_STEPS_APPEND_OR_INIT(ptr_ptr__steps__io,step_link,int_i,int_j) \
    cc_steps_append_or_init( (ptr_ptr__steps__io), (step_link), cc_pos( (int_i), (int_j) ) )


/**
    A queue of steps.
*/
typedef struct CcSteps {
    CcStepLinkEnum step_link; /**< Step link enum. */
    CcPos pos; /**< Position. */

// TODO :: side-effects

    struct CcSteps * prev; /**< Link to previous position. */
    struct CcSteps * next; /**< Link to next position. */
} CcSteps;

/**
    Function allocates a new linked position.

    @param step_link A step link enum.
    @param pos A position.

    @return Pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcSteps * cc_steps__new( CcStepLinkEnum step_link,
                         CcPos pos );

/**
    Function appends a newly allocated linked position to a given linked list.

    @param pos_link__io _Input/output_ parameter, linked list.
    @param step_link A step link enum.
    @param pos A position.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcSteps * cc_steps_append( CcSteps * restrict pos_link__io,
                           CcStepLinkEnum step_link,
                           CcPos pos );

/**
    Allocates a new linked position, appends it to a linked list.

    @param pos_link__io _Input/output_ parameter, linked list, can be `NULL`.
    @param step_link A step link enum.
    @param pos A position.

    @note
    Linked list `*pos_link__io` can be `NULL`, a linked position will still be
    allocated, and returned.

    @note
    If linked list `*pos_link__io` is `NULL`, it will be initialized,
    with a newly allocated linked position as its first element.

    @return A weak pointer to a newly allocated linked position if successful, `NULL` otherwise.
*/
CcSteps * cc_steps_append_or_init( CcSteps ** restrict pos_link__io,
                                   CcStepLinkEnum step_link,
                                   CcPos pos );

/**
    Frees all positions in a linked list.

    @param pos_link__f Linked list of positions.

    @return `true` if successful, `false` otherwise.
*/
bool cc_steps_free_all( CcSteps ** restrict pos_link__f );


#endif /* __CC_STEPS_H__ */
