// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STEPS_H__
#define __CC_STEPS_H__

#include <stdbool.h>

#include "cc_pos.h"
#include "cc_parse_defs.h"


/**
    A queue of steps.
*/
typedef struct CcSteps {
    CcStepLinkEnum step_link; /**< A step link enum. */
    CcPos pos; /**< A position. */

// TODO :: side-effects

    struct CcSteps * prev; /**< Link to a previous position. */
    struct CcSteps * next; /**< Link to a next position. */
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
