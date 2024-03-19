// Copyright (c) 2021, 2022, 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_UTILS_H__
#define __CC_PATH_UTILS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

/**
    @file cc_path_utils.h
    @brief Path utils.
*/


bool cc_is_step_capture( CcPieceEnum activator,
                         CcPieceEnum piece,
                         CcPos step,
                         CcPos step_2 );

bool cc_is_step_miracle( CcPieceEnum piece, CcPos step );

bool cc_is_step_shamans_capture( CcPieceEnum piece, CcPos step );

bool cc_is_the_same_color( CcPieceEnum piece, CcPos pos );


// TODO ::
//
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
//                              CcPptLink * restrict positions );
//
// TODO ::


#endif /* __CC_PATH_UTILS_H__ */
