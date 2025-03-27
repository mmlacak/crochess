// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_UTILS_H__
#define __CC_PATH_UTILS_H__

#include <stdbool.h>

#include "cc_piece.h"
// #include "cc_chessboard.h"
#include "cc_game.h"

#include "cc_pos.h"
#include "cc_typed_step_defs.h"
#include "cc_path.h"

#include "cc_rules_defs.h"


// todo :: DOCS

bool cc_path_side_effect( CcChessboard * cb,
                          CcPosDesc moving,
                          CcPosDesc encounter,
                          CcTranceJourneyTypeEnum trance_journey_type,
                          CcPos displacement,
                          CcSideEffectLink ** side_effect_link__o_a );

// static CcPathLink * _cc_path_segment_one_step__new( CcSideEffect side_effect,
//                                                     CcGame * game,
//                                                     CcPosDesc moving,
//                                                     CcPos current_pos,
//                                                     CcMomentumUsage momentum,
//                                                     CcTypedStep step );

// static CcPathLink * _cc_path_one_step__new( CcSideEffect side_effect,
//                                             CcGame * game,
//                                             CcPosDesc moving,
//                                             CcPos current_pos,
//                                             CcMomentumUsage momentum,
//                                             CcTypedStep step );

CcPathLink * cc_path_tree_one_step__new( CcGame * game,
                                         CcPosDesc moving );


#endif /* __CC_PATH_UTILS_H__ */
