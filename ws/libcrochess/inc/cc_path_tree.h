// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_TREE_H__
#define __CC_PATH_TREE_H__

#include <stdbool.h>

#include "cc_piece.h"
// #include "cc_chessboard.h"
#include "cc_game.h"

#include "cc_pos.h"
#include "cc_typed_step_defs.h"
#include "cc_path_ctx.h"
#include "cc_path.h"


// todo :: DOCS

bool cc_path_side_effect( CcPosDesc moving_from,
                          CcTypedStep last_step,
                          CcPosDesc encounter,
                          CcPathContext * path_ctx__io,
                          CcPathSideEffectLink ** side_effect_link__o_a );

CcPathLink * cc_path_segment_one_step__new( CcSideEffect side_effect,
                                            CcPosDesc moving_from,
                                            CcTypedStep step,
                                            CcPathContext * path_ctx__io );

bool cc_path_tree( CcSideEffect side_effect,
                   CcPosDesc moving,
                   CcPathContext * path_ctx__io,
                   CcPathLink * pl__io );

bool cc_path_tree_init__new( CcGame * game,
                             CcPosDesc moving,
                             CcPathLink ** path_link__iod_a,
                             CcPathContext ** path_ctx__iod_a );


#endif /* __CC_PATH_TREE_H__ */
