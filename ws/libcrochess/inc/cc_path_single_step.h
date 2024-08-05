// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_SINGLE_STEP_H__
#define __CC_PATH_SINGLE_STEP_H__

#include <stdbool.h>

#include "cc_piece.h"
// #include "cc_variant.h"
#include "cc_chessboard.h"

#include "cc_pos.h"
#include "cc_pos_defs.h"
#include "cc_pos_utils.h"


// static bool cc_path_pawn( CcChessboard * cb,
//                           CcPosDesc pawn,
//                           cc_tag_t tag,
//                           CcPos from_pos,
//                           cc_uint_t momentum,
//                           bool is_accumulating_momentum,
//                           CcPosDescLink * already_traversed__d,
//                           CcPathLink ** path__o );


// TODO :: DOCS
bool cc_path_single_step( CcChessboard * cb,
                          cc_piece_t piece,
                          cc_tag_t tag,
                          cc_piece_t activator,
                          CcPos from_pos,
                          cc_uint_t momentum,
                          bool is_accumulating_momentum,
                          CcPathLink ** path__o );


#endif /* __CC_PATH_SINGLE_STEP_H__ */
