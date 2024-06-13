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

/**
    @file cc_path_single_step.h
    @brief Path generating, filtering for single-step pieces.
*/


// static bool cc_path_pawn( CcChessboard * cb,
//                           CcPosDesc pawn,
//                           CcTagEnum tag,
//                           CcPos from_pos,
//                           uint momentum,
//                           bool is_accumulating_momentum,
//                           CcPosDescLink * already_traversed__d,
//                           CcPathLink ** path__o );


bool cc_path_single_step( CcChessboard * cb,
                          CcPieceEnum piece,
                          CcTagEnum tag,
                          CcPieceEnum activator,
                          CcPos from_pos,
                          uint momentum,
                          bool is_accumulating_momentum,
                          CcPathLink ** path__o );


#endif /* __CC_PATH_SINGLE_STEP_H__ */
