// Copyright (c) 2024, 2025 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_UTILS_H__
#define __CC_PATH_UTILS_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_chessboard.h"

#include "cc_pos.h"
#include "cc_typed_step_defs.h"
#include "cc_path.h"

// todo :: DOCS

// static CcPathLink * _cc_path_one_step__new( CcChessboard * cb,
//                                             CcPosDesc pd,
//                                             CcTypedStep const * step,
//                                             CcSideEffect side_effect,
//                                             CcMomentum momentum );

CcPathLink * cc_path_tree_one_step__new( CcChessboard * cb,
                                         CcPosDesc pd );


#endif /* __CC_PATH_UTILS_H__ */
