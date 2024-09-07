// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_UTILS_H__
#define __CC_POS_UTILS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos, cc_uint_t momentum );

bool cc_calc_checked_momentum( cc_uint_t * momentum__io, CcMaybeBoolEnum accumulating );

// TODO :: REDO
CcPathLink * cc_build_path_segment__new( CcChessboard * cb,
                                         CcPos pos,
                                         cc_uint_t momentum,
                                         CcMaybeBoolEnum accumulating,
                                         CcTypedStepLink * steps );


bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected__d,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io );


#endif /* __CC_POS_UTILS_H__ */
