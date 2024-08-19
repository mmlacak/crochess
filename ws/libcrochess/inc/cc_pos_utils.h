// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_UTILS_H__
#define __CC_POS_UTILS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos, cc_uint_t momentum );

bool cc_calc_checked_momentum( cc_uint_t * momentum__io, bool accumulating );

CcPosDescLink * cc_apply_steps_to_position__new( CcChessboard * cb,
                                                 CcPos pos,
                                                 cc_uint_t momentum,
                                                 bool accumulating,
                                                 CcTypedStepLink * steps );


bool cc_append_pos_to_pos_desc_link( CcChessboard * cb,
                                     CcPos pos,
                                     cc_uint_t momentum,
                                     CcPosDescLink ** pdl__iod_a );

bool cc_validate_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link );

// TODO :: DOCS
bool cc_update_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link__io );

// TODO :: DOCS
bool cc_apply_pos_desc_link( CcChessboard ** cb__io_r, CcPosDescLink * pd_link );


// TODO :: DOCS
bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected,
                        CcPieceType piece,
                        bool include_opponent,
                        CcPos * pos__io );


#endif /* __CC_POS_UTILS_H__ */
