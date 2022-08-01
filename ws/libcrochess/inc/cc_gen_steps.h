// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_STEPS_H__
#define __CC_GEN_STEPS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


#define CC_STEPS_BISHOP_LEN (4)
#define CC_STEPS_BISHOP_SIZE (CC_STEPS_BISHOP_LEN + 1)
extern CcPos const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ];


// DOCS
bool cc_piece_pos_iter( CcChessboard * restrict cb_before_activation,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io );

CcPosLink * cc_piece_path_iter__new( CcChessboard * restrict cb_before_activation,
                                     CcPieceEnum piece,
                                     CcPos start,
                                     CcPos destination );


#endif /* __CC_GEN_STEPS_H__ */
