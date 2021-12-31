// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_STEPS_H__
#define __CC_GEN_STEPS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


// DOCS
bool cc_gen_steps_piece_pos_iter( CcChessboard * restrict cb,
                                  CcPieceEnum piece,
                                  bool include_opposite,
                                  CcPos * restrict pos__io );


#endif /* __CC_GEN_STEPS_H__ */
