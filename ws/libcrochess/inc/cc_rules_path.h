// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_PATH_H__
#define __CC_RULES_PATH_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"


bool cc_is_pawn_capture_valid( CcChessboard * restrict cb,
                               CcPieceEnum piece,
                               CcPos start,
                               CcPos destination );

bool cc_is_pawn_step_valid( CcChessboard * restrict cb,
                            CcPieceEnum piece,
                            CcPos start,
                            CcPos destination );


#endif /* __CC_RULES_PATH_H__ */
