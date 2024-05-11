// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHECKS_H__
#define __CC_CHECKS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"

#include "cc_pos.h"
#include "cc_chessboard.h"


bool cc_check_step_fields_are_empty( CcChessboard * cb, CcPos pos, CcPos step, int limit );

/**
    Function checks if piece is blocked at given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param piece A piece.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if piece is blocked at given position,
    - `CC_MBE_False` if piece is not blocked,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`).
*/
CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              int i,
                                              int j,
                                              CcPieceEnum piece );

/**
    Function checks if a piece can capture at given position.

    @param cb Chessboard.
    @param i File, position along horizontal axis.
    @param j Rank, position along vertical axis.
    @param piece Capturing piece.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if a piece can capture at given position,
    - `CC_MBE_False` if no capture is possible,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`).
*/
CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               int i,
                                               int j,
                                               CcPieceEnum piece );


#endif /* __CC_CHECKS_H__ */
