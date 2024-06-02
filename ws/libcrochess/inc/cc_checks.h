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
    @param piece A piece.
    @param momentum Momentum.
    @param pos Position.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if piece is blocked at given position,
    - `CC_MBE_False` if piece is not blocked,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`, or given piece was `CC_PE_None`).
*/
CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              CcPieceEnum piece,
                                              uint momentum,
                                              CcPos pos );

/**
    Function checks if a piece can capture at given position.

    @param cb Chessboard.
    @param piece Capturing piece.
    @param pos Position.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if a piece can capture at given position,
    - `CC_MBE_False` if no capture is possible,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`).
*/
CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceEnum piece,
                                               CcPos pos );

/**
    Function checks if a piece can diverge from given position.

    @param cb Chessboard.
    @param piece Capturing piece.
    @param momentum Momentum.
    @param activator An activator.
    @param pos Position.

    @note
    Activator is last material (i.e. non-Wave) piece preceding the Wave in a cascade.

    @see CcMaybeBoolEnum

    @return
    One of `CcMaybeBoolEnum` values:
    - `CC_MBE_True` if a piece can diverge from given position,
    - `CC_MBE_False` if no divergence is possible,
    - `CC_MBE_Void` in case of error (given chessboard was `NULL`).
*/
CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceEnum piece,
                                               uint momentum,
                                               CcPieceEnum activator,
                                               CcPos pos );


#endif /* __CC_CHECKS_H__ */
