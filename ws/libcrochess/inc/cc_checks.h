// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHECKS_H__
#define __CC_CHECKS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"

#include "cc_pos.h"
#include "cc_chessboard.h"


// TODO :: DOCS
bool cc_check_step_fields_are_empty( CcChessboard * cb, CcPos pos, CcPos step, int limit );

// TODO :: DOCS
bool cc_check_momentum_for_movement( CcPieceEnum piece, cc_uint momentum );

// TODO :: DOCS
bool cc_check_losing_tag_for_piece( CcPieceEnum piece, CcLosingTagEnum lte );

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              CcPieceEnum piece,
                                              cc_uint momentum,
                                              CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceEnum piece,
                                               cc_uint momentum,
                                               CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceEnum piece,
                                               cc_uint momentum,
                                               CcPieceEnum activator,
                                               CcPos pos );

// TODO :: DOCS
bool cc_check_pawn_can_rush( CcPieceEnum pawn, CcTagEnum tag, CcTypedStep step );


#endif /* __CC_CHECKS_H__ */
