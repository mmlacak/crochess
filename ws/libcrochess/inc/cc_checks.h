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
bool cc_check_momentum_for_movement( cc_piece_t piece, cc_uint_t momentum );

// TODO :: DOCS
bool cc_check_losing_tag_for_piece( cc_piece_t piece, CcLosingTagEnum lte );

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              cc_piece_t piece,
                                              cc_uint_t momentum,
                                              CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               cc_piece_t piece,
                                               cc_uint_t momentum,
                                               CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               cc_piece_t piece,
                                               cc_uint_t momentum,
                                               cc_piece_t activator,
                                               CcPos pos );

// TODO :: DOCS
bool cc_check_pawn_can_rush( cc_piece_t pawn, cc_tag_t tag, CcTypedStep step );


#endif /* __CC_CHECKS_H__ */
