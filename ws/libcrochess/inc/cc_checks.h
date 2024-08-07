// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHECKS_H__
#define __CC_CHECKS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"

#include "cc_pos.h"
#include "cc_chessboard.h"


#define CC_CHECK_STEPS_NO_LIMIT (0)

// TODO :: Wave, transparency comparison, ... or ...
// TODO :: maybe add function just for checking castling (?)
bool cc_check_step_fields_are_empty( CcChessboard * cb,
                                     CcPos pos,
                                     CcPos step,
                                     cc_uint_t limit__d,
                                     bool check_pos );

bool cc_check_momentum_for_movement( CcPieceType piece, cc_uint_t momentum );

bool cc_check_losing_tag_for_piece( CcPieceType piece, CcLosingTagEnum lte );

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              CcPieceType piece,
                                              cc_uint_t momentum,
                                              CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               cc_uint_t momentum,
                                               CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               cc_uint_t momentum,
                                               CcPieceType activator,
                                               CcPos pos );


#endif /* __CC_CHECKS_H__ */
