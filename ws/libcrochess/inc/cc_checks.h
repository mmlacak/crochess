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


// // todo :: REDO
// CcMaybeBoolEnum cc_check_momentum_for_next_step( CcPieceType piece,
//                                                  CcTagType tag,
//                                                  cc_uint_t * momentum__io );

bool cc_check_piece_can_lose_tag( CcPieceType piece, CcLosingTagType ltt );

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              CcPieceType piece,
                                              CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               CcPos pos );

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               cc_uint_t momentum,
                                               CcPieceType activator,
                                               CcPos pos );

CcMaybeBoolEnum cc_check_castling_step_fields( CcChessboard * cb,
                                               CcPos king_start,
                                               CcPos king_dest,
                                               CcPos rook_start,
                                               CcPos rook_dest );


#endif /* __CC_CHECKS_H__ */
