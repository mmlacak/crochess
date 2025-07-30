// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_CHECKS_H__
#define __CC_CHECKS_H__

#include <stdbool.h>

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_tag.h"

#include "cc_pos.h"
#include "cc_chessboard.h"
#include "cc_move.h"
#include "cc_game.h"

#include "cc_typed_step.h"
#include "cc_typed_step_defs.h"


#define CC_CHECK_STEPS_NO_LIMIT (0)


bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse );

bool cc_check_piece_can_lose_tag( CcPieceTagType piece,
                                  CcLosingTagType ltt,
                                  bool compare_tag_and_losing_tag );

// TODO :: DOCS
bool cc_check_piece_can_capture_other( CcPieceTagType moving, CcPieceTagType still );


// TODO :: DOCS
bool cc_check_piece_is_blocked_at( CcChessboard * cb,
                                   CcPieceTagType piece,
                                   CcPos pos );

// TODO :: DOCS
bool cc_check_piece_can_capture_at( CcChessboard * cb,
                                    CcPieceTagType piece,
                                    CcPos pos );

// TODO :: DOCS
bool cc_check_piece_can_diverge_at( CcChessboard * cb,
                                    CcPieceTagType piece,
                                    cc_uint_t momentum,
                                    CcPieceTagType activator,
                                    CcPos pos );

CcMaybeBoolEnum cc_check_castling_step_fields( CcChessboard * cb,
                                               CcPos king_start,
                                               CcPos king_dest,
                                               CcPos rook_start,
                                               CcPos rook_dest );

CcMaybeBoolEnum cc_check_piece_can_activate( CcPieceTagType moving,
                                             CcPieceTagType encounter,
                                             cc_uint_t momentum,
                                             CcStepTypeEnum step_type );

CcMaybeBoolEnum cc_check_piece_can_activate_at( CcChessboard * cb,
                                                CcPieceTagType moving,
                                                CcActivationDesc act_desc,
                                                CcPos destination,
                                                CcStepTypeEnum step_type );

CcMaybeBoolEnum cc_find_en_passant_target( CcChessboard * cb,
                                           CcPieceTagType private,
                                           CcActivationDesc act_desc,
                                           CcPos destination,
                                           CcPosDesc * target__o );


#endif /* __CC_CHECKS_H__ */
