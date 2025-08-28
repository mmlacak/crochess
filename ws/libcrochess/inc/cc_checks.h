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

//
// Piece checks

// TODO :: DOCS
bool cc_piece_is_one_step( CcPieceTagType piece,
                                 CcPieceTagType activator );

// TODO :: DOCS
bool cc_piece_is_two_step( CcPieceTagType piece,
                                 CcPieceTagType activator );

// TODO :: DOCS
bool cc_piece_is_many_steps( CcPieceTagType piece );

bool cc_check_piece_can_lose_tag( CcPieceTagType piece,
                                  CcLosingTagType ltt,
                                  bool compare_tag_and_losing_tag );

bool cc_check_piece_is_blocked( CcPieceTagType moving,
                                CcPieceTagType encounter,
                                cc_uint_t momentum );

bool cc_check_piece_can_step_over( CcPieceTagType moving,
                                   CcPieceTagType encounter,
                                   cc_uint_t momentum );

bool cc_check_piece_can_capture( CcPieceTagType moving,
                                 CcPieceTagType encounter );

bool cc_check_piece_can_activate( CcPieceTagType moving,
                                  CcPieceTagType encounter,
                                  cc_uint_t momentum,
                                  CcStepTypeEnum step_type );

//
// Positional checks

bool cc_check_piece_is_blocked_at( CcChessboard * cb,
                                   CcPieceTagType moving,
                                   CcActivationDesc act_desc,
                                   bool is_first_ply,
                                   CcPos pos );

bool cc_check_piece_can_capture_at( CcChessboard * cb,
                                    CcPieceTagType moving,
                                    CcPos pos );

bool cc_check_piece_can_activate_at( CcChessboard * cb,
                                     CcPieceTagType moving,
                                     CcActivationDesc act_desc,
                                     bool is_first_ply,
                                     CcPos destination,
                                     CcStepTypeEnum step_type );

bool cc_check_piece_can_diverge_at( CcChessboard * cb,
                                    CcPieceTagType moving,
                                    cc_uint_t momentum,
                                    CcPieceTagType activator,
                                    CcPos pos );

bool cc_check_castling_step_fields( CcChessboard * cb,
                                    CcPos king_start,
                                    CcPos king_dest,
                                    CcPos rook_start,
                                    CcPos rook_dest );

bool cc_find_en_passant_target( CcChessboard * cb,
                                CcPieceTagType private,
                                CcActivationDesc act_desc,
                                bool is_first_ply,
                                CcPos destination,
                                CcPosDesc * target__o );


#endif /* __CC_CHECKS_H__ */
