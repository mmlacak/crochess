// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULE_STEPS_H__
#define __CC_RULE_STEPS_H__

#include "cc_piece.h"
#include "cc_pos.h"
#include "cc_game.h"


// DOCS
bool cc_rule_steps_piece_pos_iter( CcChessboard * restrict cb,
                                   char piece_symbol,
                                   CcPieceEnum * restrict piece_o,
                                   CcPos * restrict start_o,
                                   bool initialize_iter );

// DOCS
bool cc_rule_steps_find_piece_start_pos( CcChessboard * restrict cb,
                                         CcPlyLinkEnum ple,
                                         char piece_symbol,
                                         int * restrict disamb_i_d,
                                         int * restrict disamb_j_d,
                                         int dest_i,
                                         int dest_j,
                                         CcPieceEnum * restrict piece_o,
                                         CcPos * restrict start_o );


// DOCS
bool cc_rule_steps_check_bishop( CcChessboard * restrict cb,
                                 CcPlyLinkEnum ple,
                                 CcPieceEnum piece,
                                 CcPos start,
                                 CcPos dest,
                                 CcPosLink ** restrict steps_o );

// DOCS
bool cc_rule_steps_check_movement( CcChessboard * restrict cb,
                                   CcPlyLinkEnum ple,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest,
                                   CcPosLink ** restrict steps_o );


#endif /* __CC_RULE_STEPS_H__ */
