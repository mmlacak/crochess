// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULE_STEPS_H__
#define __CC_RULE_STEPS_H__

#include "cc_piece.h"
#include "cc_pos.h"
#include "cc_game.h"


// DOCS
bool cc_rule_steps_piece_pos_iter( CcChessboard const * const restrict cb,
                                   char const piece_symbol,
                                   CcPieceEnum * const restrict piece_o,
                                   CcPos * const restrict start_o );

// DOCS
bool cc_rule_steps_find_piece_start_pos( CcChessboard const * const restrict cb,
                                         CcPlyLinkEnum const ple,
                                         char const piece_symbol,
                                         int const * const restrict disamb_i_d,
                                         int const * const restrict disamb_j_d,
                                         int const dest_i,
                                         int const dest_j,
                                         CcPieceEnum * const restrict piece_o,
                                         CcPos * const restrict start_o );


// DOCS
bool cc_rule_steps_check_bishop( CcChessboard const * const restrict cb,
                                 CcPlyLinkEnum const ple,
                                 CcPieceEnum const piece,
                                 CcPos const start,
                                 CcPos const dest,
                                 CcPosLink ** const restrict steps_o );

// DOCS
bool cc_rule_steps_check_movement( CcChessboard const * const restrict cb,
                                   CcPlyLinkEnum const ple,
                                   CcPieceEnum const piece,
                                   CcPos const start,
                                   CcPos const dest,
                                   CcPosLink ** const restrict steps_o );


#endif /* __CC_RULE_STEPS_H__ */
