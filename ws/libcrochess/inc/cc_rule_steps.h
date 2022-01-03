// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULE_STEPS_H__
#define __CC_RULE_STEPS_H__

#include "cc_piece.h"
#include "cc_pos.h"
#include "cc_game.h"


// DOCS
bool cc_rule_steps_find_piece_start_pos( CcGame * restrict game,
                                         CcPlyLinkEnum ple,
                                         CcPieceEnum piece,
                                         bool include_opposite,
                                         int disamb_i__d,
                                         int disamb_j__d,
                                         CcPos dest,
                                         CcPos * restrict start__o );


// TODO :: REDESIGN
bool cc_rule_steps_is_ply_allowed( CcGame * restrict game,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest );
// TODO :: REDESIGN


// DOCS
bool cc_rule_steps_check_bishop( CcGame * restrict game,
                                 CcPlyLinkEnum ple,
                                 CcPieceEnum piece,
                                 CcPos start,
                                 CcPos dest,
                                 CcPosLink ** restrict pls__o );

// DOCS
bool cc_rule_steps_check_movement( CcGame * restrict game,
                                   CcPlyLinkEnum ple,
                                   CcPieceEnum piece,
                                   CcPos start,
                                   CcPos dest,
                                   CcPosLink ** restrict pls__o );


#endif /* __CC_RULE_STEPS_H__ */
