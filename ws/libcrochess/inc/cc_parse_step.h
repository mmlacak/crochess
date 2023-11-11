// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_STEP_H__
#define __CC_PARSE_STEP_H__

#include "cc_step.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static bool cc_check_step_link( CcStepLinkEnum sle,
//                                 char const * restrict step_start_an,
//                                 char const * restrict step_end_an,
//                                 CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_parsed_pos( char const * restrict step_start_an,
//                                  char const * restrict step_end_an,
//                                  CcStepLinkEnum sle,
//                                  CcPos * restrict pos__o,
//                                  char const ** restrict pos_end_an__o,
//                                  CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_parse_step( char const * restrict step_start_an,
//                            char const * restrict step_end_an,
//                            char const * restrict steps_end_an,
//                            CcGame * restrict game,
//                            CcPosPieceTag before_ply_start,
//                            CcStep ** restrict step__o,
//                            CcChessboard ** restrict cb__io,
//                            CcParseMsg ** restrict parse_msgs__iod );


bool cc_parse_steps( char const * restrict steps_start_an,
                     char const * restrict ply_end_an,
                     CcGame * restrict game,
                     CcPosPieceTag last_ply_destination,
                     CcStep ** restrict steps__o,
                     CcChessboard ** restrict cb__io,
                     CcParseMsg ** restrict parse_msgs__iod );


#endif /* __CC_PARSE_STEP_H__ */
