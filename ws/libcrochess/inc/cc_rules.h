// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_H__
#define __CC_RULES_H__

#include <stdbool.h>

#include "cc_game.h"
#include "cc_steps.h"
#include "cc_parse_msgs.h"


// static bool cc_append_steps( CcGame * restrict game,
//                              char const * restrict ply_start_str,
//                              char const * restrict ply_end_str,
//                              CcChessboard * restrict cb_before_activation,
//                              CcSteps ** restrict steps__io,
//                              CcParseMsgs ** restrict parse_msgs__io )

// // static bool cc_do_make_plies( char const * restrict move_an_str,
// //                               CcGame * restrict game__io,
// //                               CcParseMsgs ** restrict parse_msgs__io )
//
// static bool cc_make_plies( char const * restrict move_an_str,
//                            CcGame * restrict game,
//                            CcChessboard ** restrict cb__o,
//                            CcGameStatusEnum * restrict gse__o,
//                            CcParseMsgs ** restrict parse_msgs__io )


bool cc_make_move( char const * restrict move_an_str,
                   CcGame * restrict game,
                   CcChessboard ** restrict cb__o,
                   CcGameStatusEnum * restrict gse__o,
                   CcParseMsgs ** restrict parse_msgs__io );

bool cc_apply_move( char const * restrict move_an_str,
                    CcGame * restrict game__io,
                    CcParseMsgs ** restrict parse_msgs__io );


#endif /* __CC_RULES_H__ */
