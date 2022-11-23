// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_MOVE_H__
#define __CC_PARSE_MOVE_H__

// #include "cc_chessboard.h"

// // #include "cc_step.h"
// #include "cc_ply.h"
#include "cc_move.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static bool cc_check_pre_plies_status( char const char_an,
//                                        CcGame * restrict game__io,
//                                        CcParseMsg ** restrict parse_msgs__io,
//                                        bool is_resign,
//                                        bool is_end,
//                                        bool is_won,
//                                        size_t max_len__d,
//                                        char const * restrict msg, ... )


// DOCS
bool cc_parse_move( char const * restrict move_an,
                    CcGame * restrict game,
                    CcMove ** restrict move__o,
                    CcParseMsg ** restrict parse_msgs__io );


#endif /* __CC_PARSE_MOVE_H__ */
