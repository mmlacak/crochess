// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_STEP_H__
#define __CC_PARSE_STEP_H__

#include "cc_step.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static bool _cc_fail_with_msg_in_step( char const * msg_fmt,
//                                        char const * step_start_an,
//                                        char const * step_end_an,
//                                        CcParseMsg ** parse_msgs__iod );

// static bool _cc_parse_step( char const * step_start_an,
//                             char const * step_end_an,
//                             char const * steps_end_an,
//                             bool is_turn_light,
//                             cc_uint_t size,
//                             bool is_first_step,
//                             CcStep ** steps__o,
//                             CcParseMsg ** parse_msgs__iod );


bool cc_parse_steps( char const * steps_start_an,
                     char const * steps_end_an,
                     bool is_turn_light,
                     cc_uint_t board_size,
                     CcStep ** steps__o,
                     CcParseMsg ** parse_msgs__iod );


#endif /* __CC_PARSE_STEP_H__ */
