// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_SIDE_EFFECT_H__
#define __CC_PARSE_SIDE_EFFECT_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_side_effect.h"
// #include "cc_game.h"

#include "cc_parse_msg.h"

// static bool _cc_fail_with_msg_unrecognized_piece_symbol( char piece_symbol,
//                                                          char const * start_an,
//                                                          char const * end_an,
//                                                          CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_in_step( char const * msg_fmt,
//                                        char const * start_an,
//                                        char const * end_an,
//                                        CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_piece_in_side_effect( char const * msg_fmt,
//                                                     CcPieceType piece,
//                                                     bool capitalize,
//                                                     bool empty_field,
//                                                     char const * start_an,
//                                                     char const * end_an,
//                                                     CcParseMsg ** parse_msgs__iod );


// DOCS
bool cc_parse_side_effect( char const * side_effect_an,
                           char const * step_start_an,
                           char const * step_end_an,
                           bool is_turn_light,
                           cc_uint_t board_size,
                           CcSideEffect * side_effect__o,
                           CcParseMsg ** parse_msgs__iod );


#endif /* __CC_PARSE_SIDE_EFFECT_H__ */
