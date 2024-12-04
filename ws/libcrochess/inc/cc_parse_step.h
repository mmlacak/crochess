// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_STEP_H__
#define __CC_PARSE_STEP_H__

#include "cc_parsed_step.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static void _cc_add_msg_invalid_step_link( char const * step_start_an,
//                                            char const * step_end_an,
//                                            CcParseMsg ** parse_msgs__iod );

// static bool _cc_check_parsed_pos( char const * step_start_an,
//                                   char const * step_end_an,
//                                   CcStepLinkTypeEnum sle,
//                                   CcPos * pos__o,
//                                   char const ** pos_end_an__o,
//                                   CcParseMsg ** parse_msgs__iod );

// static bool _cc_fill_in_castling_partial_destination( /* char const * step_start_an, */
//                                                       /* char const * step_end_an, */
//                                                       CcGame * game,
//                                                       CcPosDesc before_ply_start,
//                                                       char const * pos_end_an,
//                                                       CcPos * destination__io /* ,
//                                                       CcParseMsg ** parse_msgs__iod */ );

// static bool _cc_parse_step( char const * step_start_an,
//                             char const * step_end_an,
//                             char const * steps_end_an,
//                             CcGame * game,
//                             CcPosDesc before_ply_start,
//                             bool is_first_step,
//                             bool * had_disambiguation__io,
//                             CcStep ** step__o,
//                             CcChessboard ** cb__io,
//                             CcParseMsg ** parse_msgs__iod );


// DOCS
bool cc_parse_steps( char const * steps_start_an,
                     char const * ply_end_an,
                     CcGame * game,
                     CcPosDesc last_ply_destination,
                     CcStep ** steps__o,
                     CcChessboard ** cb__io,
                     CcParseMsg ** parse_msgs__iod );


#endif /* __CC_PARSE_STEP_H__ */
