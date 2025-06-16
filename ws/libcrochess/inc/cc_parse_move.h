// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_MOVE_H__
#define __CC_PARSE_MOVE_H__

#include "cc_defines.h"

// #include "cc_step.h"
// #include "cc_ply.h"
#include "cc_move.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static CcMaybeBoolEnum _cc_parse_standalone_status( char const * move_an,
//                                                     CcMove ** move__io,
//                                                     CcParseMsg ** parse_msgs__iod );

// static CcMaybeBoolEnum _cc_parse_move_status( char const * move_an,
//                                               CcMove ** move__io,
//                                               CcParseMsg ** parse_msgs__iod );


bool cc_parse_move( char const * move_an,
                    CcGame * game,
                    CcMove ** move__o,
                    CcParseMsg ** parse_msgs__iod );


#endif /* __CC_PARSE_MOVE_H__ */
