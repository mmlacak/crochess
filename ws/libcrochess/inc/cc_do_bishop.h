// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DO_BISHOP_H__
#define __CC_DO_BISHOP_H__

#include "cc_game.h"

#include "cc_parse_msg.h"


// TODO :: DOCS


bool cc_do_bishop( char const * path_start_an,
                   char const * ply_end_an,
                   CcGame * game,
                   CcPosDesc * before_ply__io,
                   CcChessboard ** cb__io,
                   CcParseMsg ** parse_msgs__iod );


#endif /* __CC_DO_BISHOP_H__ */
