// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_H__
#define __CC_RULES_H__

#include <stdbool.h>

#include "cc_game.h"
#include "cc_parse_msg.h"


// DOCS
bool cc_rules_do_moves( CcGame ** restrict game_io__r,
                        CcMove ** restrict moves__n,
                        CcDoMoveEnum dme );


// DOCS
bool cc_rules_make_move( CcGame ** restrict game_io__r,
                         char const * restrict move_str,
                         CcParseMsg ** restrict parse_msgs__io );


#endif /* __CC_RULES_H__ */
