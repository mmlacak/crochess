// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.

#ifndef __CC_RULES_H__
#define __CC_RULES_H__

#include <stdbool.h>

#include "cc_game.h"


// DOCS
bool cc_rules_do_moves( CcGame ** restrict game_io__r,
                        CcMove ** const restrict moves__n,
                        CcDoMoveEnum dme );


// DOCS
bool cc_rules_make_move( CcGame ** restrict game_io__r,
                         char const * const restrict move_an );


#endif /* __CC_RULES_H__ */
