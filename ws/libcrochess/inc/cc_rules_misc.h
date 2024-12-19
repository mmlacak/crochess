// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_MISC_H__
#define __CC_RULES_MISC_H__

#include <stdbool.h>

#include "cc_tag.h"
#include "cc_move.h"
#include "cc_game.h"


// DOCS
bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse );


#endif /* __CC_RULES_MISC_H__ */
