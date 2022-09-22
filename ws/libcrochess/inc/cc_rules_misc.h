// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_MISC_H__
#define __CC_RULES_MISC_H__

#include <stdbool.h>

#include "cc_tag.h"
#include "cc_moves.h"
#include "cc_game.h"

#include "cc_parse_defs.h"


typedef enum CcLosingTagCheckResultEnum
{
    CC_LTCRE_TagNotFound = -1,
    CC_LTCRE_NoTag = 0,
    CC_LTCRE_TagLost = 1,
} CcLosingTagCheckResultEnum;


bool cc_check_valid_draw_offer_exists( CcMoves * restrict moves,
                                       CcGameStatusEnum gse );

CcLosingTagCheckResultEnum cc_check_losing_tag( CcLosingTagEnum lte,
                                                CcTagEnum te );

bool cc_delete_en_passant_tag( CcChessboard * restrict cb );


#endif /* __CC_RULES_MISC_H__ */
