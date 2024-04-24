// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_MISC_H__
#define __CC_RULES_MISC_H__

#include <stdbool.h>

#include "cc_tag.h"
#include "cc_move.h"
#include "cc_game.h"

// #include "cc_parse_defs.h"


// static int cc_an_str_ends_with_draw_offer( char const * an_start,
//                                            char const * an_end__d,
//                                            size_t max_len__d );


bool cc_check_valid_draw_offer_exists( CcMove * moves,
                                       CcGameStatusEnum gse );

bool cc_check_promote_or_tag( CcChessboard * cb,
                              CcPieceEnum pawn,
                              CcPos start,
                              CcPos destination );

bool cc_delete_all_en_passant_tags( CcChessboard * cb );


#endif /* __CC_RULES_MISC_H__ */
