// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_MISC_H__
#define __CC_RULES_MISC_H__

#include <stdbool.h>

#include "cc_tag.h"
#include "cc_move.h"
#include "cc_game.h"

#include "cc_parse_defs.h"


// static int cc_an_str_ends_with_draw_offer( char const * restrict an_start,
//                                            char const * restrict an_end__d,
//                                            size_t max_len__d )


bool cc_check_valid_draw_offer_exists( CcMove * restrict moves,
                                       CcGameStatusEnum gse );

int cc_promoting_rank( CcChessboard * restrict cb, bool is_light );

// TODO :: DELETE
// bool cc_check_tag_is_lost( CcTagEnum lost, CcTagEnum tag );

bool cc_check_promote_or_tag( CcChessboard * restrict cb,
                              CcPieceEnum piece,
                              CcPos start,
                              CcPos destination );

bool cc_delete_en_passant_tag( CcChessboard * restrict cb );


#endif /* __CC_RULES_MISC_H__ */
