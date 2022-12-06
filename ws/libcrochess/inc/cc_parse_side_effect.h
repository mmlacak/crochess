// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_SIDE_EFFECT_H__
#define __CC_PARSE_SIDE_EFFECT_H__

#include "cc_piece.h"
#include "cc_side_effect.h"
#include "cc_game.h"

#include "cc_parse_msg.h"



bool cc_parse_side_effect( char const * restrict an_str,
                           char const * restrict step_start,
                           char const * restrict step_end,
                           CcGame * restrict game,
                           CcChessboard * restrict cb,
                           CcPos step_pos,
                           CcSideEffect * restrict side_effect__o,
                           CcParseMsg ** restrict parse_msgs__iod );


#endif /* __CC_PARSE_SIDE_EFFECT_H__ */
