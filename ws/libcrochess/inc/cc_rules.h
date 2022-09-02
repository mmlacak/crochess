// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_H__
#define __CC_RULES_H__

#include <stdbool.h>

#include "cc_game.h"
#include "cc_parse_msgs.h"


// /**
//     Applies a move (algebraic notation string) to a chessboard.

//     @param move_an_str A complete move, algebraic notation string.
//     @param cb__io An _input/output_ parameter, chessboard to be altered.

//     @return `true` if successful, `false` otherwise.
// */
bool cc_make_move( char const * restrict move_an_str,
                   CcGame * restrict game__io,
                   CcParseMsgs ** restrict parse_msgs__io );



#endif /* __CC_RULES_H__ */
