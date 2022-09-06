// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_H__
#define __CC_RULES_H__

#include <stdbool.h>

#include "cc_game.h"
#include "cc_steps.h"
#include "cc_parse_msgs.h"


bool cc_append_steps( char const * restrict ply_start_str,
                      char const * restrict ply_end_str,
                      CcChessboard * restrict cb,
                      CcSteps ** restrict steps__io,
                      CcParseMsgs ** restrict parse_msgs__io );

bool cc_do_make_plies( char const * restrict move_an_str,
                       CcGame * restrict game__io,
                       CcParseMsgs ** restrict parse_msgs__io );

// /**
//     Applies a move (algebraic notation string) to a chessboard.

//     @param move_an_str A complete move, algebraic notation string.
//     @param cb__io An _input/output_ parameter, chessboard to be altered.

//     @return `true` if successful, `false` otherwise.
// */
bool cc_do_make_move( char const * restrict move_an_str,
                      CcGame * restrict game__io,
                      CcParseMsgs ** restrict parse_msgs__io );


#endif /* __CC_RULES_H__ */
