// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_PLY_H__
#define __CC_PARSE_PLY_H__

// #include "cc_step.h"
#include "cc_ply.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static void cc_add_msg_invalid_ply_link( char const * restrict ply_start_an,
//                                          char const * restrict ply_end_an,
//                                          CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_king_ply( CcChessboard * restrict cb,
//                                CcPieceEnum king,
//                                CcPos * restrict pos__o,
//                                CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_piece_can_be_activated( CcPieceEnum piece,
//                                              char const * restrict ply_start_an,
//                                              char const * restrict ply_end_an,
//                                              CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_parse_ply( char const * restrict ply_start_an,
//                           char const * restrict ply_end_an,
//                           CcGame * restrict game,
//                           CcPosPieceTag * restrict before_ply_start__io,
//                           bool is_first_ply,
//                           CcPly ** restrict ply__o,
//                           CcChessboard ** restrict cb__io,
//                           CcParseMsg ** restrict parse_msgs__iod );


bool cc_parse_plies( char const * restrict move_an,
                     CcGame * restrict game,
                     CcPly ** restrict plies__o,
                     CcParseMsg ** restrict parse_msgs__iod );


#endif /* __CC_PARSE_PLY_H__ */
