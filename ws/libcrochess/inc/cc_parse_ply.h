// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_PLY_H__
#define __CC_PARSE_PLY_H__

// #include "cc_step.h"
#include "cc_ply.h"
#include "cc_game.h"

#include "cc_parse_msg.h"


// static bool _cc_fail_with_msg_invalid_ply_link( char const * ply_start_an,
//                                                 char const * ply_end_an,
//                                                 CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_invalid_first_ply_link( CcPlyLinkTypeEnum plte,
//                                                       char const * ply_start_an,
//                                                       char const * ply_end_an,
//                                                       CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_invalid_piece_symbol( char piece_symbol,
//                                                     char const * ply_start_an,
//                                                     char const * ply_end_an,
//                                                     CcParseMsg ** parse_msgs__iod );

// static bool _cc_check_king_ply( CcChessboard * cb,
//                                 CcPieceType king,
//                                 CcPos * pos__o,
//                                 CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_unexpected_piece_type( CcPieceType piece,
//                                                      char piece_symbol,
//                                                      char const * ply_start_an,
//                                                      char const * ply_end_an,
//                                                      CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_unexpected_piece( CcPos pos,
//                                                 char piece_symbol,
//                                                 CcPieceType piece,
//                                                 char const * ply_start_an,
//                                                 char const * ply_end_an,
//                                                 CcParseMsg ** parse_msgs__iod );

// static bool _cc_check_piece_can_be_activated( CcPieceType piece,
//                                               char const * ply_start_an,
//                                               char const * ply_end_an,
//                                               CcParseMsg ** parse_msgs__iod );

// static bool _cc_fail_with_msg_piece_cannot_lose_tag( CcPieceType piece,
//                                                      CcLosingTagType ltt,
//                                                      char const * ply_start_an,
//                                                      char const * ply_end_an,
//                                                      CcParseMsg ** parse_msgs__iod );

// static bool _cc_parse_ply( char const * ply_start_an,
//                            char const * ply_end_an,
//                            CcGame * game,
//                            CcPosDesc * before_ply__io,
//                            bool is_first_ply,
//                            CcPly ** ply__o,
//                            CcChessboard ** cb__io,
//                            CcParseMsg ** parse_msgs__iod );


// DOCS
bool cc_parse_plies( char const * move_an,
                     CcGame * game,
                     CcPly ** plies__o,
                     CcParseMsg ** parse_msgs__iod );


#endif /* __CC_PARSE_PLY_H__ */
