// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_SIDE_EFFECT_H__
#define __CC_PARSE_SIDE_EFFECT_H__

#include "cc_piece.h"
#include "cc_side_effect.h"
#include "cc_game.h"

#include "cc_parse_msg.h"

// static bool cc_check_piece_has_congruent_type( char piece_symbol,
//                                                CcPieceEnum piece,
//                                                char const * restrict step_start_an,
//                                                char const * restrict step_end_an,
//                                                CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_piece_can_be_captured( CcPieceEnum piece,
//                                             char const * restrict step_start_an,
//                                             char const * restrict step_end_an,
//                                             CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_piece_symbol_is_valid( char piece_symbol,
//                                             char const * restrict step_start_an,
//                                             char const * restrict step_end_an,
//                                             CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_promote_to_piece_is_valid( CcPieceEnum promote_to,
//                                                 char const * restrict step_start_an,
//                                                 char const * restrict step_end_an,
//                                                 CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_piece_can_be_displaced( CcPieceEnum piece,
//                                              char const * restrict step_start_an,
//                                              char const * restrict step_end_an,
//                                              CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_parse_and_check_position( char const * pos_an,
//                                          CcPos pos,
//                                          char const * pos_end_an,
//                                          char const * restrict step_start_an,
//                                          char const * restrict step_end_an,
//                                          CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_position_is_on_board( CcPos pos,
//                                            CcChessboard * restrict cb,
//                                            char const * msg_fmt,
//                                            char const * restrict step_start_an,
//                                            char const * restrict step_end_an,
//                                            CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_promoting_piece_is_pawn( CcPieceEnum piece,
//                                               char const * restrict msg_fmt,
//                                               char const * restrict step_start_an,
//                                               char const * restrict step_end_an,
//                                               CcParseMsg ** restrict parse_msgs__iod ;

// static bool cc_check_piece_can_be_converted( CcPieceEnum piece,
//                                              char const * restrict step_start_an,
//                                              char const * restrict step_end_an,
//                                              CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_failed_conversion( CcPieceEnum piece,
//                                         char const * restrict step_start_an,
//                                         char const * restrict step_end_an,
//                                         CcParseMsg ** restrict parse_msgs__iod );

// static bool cc_check_field_is_empty( CcPieceEnum piece,
//                                      char const * restrict step_start_an,
//                                      char const * restrict step_end_an,
//                                      CcParseMsg ** restrict parse_msgs__iod );


bool cc_parse_side_effect( char const * restrict side_effect_an,
                           char const * restrict step_start_an,
                           char const * restrict step_end_an,
                           CcGame * restrict game,
                           CcPosPieceTag before_ply_start,
                           CcChessboard * restrict cb,
                           CcStepLinkEnum sle,
                           CcPos step_pos,
                           CcSideEffect * restrict side_effect__o,
                           CcParseMsg ** restrict parse_msgs__iod );


#endif /* __CC_PARSE_SIDE_EFFECT_H__ */
