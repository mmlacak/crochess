// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_SIDE_EFFECT_H__
#define __CC_PARSE_SIDE_EFFECT_H__

#include "cc_piece.h"
#include "cc_parsed_side_effect.h"
#include "cc_game.h"

#include "cc_parse_msg.h"

// static bool cc_check_piece_has_congruent_type( char piece_symbol,
//                                                CcPieceType piece,
//                                                char const * step_start_an,
//                                                char const * step_end_an,
//                                                CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_can_be_captured( CcPieceType piece,
//                                             char const * step_start_an,
//                                             char const * step_end_an,
//                                             CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_symbol_is_valid( char piece_symbol,
//                                             char const * step_start_an,
//                                             char const * step_end_an,
//                                             CcParseMsg ** parse_msgs__iod );

// static bool cc_check_promote_to_piece_is_valid( CcPieceType promote_to,
//                                                 char const * step_start_an,
//                                                 char const * step_end_an,
//                                                 CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_can_be_displaced( CcPieceType piece,
//                                              char const * step_start_an,
//                                              char const * step_end_an,
//                                              CcParseMsg ** parse_msgs__iod );

// static bool cc_parse_and_check_position( char const * pos_an,
//                                          CcPos pos,
//                                          char const * pos_end_an,
//                                          char const * msg_fmt,
//                                          char const * step_start_an,
//                                          char const * step_end_an,
//                                          CcParseMsg ** parse_msgs__iod );

// static bool cc_check_position_is_on_board( CcPos pos,
//                                            CcChessboard * cb,
//                                            char const * msg_fmt,
//                                            char const * step_start_an,
//                                            char const * step_end_an,
//                                            CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_can_capture_en_passant( CcPieceType piece,
//                                                    char const * step_start_an,
//                                                    char const * step_end_an,
//                                                    CcParseMsg ** parse_msgs__iod );

// static bool cc_check_field_is_empty( CcPieceType piece,
//                                      char const * msg_fmt,
//                                      char const * step_start_an,
//                                      char const * step_end_an,
//                                      CcParseMsg ** parse_msgs__iod );

// static bool cc_check_promoting_piece_is_pawn( CcPieceType piece,
//                                               char const * msg_fmt,
//                                               char const * step_start_an,
//                                               char const * step_end_an,
//                                               CcParseMsg ** parse_msgs__iod ;

// static bool cc_check_piece_can_be_converted( CcPieceType piece,
//                                              char const * step_start_an,
//                                              char const * step_end_an,
//                                              CcParseMsg ** parse_msgs__iod );

// static bool cc_check_failed_conversion( CcPieceType piece,
//                                         char const * step_start_an,
//                                         char const * step_end_an,
//                                         CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_can_be_resurrected( CcPieceType piece,
//                                                char const * step_start_an,
//                                                char const * step_end_an,
//                                                CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_is_castling_king( CcPosDesc before_ply_start,
//                                              char const * step_start_an,
//                                              char const * step_end_an,
//                                              CcParseMsg ** parse_msgs__iod );

// static bool cc_check_piece_is_rook_to_castle( CcPieceType piece,
//                                               char const * step_start_an,
//                                               char const * step_end_an,
//                                               CcParseMsg ** parse_msgs__iod );

// static bool cc_check_king_and_rook_can_castle( CcPosDesc before_ply_start,
//                                                CcChessboard * cb,
//                                                CcPos * step_pos__io,
//                                                CcPos * rook_dest__io,
//                                                CcPieceType * rook__o,
//                                                CcPos * rook_init__o,
//                                                char const * step_start_an,
//                                                char const * step_end_an,
//                                                CcParseMsg ** parse_msgs__iod );


// DOCS
bool cc_parse_side_effect( char const * side_effect_an,
                           char const * step_start_an,
                           char const * step_end_an,
                           CcGame * game,
                           CcPosDesc before_ply_start,
                           CcChessboard * cb,
                           CcParsedStepLinkEnum sle,
                           CcPos * step_pos__io,
                           CcParsedSideEffect * side_effect__o,
                           CcParseMsg ** parse_msgs__iod );


#endif /* __CC_PARSE_SIDE_EFFECT_H__ */
