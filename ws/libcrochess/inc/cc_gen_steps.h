// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_GEN_STEPS_H__
#define __CC_GEN_STEPS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

/**
    @file cc_gen_steps.h
    @brief Step generators.
*/


// DOCS
bool cc_piece_pos_iter( CcChessboard * restrict cb_before_activation,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io );


bool cc_check_path_args( CcChessboard * restrict cb_before_activation,
                         CcPos start,
                         CcPos destination );

bool cc_is_step_capture( CcPieceEnum activator,
                         CcPieceEnum piece,
                         CcPos step,
                         CcPos step_2 );

bool cc_is_step_miracle( CcPieceEnum piece, CcPos step );

bool cc_is_ply_valid( CcChessboard * restrict cb_before_activation,
                      CcPieceEnum activator,
                      CcPos start,
                      CcPos destination,
                      CcPos step,
                      CcPos step_2 );

CcPosLink * cc_link_positions( CcChessboard * restrict cb_before_activation,
                               CcPos start,
                               CcPos destination,
                               CcPos step,
                               CcPos step_2 );

bool cc_is_activation_valid( CcChessboard * restrict cb_before_activation,
                             CcPieceEnum activator,
                             CcPos start,
                             CcPos destination,
                             CcPieceEnum expected_type );

bool cc_is_the_same_color( CcPieceEnum piece, CcPos pos );


CcPosLink * cc_path_knight__new( CcChessboard * restrict cb_before_activation,
                                 CcPieceEnum activator,
                                 CcPos start,
                                 CcPos destination );

CcPosLink * cc_path_bishop__new( CcChessboard * restrict cb_before_activation,
                                 CcPieceEnum activator,
                                 CcPos start,
                                 CcPos destination );

CcPosLink * cc_path_rook__new( CcChessboard * restrict cb_before_activation,
                               CcPieceEnum activator,
                               CcPos start,
                               CcPos destination );

CcPosLink * cc_path_queen__new( CcChessboard * restrict cb_before_activation,
                                CcPieceEnum activator,
                                CcPos start,
                                CcPos destination );

CcPosLink * cc_path_king__new( CcChessboard * restrict cb_before_activation,
                               CcPieceEnum activator,
                               CcPos start,
                               CcPos destination );

CcPosLink * cc_path_pegasus__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum activator,
                                  CcPos start,
                                  CcPos destination );

CcPosLink * cc_path_unicorn__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum activator,
                                  CcPos start,
                                  CcPos destination );

CcPosLink * cc_path_star__new( CcChessboard * restrict cb_before_activation,
                               CcPieceEnum activator,
                               CcPos start,
                               CcPos destination );

CcPosLink * cc_path_starchild__new( CcChessboard * restrict cb_before_activation,
                                    CcPieceEnum activator,
                                    CcPos start,
                                    CcPos destination );


CcPosLink * cc_shortest_path__new( CcChessboard * restrict cb_before_activation,
                                   CcPieceEnum activator,
                                   CcPos start,
                                   CcPos destination );

CcPosLink * cc_longest_path__new( CcChessboard * restrict cb_before_activation,
                                  CcPieceEnum activator,
                                  CcPos start,
                                  CcPos destination );


#endif /* __CC_GEN_STEPS_H__ */
