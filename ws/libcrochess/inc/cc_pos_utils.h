// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_UTILS_H__
#define __CC_POS_UTILS_H__

#include "cc_defines.h"
#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

/**
    @file cc_pos_utils.h
    @brief Pos utils.
*/


/**
    Function converts position to position descriptor (i.e. the one
    containing piece, and tag at that location on a chessboard).

    @param cb A chessboard.
    @param pos A position.
    @param momentum Momentum.

    @note
    If chessboard is not given, piece and tag members are not updated,
    returned value still contains a given position.

    @return Position descriptor.
*/
CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos, uint momentum );

bool cc_calc_checked_momentum( uint * momentum__io, bool accumulating );

CcPosDescLink * cc_convert_steps_to_positions__new( CcChessboard * cb,
                                                    CcPos current_pos,
                                                    uint current_momentum,
                                                    bool is_accumulating_momentum,
                                                    CcTypedStepLink * steps );


bool cc_append_checked_pos_to_pos_desc_link( CcChessboard * cb,
                                             CcPos destination,
                                             uint momentum,
                                             CcPosDescLink ** pptl__iod_a );

bool cc_validate_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link );

bool cc_update_pos_desc_link( CcChessboard * cb, CcPosDescLink * pd_link__io );

bool cc_apply_pos_desc_link( CcChessboard ** cb__io_r, CcPosDescLink * pd_link );


// DOCS
bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * pos__io );


#endif /* __CC_POS_UTILS_H__ */
