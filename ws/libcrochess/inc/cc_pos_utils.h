// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_UTILS_H__
#define __CC_POS_UTILS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

/**
    @file cc_pos_utils.h
    @brief Pos utils.
*/


/**
    Function converts position to one containing piece,
    and tag at that location on a chessboard.

    @param cb A chessboard.
    @param pos A position.

    @note
    If chessboard is not given, piece and tag members are not updated,
    returned value still contains a given position.

    @return Position containing piece, and tag.
*/
CcPosPieceTag cc_convert_pos_to_ppt( CcChessboard * cb,
                                     CcPos pos );

CcPptLink * cc_convert_steps_to_positions__new( CcChessboard * cb,
                                                CcPos current_pos,
                                                CcTypedStepLink * steps );


bool cc_validate_ppt_link( CcChessboard * cb,
                           CcPptLink * ppt_link );

bool cc_update_ppt_link( CcChessboard * cb,
                         CcPptLink * ppt_link__io );

bool cc_apply_ppt_link( CcChessboard ** cb__io_a,
                        CcPptLink * ppt_link );


// DOCS
bool cc_iter_piece_pos( CcChessboard * cb,
                        CcPos expected,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * pos__io );


#endif /* __CC_POS_UTILS_H__ */
