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
CcPosPieceTag cc_convert_pos_to_ppt( CcChessboard * restrict cb,
                                     CcPos pos );

/**
    Function returns a new linked list of positions + pieces + tags.

    @param cb A chessboard.
    @param pos_link A linked list of positions.

    @return Newly allocated linked list if successful, `NULL` otherwise.
*/
CcPptLink * cc_convert_pos_link_to_ppt_link__new( CcChessboard * restrict cb,
                                                  CcPosLink * restrict pos_link );

bool cc_validate_ppt_link( CcChessboard * restrict cb,
                           CcPptLink * restrict ppt_link );

bool cc_update_ppt_link( CcChessboard * restrict cb,
                         CcPptLink * restrict ppt_link__io );

CcPptLink * cc_join_ppt_links( CcPptLink ** restrict ppt_link__iod,
                               CcPptLink ** restrict ppt_link__n );

// DOCS
bool cc_iter_piece_pos( CcChessboard * restrict cb,
                        CcPos expected,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io );



#endif /* __CC_POS_UTILS_H__ */
