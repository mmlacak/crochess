// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_RULES_MISC_H__
#define __CC_RULES_MISC_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_chessboard.h"

/**
    @file cc_rules_misc.h
    @brief Miscellaneous rules checkers.
*/


/**
    Function finds Pawn captured in en passant move.

    @param cb A chessboard.
    @param pawn_en_passant Pawn which made en passant move.
    @param step_i File where en passant Pawn moved.
    @param step_j Rank where en passant Pawn moved.
    @param pawn_o _Output_, Pawn which was captured.
    @param dist_j_o _Output_, rank where was Pawn captured.

    @note
    Position of captured Pawn is (`step_i`, `dist_j_o`).

    @return
    `true` is successful, `false` otherwise.
*/
bool cc_rule_utils_find_en_passant_target( CcChessboard * restrict cb,
                                           CcPieceEnum pawn_en_passant,
                                           int step_i,
                                           int step_j,
                                           CcPieceEnum * restrict pawn_o,
                                           int * restrict dist_j_o );

/**
    Function finds Rook which castled.

    @param cb A chessboard.
    @param king_castling King castling.
    @param step_i_K File where castling King moved.
    @param step_j_K Rank where castling King moved.
    @param dest_i_R_io _Input/output_ parameter, file where castling Rook ended.
    @param rook_o _Output_, Rook which castled.
    @param start_i_R_o _Output_, file from where Rook castled.

    @note
    Starting position of castling Rook is (`start_i_R_o`, `step_j_K`).

    @note
    End position of castling Rook is (`dest_i_R_io`, `step_j_K`).

    @return
    `true` is successful, `false` otherwise.
*/
bool cc_rule_utils_find_castling_rook( CcChessboard * restrict cb,
                                       CcPieceEnum king_castling,
                                       int step_i_K,
                                       int step_j_K,
                                       int * restrict dest_i_R_io,
                                       CcPieceEnum * restrict rook_o,
                                       int * restrict start_i_R_o );


#endif /* __CC_RULES_MISC_H__ */
