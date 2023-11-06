// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PATH_GENS_H__
#define __CC_PATH_GENS_H__

#include "cc_piece.h"
#include "cc_chessboard.h"
#include "cc_pos.h"

/**
    @file cc_path_gens.h
    @brief Path generators.
*/


// DOCS
bool cc_iter_piece_pos( CcChessboard * restrict cb_before_activation,
                        CcPos expected,
                        CcPieceEnum piece,
                        bool include_opponent,
                        CcPos * restrict pos__io );


bool cc_is_step_capture( CcPieceEnum activator,
                         CcPieceEnum piece,
                         CcPos step,
                         CcPos step_2 );

bool cc_is_step_miracle( CcPieceEnum piece, CcPos step );

bool cc_is_step_shamans_capture( CcPieceEnum piece, CcPos step );

bool cc_is_the_same_color( CcPieceEnum piece, CcPos pos );


#endif /* __CC_PATH_GENS_H__ */
