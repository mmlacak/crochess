// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_RULE_UTILS_H__
#define __CC_RULE_UTILS_H__

#include <stdbool.h>

#include "cc_piece.h"
#include "cc_chessboard.h"


// *** DOCS ***
bool cc_rule_utils_find_en_passant_target( CcChessboard const * const restrict cb,
                                           CcPieceEnum pawn_en_passant,
                                           int const step_i,
                                           int const step_j,
                                           CcPieceEnum * const restrict pawn_o,
                                           int * const restrict dist_j_o );


#endif /* __CC_RULE_UTILS_H__ */
