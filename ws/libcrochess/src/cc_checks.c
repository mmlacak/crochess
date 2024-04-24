// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"


bool cc_check_step_fields_are_empty( CcChessboard * cb, CcPos pos, CcPos step, int limit ) {
    if ( !cb ) return false;
    if ( limit <= 0 ) return false;

    CcPos current = pos;

    for ( int count = 0;
          ( count < limit ) && cc_chessboard_is_pos_on_board( cb, current.i, current.j );
          ++count ) {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, current.i, current.j );
        if ( !CC_PIECE_IS_NONE( pe ) ) return false;

        current = cc_pos_add( current, step, 1 );
    }

    return true;
}
