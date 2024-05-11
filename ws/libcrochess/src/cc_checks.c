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

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              int i,
                                              int j,
                                              CcPieceEnum piece ) {
    if ( !cb ) return CC_MBE_Void;

    return CC_MBE_Void; // TODO
}

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               int i,
                                               int j,
                                               CcPieceEnum piece ) {
    if ( !CC_PIECE_CAN_CAPTURE( piece ) ) return CC_MBE_False; // <i> This weeds out pieces without owner.

    if ( !cb ) return CC_MBE_Void;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, i, j );
    if ( !CC_PIECE_CAN_BE_CAPTURED( pe ) ) return CC_MBE_False; // <i> Also weeds out other pieces without owner.

    return CC_BOOL_TO_MAYBE( cc_piece_has_different_owner( piece, pe ) );
}
