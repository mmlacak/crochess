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
                                              CcPieceEnum piece,
                                              CcPos pos ) {
    if ( CC_PIECE_IS_NONE( piece ) ) return CC_MBE_Void;
    if ( !cb ) return CC_MBE_Void;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_NONE( pe ) ) return CC_MBE_False;

    if ( CC_PIECE_IS_OPAQUE( pe ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( piece ) )
            return CC_MBE_True;

    if ( CC_PIECE_IS_OPAQUE( piece ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( pe ) )
            return CC_MBE_True;

    if ( CC_PIECE_IS_SEMI_OPAQUE( piece ) )
        if ( CC_PIECE_IS_SEMI_OPAQUE( pe ) )
            return CC_MBE_True;

    return CC_MBE_False;
}

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceEnum piece,
                                               CcPos pos ) {
    if ( !CC_PIECE_CAN_CAPTURE( piece ) ) return CC_MBE_False; // <i> This weeds out pieces without owner.

    if ( !cb ) return CC_MBE_Void;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( !CC_PIECE_CAN_BE_CAPTURED( pe ) ) return CC_MBE_False; // <i> Also weeds out other pieces without owner.

    return CC_BOOL_TO_MAYBE( cc_piece_has_different_owner( piece, pe ) );
}

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceEnum piece,
                                               CcPos pos ) {
    if ( !CC_PIECE_CAN_BE_DIVERGED( piece ) ) return CC_MBE_False;

    // TODO :: add activator, momentum

    if ( !cb ) return CC_MBE_Void;

    CcPieceEnum pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_STARCHILD( pe ) ) return CC_MBE_True;

    // TODO :: handle activator --> Wave

    if ( CC_PIECE_IS_SHAMAN( pe ) )
        return CC_BOOL_TO_MAYBE( cc_piece_has_same_owner( piece, pe ) );
    else
        return CC_MBE_False;
}
