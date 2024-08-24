// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"


// TODO :: Wave, transparency comparison, ... or ...
// TODO :: maybe add function just for checking castling (?)
// TODO :: see cc_check_piece_is_blocked_at()
CcMaybeBoolEnum cc_check_step_fields_are_empty( CcChessboard * cb,
                                                CcPos pos,
                                                CcPos step,
                                                cc_uint_t limit__d,
                                                bool check_pos ) {
    if ( !cb ) return CC_MBE_Void;

    CcPos current = pos;
    cc_uint_t count = 0;

    do {
        if ( !cc_chessboard_is_pos_on_board( cb, current.i, current.j ) )
            break;

        if ( ( count > 0 ) || check_pos ) {
            // TODO :: Wave, transparency comparison, ...

            CcPieceType pe = cc_chessboard_get_piece( cb, current.i, current.j );
            if ( !CC_PIECE_IS_NONE( pe ) ) return CC_MBE_False;
        }

        if ( ( limit__d != CC_CHECK_STEPS_NO_LIMIT ) && ( count >= limit__d ) )
            break;

        current = cc_pos_add( current, step, 1 );

        ++count;
    } while ( true );

    return ( count > 0 ) ? CC_MBE_True
                         : CC_MBE_Void;
}

bool cc_check_momentum_for_movement( CcPieceType piece, cc_uint_t momentum ) {
    if ( CC_PIECE_IS_WEIGHTLESS( piece ) ) {
        return true;
    } else {
        return ( momentum > 0 );
    }
}

bool cc_check_losing_tag_for_piece( CcPieceType piece, CcLosingTagEnum lte ) {
    if ( lte == CC_LTE_NoneLost ) {
        return true;
    } else if ( CC_PIECE_IS_PAWN( piece ) ) {
        return ( ( lte == CC_LTE_RushingTagLost ) || ( lte == CC_LTE_DelayedPromotionLost ) );
    } else if ( CC_PIECE_IS_ROOK( piece ) || CC_PIECE_IS_KING( piece ) ) {
        return ( lte == CC_LTE_CastlingTagLost );
    } else
        return false;
}

CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb,
                                              CcPieceType piece,
                                              cc_uint_t momentum,
                                              CcPos pos ) {
    if ( CC_PIECE_IS_NONE( piece ) ) return CC_MBE_Void;
    if ( !cb ) return CC_MBE_Void;

    CcPieceType pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_NONE( pe ) ) return CC_MBE_False;

    if ( CC_PIECE_IS_WAVE( piece ) ) {
        return CC_BOOL_TO_MAYBE( CC_PIECE_IS_OPAQUE( pe ) );
    }

    if ( CC_PIECE_IS_OPAQUE( pe ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( piece ) )
            return CC_MBE_True;

    if ( CC_PIECE_IS_OPAQUE( piece ) )
        if ( !CC_PIECE_IS_COMPLETELY_TRANSPARENT( pe ) )
            return CC_MBE_True;

    if ( CC_PIECE_IS_SEMI_OPAQUE( piece ) )
        if ( CC_PIECE_IS_SEMI_OPAQUE( pe ) )
            return CC_MBE_True;

    return CC_BOOL_TO_MAYBE( cc_check_momentum_for_movement( piece, momentum ) );
}

CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               cc_uint_t momentum,
                                               CcPos pos ) {
    if ( CC_PIECE_IS_NONE( piece ) ) return CC_MBE_Void;

    if ( !CC_PIECE_CAN_CAPTURE( piece ) ) return CC_MBE_False; // This weeds out pieces without owner.

    if ( !cc_check_momentum_for_movement( piece, momentum ) ) return CC_MBE_False;

    if ( !cb ) return CC_MBE_Void;

    CcPieceType pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( !CC_PIECE_CAN_BE_CAPTURED( pe ) ) return CC_MBE_False; // Also weeds out other pieces without owner.

    return CC_BOOL_TO_MAYBE( cc_piece_has_different_owner( piece, pe ) );
}

CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb,
                                               CcPieceType piece,
                                               cc_uint_t momentum,
                                               CcPieceType activator,
                                               CcPos pos ) {
    if ( CC_PIECE_IS_NONE( piece ) ) return CC_MBE_Void;

    if ( !cc_check_momentum_for_movement( piece, momentum ) ) return CC_MBE_False;

    if ( CC_PIECE_IS_WAVE( piece ) ) {
        // [i] Not needed, checked within CC_WAVE_CAN_BE_DIVERGED() below.
        // if ( !CC_PIECE_IS_ACTIVATOR( activator ) ) return CC_MBE_False;

        if ( !CC_WAVE_CAN_BE_DIVERGED( activator ) ) return CC_MBE_False;
    } else {
        if ( !CC_PIECE_CAN_BE_DIVERGED( piece ) ) return CC_MBE_False;
    }

    if ( !cb ) return CC_MBE_Void;

    CcPieceType pe = cc_chessboard_get_piece( cb, pos.i, pos.j );
    if ( CC_PIECE_IS_STARCHILD( pe ) ) return CC_MBE_True;

    if ( CC_PIECE_IS_SHAMAN( pe ) ) {
        if ( CC_PIECE_IS_SHAMAN( piece ) )
            return CC_MBE_True;
        else
            return CC_BOOL_TO_MAYBE( cc_piece_has_same_owner( piece, pe ) );
    } else
        return CC_MBE_False;
}
