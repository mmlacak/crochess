// Copyright (c) 2023 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_checks.h"


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

    bool have_mm = cc_check_momentum_for_movement( piece, momentum );
    if ( !have_mm ) return CC_MBE_True;

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

    return CC_MBE_False;
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

CcMaybeBoolEnum cc_check_castling_step_fields( CcChessboard * cb,
                                               CcPos king_start,
                                               CcPos king_dest,
                                               CcPos rook_start,
                                               CcPos rook_dest ) {
    if ( !cb ) return CC_MBE_Void;

    if ( king_start.j != rook_start.j ) return CC_MBE_False;

    CcPieceType king = cc_chessboard_get_piece( cb, king_start.i, king_start.j );
    if ( !CC_PIECE_IS_KING( king ) ) return CC_MBE_False;

    CcPieceType rook = cc_chessboard_get_piece( cb, rook_start.i, rook_start.j );
    if ( !CC_PIECE_IS_ROOK( rook ) ) return CC_MBE_False;

    if ( !cc_piece_has_same_color( king, rook ) ) return CC_MBE_False;

    CcTagType king_tag = cc_chessboard_get_tag( cb, king_start.i, king_start.j );
    if ( !CC_TAG_CAN_CASTLE( king_tag ) ) return CC_MBE_False;

    CcTagType rook_tag = cc_chessboard_get_tag( cb, rook_start.i, rook_start.j );
    if ( !CC_TAG_CAN_CASTLE( rook_tag ) ) return CC_MBE_False;

    CcPieceType empty_for_king = cc_chessboard_get_piece( cb, king_dest.i, king_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_king ) ) return CC_MBE_False;

    CcPieceType empty_for_rook = cc_chessboard_get_piece( cb, rook_dest.i, rook_dest.j );
    if ( !CC_PIECE_IS_NONE( empty_for_rook ) ) return CC_MBE_False;

    bool is_queen_side = ( king_start.i > rook_start.i );
    CcPos king_step = is_queen_side ? CC_POS_CAST( -1, 0 ) : CC_POS_CAST( 1, 0 );

    CcPos current = cc_pos_add( king_start, king_step, 1 );
    cc_uint_t momentum = 1;

    do {
        // [i] Rook is semi-opaque just like King, so it's enough to check only King
        //     against all fields in-between the two.
        if ( CC_MAYBE_IS_TRUE( cc_check_piece_is_blocked_at( cb, king, momentum, current ) ) )
            return CC_MBE_False;

        current = cc_pos_add( current, king_step, 1 );

        ++momentum;
    } while ( !cc_pos_is_equal( rook_dest, current ) );

    return CC_MBE_True;
}
